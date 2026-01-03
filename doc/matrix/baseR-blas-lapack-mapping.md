# Base R (R 4.5) linear algebra → BLAS/LAPACK mapping (real/double)

This document is intended as a **shared reference** for implementing an R→Fortran transpiler that lowers common base-R linear algebra operations to the **same BLAS/LAPACK** backend that R is using.

Scope:
- Focus on **real (double precision)** inputs (`double` in R; `D*` routines in BLAS/LAPACK).
- Complex variants are omitted unless trivial.
- Emphasis on:
  1) A mapping of R functions/ops to BLAS/LAPACK routines  
  2) Which routines **overwrite ("consume") inputs** (important for R copy-on-modify semantics)  
  3) **Return value structures**: R list components ↔ LAPACK output parameters
  4) BLAS/LAPACK naming + common calling patterns to drive helper/wrapper design  
  5) **Workspace query patterns** and which routines require them
  6) Notes on backend differences (OpenBLAS/MKL/Accelerate/Reference)
  7) Quickr-specific constraints (what is implemented today vs planned)

---

## 1) BLAS levels and how they relate to R operations

BLAS is traditionally grouped into "levels" that correlate strongly with performance characteristics:

- **Level 1 (vector–vector):** `ddot`, `daxpy`, `dscal`, `dnrm2`, …  
  - Low arithmetic intensity; often memory-bound.
  - In-place behavior is common: `daxpy` overwrites `y`; `dscal` overwrites `x`.

- **Level 2 (matrix–vector):** `dgemv`, `dtrsv`, `dsymv`, `dger`, …  
  - `dger` is a rank-1 update (outer product update): `A := α*x*y' + A`.
  - Often overwrites one argument (e.g., updates `A`).

- **Level 3 (matrix–matrix):** `dgemm`, `dtrsm`, `dsyrk`, `dsymm`, …  
  - Highest arithmetic intensity; typically best-optimized; the workhorse for `%*%`, `crossprod`, solves with multiple RHS, etc.
  - Often overwrites the output buffer (`C` in `dgemm`, `B` in `dtrsm`).

**Translation tip:** Prefer Level 3 routines whenever possible (R often does). Many seemingly different high-level operations reduce to `dgemm`, `dsyrk`, `dtrsm`.

---

## 2) High-level mapping table (base R functions / ops)

Notes:
- "Typical backend" reflects how base R commonly implements these operations. Some entries have multiple plausible routes; where base R behavior varies by options (e.g. `qr(LAPACK=TRUE)`), the table notes it explicitly.
- "Consumes input?" refers to whether the routine overwrites its input arrays (you must copy if the original is needed later).
- "R return type" indicates whether R returns a simple matrix/vector or a structured list.

| Category | Base R function / op | What it does | Typical BLAS / LAPACK backend | Consumes input? | R return type |
|---|---|---|---|---|---|
| Creating | `matrix()`, `cbind()`, `rbind()` | Build/bind matrices | R internals (copies) | No | matrix |
| Creating / special | `diag(n)` / `diag(v)` / `diag(X)` | Identity / diagonal / extract diag | R internals | No | matrix/vector |
| Creating | `t(X)` | Transpose | Often a copy / stride change; transpose flags used in BLAS combos | No | matrix |
| Creating | `outer(x,y,FUN="*")` | Outer product | **BLAS `dger`** (rank-1 update) for `FUN="*"`; otherwise loops | `dger` overwrites `A` | matrix |
| Creating | `%o%` | Outer product | Typically `outer(x,y,"*")` → `dger`/loops | see above | matrix |
| Creating | `kronecker(X,Y)` | Kronecker product | Usually R/C loops; may call BLAS in blocks in some impls | No | matrix |
| Basic arithmetic | `+`, `-`, `*`, `/`, `^` | Elementwise ops | Intrinsics / loops | No | matrix |
| Multiply | `%*%` | Matrix multiply | **BLAS `dgemm`** (general); possible opt: `dsymm` if symmetry exploited | No (reads A,B; writes C) | matrix |
| Cross products | `crossprod(X,Y)` | `t(X) %*% Y` | **BLAS `dgemm`**; if `Y` missing: **`dsyrk`** | No (writes result) | matrix |
| Cross products | `tcrossprod(X,Y)` | `X %*% t(Y)` | **BLAS `dgemm`**; if `Y` missing: **`dsyrk`** | No (writes result) | matrix |
| Reductions | `rowSums/Means`, `colSums/Means` | Row/col reductions | Intrinsics / loops (worth supporting) | No | vector |
| Determinant | `det(X)` / `determinant(X)` | Determinant (sign+modulus, log) | **LAPACK `dgetrf`** (LU) | Yes (LU overwrites A) | scalar / **list** |
| Linear solve | `solve(A, b)` / `solve(A)` | Solve Ax=b / inverse | **LAPACK `dgesv`**; inverse via `dgetrf` + `dgetri` | Yes (overwrites A, and B) | matrix |
| Triangular solve | `forwardsolve(L,b)` | Solve Lx=b | **BLAS `dtrsm`** (multi-RHS); `dtrsv` for vector RHS | Yes (overwrites RHS) | matrix/vector |
| Triangular solve | `backsolve(U,b)` | Solve Ux=b | **BLAS `dtrsm`** / `dtrsv` | Yes (overwrites RHS) | matrix/vector |
| Cholesky | `chol(A)` | Cholesky (SPD) | **LAPACK `dpotrf`** | Yes (overwrites A) | matrix (+attrs) |
| Cholesky (pivoted) | `chol(A, pivot=TRUE)` | Pivoted Cholesky | **LAPACK `dpstrf`** | Yes | matrix (+attrs) |
| Cholesky inverse | `chol2inv(R)` | Inverse from Cholesky factor | **LAPACK `dpotri`** | Yes (overwrites factor) | matrix |
| Cholesky solve | *(via backsolve)* | Solve from Cholesky factor | **LAPACK `dpotrs`** or **BLAS `dtrsm`** ×2 | Yes (overwrites RHS) | matrix |
| QR decomposition | `qr(X)` | QR factorization | Default **LINPACK `dqrdc2`**; if `LAPACK=TRUE`: **LAPACK `dgeqp3`** | Yes (overwrites A) | **list** (class "qr") |
| QR solve | `qr.solve(X,b)` | Solve/LS via QR | QR (`dgeqp3`/`dgeqrf`) + apply Q (`dormqr`) + triangular solve | Yes | matrix |
| Eigen (symmetric) | `eigen(A, symmetric=TRUE)` | Eigenvalues/vectors | **LAPACK `dsyevr`** (default); or `dsyevd`/`dsyev` | Yes (overwrites A) | **list** (class "eigen") |
| Eigen (general) | `eigen(A, symmetric=FALSE)` | General eigenproblem | **LAPACK `dgeev`** | Yes (overwrites A) | **list** (class "eigen") |
| SVD | `svd(X)` | Singular value decomposition | **LAPACK `dgesdd`** (fast) or `dgesvd` | Yes (destroys A) | **list** |
| Norms | `norm(X, type)` | Matrix norms | **LAPACK `dlange`** for `1/I/F/M`; spectral norm uses SVD | `dlange` No; SVD Yes | scalar |
| Cov/Cor | `cov(X)`, `cor(X)` | Covariance / correlation | Center/scale + `crossprod` (BLAS) + scalar ops | No (but may allocate) | matrix |
| Conditioning | `rcond(A)`, `kappa(A)` | Reciprocal cond / cond number | `dlange` + `dgecon` (with LU) / `dtrcon` | Factorization consumes A | scalar |
| Distances | `dist(X)` | Pairwise distances | R/C loops | No | dist object |
| Triangular masks | `lower.tri(X)`, `upper.tri(X)` | Logical triangle masks | R internals | No | logical matrix |
| Reshape/permute | `dim<-`, `aperm()` | Reshape / permute | R internals | No | array |

---

## 3) R return structures ↔ LAPACK output parameters

R functions return user-friendly list structures, while LAPACK uses in-place overwriting with multiple output parameters. This section details the mapping.
These are a **target interface**; quickr does not yet have list return values or `$` access lowering.
When implemented, prefer name-mangling with explicit properties (e.g., `x__values__`, `x__vectors__`) and a `$` lowering rule that rewrites `x$values` → `x__values__`.

### 3.1 `qr()` → DGEQP3 / DGEQRF / DQRDC2

**R returns** a list of class `"qr"` with components:
| Component | Description |
|-----------|-------------|
| `qr` | Matrix (same dims as input): upper triangle = R; lower triangle = Householder reflector info |
| `rank` | Integer: numerical rank (LINPACK computes this; LAPACK always returns full rank) |
| `pivot` | Integer vector of length `ncol(x)`: column permutation |
| `qraux` | Numeric vector of length `ncol(x)`: scalar factors (τ) for reconstructing Q |

With `LAPACK=TRUE`, the object has attribute `"useLAPACK"` = TRUE.

**LAPACK DGEQP3 signature:**
```fortran
DGEQP3(M, N, A, LDA, JPVT, TAU, WORK, LWORK, INFO)
```
| Parameter | Direction | Maps to R |
|-----------|-----------|-----------|
| `A` | in/out | Input matrix → `qr$qr` (packed R + reflectors) |
| `JPVT` | in/out | `qr$pivot` |
| `TAU` | out | `qr$qraux` |
| `INFO` | out | Error status (0 = success) |

**LAPACK DGEQRF** (unpivoted) omits `JPVT`; R's LINPACK `dqrdc2` uses a modified tolerance-based pivoting strategy.

---

### 3.2 `eigen()` → DSYEVR / DGEEV

**R returns** a list of class `"eigen"` with components:
| Component | Description |
|-----------|-------------|
| `values` | Numeric vector: eigenvalues (sorted by decreasing absolute value) |
| `vectors` | Matrix: columns are unit eigenvectors (NULL if `only.values=TRUE`) |

For symmetric matrices, both are always real. For asymmetric matrices, complex conjugate pairs appear consecutively.

**LAPACK DSYEVR signature** (symmetric, Relatively Robust Representations—fastest):
```fortran
DSYEVR(JOBZ, RANGE, UPLO, N, A, LDA, VL, VU, IL, IU, ABSTOL, 
       M, W, Z, LDZ, ISUPPZ, WORK, LWORK, IWORK, LIWORK, INFO)
```
| Parameter | Direction | Maps to R |
|-----------|-----------|-----------|
| `W` | out | `eigen()$values` (first M elements, ascending order—R reverses) |
| `Z` | out | `eigen()$vectors` (columns) |
| `M` | out | Count of eigenvalues found |
| `INFO` | out | Error status |

**LAPACK DGEEV signature** (general non-symmetric):
```fortran
DGEEV(JOBVL, JOBVR, N, A, LDA, WR, WI, VL, LDVL, VR, LDVR, WORK, LWORK, INFO)
```
| Parameter | Direction | Maps to R |
|-----------|-----------|-----------|
| `WR`, `WI` | out | Real/imaginary parts → combined into `eigen()$values` (complex if needed) |
| `VR` | out | Right eigenvectors → `eigen()$vectors` |
| `INFO` | out | If >0: QR algorithm failed; eigenvalues INFO+1:N are valid |

---

### 3.3 `svd()` → DGESDD / DGESVD

**R returns** a list with components:
| Component | Description |
|-----------|-------------|
| `d` | Numeric vector: singular values in decreasing order, length min(n,p) |
| `u` | Matrix (n × nu): left singular vectors as columns |
| `v` | Matrix (p × nv): right singular vectors as columns |

**LAPACK DGESDD signature** (divide-and-conquer, faster for large matrices):
```fortran
DGESDD(JOBZ, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, IWORK, INFO)
```
| Parameter | Direction | Maps to R |
|-----------|-----------|-----------|
| `S` | out | `svd()$d` (singular values, descending) |
| `U` | out | `svd()$u` |
| `VT` | out | **Transpose** of `svd()$v` (R transposes this back) |
| `IWORK` | workspace | Integer array of size 8×min(M,N)—**mandatory** |
| `INFO` | out | If >0: DBDSDC did not converge |

**LAPACK DGESVD** (QR-based, smaller workspace):
```fortran
DGESVD(JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, INFO)
```

---

### 3.4 `chol()` → DPOTRF / DPSTRF

**R returns** an upper triangular matrix R such that `t(R) %*% R == x`.

With `pivot=TRUE`, R adds attributes:
| Attribute | Description |
|-----------|-------------|
| `"pivot"` | Integer permutation vector |
| `"rank"` | Numerical rank |

Relationship: `t(R) %*% R == x[pivot, pivot]`

**LAPACK DPOTRF signature:**
```fortran
DPOTRF(UPLO, N, A, LDA, INFO)
```
| Parameter | Direction | Notes |
|-----------|-----------|-------|
| `A` | in/out | Input SPD matrix → output factor in upper (UPLO='U') or lower triangle |
| `INFO` | out | If >0: leading minor of order INFO not positive definite |

**LAPACK DPSTRF signature** (pivoted):
```fortran
DPSTRF(UPLO, N, A, LDA, PIV, RANK, TOL, WORK, INFO)
```
| Parameter | Direction | Maps to R |
|-----------|-----------|-----------|
| `PIV` | out | `attr(result, "pivot")` |
| `RANK` | out | `attr(result, "rank")` |
| `WORK` | workspace | Fixed size 2×N—**no query needed** |

---

### 3.5 `determinant()` → DGETRF

**R returns** a list of class `"det"` with components:
| Component | Description |
|-----------|-------------|
| `modulus` | log|det| (with attribute `"logarithm"` = TRUE by default) |
| `sign` | +1 or -1 |

**LAPACK DGETRF signature:**
```fortran
DGETRF(M, N, A, LDA, IPIV, INFO)
```

The determinant is computed as: `sign = (-1)^(number of row swaps)`, `modulus = Σ log|U[i,i]|`

| Parameter | Direction | Notes |
|-----------|-----------|-------|
| `A` | in/out | Stores L (unit lower, below diag) and U (upper, including diag) |
| `IPIV` | out | Pivot indices for sign calculation |
| `INFO` | out | If >0: U(INFO,INFO) = 0, matrix singular |

---

### 3.6 `solve()` → DGESV / DGETRF + DGETRI

**R returns** a single matrix: the solution X to AX=B, or A⁻¹ if b is missing.

**LAPACK DGESV signature** (combined factor + solve):
```fortran
DGESV(N, NRHS, A, LDA, IPIV, B, LDB, INFO)
```
| Parameter | Direction | Notes |
|-----------|-----------|-------|
| `A` | in/out | Overwritten with LU factors |
| `B` | in/out | RHS on input → solution X on output |
| `IPIV` | out | Pivot indices |
| `INFO` | out | If >0: U(INFO,INFO) = 0, no solution computed |

For inverse only: R uses `dgetrf` (factor) + `dgetri` (invert from factors).

---

## 4) Detailed mapping for the most common operations

### 4.1 Matrix multiply: `%*%`
**Backend:** `dgemm` (Level 3).  
R typically computes:
- `C = A %*% B` as a fresh allocation, then `dgemm` writes into `C`.
- For transposed variants, R uses BLAS transpose flags rather than materializing `t(A)`.

**Consumes input?** No (reads `A`, `B`; writes `C`).

**Helper candidate:** `gemm(A, transA, B, transB) -> C` and `gemv()` fast path for vector cases.

---

### 4.2 `crossprod()` / `tcrossprod()`
- `crossprod(X, Y)` = `t(X) %*% Y` → `dgemm` with `transA='T'`.
- `crossprod(X)` = `t(X) %*% X` → `dsyrk` (symmetric rank-k update) is a common optimized path.
- Similarly for `tcrossprod`.

**Consumes input?** No.

**Helper candidates:** `crossprod(X, Y?)`, `syrk(X)` returning symmetric output (choose triangle convention).

---

### 4.3 `solve(A,b)` (general dense)
Typical route:
1. **LU factorization**: `dgetrf(A, ipiv)`  
2. **Solve**: `dgetrs(LU, ipiv, B)` or use driver `dgesv` which does both.

**Consumes input?**
- `dgetrf` overwrites `A` (stores `L` and `U` in-place).
- `dgetrs` overwrites `B` with the solution.
So yes: you must **copy A and B** unless you've proven they are dead afterwards.

**Inverse (`solve(A)`):**
- often `dgetrf` + `dgetri` (inversion from LU). Also consumes the factor storage and uses workspace.

**Helper candidates:**
- `lu_factor(A) -> (LU, ipiv)`
- `lu_solve(LU, ipiv, B) -> X`
- `invert_from_lu(LU, ipiv) -> Ainv`

---

### 4.4 Triangular solves: `forwardsolve`, `backsolve`
**Backend:** `dtrsm` (Level 3) for multiple RHS; `dtrsv` (Level 2) for vector RHS.

**Consumes input?** Yes: BLAS writes the solution into the RHS buffer (`B`).

**Helper candidate:** `trsm(uplo, trans, diag, Atri, B) -> overwrites B`.

---

### 4.5 Cholesky: `chol`, `chol2inv`, and Cholesky-based solve
- `chol(A)` → `dpotrf(uplo='U' by default in R)`.  
- `chol(A, pivot=TRUE)` → `dpstrf` (pivoted Cholesky; returns rank/pivot).
- `chol2inv(R)` → `dpotri` (inverts from Cholesky factor).

**Cholesky-based solve (`dpotrs`):**
For SPD systems, solving via Cholesky is faster than LU:
```fortran
DPOTRS(UPLO, N, NRHS, A, LDA, B, LDB, INFO)
```
- `A` contains the Cholesky factor from `dpotrf`
- `B` is overwritten with solution X
- Internally performs two triangular solves via `dtrsm`

R doesn't expose `dpotrs` directly, but `backsolve(R, backsolve(R, b, transpose=TRUE))` achieves the same result. A transpiler can recognize this pattern and emit `dpotrs`.

**Consumes input?** Yes: `dpotrf`, `dpstrf`, `dpotri`, and `dpotrs` all overwrite their input matrix/RHS storage.

**Helper candidates:**
- `chol_factor(A, uplo) -> R`
- `chol_invert(R, uplo) -> Ainv`
- `chol_solve(R, B, uplo) -> X` via `dpotrs` (faster than generic LU solve for SPD)

---

### 4.6 QR: `qr`, `qr.solve`, QR family helpers
Important nuance:
- `qr()` default in base R uses **LINPACK** (`dqrdc2`) unless `LAPACK=TRUE`.
- With `LAPACK=TRUE`, typical LAPACK is **`dgeqp3`** (pivoted QR).
- The LINPACK `dqrdc2` differs from standard `DQRDC` by using a tolerance-based pivoting strategy that moves near-zero 2-norm columns to the right edge—important for rank-deficient detection.

Common LAPACK sequence:
1) `dgeqp3` (or `dgeqrf` for unpivoted QR)  
2) Apply/construct Q via `dormqr` / `dorgqr`  
3) Solve triangular system in R via `dtrsm`/`dtrsv` (or `dtrtrs` LAPACK)

**Key routines for Q manipulation:**
- `dormqr`: Apply Q or Q^T to a matrix **without forming Q explicitly** (memory-efficient)
- `dorgqr`: Explicitly construct the orthogonal matrix Q (requires more memory)

Prefer `dormqr` when you only need Q^T * B or Q * B.

**Consumes input?** Yes: QR routines overwrite `A` with `R` and reflector data.

**Helper candidates:**
- `qr_factor(A, pivoted=TRUE) -> (qr, tau, jpvt)`
- `apply_qt(qr, tau, B) -> Q^T B` using `dormqr`
- `apply_q(qr, tau, B) -> Q B` using `dormqr`
- `form_q(qr, tau) -> Q` using `dorgqr` (only when explicit Q needed)
- `tri_solve(R, B)`

Also consider implementing:
- `qr.Q`, `qr.R`, `qr.coef`, `qr.qty`, `qr.qy`, `qr.resid`, `qr.fitted`  
These are common in regression pipelines and are built from the same primitives.

---

### 4.7 Eigen: `eigen`
- **Symmetric real:** `dsyevr` is the **default and fastest** option (Relatively Robust Representations algorithm). Alternatives `dsyevd` (divide-and-conquer) and `dsyev` (classic QR) exist but are typically slower.
- **General real:** `dgeev`.

`dsyevr` benefits:
- Supports computing eigenvalue subsets (by index range or value range)
- Generally O(n²) for eigenvectors vs O(n³) for `dsyev`
- Best performance with optimized BLAS

**Consumes input?** Yes: these routines overwrite the input matrix during reduction steps.

**Helper candidates:**
- `sym_eigen(A, want_vectors, range?) -> (values, vectors)`
- `gen_eigen(A, want_vectors) -> (values_real, values_imag, vectors)`

---

### 4.8 SVD: `svd`
- Commonly `dgesdd` (divide-and-conquer) which is **6-7× faster** than `dgesvd` for large matrices when computing singular vectors.
- `dgesvd` (QR-based) may be preferred for smaller workspace or when only singular values are needed.

**Consumes input?** Yes: SVD routines overwrite/destroy `A`.

**Helper candidates:**
- `svd(A, jobu, jobvt) -> (s, U, VT)` with workspace query helper.

---

### 4.9 Norms: `norm`
- `dlange` for `"1"`, `"I"`, `"F"`, `"M"` is straightforward and non-destructive.
- `"2"` norm (spectral norm) usually computed via SVD (expensive and destructive).
- Revisit implementation: `norm(x, "2")` maps to BLAS `dnrm2` for vector inputs, but to LAPACK `dlange` (or SVD for spectral norm) for matrix inputs. We should implement `norm()` with this vector vs matrix split.

**Helper candidate:** `lange(A, norm_type) -> scalar`.

---

### 4.10 Covariance/correlation: `cov`, `cor`
Typically implemented as:
1) Center columns (subtract means), optional scaling to unit variance (for `cor`)
2) Compute `crossprod(centered)` / divide by `(n-1)`  
So: scalar ops + `crossprod` (BLAS).

**Consumes input?** No, unless you perform centering in-place on shared objects (your transpiler can choose).

---

## 5) "Consumes input" rules of thumb (crucial for R semantics)

R's visible behavior is "copy-on-modify": user variables are conceptually preserved across operations unless explicitly assigned.

### 5.1 BLAS routines
- **Most BLAS level-3 (`dgemm`, `dsyrk`)**: read inputs, write output buffer; safe if output is separate.
- **Triangular solves (`dtrsm`, `dtrsv`)**: overwrite RHS (`B` or vector).
- **Vector ops (`daxpy`, `dscal`)**: overwrite one operand (`y` or `x`).

### 5.2 LAPACK routines
Nearly all LAPACK "driver" and factorization routines **overwrite** their main matrix input:
- LU: `dgetrf`, solve driver: `dgesv`
- Cholesky: `dpotrf`, `dpstrf`, `dpotri`, `dpotrs`
- QR: `dgeqp3`, `dgeqrf`, plus `dorgqr`/`dormqr` overwriting work buffers
- Eigen: `dsyevr`, `dsyevd`, `dsyev`, `dgeev`
- SVD: `dgesdd`, `dgesvd`

### 5.3 Practical transpiler policy
- Default: **copy** inputs to any LAPACK call that overwrites them.
- If you implement def-use / liveness analysis, you can skip copies when the input is provably dead after the call.

---

## 6) Workspace query patterns

Many LAPACK routines require workspace arrays whose optimal size depends on problem dimensions and algorithm internals. The **LWORK=-1 convention** triggers a workspace query.

### 6.1 Routines requiring workspace queries

| Routine | Query parameter(s) | Minimum workspace | Notes |
|---------|-------------------|-------------------|-------|
| `dgeqp3` | LWORK=-1 | 3×N + 1 | Optimal: 2×N + (N+1)×NB |
| `dgeqrf` | LWORK=-1 | max(1, N) | Optimal: N×NB |
| `dsyevr` | LWORK=-1, LIWORK=-1 | 26×N (work), 10×N (iwork) | Also needs ISUPPZ array |
| `dsyevd` | LWORK=-1, LIWORK=-1 | 1 + 6×N + 2×N² (work) | Large workspace |
| `dsyev` | LWORK=-1 | max(1, 3×N-1) | |
| `dgeev` | LWORK=-1 | 3×N (no vectors), 4×N (with vectors) | |
| `dgesdd` | LWORK=-1 | Complex; depends on JOBZ | Also needs IWORK(8×min(M,N)) |
| `dgesvd` | LWORK=-1 | max(3×min(M,N)+max(M,N), 5×min(M,N)-4) | |
| `dgetri` | LWORK=-1 | N | For matrix inversion |
| `dorgqr` | LWORK=-1 | max(1, N) | For explicit Q formation |
| `dormqr` | LWORK=-1 | max(1, N) | For Q application |

### 6.2 Routines NOT requiring workspace queries

These can be called directly without a query step:

| Routine | Workspace | Notes |
|---------|-----------|-------|
| `dpotrf` | None | Cholesky factorization |
| `dpstrf` | Fixed: 2×N | Pivoted Cholesky |
| `dpotrs` | None | Cholesky solve |
| `dpotri` | None | Cholesky inverse |
| `dgetrf` | None | LU factorization |
| `dgetrs` | None | LU solve |
| `dgesv` | None | Combined LU factor + solve |
| `dtrsm` | None | Triangular solve (BLAS) |
| `dgemm` | None | Matrix multiply (BLAS) |
| `dlange` | Conditional: N for '1' and 'I' norms | Matrix norms |

### 6.3 Standard workspace query pattern

```fortran
! Step 1: Query optimal workspace
LWORK = -1
CALL DSYEVR(..., WORK, LWORK, IWORK, LIWORK, INFO)
LWORK = INT(WORK(1))
LIWORK = IWORK(1)

! Step 2: Allocate workspace
ALLOCATE(WORK(LWORK), IWORK(LIWORK))

! Step 3: Actual computation
CALL DSYEVR(..., WORK, LWORK, IWORK, LIWORK, INFO)
```

**Helper candidate:** `workspace_query(routine, args...) -> (lwork, liwork?)` that encapsulates this pattern.

---

## 7) INFO parameter conventions (error handling)

All LAPACK routines return an `INFO` parameter with consistent semantics:

| INFO value | Meaning |
|------------|---------|
| `= 0` | Success |
| `< 0` | Argument `-INFO` had an illegal value |
| `> 0` | Computational failure (routine-specific) |

**Quickr note:** Today, quickr cannot propagate Fortran-side errors back to R. INFO handling is therefore a design target,
and any `check_info()` helper is deferred until we have an error-bridge mechanism. Until then, the transpiler should either
ignore INFO (documented) or surface it via explicit output variables.

### 7.1 Routine-specific INFO > 0 meanings

| Routine | INFO > 0 meaning |
|---------|------------------|
| `dpotrf` | Leading minor of order INFO not positive definite |
| `dpstrf` | Matrix is rank deficient; RANK < N |
| `dgetrf` | U(INFO,INFO) is exactly zero—matrix singular |
| `dgesv` | U(INFO,INFO) is exactly zero—no solution computed |
| `dgeev` | QR algorithm failed to converge; eigenvalues INFO+1:N are valid |
| `dsyevr` | Internal error (rare with IEEE-754 arithmetic) |
| `dgesdd` | DBDSDC (bidiagonal SVD) did not converge |
| `dgesvd` | DBDSQR did not converge; INFO superdiagonals did not converge to zero |

---

## 8) BLAS/LAPACK naming conventions and call-patterns (for helper design)

### 8.1 Naming
Most routine names encode:
- **Precision/type:** `d` = double real (`s` single real, `z` complex double, `c` complex float)
- **Matrix type:**  
  - `ge` general, `sy` symmetric, `po` SPD, `tr` triangular, …
- **Operation:**  
  - `mm` multiply (BLAS), `sv` solve (driver), `trf` factorize, `trs` triangular solve (from factors),  
  - `qrf` QR factor, `qp3` QR with pivoting,  
  - `ev` eigen, `evr` eigen (RRR), `evd` eigen (D&C),
  - `svd` SVD, `sdd` SVD divide&conquer,  
  - `con` condition estimate, `lan`/`lange` norm.

Examples:
- `dgemm` = double general matrix-matrix multiply
- `dgesv` = double general solve driver (uses LU)
- `dgetrf` = LU factorization
- `dpotrf` = Cholesky factorization
- `dpotrs` = Cholesky triangular solve
- `dsyevr` = symmetric eigen (robust, fast driver—RRR algorithm)
- `dgeev` = general eigen
- `dgesdd` = SVD divide&conquer

### 8.2 Common arguments (design these into wrappers)
- Dimensions: `m,n,k`, `nrhs`
- Leading dimensions: `lda`, `ldb`, `ldc` (for column-major arrays, typically `lda = nrow(A)` in R terms)
- Option flags: `trans`, `uplo`, `side`, `diag`, `jobz`, `jobu`, `jobvt`, `range`
- Pivot arrays: `ipiv` (LU), `jpvt` (pivoted QR), `piv` (pivoted Cholesky)
- Workspace:
  - Many LAPACK routines want `work`, `lwork` and sometimes `iwork`, `liwork`
  - Standard pattern: **workspace query** with `lwork=-1`, then allocate (see Section 6)

### 8.3 Column-major layout
R matrices are stored column-major already, matching Fortran, which is great:
- A direct lowering to Fortran calls can pass R's contiguous storage (subject to how you marshal memory between R and your compiled code).
- For transposes, prefer BLAS `TRANS` flags rather than materializing `t(A)`.

---

## 9) Backend notes (OpenBLAS, MKL, Accelerate, reference)

R is typically linked to:
- **OpenBLAS** on many Linux distros,
- **Reference BLAS/LAPACK** (or a bundled one) on some Windows builds,
- **Apple Accelerate** on macOS (varies by build and user configuration),
- **MKL** if configured by the user or via specific distributions.

### 9.1 API portability
The **BLAS/LAPACK API** is stable; your Fortran calls should work across backends as long as the symbols are available and you link exactly like R (same `libblas`/`liblapack` or unified library).

### 9.2 Differences that may show up
- **Threading behavior:** OpenBLAS/MKL can be multithreaded; Accelerate too
- **Performance characteristics:** Vary significantly by backend
- **Minor floating-point differences:** Due to reordering / SIMD / threading

### 9.3 Thread safety considerations
When R code uses parallel processing (e.g., `parallel` package, `foreach`), nested parallelism with threaded BLAS can cause:
- Thread oversubscription (more threads than cores)
- Contention and slowdown
- In rare cases, hangs

**Recommendations:**
- Consider setting `OMP_NUM_THREADS=1` or `OPENBLAS_NUM_THREADS=1` in nested parallel contexts
- MKL: use `MKL_NUM_THREADS` or `mkl_set_num_threads()`
- Your transpiler may want to emit thread-count management around parallel regions

### 9.4 Integer overflow considerations
For very large matrices (n > ~46,340 for 32-bit integers), integer overflow can occur in:
- Leading dimension calculations (LDA × N)
- Workspace size calculations
- Index computations within BLAS/LAPACK

Most R installations use 32-bit BLAS integers. For matrices exceeding ~2 billion elements:
- Check for 64-bit BLAS/LAPACK (ILP64 interface)
- R itself has limits around `2^31 - 1` elements per vector/matrix

**Recommendation:** Add overflow checks for `nrow * ncol` and `lda * n` before LAPACK calls when targeting large matrices.

### 9.5 Matching R results
To match R results as closely as possible, build and link against the **same BLAS/LAPACK** that R is using for that session. Check with:
```r
La_library()   # Shows LAPACK library path
extSoftVersion()["BLAS"]  # Shows BLAS info
```

---

## 10) Suggested helper/wrapper API for the transpiler

A minimal set of Fortran-callable helpers covers most mapped R ops:

### BLAS helpers
- `gemm(transA, transB, A, B) -> C`
- `syrk(trans, X) -> SymmetricC`
- `trsm(side, uplo, trans, diag, Atri, B)  ! overwrites B`
- `trsv(uplo, trans, diag, Atri, x)  ! overwrites x (vector case)`
- `ger(x, y, A, alpha)  ! A := alpha*x*y' + A`

### LAPACK helpers (factorizations)
- `lu_factor(A) -> (LU, ipiv, info)`
- `lu_solve(LU, ipiv, B) -> X  ! overwrites B buffer`
- `lu_invert(LU, ipiv) -> Ainv`
- `chol_factor(A, uplo) -> (R, info)`
- `chol_solve(R, B, uplo) -> X  ! via dpotrs, overwrites B`
- `chol_invert(R, uplo) -> Ainv`
- `qr_factor_pivoted(A) -> (QR, tau, jpvt, info)`
- `qr_factor_unpivoted(A) -> (QR, tau, info)`

### LAPACK helpers (Q manipulation)
- `apply_qt(QR, tau, B) -> Q^T B  ! via dormqr`
- `apply_q(QR, tau, B) -> Q B  ! via dormqr`
- `form_q(QR, tau) -> Q  ! via dorgqr (when explicit Q needed)`

### LAPACK helpers (decompositions)
- `sym_eigen(A, want_vectors, range?) -> (w, Z, info)`
- `gen_eigen(A, want_vectors) -> (wr, wi, VR, info)` (or pack into complex)
- `svd_dc(A, want_u, want_vt) -> (s, U, VT, info)  ! dgesdd`
- `svd_qr(A, want_u, want_vt) -> (s, U, VT, info)  ! dgesvd`
- `lange(A, type) -> norm`

### Workspace helper
- `workspace_query(routine, args...) -> (lwork, liwork?)` (patternized per routine family)

### Error handling helper
- `check_info(info, routine_name) -> raises error with meaningful message` (**planned**; blocked on Fortran→R error propagation)

---

## 11) Implementation checklist (translation rules)

1. Determine if operation can be expressed as BLAS-3 (`dgemm`, `dsyrk`, `dtrsm`)  
2. Otherwise pick LAPACK driver (solve/eigen/svd/qr/chol)  
3. For LAPACK routines requiring workspace:
   - Emit workspace query call (LWORK=-1)
   - Allocate workspace
   - Emit actual call
4. Enforce R semantics:
   - if the routine overwrites inputs: copy unless the variable is dead afterward
5. Normalize to double precision (`real*8`) for R numerics
6. Handle `NA/NaN/Inf` policy:
   - base R sometimes checks for finite values before calling LAPACK; decide whether to emulate those checks
7. Handle INFO return:
   - Check for errors after each LAPACK call
   - Map INFO codes to meaningful error messages
   - **Quickr note:** defer error propagation until we have a Fortran→R error bridge; consider returning INFO as an explicit output in the interim
8. Threading control:
   - consider aligning to R's threading settings (OpenBLAS/MKL environment variables)
   - manage thread counts in nested parallel contexts
9. Large matrix safety:
   - add overflow checks for matrices approaching 32-bit integer limits

---

## 12) Notes on "comprehensive coverage" vs "high value"

Even though base R has many entry points, most linear-algebra-heavy code funnels through:
- `%*%`, `crossprod`, `tcrossprod`
- `solve`, `chol`, `qr`, `eigen`, `svd`
- `forwardsolve`, `backsolve`
- `norm`, `rcond`, `kappa`
- `cov`, `cor` (built from centering + crossprod)

Supporting these well (plus the QR helper family) typically covers the bulk of real-world usage.

---

## 13) Quickr implementation notes (current vs planned)

- **List returns and `$` access:** Not implemented yet. Planned approach is name-mangling:
  `eigen(x)` returns `x__values__`, `x__vectors__`, and `$` lowers to the mangled name.
- **Transpose lowering:** Prefer BLAS `TRANS` flags for `t(A) %*% B` and `A %*% t(B)` to avoid materializing transposes.
- **Conformability checks:** Emitted code should validate dimensions and stop with R-like errors when shapes do not align.

## Appendix A: Quick reference — R function to LAPACK routine

| R function | Primary LAPACK | Secondary/Alternative | Returns list? |
|------------|---------------|----------------------|---------------|
| `qr()` | `dqrdc2` (LINPACK) | `dgeqp3` (LAPACK=TRUE) | Yes |
| `qr.solve()` | `dqrdc2`/`dgeqp3` + `dormqr` + `dtrsm` | | No |
| `eigen(symmetric=TRUE)` | `dsyevr` | `dsyevd`, `dsyev` | Yes |
| `eigen(symmetric=FALSE)` | `dgeev` | | Yes |
| `svd()` | `dgesdd` | `dgesvd` | Yes |
| `chol()` | `dpotrf` | `dpstrf` (pivot=TRUE) | No (+attrs) |
| `chol2inv()` | `dpotri` | | No |
| `solve(A,b)` | `dgesv` | `dgetrf` + `dgetrs` | No |
| `solve(A)` | `dgetrf` + `dgetri` | | No |
| `det()` / `determinant()` | `dgetrf` | | Scalar / Yes |
| `norm()` | `dlange` | SVD for type="2" | Scalar |
| `rcond()` | `dgetrf` + `dgecon` | | Scalar |
| `forwardsolve()` | `dtrsm` | `dtrsv` | No |
| `backsolve()` | `dtrsm` | `dtrsv` | No |
| `crossprod(X)` | `dsyrk` | `dgemm` | No |
| `crossprod(X,Y)` | `dgemm` | | No |
| `%*%` | `dgemm` | `dgemv` for vec | No |

---

## Appendix B: Workspace requirements summary

| Routine | Needs query? | Minimum workspace formula |
|---------|-------------|---------------------------|
| `dgeqp3` | Yes | 3N + 1 |
| `dgeqrf` | Yes | max(1, N) |
| `dsyevr` | Yes | WORK: 26N, IWORK: 10N |
| `dsyevd` | Yes | WORK: 1+6N+2N², IWORK: 3+5N |
| `dsyev` | Yes | 3N - 1 |
| `dgeev` | Yes | 4N (with vectors) |
| `dgesdd` | Yes | Complex; also IWORK: 8×min(M,N) |
| `dgesvd` | Yes | max(3mn+mx, 5mn-4) where mn=min, mx=max |
| `dgetri` | Yes | N |
| `dpotrf` | No | — |
| `dpstrf` | No | 2N (fixed) |
| `dgetrf` | No | — |
| `dgesv` | No | — |
| `dpotrs` | No | — |
