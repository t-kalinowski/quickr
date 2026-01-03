# Destination Inference and Temporary Minimization

This document explains how quickr avoids unnecessary temporaries in BLAS-backed
matrix operations by inferring the output variable ahead of time. It starts with
core concepts needed to understand the flow.

## Prerequisites

### The `<-` handler
- All assignments in the compiled function flow through `r2f_handlers[["<-"]]` in
  `R/r2f.R`.
- This handler decides whether the RHS can write directly into the LHS (no extra
  temporary), or whether a temporary result is needed and then assigned to the
  LHS.

### Scope
- A scope is an ordered environment that stores declared variables (`Variable`
  objects) and is threaded through the compiler.
- When a symbol is added to the scope, it becomes a declared Fortran variable in
  the manifest and is available for subsequent compilation steps.

### `Variable` class
- `Variable` objects carry:
  - `mode` (type): `double`, `integer`, `logical`, etc.
  - `dims`: list of dimensions (`list(m, n)` for matrices)
  - `name`: the Fortran variable name
- These are used for compile-time checks, Fortran declarations, and BLAS
  decisions.

### Manifest
- The manifest is the Fortran declaration block built from all `Variable`
  bindings in the scope.
- It is generated after compilation of the body, so it reflects all variables
  that ended up in scope.

### Hoist and temporaries
- Some expressions need pre-statements (e.g., assign a computed expression to a
  temporary before calling BLAS). These are emitted via `hoist`.
- The `hoist` object provides `declare_tmp()` and `emit()` to manage these
  temporary variables and pre-statements.

## Why destination inference exists

BLAS calls (like `dgemm`) want a destination buffer. If the compiler can provide
an existing output variable, BLAS can write directly into it. This avoids:
- allocating a temporary output array
- emitting a separate assignment `out = tmp_result`

Without inference, the compiler does not know the output shape early enough to
create and pass a destination variable into the RHS compilation.

## The inference pipeline (summary)

1) Assignment handler sees `out <- rhs`.
2) If the RHS handler supports a destination, `dest_infer_for_call()` tries to
   pre-compute the output shape as a `Variable`.
3) If inference succeeds, that `Variable` is inserted into scope under the LHS
   symbol. This makes it a declared Fortran variable.
4) The RHS is compiled with `dest=var` so BLAS helpers can write into it.
5) If the RHS wrote directly into `dest`, the assignment handler emits nothing.

## The key functions and their roles

### `dest_supported_for_call(call)`
- Checks whether a handler supports `dest`.
- Uses `attr(handler, "dest_supported")`.

### `dest_infer_for_call(call, scope)`
- Calls the handler's `dest_infer` function if it exists.
- Returns a `Variable` or `NULL`.

### Inference helpers for matrix ops
- `infer_dest_matmul()`
  - Computes output dims for `%*%` from declared input dims.
- `infer_dest_crossprod_like()`
  - Computes output dims for `crossprod()` and `tcrossprod()`.
- `infer_dest_outer()`
  - Computes output dims for `outer()` when both inputs are vectors.
- `infer_dest_triangular()`
  - Computes output dims for `forwardsolve()` / `backsolve()` from the RHS.

These functions are conservative: they only infer if inputs are plain symbols
already declared in scope.

## How direct output writes are decided

In BLAS helpers (e.g., `gemm()`):
- If a `dest` is passed, `can_use_output()` decides whether it is safe.
- Current rules:
  - `dest` exists
  - `dest@mode == "double"` (BLAS outputs are doubles)
  - `dest` does not alias input names
- If allowed, BLAS writes directly into `dest` and the returned `Fortran` result
  is tagged with `writes_to_dest = TRUE`.

The assignment handler then skips emitting `out = ...` because the output is
already written by BLAS.

## When temporaries are still created

### Case: expression inputs
If an operand is not a symbol (e.g. `a + b`), `ensure_blas_operand_name()` hoists
it into a temporary variable before calling BLAS.

### Case: inference fails
If inference cannot determine output dims (non-symbol inputs, unknown dims), the
compiler cannot predeclare the LHS and therefore cannot pass a destination.
This forces:
- a temporary BLAS output
- a later assignment to the LHS

## Example flow: `out <- a %*% b`

1) `infer_dest_matmul()` computes dims and creates a `Variable`.
2) The assignment handler inserts it into scope.
3) `%*%` handler calls `gemm()` with `dest=out`.
4) `gemm()` writes directly into `out` and returns `writes_to_dest = TRUE`.
5) The assignment handler emits no extra assignment.

## Example flow: `out <- (a + b) %*% c`

1) Inference fails because the left operand is not a symbol.
2) No destination is passed to `%*%`.
3) `ensure_blas_operand_name()` hoists `a + b` into a temp.
4) `gemm()` writes to a temp output.
5) Assignment emits `out = tmp_result`.

## Design notes and trade-offs

- Inference is intentionally conservative to avoid guessing shapes.
- Temporaries are minimized when operands are declared symbols.
- BLAS outputs are treated as double-only, so destinations must be `double`.
- The manifest is a result of scope contents; it is not used to decide whether
  direct output writes are allowed.

## Quick checklist for debugging temp creation

- Are operands declared symbols in scope?
- Does the handler have `dest_supported = TRUE`?
- Does inference return a `Variable` (non-NULL)?
- Is `dest@mode` double?
- Does `can_use_output()` reject due to aliasing?

