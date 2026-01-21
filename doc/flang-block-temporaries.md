# Flang, `block`-scoped temporaries, and stack vs heap allocation

This note documents why quickr uses Fortran `block` scopes for temporaries, what we observed when compiling quickr-generated Fortran with different compilers (flang vs gfortran), and the practical trade-offs for users and maintainers.

## Why quickr uses `block` for temporaries

quickr emits intermediate temporaries (e.g. work buffers and copies of inputs for LAPACK calls) into a Fortran `block` so their lifetime is limited:

- It reduces *peak* live memory within a subroutine by constraining temporary lifetimes to the smallest region that needs them.
- It gives the compiler clearer lifetime boundaries (enter/exit) so storage can be reclaimed as early as possible.

Example shape (simplified):

```fortran
block
  real(c_double) :: tmp(m, n)
  ! ... use tmp ...
end block
! tmp is out of scope here
```

## Minimal example and generated Fortran

This minimal quickr function forces quickr to create runtime-sized temporaries in a `block` (rectangular least-squares solve):

```r
f_rect <- function(X, y) {
  declare(type(X = double(n, k)), type(y = double(n)))
  solve(X, y)
}
```

The generated Fortran (see `scratch/solve_rect.f90`) includes automatic arrays with runtime bounds inside a `block`, e.g.:

```fortran
block
  real(c_double) :: btmp1_(X__dim_1_, X__dim_2_)
  real(c_double) :: btmp2_(max(X__dim_1_, X__dim_2_), 1)
  ! ...
end block
```

These are *explicit local variables* (not compiler-invented temporaries), and their bounds are only known at runtime.

## What flang does: stack allocation via `alloca`

When we compile the generated Fortran with flang to LLVM IR:

```sh
flang-new -S -emit-llvm -O0 scratch/solve_rect.f90 -o scratch/solve_rect.ll
```

the IR shows `block` entry/exit implemented as a stack save/restore, with runtime-sized locals allocated on the stack:

```llvm
%34 = call ptr @llvm.stacksave.p0()
%45 = alloca double, i64 %44, align 8
%52 = alloca double, i64 %51, align 8
%64 = alloca double, i64 %63, align 8
call void @llvm.stackrestore.p0(ptr %34)
```

Interpretation:

- The `block` lifetime is respected (stack space is released at `stackrestore`).
- But the storage comes from the thread stack. Large `m*n` can exceed OS stack limits and segfault.

The flang assembly (`scratch/solve_rect_flang.s`) also reflects stack allocation: it computes an aligned size and adjusts `sp` (stack pointer) down by that amount, rather than calling `malloc`.

## What gfortran does: heap allocation via `malloc/free`

When we compile the same Fortran with gfortran:

```sh
gfortran -S -O0 scratch/solve_rect.f90 -o scratch/solve_rect_gfortran.s
```

the assembly shows heap allocation and cleanup for these runtime-sized locals:

- Calls to `_malloc` appear at the points where runtime-sized arrays are created.
- Calls to `_free` appear at the end of the scope/cleanup path.

Example snippet shape (see `scratch/solve_rect_gfortran.s`):

```asm
bl _malloc
...
bl _malloc
...
bl _malloc
...
bl _free
bl _free
bl _free
```

This behavior matches the common gfortran strategy of heap-allocating large/unknown-size locals, which avoids stack overflows at the cost of allocator overhead.

## Why flang flags didn’t help here

`flang-new --help` exposes:

- `-fno-stack-arrays` (“Allocate array temporaries on the heap (default)”)
- `-fstack-arrays` (“Attempt to allocate array temporaries on the stack, no matter their size”)

However, these apply to *compiler-generated array temporaries*. In this case, quickr emits explicit local automatic arrays in the source:

```fortran
real(c_double) :: btmp1_(X__dim_1_, X__dim_2_)
```

Recompiling `scratch/solve_rect.f90` with `-fno-stack-arrays` still produced dynamic `alloca` in LLVM IR for these locals, i.e. flang continued to allocate them on the stack.

## Trade-offs and implications

**Stack allocation (flang behavior here)**

- Pros: fast allocation, fast reclamation (especially with `block`), low fragmentation.
- Cons: brittle for large runtime sizes; depends on OS thread stack limits; may crash abruptly (segfault) rather than produce a clean R error.

**Heap allocation (gfortran behavior here)**

- Pros: can handle much larger work arrays without stack overflow; less sensitive to stack limits.
- Cons: allocator overhead; potential fragmentation; requires reliable cleanup on all exit paths.

**`block` is still useful**

Even when heap-allocating, `block` remains a good design for minimizing peak live memory: it constrains lifetimes so temporaries can be freed early (by the compiler/runtime).

## User guidance: choosing a different Fortran compiler

quickr selects a Fortran compiler based on a single global option:

```r
options(quickr.fortran_compiler = "gfortran")  # force gfortran
options(quickr.fortran_compiler = "flang")     # force flang
options(quickr.fortran_compiler = "auto")      # default behavior
```

Notes:

- On macOS, `auto` currently prefers flang if it is available.
- If flang compilation fails, quickr falls back to gfortran and disables automatic flang preference for the rest of the session.
- When OpenMP is used, quickr may avoid flang unless it was explicitly requested.

## Recommended direction for quickr (future work)

If we want `block`-scoped temporaries *and* robust behavior across compilers:

- Avoid emitting large runtime-sized temporaries as automatic arrays.
- Emit them as `allocatable` locals within the `block` and `allocate(...)` them explicitly, so they live on the heap while still having short lifetimes.

This keeps the original motivation for `block` (reduced peak memory) while avoiding flang’s stack allocation behavior for large runtime-sized locals.

