# hsblas
**Currently in heavy WIP**
A wrapper around BLAS/LAPACK with the goal to provide everyone with the level of
abstraction they want.

## Dependencies
### CBLAS/LAPACKE
As this is a wrapper around BLAS/LAPACK, naturally those libraries need to be
found when building. The BLAS & LAPACK libraries are Fortran libraries, but
to build hsblas, the C wrappers are required. The names used to link against
them are:
- `lapacke`: The C wrapper around LAPACK
- `cblas`: The C wrapper around BLAS

