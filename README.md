# hsblas
**Currently in heavy WIP**
A wrapper around BLAS/LAPACK with the goal to provide everyone with the level of
abstraction they want.

# Dependencies
## CBLAS/LAPACKE
As this is a wrapper around BLAS/LAPACK, naturally those libraries need to be
found when building. The BLAS & LAPACK libraries are Fortran libraries, but
to build hsblas, the C wrappers are required. These 2 interfaces are called
CBLAS and LAPACKE. Unfortunately, there is not a standartized means of packaging
these symbols. As such what and how to link to them can be highly implementation
specific. This mess is handeled in hsblas via cabal flags as follows:

| Flag         | Default | Library name          | Use case                                      |
| ------------ | ------- | --------------------- | --------------------------------------------- |
| `OpenBLAS`   | False   | `openblas`            | Use OpenBLAS                                  |
| `Accelerate` | True    | Use framework linking | Use Accelerate when on a darwin system        |
| `CBLAS`      | True    | `cblas`               | The BLAS C API is exposed through cblas       |
| `LAPACK`     | False   | `lapack`              | The LAPACK C API is exposed through `lapack`  |
| `LAPACKE`    | True    | `lapacke`             | The LAPACK C API is exposed through `lapacke` |

While being needlessly verbose, the following table gives a complete overview of which
libraries are searched and linked to:

| `OpenBLAS` | `Accelerate` | `CBLAS` | `LAPACK` | `LAPACKE` | OS     | external libraries                    |
| ---------- | ------------ | ------- | -------- | --------- | ------ | ------------------------------------- |
| True       |              |         |          |           |        | `openblas`                            |
| False      | True         |         |          |           | Darwin | Apple framework linking to Accelerate |
| False      | False        | False   | False    | False     |        | `blas`                                |
| False      | False        | True    | False    | False     |        | `cblas`                               |
| False      | False        | False   | True     | False     |        | `blas`, `lapack`                      |
| False      | False        | True    | True     | False     |        | `cblas`, `lapack`                     |
| False      | False        | False   | False    | True      |        | `blas`, `lapacke`                     |
| False      | False        | True    | False    | True      |        | `cblas`, `lapacke`                    |
| False      | False        | False   | True     | True      |        | `blas`, `lapack`, `lapacke`           |
| False      | False        | True    | True     | True      |        | `cblas`, `lapack`, `lapacke`          |

where blank cells can be anything / are ignored
