
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vacactions

<!-- badges: start -->

<!-- badges: end -->

The goal of vacactions is to have a place to store and documents the
little functions that I write to help me in my pharmacometric projects
but that are not big enough to warrant a specific package.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Vincent-AC/vacactions")
```

Some functions of the package depend on the python package pharmpy.
Please go [here](https://github.com/pharmpy/pharmpy) for installation
instructions

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(vacactions)
## Create multiple models that are a copy of an input one, with updated inits.
create_updated_modfiles("run1.mod",c("run2.mod","run3.mod"))
## 
## Create multiple models that are a copy of multiple original models, with updated inits.
## run3 will be an updated run1 and run4 will be an updated run2
create_updated_modfiles(c("run1.mod","run2.mod"),c("run3.mod","run4.mod"))
```
