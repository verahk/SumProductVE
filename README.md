
<!-- README.md is generated from README.Rmd. Please edit that file -->

# SumProductVE

<!-- badges: start -->
<!-- badges: end -->

The goal of SumProductVE is to perform exact inference in Bayesian
Networks. The package contains an implementation of the factor-product
and Sum-Product-VE algorithm given in Koller (2009), for exact inference
in discrete Bayesian Networks. The factor-product procedure is
implemented in Rcpp for speed and can be applied to factors (conditional
probability tables, CPTs) represented as multidimensional arrays (as in
the bnlearn-package).

## Installation

You can install the development version of SumProductVE from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("verahk/SumProductVE")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(SumProductVE)

## basic example code
new_factor <- function(val, dim, scope) array(val, dim, setNames(vector("list", length(dim)), scope))
z <- new_factor(c(.5, .5), 2, "z")
x <- new_factor(c(0, 1),  2, "x")
y <- new_factor(c(.1, .9,
                  .2, .8,
                  .3, .7,
                  .4, .6), c(2, 2, 2), c("y", "x", "z"))

factor_product(list(x, y, z))  # joint distribution
#> , , 1
#> 
#>       y
#> x      [,1] [,2]
#>   [1,]  0.0  0.0
#>   [2,]  0.1  0.4
#> 
#> , , 2
#> 
#>       y
#> x      [,1] [,2]
#>   [1,]  0.0  0.0
#>   [2,]  0.2  0.3
```

``` r
sum_product_ve(list(x, y, z), elim = character(0))  # same
#> , , 1
#> 
#>       y
#> x      [,1] [,2]
#>   [1,]  0.0  0.0
#>   [2,]  0.1  0.4
#> 
#> , , 2
#> 
#>       y
#> x      [,1] [,2]
#>   [1,]  0.0  0.0
#>   [2,]  0.2  0.3
```

``` r
sum_product_ve(list(x, y, z), elim = "y")           # marginal distribution of x and y
#>       z
#> x      [,1] [,2]
#>   [1,]  0.0  0.0
#>   [2,]  0.5  0.5
```

## Example 2: cpquery from list of CPTs

``` r
cpts <- list(z = z, x = x, y = y)
cpquery(cpts, y = "y")
#> [1] 0.3 0.7
```

## Example 3:

``` r
g <- bnlearn::empty.graph(names(cpts))
bnlearn::amat(g) <- dag_from_cpts(cpts)
bn <- bnlearn::custom.fit(g, cpts)
bn 
#> 
#>   Bayesian network parameters
#> 
#>   Parameters of node z (multinomial distribution)
#> 
#> Conditional probability table:
#>  z
#>   A   B 
#> 0.5 0.5 
#> 
#>   Parameters of node x (multinomial distribution)
#> 
#> Conditional probability table:
#>  x
#> A B 
#> 0 1 
#> 
#>   Parameters of node y (multinomial distribution)
#> 
#> Conditional probability table:
#>  
#> , , x = A
#> 
#>    z
#> y     A   B
#>   A 0.1 0.3
#>   B 0.9 0.7
#> 
#> , , x = B
#> 
#>    z
#> y     A   B
#>   A 0.2 0.4
#>   B 0.8 0.6
```

``` r

cpquery(bn, "y")
#> [1] 0.3 0.7
```
