% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{factor_product}
\alias{factor_product}
\title{Compute product of factors}
\usage{
factor_product(factors)
}
\arguments{
\item{factors}{list of arrays representing values of factors, see details.}
}
\value{
an array with the factor product of all factors in `factors`
}
\description{
A Rcpp implementation of the factor product algortihm in Koller (2009).
}
\details{
Each factor `x` in `factors` must be a numeric vector with length 1
or have the following attributes (e.g. an array in R):

- x.attr("dim") the cardinality of each variable in `x`
- x.attr("dimnames") a named list where the names are the names of the variable in the factor (its scope).
}
\examples{

new_factor <- function(data, dim, scope) array(data, dim, setNames(vector("list", length(dim)), scope))
x <- new_factor(1:10, c(2, 5), c("a", "b"))
y <- new_factor(1:5, 5, c("b"))
factor_product(list(x, y))
}
