#' Compute factor product of two arrays
#'
#' @param x,y arrays. Factors to be multiplied. One dimension for each variable
#'    in scope. Dimensions must be named (to find common variables).
#'
#' @return an array with the factor product of x and y
factor_product_asplit <- function(x, y){

  # find common vars
  xvars  <- names(dimnames(x))
  yvars  <- names(dimnames(y))
  sepset <- intersect(xvars, yvars)
  stopifnot((length(sepset) == 0) | (identical(dimnames(x)[sepset], dimnames(y)[sepset])))

  # cardinality
  k <- c(dim(x), dim(y))[!duplicated(c(xvars, yvars))]

  # sort dims
  x <- aperm(x, c(sepset, setdiff(xvars, sepset)))
  y <- aperm(y, c(sepset, setdiff(yvars, sepset)))


  xlong  <- rep(x, prod(k[setdiff(yvars, xvars)]))

  # inflate y-array by repeating elements for each
  if (length(yvars) > 1 & any(yvars %in% sepset) & !all(yvars %in% sepset)){
    ysplit <- asplit(y, setdiff(yvars, sepset))
  } else {
    ysplit <- list(y)
  }

  ylong  <- unlist(rep(ysplit, each = prod(k[setdiff(xvars, yvars)])))


  array(xlong*ylong,
        dim = k[unique(c(sepset, xvars, yvars))],
        dimnames = c(dimnames(x), dimnames(y))[unique(c(sepset, xvars, yvars))])

}

