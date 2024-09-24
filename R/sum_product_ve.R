#' Sum-Product Variable-Elimination algorithm
#'
#' Compute sum-product and marginalize out variables from factors stored as arrays.
#'
#' @param factors list of factors, each either a scalar or an array, see [factor_product] and examples.
#' @param elim (character vector) names of variables to be eliminated, in order of elimination.
#' @return
#' - `sum_product_ve` an array with the sum-product of `factors` after eliminating `elim`.
#' @export
#' @seealso [factor_product(), cpquery_from_cpts()]
#' @examples
#'
#' new_factor <- function(data, dim, scope) array(data, dim, setNames(vector("list", length(dim)), scope))
#' x <- new_factor(c(0, 1), 2, "x")
#' y <- new_factor(c(.25, .75, .75, .25), c(2, 2), c("y", "x"))
#'
#' # joint prob
#' sum_product_ve(list(x, y), character(0))
#'
#' # sum out "y"
#' sum_product_ve(list(x, y), "y")
#'
#' # sum out "x"
#' sum_product_ve(list(x, y), "x")
#'
#' # sum out both
#' sum_product_ve(list(x, y), c("y", "x")) # 1 (scalar)
#'
#' # product of empty list is sat equal to 1
#' sum_product_ve(list(), "x")
 #'
#' # vectors without dimension attributes throws an error
#' \dontrun{
#' sum_product_ve(list(x, 2))   # x*2
#' sum_product_ve(list(2, 2))   # 4
#' sum_product_ve(list(1:2, 2)) # error
#' }
sum_product_ve <- function(factors, elim = character(0)){
  for (z in elim){
    factors <- sum_product_elim_var(factors, z)
  }
  factor_product(factors)
}

#' @rdname sum_product_ve
#' @param z (character) name of variable two eliminate
#' @return
#' - `sum_product_ve`: a list with factors after computing the sum-product w.r.t `z`
#' @export
sum_product_elim_var <- function(factors, z){
  stopifnot(is.list(factors))
  if (length(z) == 0) {
    return(factors)
  }

  # identify which factors have z in scope
  scopes <- lapply(factors, function(f) names(dimnames(f)))
  z_in_scope <- vapply(scopes, function(scope) match(z, scope, 0L) > 0, logical(1))
  if (!any(z_in_scope)){
    return(factors)
  } else {

    # compute factor product of factors with z in scope
    f <- factor_product(factors[z_in_scope])

    # sum out z
    scope <- names(dimnames(f))
    new_factor <- sum_out_margin(f, match(z, scope))

    # return list with factors
    return(c(factors[!z_in_scope], list(new_factor)))
  }
}

sum_out_margin <- function(x, margin) {
  # compute sums of array x over dimensions margin
  nlev <- dim(x)

  if (length(nlev) <= 1) {
    sum(x)  # cannot use colSums on array with less than 2 dimensions
  } else {

    perm <- c(margin, seq_along(nlev)[-margin])
    tmp <- colSums(aperm(x, perm), dims = length(margin))

    # return array with dim and dimnames
    array(tmp, nlev[-margin], dimnames(x)[-margin])
  }
}

