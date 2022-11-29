



#' Title
#'
#' @param x,y arrays. Factors to be multiplied. One dimension for each variable
#'    in scope. Dimensions must be named (to find common variables).
#'
#' @return an array with the factor product of x and y
#' @export
#'
factor_product <- function(x, y){

  # find common vars
  vars_x  <- names(dimnames(x))
  vars_y  <- names(dimnames(y))
  sepset <- intersect(vars_x, vars_y)
  if (length(sepset) == 0) {
    print("No overlapping variable names")
    return(outer(x, y))
  } else {

    vars  <- union(vars_x, vars_y)
    cards <- setNames(c(dim(x), dim(y)[!vars_y %in% sepset]), vars)

    stride_x <- setNames(c(1, cumprod(dim(x)[-length(vars_x)])), vars_x)
    stride_y <- setNames(c(1, cumprod(dim(y)[-length(vars_y)])), vars_y)

    j <- 0
    k <- 0
    assignment <- cards*0
    out <- array(dim = cards,
                 dimnames = c(dimnames(x), dimnames(y)[!vars_y %in% sepset]))
    for (i in seq_along(out)){
      out[i] <- x[j+1]*y[k+1]
      for (v in vars){
        assignment[v] <- assignment[v]+1
        if (assignment[v] == cards[v]){
          assignment[v] <- 0
          if (any(v == vars_x)) j <- j - (cards[v]-1)*stride_x[v]
          if (any(v == vars_y)) k <- k - (cards[v]-1)*stride_y[v]
        } else {
          if (any(v == vars_x)) j <- j + stride_x[v]
          if (any(v == vars_y)) k <- k + stride_y[v]
          break
        }
      }
    }
    return(out)
  }
}
