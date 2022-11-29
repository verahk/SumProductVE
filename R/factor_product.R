

#' Title
#'
#' @param x,y arrays. Factors to be multiplied. One dimension for each variable
#'    in scope. Dimensions must be named (to find common variables).
#'
#' @return an array with the factor product of x and y
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
    cards <- c(dim(x), dim(y)[!vars_y %in% sepset])

    stride_x <- stride_y <- cards*0
    stride_x[vars%in%vars_x] <- c(1, cumprod(dim(x)[-length(vars_x)]))
    stride_y[vars%in%vars_y] <- c(1, cumprod(dim(y)[-length(vars_y)]))


    j <- 0
    k <- 0
    assignment <- 0*cards
    out <- array(dim = cards,
                 dimnames = c(dimnames(x), dimnames(y)[!vars_y %in% sepset]))
    for (i in seq_along(out)){
      out[i] <- x[j+1]*y[k+1]
      for (l in seq_along(cards)){
        assignment[l] <- assignment[l]+1
        if (assignment[l] == cards[l]){
          assignment[l] <- 0
          j <- j - (cards[l]-1)*stride_x[l]
          k <- k - (cards[l]-1)*stride_y[l]
        } else {
          j <- j + stride_x[l]
          k <- k + stride_y[l]
          break
        }
      }
    }
    return(out)
  }
}

