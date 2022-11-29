

sum_product_ve <- function(factors, vars_to_elim){
  for (z in vars_to_elim){
    factors <- sum_product_elim_var(factors, z)
  }
  Reduce(factor_product, factors)
}

sum_product_elim_var <- function(factors, z){
  indx <- unlist(lapply(factors, function(x) z %in% names(dimnames(x))))
  if (any(indx)) {
    prod <- Reduce(factor_product, factors[indx])
    vars <- dimnames(prod)
    z_pos <- which(names(vars) == z)

      if (length(vars) == 1){
      tau <- sum(prod)
    } else if (z_pos == 1){
      tau <- colSums(prod, dims = 1)
      if (!is.array(tau)){
        tau <- array(tau,
                     dim = lengths(vars[-z_pos]),
                     dimnames = vars[-z_pos])
      }
    } else {
      stride <- c(1, cumsum(lengths(vars)[1:(z_pos-1)]))[z_pos]
      tau <- colSums(aperm(prod, c(z_pos, seq_along(vars)[-z_pos])), dims = 1)
    }

    return(c(factors[!indx], list(tau)))
  } else {
    return(factors)
  }
}
