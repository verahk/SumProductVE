
#' Exact computation of conditional probability distributions
#'
#' Compute conditional probability distributions from a set of CPTs associated
#' associated with a Bayesian network, using the variable elimination algorithm.
#'
#' @param cpts (list) a discrete [bnlearn::bn.fit] object or a
#'  list of [base::array]s that specifies a conditional probability table $P(X_i| Pa(X_i))$
#'  For each array the `dimnames` attribute must be a named list, indicating the
#'  scope of the vector - i.e. the names of X_i and Pa(X_i).
#' @param y (integer vector)
#'  position of the (set of) variable(s) in `cpts`
#' @param x (integer vector)
#'  position of variables that constitutes the conditioning set.
#'  Defaults to the `integer(0)`
#' @param anc (logical vector)
#'  of length `n` indicating ancestors of `y` and/or `x`.
#'  If `NULL` (default), the ancestors is computed via the DAG implied by `cpts`.
#' @details
#'  - On the `anc` argument and elimination order:
#'  With the intention to reduce computation time, a pre-calculation of the set of
#'  ancestors of `x` and `y` is included as default and the remainding CPTs are
#'  left out of the sum-product-VE algorithm is to avoid summation over marginal
#'  distributions that sums to 1.
#'  As computing ancestor relations also can be computationally intensive, one might be
#'  better of setting `anc = rep(n, TRUE)` in small DAGs or if the set of ancestors
#'  include many variables.
#'  The elimination order is in the order of the CPTs. If the topological ordering
#'  is known, reordering the CPTs can also reduce compuation time.
#' @return an array with `length(y, x)` dimensions, with the conditional probabilities of `y` given `x`.
#' @export
#' @examples
#'
#' # specify a set of CPTs
#' new_factor <- function(val, dim, scope) array(val, dim, setNames(vector("list", length(dim)), scope))
#' z <- new_factor(c(.5, .5), 2, "z")
#' x <- new_factor(c(.5, .5),  2, "x")
#' y <- new_factor(c(.1, .9,
#'                   .2, .8,
#'                   .3, .7,
#'                   .4, .6), c(2, 2, 2), c("y", "x", "z"))
#' cpts <- list(z = z, x = x, y = y)
#'
#' # marginal prob
#' cpquery(cpts, y = "y")
#'
#' # conditional probs
#' cpquery(cpts, y = "y", x = "x")
#' stopifnot(cpquery(cpts, y =  "y", x = c("x", "z")) == y)
#'
#' # obtain DAG from cpts
#' dag <- dag_from_cpts(cpts)
#' dag
#'
#' # identify ancestors of a set of nodes
#' anc <- areAncestors(dag, 1:2)  # a node is an ancestor of itself
#' anc
#'
cpquery <- function(cpts, y, x = integer(0), anc = NULL) {
  varnames <- names(cpts)
  stopifnot(!anyDuplicated(varnames))

  # map to position
  if (is.character(y)) {
    y <- match(y, varnames)
  }
  if (is.character(x)) {
    x <- match(x, varnames)
  }

  if (inherits(cpts, "bn.fit")) {
    stopifnot(inherits(cpts, "bn.fit.dnet"))
    cpts <- cpts_from_bn(cpts)
  }

  # identify ancestors
  keep <- c(y, x)
  if (is.null(anc)) {
    dag  <- dag_from_cpts(cpts)
    anc  <- areAncestors(dag, keep)
  } else {
    stopifnot(length(anc) == length(cpts) && all(anc[keep]))
  }

  cpquery_internal(cpts, y, x, anc)
}


cpquery_internal <- function(cpts, y, x, anc) {
  # inner function with no checks
  varnames <- names(cpts)
  keep <- c(y, x)

  # compute marg joint prob
  pyx  <- sum_product_ve(cpts[anc], varnames[replace(anc, keep, FALSE)])

  # compute conditional prob
  if (length(keep) > 1) {
    # permute array
    pyx  <- aperm(pyx, varnames[keep])

    if (!is.null(x)) {
      # compute cond prob p(y|x)
      ny <- length(y)
      ky <- prod(dim(pyx)[seq_len(ny)])
      px <- colSums(pyx, ny)
      pyx <- pyx/rep(px, each = ky)
    }
  }

  return(pyx)
}

#' @rdname cpquery
#' @export
#' @return
#' - `dag_from_cpts`: a n-by-n adjacency matrix of the DAG consistent with the scope of `cpts`
dag_from_cpts <- function(cpts) {
  stopifnot(!is.null(names(cpts)))
  n   <- length(cpts)
  varnames <- names(cpts)
  dag <- matrix(0, n, n)
  colnames(dag) <- rownames(dag) <- varnames
  scopes <- lapply(cpts, function(x) names(dimnames(x)))
  for (i in seq_len(n)[lengths(scopes) > 1]) {
    pa <- scopes[[i]][-1]
    dag[pa, i] <- 1
  }
  return(dag)
}


#' @rdname cpquery
#' @param dag an adjacency matrix
#' @param nodes column position of nodes
#' @export
#' @return
#' - `areAncestors`: a n-length logical vector indicating which variables are ancestors of `nodes`
areAncestors <- function(dag, nodes, seqn = seq_len(ncol(dag)), anc = rep(FALSE, ncol(dag))) {
  # recursively determine the ancestors of `nodes` in `dag`
  for (j in nodes) {
    anc[j] = TRUE  # j is an ancestor of it self
    for (i in seqn) {
      if (!anc[i] && dag[i, j] == 1) {
        anc[i] = TRUE
        anc = anc | areAncestors(dag, i, seqn, anc)
      }
    }
  }
  return(anc)
}

#' @rdname cpquery
#' @export
cpts_from_bn <- function(bn) {
  lapply(bn, function(x) {
    if (length(x$parents) == 0) {
      array(x$prob, length(x$prob), setNames(vector("list", 1), x$node))
    } else {
      x$prob
    }
  })
}
