#include <Rcpp.h>
using namespace Rcpp;



//' Compute product of factors
//'
//' A Rcpp implementation of the factor product algortihm in Koller (2009).
//'
//' @name factor_product
//' @param factors list of arrays representing values of factors, see details.
//' @details Each factor `x` in `factors` must be a numeric vector with length 1
//' or have the following attributes (e.g. an array in R):
//'
//' - x.attr("dim") the cardinality of each variable in `x`
//' - x.attr("dimnames") a named list where the names are the names of the variable in the factor (its scope).
//'
//' @returns an array with the factor product of all factors in `factors`
//' @examples
//'
//' new_factor <- function(data, dim, scope) array(data, dim, setNames(vector("list", length(dim)), scope))
//' x <- new_factor(1:10, c(2, 5), c("a", "b"))
//' y <- new_factor(1:5, 5, c("b"))
//' factor_product(list(x, y))


// HELPER FUNCTIONS -------------------------------------------------------------

NumericVector compute_stride(NumericVector nlev) {
 int n = nlev.length();

 NumericVector out(n);
 out[0] = 1;
 int cump  = 1;
 for(int i = 1; i < n; i++) {
   cump *= nlev[i-1];
   out[i] = cump;
 }
 return out;
}

NumericVector get_dim(NumericVector x) {
  if (!x.hasAttribute("dim")) stop("x does not have any dim attribute");
  return x.attr("dim");
}
List get_dimnames(NumericVector x) {
  if (!x.hasAttribute("dimnames")) stop("x does not have any dimnames attribute");
  return x.attr("dimnames");
}

CharacterVector get_scope(NumericVector x) {
  List y = get_dimnames(x);
  if (!y.hasAttribute("names")) stop("x does not have any named dimnames attribute");
  return y.attr("names");
}


// [[Rcpp::export]]
NumericVector rarray(NumericVector values,
                     NumericVector dim,
                     List dimnames,
                     CharacterVector scope = CharacterVector::create()){

  if (dimnames.size() == 0) {
    List new_list(dim.size());
    dimnames = new_list;
  }

  if (scope.size() > 0) {
    if (scope.size() != dimnames.size()) {
      stop("scope must be of same length as dim");
    }
    dimnames.attr("names") = scope;
  }

  values.attr("dim") = dim;
  values.attr("dimnames") = dimnames;
  return values;
}

// FACTOR-PRODUCT --------------------------------------------------------------
NumericVector factor_product_internal(NumericVector x, NumericVector y) {

  // if x (y) is a constant, return x*y
  if (x.length() == 1) {
    if (y.length() == 1) {
      return NumericVector {x[0]*y[0]};
    }
    NumericVector tmp (y.length(), x[0]);
    return rarray(tmp*y, get_dim(y), get_dimnames(y));
  }
  if (y.length() == 1) {
    NumericVector tmp (x.length(), y[0]);
    return rarray(tmp*x, get_dim(x), get_dimnames(x));
  }

  CharacterVector scope_x = get_scope(x);
  NumericVector nlev_x = get_dim(x);
  NumericVector stride_x = compute_stride(nlev_x);

  CharacterVector scope_y = get_scope(y);
  NumericVector nlev_y = get_dim(y);
  NumericVector stride_y = compute_stride(nlev_y);

  // compute scope, cardinality, stride  and length for new factor - init by x
  CharacterVector new_scope = clone(scope_x);
  NumericVector   new_nlev  = clone(nlev_x);
  NumericVector   new_stride_x = clone(stride_x);
  NumericVector   new_stride_y (scope_x.length(), 0); // init as zero
  int len = 1;
  for (int i = 0; i < scope_x.length(); i++) {
   len *= nlev_x[i];
  }
  for (int i = 0; i < scope_y.length(); i++) {
   String v = scope_y[i];     // name of variable
   bool matched = false;      // indicator for v in scope of X
   for (int j = 0; j < scope_x.length(); j++) {
     if (v == scope_x[j]) {
       if (nlev_y[i] != nlev_x[j]) {
         // stop if x and y implies different cardinality of v
         stop("the dimensions of x and y do not match");
       }
       matched = true;
       new_stride_y[j] = stride_y[i];
       break;
     }
   }
   if (!matched) {
     // if v is not in scope of y, add to scope of new factor
     new_scope.push_back(scope_y[i]);
     // update cardinality and strides accordingly
     new_nlev.push_back(nlev_y[i]);
     new_stride_x.push_back(0);            // zero for vars not in scope
     new_stride_y.push_back(stride_y[i]);

     // update length of new vector
     len *= nlev_y[i];
   }
  }

  if (log10(len)/log10(2) > 25) {
   stop("new vector exceeds 2**25 elements");
  }
  int n = new_scope.size();

  // compute factor product
  NumericVector out(len);
  NumericVector ass(n);
  int j = 0;
  int k = 0;
  for (int i = 0; i < len; i++){

   if (i % 1000 == 0)
     Rcpp::checkUserInterrupt();

   out[i] = x[j]*y[k];

   for (int v = 0; v < n; v++){
     if (ass[v] == new_nlev[v]-1){
       ass[v] = 0;
       j = j - (new_nlev[v]-1)*new_stride_x[v];
       k = k - (new_nlev[v]-1)*new_stride_y[v];
     } else {
       ass[v] = ass[v]+1;
       j = j + new_stride_x[v];
       k = k + new_stride_y[v];
       break;
     }
   }
  }

  return rarray(out, new_nlev, List::create(), new_scope);
}


//' @rdname factor_product
//' @export
//'
// [[Rcpp::export]]
NumericVector factor_product(List factors) {
  int n = factors.length();
  if (n == 0) {
    return NumericVector {1};
  } else if (n == 1){
    NumericVector f = factors[0];
    if (f.length() > 1 && !f.hasAttribute("dim")) {
      stop("factors[0] is of length > 1 but has no dimension attribute");
    } else {
      return f;
    }
  } else {
    NumericVector f = factors[0];
    for (int i = 1; i < n; i++)
      f = factor_product_internal(f, factors[i]);
    return f;
  }
}


/*** R

rarray(1:4, c(2, 2), vector("list", 2), c("a", "b"))
rarray(1:4, c(2, 2), list(), c("a", "b"))

new_factor <- function(value, dim, scope) rarray(value, dim, list(), scope)
#k <- rep(2, 3)
#x <- new_factor(seq_len(prod(k)), k, letters[seq_along(k)])
#y <- new_factor(seq_len(prod(k)), k, LETTERS[seq_along(k)])
#factor_product(x, y)


x <- new_factor(1:10, 10, "a")
y <- new_factor(10:1, 10, "b")
factor_product(list(x, y))
*/

