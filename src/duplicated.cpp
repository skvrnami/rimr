#include <Rcpp.h>
using namespace Rcpp;

bool contains_int(int x, IntegerVector v){
    bool value = false;
    for(int i=0; i<v.length(); ++i){
        if(x == v[i]){
            value = true;
            return value;
        }
    }
    return value;
}

// [[Rcpp::export]]
IntegerVector find_duplicated_values(IntegerVector x) {
  IntegerVector existing;
  IntegerVector duplicated;
  for(int i=0; i<x.length(); ++i){
      if(!contains_int(x[i], existing)){
          existing.push_back(x[i]);
      }else{
          if(!IntegerVector::is_na(x[i])){
            duplicated.push_back(x[i]);
          }
      }
  }
  return duplicated;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.

/*** R
find_duplicated_values(c(1,1,2,3,3))
find_duplicated_values(c(1,NA,NA,3,1))
*/
