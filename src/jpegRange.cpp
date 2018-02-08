#include <Rcpp.h>

#include "jpegRange.h"

using namespace Rcpp;

//' Clip values to jpeg Range
//' 
//' Transforms a pixel value to fit in the range [0..255]
//' 
//' @param x pixel value
//' 
//' @return truncated pixel value
//' 
//' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
//' 
//' @export
// [[Rcpp::export]]
int clipToJpegRange(int x){
  if(x>255){
    return 255;
  } else if(x < 0){
    return 0;
  } else{
    return x;
  }
}