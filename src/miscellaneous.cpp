#include <Rcpp.h>

//#define HAVE_BOOLEAN
//typedef unsigned char boolean;
//#define FALSE	0	
//#define TRUE	1


extern "C" {
  #include <jpeglib.h> 
  #include <jerror.h> 
}

using namespace Rcpp;


//' Current libjpeg Version
//' 
//' Returns the current libjpeg compatibility version
//' 
//' @return current libjpeg compatibility version
//' 
//' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
//' 
//' @export
// [[Rcpp::export]]
int getLibjpegVersion() {
   return JPEG_LIB_VERSION;
}



//' Rounding in C
//' 
//' Exposes the round function of C to R. In contrast to the R alternative it rounds 0.5 up.
//' 
//' @param x value to be rounded
//' 
//' @return rounded value
//' 
//' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
//' 
// [[Rcpp::export]]
int roundC_wrapper(double x) {
   return round(x);
}
