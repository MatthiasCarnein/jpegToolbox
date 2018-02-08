#include <Rcpp.h>

#include "jpegRange.h"

using namespace Rcpp;


//' Jpeg Quantization of image
//' 
//' applies a given quantization table to an image
//' 
//' @param image image object of class jpegImage
//' @param quantizationTable list of quantization tables for luminance and optionally chrominance
//' 
//' @return quantized image
//' 
//' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List quantize(Rcpp::List image, Rcpp::List quantizationTable) {
  
  // copy image to leave R object untouched
  Rcpp::List modifiedImage(clone(image));
  
  int numChannel = modifiedImage["Channel"];
  
  
  Rcpp::CharacterVector channelNames = CharacterVector::create("Y", "Cb", "Cr");
  Rcpp::CharacterVector tableNames = CharacterVector::create("luminance", "chrominance", "chrominance");
  
  // for every channel
  for(int c=0; c<numChannel; c++){
    Rcpp::List channel = modifiedImage[Rcpp::as<std::string>(channelNames[c])];
    Rcpp::IntegerMatrix table = quantizationTable[Rcpp::as<std::string>(tableNames[c])];
    
    // for every row
    for(int row=0; row<channel.size(); row++){
      Rcpp::List rows = channel[row];
      // for every column
      for(int col=0; col<rows.size(); col++){
        Rcpp::NumericMatrix block = rows[col];
        Rcpp::IntegerMatrix quantizedBlock(block.nrow(), block.ncol());
        
        // divide block by table
        for(int y=0; y<block.nrow(); y++){
          for(int x =0; x<block.ncol(); x++){
            quantizedBlock(y,x) = round((double)block(y,x) / (double)table(y,x));
            // from jcdctmgr.c
            // int temp = block(y,x);
            // int qval = table(y,x);
            // #ifdef FAST_DIVIDE
            // #define DIVIDE_BY(a,b)  a /= b
            // #else
            // #define DIVIDE_BY(a,b)	if (a >= b) a /= b; else a = 0
            // #endif
            // if (temp < 0) {
            // temp = -temp;
            // temp += qval>>1;	/* for rounding */
            // DIVIDE_BY(temp, qval);
            // temp = -temp;
            // } else {
            // temp += qval>>1;	/* for rounding */
            // DIVIDE_BY(temp, qval);
            // }
            // quantizedBlock(y,x) = temp;
          }
        }
        rows[col] = quantizedBlock;
      }
    }
  }
  
  modifiedImage["Table"] = quantizationTable;
  
  
  return modifiedImage;
}




//' Jpeg dequantize of image
//' 
//' Dequantizes the compressed image using the quantization Tables provided in the object.
//' 
//' @param image image object of class jpegImage
//' 
//' @return dequantized image
//' 
//' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List dequantize(Rcpp::List image) {
  
  // copy image to leave R object untouched
  Rcpp::List modifiedImage(clone(image));
  
  Rcpp::List quantizationTable = modifiedImage["Table"];
  
  int numChannel = modifiedImage["Channel"];
  
  
  Rcpp::CharacterVector channelNames = CharacterVector::create("Y", "Cb", "Cr");
  Rcpp::CharacterVector tableNames = CharacterVector::create("luminance", "chrominance", "chrominance");
  
  // for every channel
  for(int c=0; c<numChannel; c++){
    Rcpp::List channel = modifiedImage[Rcpp::as<std::string>(channelNames[c])];
    Rcpp::IntegerMatrix table = quantizationTable[Rcpp::as<std::string>(tableNames[c])];
    
    // for every row
    for(int row=0; row<channel.size(); row++){
      Rcpp::List rows = channel[row];
      // for every column
      for(int col=0; col<rows.size(); col++){
        Rcpp::IntegerMatrix block = rows[col];
        
        // multiply block with table
        for(int y=0; y<block.nrow(); y++){
          for(int x =0; x<block.ncol(); x++){
            block(y,x) = block(y,x) * table(y,x);
          }
        }
        rows[col] = block;
      }
    }
  }
  
  
  return modifiedImage;
}

