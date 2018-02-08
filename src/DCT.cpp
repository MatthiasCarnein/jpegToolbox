#include <Rcpp.h>

#include <math.h> 
#include "jpegRange.h"

using namespace Rcpp;


//' kronecker Delta
//' 
//' Returns 1 if both inputs are the same and 0 if not
//' 
//' @param i first integer
//' @param j second integer
//' 
//' @return 1 if both integer are identical and 0 if not
//' 
//' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
//' 
//' @export
// [[Rcpp::export]]
int kroneckerDelta(int i, int j){
  if(i==j){
    return 1;
  } else {
    return 0;
  }
}


double a1d_cpp(int N, int i, int j){
  return (sqrt(2/(double)N) * cos( ((2*(double)j-1)*((double)i-1)*M_PI) / (2*(double)N) ) * (1 + ((double)kroneckerDelta(i,1) / 2) * (sqrt(2) - 2)));
}



//' 1D DCT matrix
//' 
//' Generate the 1D DCT matrix
//' 
//' @param N size of sequence to be transformed
//' 
//' @return 1D DCT matrix
//' 
//' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
//' 
//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix DCT1dmatrix_cpp(int N=8){
  Rcpp::NumericMatrix M(N,N);
  for(int i=0; i<N; i++){
    for(int j=0;j<N; j++){
      M(i,j)=a1d_cpp(N,i+1,j+1);
    }
  }
  return(M);
}

// [[Rcpp::export]]
Rcpp::NumericVector DCT1d_cpp(Rcpp::NumericVector x, int inverted=0){
  
  // get DCT matrix
  Rcpp::NumericMatrix matrix = DCT1dmatrix_cpp(x.size()); 
  
  // if inverted, transpose matrix
  if(inverted){
    Rcpp::NumericMatrix invertedMatrix(matrix.ncol(), matrix.nrow()); 
    for(int j=0; j<matrix.nrow(); j++){
      for(int i=0; i<matrix.ncol(); i++){
        invertedMatrix(i,j) = matrix(j,i);
      }
    }
    matrix = invertedMatrix;
  }
  
  // matrix multiplication
  Rcpp::NumericVector result(x.size());
  for(int j=0; j<matrix.nrow(); j++){
    double sum=0.0;
    for(int i=0; i<matrix.ncol(); i++){
      sum += (matrix(j, i) * x(i));
    }
    result(j) = sum;
  }
  
  return result;
}



// [[Rcpp::export]]
Rcpp::NumericMatrix DCT2d_cpp(Rcpp::NumericMatrix x, int inverted=0){
  
  // copy to leave R object untouched
  Rcpp::NumericMatrix matrix(clone(x));
  
  // for every row
  for(int row=0; row<matrix.nrow(); row++){
    matrix(row,_) = DCT1d_cpp(matrix(row,_), inverted);
  }
  // for every column
  for(int col=0; col<matrix.ncol(); col++){
    matrix(_,col) = DCT1d_cpp(matrix(_,col), inverted);
  }
  return(matrix);
}



Rcpp::NumericMatrix shiftMatrix(Rcpp::NumericMatrix matrix, int shift=-128, int rounding=0){
  for(int y=0; y<matrix.nrow(); y++){
    for(int x=0; x<matrix.ncol(); x++){
      if(!rounding){
        matrix(y,x) += shift;
      } else{
        matrix(y,x) = clipToJpegRange(round(matrix(y,x)+shift));
      }
    }
  }
  return(matrix);
}


Rcpp::NumericMatrix multiplyMatrix(Rcpp::NumericMatrix matrix, double factor){
  for(int y=0; y<matrix.nrow(); y++){
    for(int x=0; x<matrix.ncol(); x++){
      matrix(y,x) = matrix(y,x) * factor;
    }
  }
  return(matrix);
}


// [[Rcpp::export]]
Rcpp::List DCTimage_cpp(Rcpp::List image, int inverted=0, int DCTscaling=0){
  
  // copy image to leave R object untouched
  Rcpp::List modifiedImage(clone(image));
  
  Rcpp::CharacterVector channelNames = CharacterVector::create("Y", "Cb", "Cr");
  
  Rcpp::IntegerVector subsampling = modifiedImage["Subsampling"];
  
  // for every channel
  for(int c=0; c<channelNames.size(); c++){
    if(modifiedImage.containsElementNamed(channelNames[c])){
      Rcpp::List channel = modifiedImage[Rcpp::as<std::string>(channelNames[c])];
      
      int h = (subsampling[0] / subsampling[c*2]);
      int v = (subsampling[1] / subsampling[c*2+1]);
      
      // for every row
      for(int row=0; row<channel.size(); row++){
        Rcpp::List rows = channel[row];
        // for every column
        for(int col=0; col<rows.size(); col++){
          
          Rcpp::NumericMatrix block = rows[col];
          
          // scale before fancy upsampling v8
          if(DCTscaling && inverted && c>0){
            if((v==1 && h>1) || (h==1 && v>1)){
              block = multiplyMatrix(block, (double)177/125);
            } else{
              block = multiplyMatrix(block, (double)2);
            }
          }
          
          // pad before fancy upsampling
          if(DCTscaling && inverted && c>0){
            
            int paddedCols=8;
            int paddedRows=8;
            
            if(h>1) paddedCols=16;
            if(v>1) paddedRows=16;
            
            Rcpp::NumericMatrix blockPadded(paddedRows, paddedCols);
            for(int y=0; y<block.nrow(); y++){
              for(int x=0; x<block.ncol(); x++){
                blockPadded(y,x) = block(y,x);
              }
            }
            block = blockPadded;
          }
          
          // shift -128 before DCT
          if(!inverted) block = shiftMatrix(block, -128, 0);
          
          // perform DCT on block
          block = DCT2d_cpp(block, inverted);
          
          // scale after fancy subsampling v8
          if(DCTscaling && !inverted && c>0){
            if((v==1 && h>1) || (h==1 && v>1)){
              block = multiplyMatrix(block, (double)125/177);
            } else {
              block = multiplyMatrix(block, (double)1/2);
            }
            // take upper left 8x8 matrix (for fancy subsampling)
            block = block(Range(0,7), Range(0,7)); 
          }
          
          // shift +128 after IDCT
          if(inverted) block = shiftMatrix(block, 128, 1);
          
          rows[col] = block;
        }
      }
    }
  }
  
  return modifiedImage;
}










// [[Rcpp::export]]
Rcpp::List DCTimage_cpp_plain(Rcpp::List image, int inverted=0){
  
  // copy image to leave R object untouched
  Rcpp::List modifiedImage(clone(image));
  
  Rcpp::CharacterVector channelNames = CharacterVector::create("Y", "Cb", "Cr");
  
  // for every channel
  for(int c=0; c<channelNames.size(); c++){
    if(modifiedImage.containsElementNamed(channelNames[c])){
      Rcpp::List channel = modifiedImage[Rcpp::as<std::string>(channelNames[c])];
            
      // for every row
      for(int row=0; row<channel.size(); row++){
        Rcpp::List rows = channel[row];
        // for every column
        for(int col=0; col<rows.size(); col++){
          
          Rcpp::NumericMatrix block = rows[col];
                              
          // perform DCT on block
          block = DCT2d_cpp(block, inverted);
          
          rows[col] = block;
        }
      }
    }
  }
  
  return modifiedImage;
}