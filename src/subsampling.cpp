#include <Rcpp.h>
#include "jpegRange.h"

using namespace Rcpp;

//' Subsample image wrapper
//' 
//' Reduces the chrominance values of an image according to the provided subsampling factors.
//' 
//' @param image list of integer matrix of pixel values, typically an object of class jpegImage
//' @param subsampling subsampling factors in the form c(H1, V1, H2, V2, H3, V3)
//' 
//' @return image with subsampled Cb and Cr channels
//' 
//' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
//' 
// [[Rcpp::export]]
Rcpp::List subsampling_simple(Rcpp::List image, Rcpp::IntegerVector subsampling) {
  
  // copy image to leave R object untouched
  Rcpp::List modifiedImage(clone(image));
  
  Rcpp::CharacterVector channelNames = CharacterVector::create("Y", "Cb", "Cr");
  
  
  // for Cb and Cr channel
  for(int c=1; c<3; c++){
    // get matrix
    Rcpp::IntegerMatrix matrix = modifiedImage[Rcpp::as<std::string>(channelNames[c])];
    
    // get subsampling ratio
    int h = (subsampling[0] / subsampling[c*2]);
    int v = (subsampling[1] / subsampling[c*2+1]);
    
    // create smaller matrix holding subsampled values
    Rcpp::IntegerMatrix subsampled(matrix.nrow() / v, matrix.ncol() / h);
    
    // for every column
    for(int x=0; x<subsampled.ncol(); x++){
      // for every row
      for(int y=0; y<subsampled.nrow(); y++){
        
        // take mean of block
        int sum = 0;
        for(int xi = 0; xi<h; xi++){
          for(int yi = 0; yi<v; yi++){
            sum += matrix(y*v+yi, x*h+xi);
          }
        }
        subsampled(y,x) = round((double)sum/(h*v)); // take mean and round
        
      }
    }
    // overwrite original matrix with subsampled values
    modifiedImage[Rcpp::as<std::string>(channelNames[c])] = subsampled;
  }
  modifiedImage["Subsampling"] = subsampling;
  
  return modifiedImage;
}


//' Upsample image
//' 
//' Extrapolate the reduced chrominance values of an image according to the provided subsampling factors in order to reconstruct the original image.
//' 
//' @param image list of integer matrix of pixel values, typically an object of class jpegImage
//' 
//' @return image with upsampled Cb and Cr channels
//' 
//' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
//' 
// [[Rcpp::export]]
Rcpp::List upsampling_simple(Rcpp::List image) {
  
  // copy image to leave R object untouched
  Rcpp::List modifiedImage(clone(image));
  
  Rcpp::IntegerVector subsampling = modifiedImage["Subsampling"];
  
  Rcpp::CharacterVector channelNames = CharacterVector::create("Y", "Cb", "Cr");
  
  // for Cb and Cr channel
  for(int c=1; c<3; c++){
    // get matrix
    Rcpp::IntegerMatrix matrix = modifiedImage[Rcpp::as<std::string>(channelNames[c])];
    
    // get subsampling ratio
    int h = (subsampling[0] / subsampling[c*2]);
    int v = (subsampling[1] / subsampling[c*2+1]);
    
    // create matrix holding upsampled values
    Rcpp::IntegerMatrix upsampled(matrix.nrow(), matrix.ncol());
    
    // for every column
    for(int x=0; x<matrix.ncol()/h; x++){
      // for every row
      for(int y=0; y<matrix.nrow()/v; y++){
        
        // fill entire block with values
        for(int xi = 0; xi<h; xi++){
          for(int yi = 0; yi<v; yi++){
            upsampled(y*v+yi, x*h+xi) = matrix(y,x);
          }
        }
        
      }
    }
    // overwrite original matrix with upsampled values
    modifiedImage[Rcpp::as<std::string>(channelNames[c])] = upsampled;
  }
  return modifiedImage;
}


// [[Rcpp::export]]
Rcpp::List subsampling_fancy8(Rcpp::List image, Rcpp::IntegerVector subsampling) {
  
  // copy image to leave R object untouched
  Rcpp::List modifiedImage(clone(image));
  
  Rcpp::CharacterVector channelNames = CharacterVector::create("Y", "Cb", "Cr");
  
  // for Cb and Cr channel
  for(int c=1; c<3; c++){
    
    // get matrix
    Rcpp::IntegerMatrix matrix = modifiedImage[Rcpp::as<std::string>(channelNames[c])];
    
    // get subsampling ratio
    int h = (subsampling[0] / subsampling[c*2]);
    int v = (subsampling[1] / subsampling[c*2+1]);
    
    // additionally apply simpel subsampling before fancy
    if(v>2 || h>2){
      h=h/2;
      
      if(v>1){
        v=v/2;
      }
      
      // create matrix holding pre-subsampled values
      Rcpp::IntegerMatrix subsampled(matrix.nrow() / v, matrix.ncol() / h);
      
      // for every column
      for(int x=0; x<subsampled.ncol(); x++){
        // for every row
        for(int y=0; y<subsampled.nrow(); y++){
          
          // take mean of block
          int sum = 0;
          for(int xi = 0; xi<h; xi++){
            for(int yi = 0; yi<v; yi++){
              sum += matrix(y*v+yi, x*h+xi);
            }
          }
          subsampled(y,x) = clipToJpegRange(round((double)sum/(h*v))); // take mean and round
          
        }
      }
      
      // overwrite original matrix with subsampled values
      modifiedImage[Rcpp::as<std::string>(channelNames[c])] = subsampled;
      
    }
    
    modifiedImage["Subsampling"] = subsampling;
  }
  return modifiedImage;
}



// [[Rcpp::export]]
Rcpp::List upsampling_fancy8(Rcpp::List image) {
  
  // copy image to leave R object untouched
  Rcpp::List modifiedImage(clone(image));
  
  Rcpp::IntegerVector subsampling = modifiedImage["Subsampling"];
  
  Rcpp::CharacterVector channelNames = CharacterVector::create("Y", "Cb", "Cr");
  
  // for Cb and Cr channel
  for(int c=1; c<3; c++){
    
    // get matrix
    Rcpp::IntegerMatrix matrix = modifiedImage[Rcpp::as<std::string>(channelNames[c])];
    
    // get subsampling ratio
    int h = (subsampling[0] / subsampling[c*2]);
    int v = (subsampling[1] / subsampling[c*2+1]);
    
    // additionally apply simpel upsampling after fancy
    if(v>2 || h>2){
      h=h/2;
      
      if(v>1){
        v=v/2;
      }
      
      // create matrix holding pre-subsampled values
      Rcpp::IntegerMatrix upsampled(matrix.nrow() * v, matrix.ncol() * h);
      
      // for every column
      for(int x=0; x<matrix.ncol(); x++){
        // for every row
        for(int y=0; y<matrix.nrow(); y++){
          
          // copy value to entire block
          for(int xi = 0; xi<h; xi++){
            for(int yi = 0; yi<v; yi++){
              upsampled(y*v+yi, x*h+xi) = matrix(y,x);
            }
          }
            
        }
      }
      
      // overwrite original matrix with upsampled values
      modifiedImage[Rcpp::as<std::string>(channelNames[c])] = upsampled;
      
    }
  }
  return modifiedImage;
}




// [[Rcpp::export]]
Rcpp::List upsampling_fancy6(Rcpp::List image) {
  
  // copy image to leave R object untouched
  Rcpp::List modifiedImage(clone(image));
  
  Rcpp::IntegerVector subsampling = modifiedImage["Subsampling"];
  
  Rcpp::CharacterVector channelNames = CharacterVector::create("Y", "Cb", "Cr");
  
  // for Cb and Cr channel
  for(int c=1; c<3; c++){
    // get matrix
    Rcpp::IntegerMatrix matrix = modifiedImage[Rcpp::as<std::string>(channelNames[c])];
    
    // get subsampling ratio
    int h = (subsampling[0] / subsampling[c*2]);
    int v = (subsampling[1] / subsampling[c*2+1]);
    
    // create matrix holding upsampled values
    Rcpp::IntegerMatrix upsampled(matrix.nrow(), matrix.ncol());
    
    // iterate over chrominance values
    // for every column
    for(int x=0; x<(matrix.ncol()/h)-1; x++){
      // for every row
      for(int y=0; y<(matrix.nrow()/v)-1; y++){
        upsampled(y*v+1,x*h+1) = round(9.0/16.0 * (double)matrix(y,x) + 3.0/16.0 * (double)matrix(y+1,x) + 3.0/16.0 * (double)matrix(y,x+1) + 1.0/16.0 * (double)matrix(y+1,x+1));
        upsampled(y*v+1+1,x*h+1) = round(9.0/16.0 * (double)matrix(y+1,x) + 3.0/16.0 * (double)matrix(y,x) + 3.0/16.0 * (double)matrix(y+1,x+1) + 1.0/16.0 * (double)matrix(y,x+1));
        upsampled(y*v+1,x*h+1+1) = round(9.0/16.0 * (double)matrix(y,x+1) + 3.0/16.0 * (double)matrix(y,x) + 3.0/16.0 * (double)matrix(y+1,x+1) + 1.0/16.0 * (double)matrix(y+1,x));
        upsampled(y*v+1+1,x*h+1+1) = round(9.0/16.0 * (double)matrix(y+1,x+1) + 3.0/16.0 * (double)matrix(y+1,x) + 3.0/16.0 * (double)matrix(y,x+1) + 1.0/16.0 * (double)matrix(y,x));
      }
    }
    // overwrite original matrix with upsampled values
    modifiedImage[Rcpp::as<std::string>(channelNames[c])] = upsampled;
  }
  return modifiedImage;
}