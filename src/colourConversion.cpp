#include <Rcpp.h>

#include "jpegRange.h"

using namespace Rcpp;



#define CENTERJSAMPLE 128
#define SCALEBITS  16
#define CBCR_OFFSET  ((int) CENTERJSAMPLE << SCALEBITS)
#define ONE_HALF	((int) 1 << (SCALEBITS-1))
#define FIX(x)		((int) ((x) * (1L<<SCALEBITS) + 0.5))



//' RGB to YCbCr conversion
//'
//' Converts from RGB colour space to YCbCr.
//' To avoid floating point arithmetic the constants are scaled up by 2^16.
//' See jccolor.c of libjpeg for more details.
//'
//' @param R red pixel value in the range of 0..255
//' @param G green pixel value in the range of 0..255
//' @param B blue pixel value in the range of 0..255
//'
//' @return vector of Y, Cb and Cr values
//'
//' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
//'
// [[Rcpp::export]]
Rcpp::IntegerVector RGBtoYCbCr_cpp(int R, int G, int B) {
  Rcpp::IntegerVector YCbCr(3);
  YCbCr[0] = ((FIX(0.299) * R + FIX(0.587) * G + FIX(0.114) * B + ONE_HALF)  >> SCALEBITS);
  YCbCr[1] = ((FIX(-0.168735892) * R - FIX(0.331264108) * G + FIX(0.5) * B + CBCR_OFFSET + ONE_HALF-1) >> SCALEBITS);
  YCbCr[2] = ((FIX(0.5) * R - FIX(0.418687589) * G - FIX(0.081312411) * B + CBCR_OFFSET + ONE_HALF-1) >> SCALEBITS);
  return YCbCr;
}


//' YCbCr to RGB conversion
//'
//' Converts from YCbCr colour space to RGB.
//' To avoid floating point arithmetic the constants are scaled up by 2^16.
//' See jdcolor.c of libjpeg for more details.
//'
//' @param Y luminance pixel value in the range of 0..255
//' @param Cb blue chrominance pixel value in the range of 0..255
//' @param Cr red chrominance value in the range of 0..255
//'
//' @return vector of R, G and B values
//'
//' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
//'
// [[Rcpp::export]]
Rcpp::IntegerVector YCbCrtoRGB_cpp(int Y, int Cb, int Cr) {
  Rcpp::IntegerVector RGB(3);
  RGB[0] = clipToJpegRange(Y + ((FIX(1.402) * (Cr - CENTERJSAMPLE) + ONE_HALF) >> SCALEBITS));
  RGB[1] = clipToJpegRange(Y + ((- FIX(0.344136286) * (Cb - CENTERJSAMPLE ) - FIX(0.714136286) * (Cr - CENTERJSAMPLE) + ONE_HALF) >> SCALEBITS));
  RGB[2] = clipToJpegRange(Y + ((FIX(1.772) * (Cb - CENTERJSAMPLE) + ONE_HALF) >> SCALEBITS));
  return RGB;
}








//' Image conversion from RGB to YCbCr
//'
//' Converts an image from RGB colorspace to YCbCr.
//' Requires that all three channels have the same resolution.
//'
//' @param image list of matrices with R, G and B values
//'
//' @return list of matrices with Y, Cb and Cr values
//'
//' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
//'
// [[Rcpp::export]]
Rcpp::List RGBtoYCbCr_image_cpp(Rcpp::List image) {

  // copy image to leave R object untouched
  Rcpp::List modifiedImage(clone(image));

  int numChannel =modifiedImage["Channel"];

  if(numChannel==1){

    // simply rename channel
    Rcpp::CharacterVector names = modifiedImage.names();
    names[modifiedImage.findName("grey")]="Y";

  } else if(numChannel==3) {
    // convert to YCbCr

    // width and height of image
    Rcpp::IntegerVector resolution = modifiedImage["Resolution"];
    int width = resolution["width"];
    int height= resolution["height"];

    // original values
    Rcpp::IntegerMatrix R = modifiedImage["red"];
    Rcpp::IntegerMatrix G = modifiedImage["green"];
    Rcpp::IntegerMatrix B = modifiedImage["blue"];

    // converted values
    Rcpp::IntegerMatrix Y(height, width);
    Rcpp::IntegerMatrix Cb(height, width);
    Rcpp::IntegerMatrix Cr(height, width);

    // for every column
    for(int x =0; x<width; x++){
      //for every row
      for(int y =0; y<height; y++){

        //      Y(y,x) = clipToJpegRange( round(0.299 * (double)R(y,x) + 0.587 * (double)G(y,x) + 0.114 * (double)B(y,x)) );
        //      Cb(y,x) = clipToJpegRange( round(-0.1687 * (double)R(y,x)  - 0.3313 * (double)G(y,x) + 0.5 * (double)B(y,x) + 128) );
        //      Cr(y,x) = clipToJpegRange( round(0.5 * (double)R(y,x) - 0.4187 * (double)G(y,x) - 0.0813 * (double)B(y,x) + 128) );
        Rcpp::IntegerVector YCbCr = RGBtoYCbCr_cpp(R(y,x), G(y,x), B(y,x));
        Y(y,x) = YCbCr[0];
        Cb(y,x) = YCbCr[1];
        Cr(y,x) = YCbCr[2];
      }
    }

    // rename channel names
    Rcpp::CharacterVector names = modifiedImage.names();
    names[modifiedImage.findName("red")] = "Y";
    names[modifiedImage.findName("green")] = "Cb";
    names[modifiedImage.findName("blue")] = "Cr";

    // overwrite with converted values
    modifiedImage["Y"] = Y;
    modifiedImage["Cb"] = Cb;
    modifiedImage["Cr"] = Cr;
  }


  return modifiedImage;
}






//' Image conversion from YCbCr to RGB
//'
//' Converts an image from YCbCr colorspace to RGB.
//' Requires that all three channels have the same resolution.
//'
//' @param image list of matrices with Y, Cb and Cr values
//'
//' @return list of matrices with R, G and B values
//'
//' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
//'
// [[Rcpp::export]]
Rcpp::List YCbCrtoRGB_image_cpp(Rcpp::List image) {

  // copy image to leave R object untouched
  Rcpp::List modifiedImage(clone(image));

  int numChannel = modifiedImage["Channel"];

  // width and height of image
  Rcpp::IntegerVector resolution = modifiedImage["Resolution"];
  int width = resolution["width"];
  int height= resolution["height"];


  if(numChannel==1){
    // simply rename channel and enforce jpeg range

    Rcpp::IntegerMatrix Y = modifiedImage["Y"];
    for(int x =0; x<width; x++){
      //for every row
      for(int y =0; y<height; y++){
        Y(y,x) = clipToJpegRange(Y(y,x));
      }
    }

    Rcpp::CharacterVector names = modifiedImage.names();
    names[modifiedImage.findName("Y")]="grey";


  } else if(numChannel==3) {
    // convert to RGB

    // original values
    Rcpp::IntegerMatrix Y = modifiedImage["Y"];
    Rcpp::IntegerMatrix Cb = modifiedImage["Cb"];
    Rcpp::IntegerMatrix Cr = modifiedImage["Cr"];

    // converted values
    Rcpp::IntegerMatrix R(height, width);
    Rcpp::IntegerMatrix G(height, width);
    Rcpp::IntegerMatrix B(height, width);

    // for every column
    for(int x =0; x<width; x++){
      //for every row
      for(int y =0; y<height; y++){

        //      R(y,x) = clipToJpegRange( round( (double)Y(y,x) + 1.402 * ( (double)Cr(y,x)-128) ) );
        //      G(y,x) = clipToJpegRange( round( (double)Y(y,x) - 0.34414 * ( (double)Cb(y,x) - 128 ) - 0.71414 * ( (double)Cr(y,x)-128) ) );
        //      B(y,x) = clipToJpegRange( round( (double)Y(y,x) + 1.772 * ( (double)Cb(y,x) - 128) ) );
        Rcpp::IntegerVector RGB = YCbCrtoRGB_cpp(Y(y,x), Cb(y,x), Cr(y,x));
        R(y,x) = RGB[0];
        G(y,x) = RGB[1];
        B(y,x) = RGB[2];
      }
    }

    // rename channel names
    Rcpp::CharacterVector names = modifiedImage.names();
    names[modifiedImage.findName("Y")] = "red";
    names[modifiedImage.findName("Cb")] = "green";
    names[modifiedImage.findName("Cr")] = "blue";

    // overwrite with converted values
    modifiedImage["red"] = R;
    modifiedImage["green"] = G;
    modifiedImage["blue"] = B;
  }

  return modifiedImage;
}
