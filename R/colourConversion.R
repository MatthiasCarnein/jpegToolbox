#' RGB to YCbCr conversion in R
#'
#' Converts from YCbCr colour space to RGB
#'
#' @param RGB vector or list of matrices with R, G and B values
#'
#' @return Vector or list of matrices with Y, Cb and Cr values
#'
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#'
#' @examples
#'
#' RGBtoYCbCr(c(60,105,24))
#'
#' @export
RGBtoYCbCr = function(RGB){
  if(class(RGB)=="jpegImage"){
    return(RGBtoYCbCr_image_cpp(RGB))
  } else{
#     YCbCr = as.integer(matrix(c(0.299, -0.1687,0.5, 0.587,-0.3313,-0.4187, 0.114,0.5,-0.0813), ncol = 3) %*% RGB + c(0,128,128)+0.5)
#     return(sapply(YCbCr, clipToJpegRange))
    RGBtoYCbCr_cpp(RGB[1], RGB[2], RGB[3])
  }
}



#' YCbCr to RGB conversion in R
#'
#' Converts from RGB colour space to YCbCr
#'
#' @param YCbCr Vector or list of matrices with Y, Cb and Cr values
#'
#' @return vector or list of matrices R, G and B values
#'
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#'
#' @examples
#' YCbCr = RGBtoYCbCr(c(60,105,24))
#' YCbCrtoRGB(YCbCr)
#'
#'
#' @export
YCbCrtoRGB = function(YCbCr){
  if(class(YCbCr)=="jpegImage"){
    YCbCrtoRGB_image_cpp(YCbCr)
  } else{
#     RGB = as.integer(matrix(c(1,1,1, 0,-0.34414,1.772,1.402,-0.71414,0), ncol = 3) %*% (YCbCr - c(0,128,128))+0.5)
#     return(sapply(RGB, clipToJpegRange))
    YCbCrtoRGB_cpp(YCbCr[1], YCbCr[2], YCbCr[3])
  }
}


#' R Image conversion from RGB to YCbCr
#'
#' Converts an image from RGB colour space to YCbCr.
#' Requires that all three channels have the same resolution.
#' Much slower than the C implementation.
#'
#' @param block list of matrices with R, G and B values
#'
#' @return list of matrices with Y, Cb and Cr values
#'
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#'
RGBtoYCbCr_image_R = function(block){
  YCbCr = mapply(function(red,green,blue){
    RGBtoYCbCr(c(red, green, blue))
  }, block$red, block$green, block$blue)

  result = apply(YCbCr, 1, function(x){
    matrix(as.list(x),nrow(block$red),ncol(block$red))
  })
  result = lapply(result, function(x){
    apply(x,c(1,2), as.integer)
  })
  names(result)=c("Y", "Cb", "Cr")
  return(result)
}



#' R Image conversion from YCbCr to RGB
#'
#' Converts an image from YCbCr colour space to RGB.
#' Requires that all three channels have the same resolution.
#' Much slower than the C implementation.
#'
#' @param block list of matrices with Y, Cb and Cr values
#'
#' @return list of matrices with R, G and B values
#'
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#'
YCbCrtoRGB_image_R = function(block){
  RGB = mapply(function(Y,Cb,Cr){
    YCbCrtoRGB(c(Y, Cb, Cr))
  }, block$Y, block$Cb, block$Cr)

  result = apply(RGB, 1, function(x){
    matrix(as.list(x),nrow(block$Y),ncol(block$Y))
  })
  result = lapply(result, function(x){
    apply(x,c(1,2), as.integer)
  })
  names(result)=c("red","green","blue")
  return(result)
}
