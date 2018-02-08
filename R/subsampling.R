#' Subsample image
#' 
#' Reduces the chrominance values of an image according to the provided subsampling factors.
#' 
#' @param image list of integer matrix of pixel values, typically an object of class jpegImage
#' @param subsampling Subsampling method as a string. Available options are 4:4:4 (1x1, 1x1, 1x1, full colour), 4:2:2 (2x1, 1x1, 1x1, 1/2 colour), 4:4:0 (1x2 1x1 1x1, 1/2 colour) and 4:2:0 (2x2, 1x1, 1x1, 1/4 colour), 4:1:1 (4x1 1x1 1x1, 1/8 colour), 4:1:0 (4x2 1x1 1x1, 1/8 colour). Colons are optional and can be ommited. Alternatively a vector of the form c(H1, V1, H2, V2, H3, V3) to provide the vertical and horizontal subsampling factors can be used.
#' @param fancyDownsampling Boolean whether DCT scaling should be used
#' @param libjpeg libjpeg version, either 6 or 8
#' 
#' @return image with subsampled Cb and Cr channels
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @export
subsampling = function(image, subsampling="4:4:4", fancyDownsampling=T, libjpeg=8){
  if(missing(subsampling) && !is.null(image$Subsampling)){
    subsampling = image$Subsampling
  } else{
    subsampling = getSubsampling(subsampling)
  }
  if(image$Channel==1){
    ## nothing to subsample
    result = image
    result$Subsampling = c(1,1)
  } else if(image$Channel==3){
    if(fancyDownsampling && libjpeg==8){
      result = subsampling_fancy8(image, subsampling)
    } else{
      result = subsampling_simple(image, subsampling)
    }
  }
  return(result)
}


#' Upsample image
#' 
#' Extrapolate the reduced chrominance values of an image according to the provided subsampling factors in order to reconstruct the original image.
#' 
#' @param image list of integer matrix of pixel values, typically an object of class jpegImage
#' @param fancyUpsampling logical whether to use fancy upsampling
#' @param libjpeg libjpeg version, either 6 or 8
#' 
#' @return image with upsampled Cb and Cr channels
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @export
upsampling = function(image, fancyUpsampling=T, libjpeg=8){
  
  if(image$Channel==1){
    ## nothing to upsample
    result = image
  } else if(image$Channel==3){
    if(fancyUpsampling && libjpeg == 8){
      result = upsampling_fancy8(image)
    } else if(fancyUpsampling && libjpeg==6){
      result = upsampling_fancy6(image)
    } else{
      result = upsampling_simple(image)
      
    }
  }
  
  return(result)
}
