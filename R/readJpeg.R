#' read jpeg Image
#' 
#' read a jpeg image from a file or memory location as matrices of pixel values
#' 
#' @param compressedImage either a filename or a raw vector of a jpg image
#' @param fancyUpsampling logical  indicating whether linear interpolation will be used to upsample reduced size chrominance information into a full-size spatial domain representation. Otherwise values are merely duplicated.
#' 
#' @return object of class jpegImage containing pixel values and additional image information
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @export
readJpeg = function(compressedImage, fancyUpsampling=T){
  if(is.character(compressedImage)){
    ## read from file
    return(readJpeg_file(compressedImage, fancyUpsampling))
  } else {
    ## read from memory
    return(readJpeg_memory(compressedImage, fancyUpsampling))
  }
}





