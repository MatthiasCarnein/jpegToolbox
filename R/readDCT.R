#' read DCT coefficients
#'
#' Read the DCT coefficients from an image
#' 
#' @param compressedImage either a filename or a raw vector of a jpg image
#' @param dequantize logical whether the DCT coefficients should be dequantized or not
#' 
#' @return object of class DCT containing the DCT values
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @export
readDCT = function(compressedImage, dequantize=F){
  if(is.character(compressedImage)){
    ## read from file
    return(readDCT_file(compressedImage, dequantize))
  } else {
    ## read from memory
    return(readDCT_memory(compressedImage, dequantize))
  }
}