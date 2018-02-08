#' Jpeg Compression
#' 
#' Custom jpeg (lossy) compression routine. Uses an object of class jpegImage and jpeg compresses it
#' 
#' @param image image of class jpegImage to compress
#' @param quality jpeg quality as integer between 0 and 100. If negative number is provided, the quantization tables from the original image are used.
#' @param subsampling Subsampling method as a string. Available options are 4:4:4 (1x1, 1x1, 1x1, full colour), 4:2:2 (2x1, 1x1, 1x1, 1/2 colour), 4:4:0 (1x2 1x1 1x1, 1/2 colour) and 4:2:0 (2x2, 1x1, 1x1, 1/4 colour), 4:1:1 (4x1 1x1 1x1, 1/4 colour) and 4:1:0 (4x2 1x1 1x1, 1/8 colour). Colons are optional and can be ommited. Alternatively a vector of the form c(H1, V1, H2, V2, H3, V3) to provide the vertical and horizontal subsampling factors can be used.
#' @param force_baseline if TRUE quantization table entries are constrained to 0...255 for full JPEG baseline compatibility
#' @param linear boolean whether linear scaling of quantiuation tables should be sued
#' @param fancyDownsampling boolean whether fancy (DCT) downsampling should be used
#' @param libjpeg libjpeg version to use, determining the subsampling and upsampling algorithm
#' 
#' @return object of class jpegImage containing the compressed values
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @examples
#' \dontrun{
#' image = readJpeg("image.jpg")
#' compressedImage = compressJpeg(image, quality = 90, subsampling = "444")
#' }
#' 
#' @export
compressJpeg = function(image, quality=100, subsampling="4:4:4", force_baseline=T, linear=F, fancyDownsampling=T, libjpeg=c(6,8)){
  
  ## color conversion to YCbCr
  image = RGBtoYCbCr(RGB = image)
  
  ## subsample
  image = subsampling(image = image, subsampling = subsampling, fancyDownsampling = fancyDownsampling, libjpeg = libjpeg)
  
  ## if fancy and libjpeg 8, use DCT scaling
  if(fancyDownsampling && libjpeg == 8){
    h = image$Subsampling[1] / image$Subsampling[3]
    v = image$Subsampling[2] / image$Subsampling[4]
    blockheights=c(8, ifelse(v > 1, 16, 8), ifelse(v > 1, 16, 8))
    blockwidths=c(8, ifelse(h > 1, 16, 8), ifelse(h > 1, 16, 8))
  } else{
    blockheights=c(8,8,8)
    blockwidths=c(8,8,8)
  }

  ## block
  image = imageToBlocks(image = image, blockheights = blockheights, blockwidths = blockwidths)
  
  ## DCT
  image = DCT(x = image)
  
  ## quantize
  if(quality < 0){
    table = image$Table
  } else{
    table = quantizationTable(quality = quality, force_baseline = force_baseline, linear = linear)
  }
  image = quantize(image = image, quantizationTable = table)
  
  return(image)
}


#' Jpeg Decompression
#' 
#' Custom jpeg decompression routine. Uses a compressed image as an object of class jpegImage and decompresses it.
#' 
#' @param image image of class jpegImage to compress
#' @param fancyUpsampling boolean whether fancy (DCT) upsampling should be used
#' 
#' @return object of class jpegImage containing the decompressed values
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @examples
#' \dontrun{
#' image = readJpeg("image.jpg")
#' compressedImage = compressJpeg(image, quality = 90, subsampling = "444")
#' decompressedImage = decompressJpeg(compressedImage)
#' plot(decompressedImage)
#' }
#' 
#' @export
decompressJpeg = function(image, fancyUpsampling=T){
  
  ## dequantize
  image = dequantize(image = image)
  
  ## IDCT
  image = IDCT(x = image, DCTscaling = fancyUpsampling)
  
  ## unblock
  image = blocksToImage(image = image)
  
  ## upsample
  image = upsampling(image = image, fancyUpsampling = fancyUpsampling)
  
  ## color conversion to RGB
  image = YCbCrtoRGB(YCbCr = image)
  
  return(image)
}






#' calculate DCT coefficients
#' 
#' Calculates the DCT coefficients of a given image
#' 
#' @param image image of class jpegImage to compress
#' 
#' @return object of class jpegImage containing the DCT coefficients
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @examples
#' \dontrun{
#' image = readJpeg("image.jpg")
#' DCT = getDCT(image)
#' }
#' 
#' @export
getDCT = function(image){
  
  ## color conversion to YCbCr
  image = RGBtoYCbCr(RGB = image)

  ## block
  image = imageToBlocks(image = image)
  
  ## DCT
  image = DCTimage_cpp_plain(image = image)
  
  return(image)
}
