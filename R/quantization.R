
#' Generate Quantization Table
#' 
#' Generates quantization tables used during jpeg compression.
#' 
#' @param quality quality factor between 0 and 100.
#' @param force_baseline if TRUE quantization table entries are constrained to 0...255 for full JPEG baseline compatibility
#' @param linear if TRUE, linear scaling is applied. If FALSE, the scaling recommended by libjpeg is used.
#' 
#' @return list of quantization tables for luminance and chrominance channel 
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @examples
#' \dontrun{
#' image = readJpeg("image.jpg")
#' sapply(0:100, function(quality){
#'   compressedImage = writeJpeg(image, quality, subsampling = "444")
#'     all(mapply(isIdentical, readJpeg(compressedImage)$Table, quantizationTable(quality)))
#' })
#' }
#' 
#' @export
quantizationTable = function(quality, force_baseline=T, linear=F){
  
  ## base matrices
  luminance = matrix(c(16,  11,  10,  16,  24,  40,  51,  61,
                       12,  12,  14,  19,  26,  58,  60,  55,
                       14,  13,  16,  24,  40,  57,  69,  56,
                       14,  17,  22,  29,  51,  87,  80,  62,
                       18,  22,  37,  56,  68, 109, 103,  77,
                       24,  35,  55,  64,  81, 104, 113,  92,
                       49,  64,  78,  87, 103, 121, 120, 101,
                       72,  92,  95,  98, 112, 100, 103,  99),8, byrow=T)
  
  
  chrominance = matrix(c(17,  18,  24,  47,  99,  99,  99,  99,
                         18,  21,  26,  66,  99,  99,  99,  99,
                         24,  26,  56,  99,  99,  99,  99,  99,
                         47,  66,  99,  99,  99,  99,  99,  99,
                         99,  99,  99,  99,  99,  99,  99,  99,
                         99,  99,  99,  99,  99,  99,  99,  99,
                         99,  99,  99,  99,  99,  99,  99,  99,
                         99,  99,  99,  99,  99,  99,  99,  99),8, byrow=T)
  
  
  if(!linear){
    ## arbitrary quality possible for linear, else enforce [0..100]
    if (quality > 100 || quality < 0){
      stop ("non-linear quality must be between 0 and 100")
    }
    
    ## The basic table is used as-is (scaling 100) for a quality of 50.
    # Qualities 50..100 are converted to scaling percentage 200 - 2*Q;
    # Qualities 1..50 are converted to scaling percentage 5000/Q.
    if (quality < 50){
      quality = trunc(5000 / quality)
    }  else {
      quality = trunc(200 - quality*2)
    }
  }
  
  
  result = list()
  result$luminance = luminance
  result$chrominance = chrominance
  
  result = lapply(result, function(x){
    
    ## scale
    x = trunc((x * quality + 50)/100)
    
    ## enforce limits
    x[x<1] = 1
    if (force_baseline){
      x[x>255] = 255 ## baseline compatibility
    } else {
      x[x>32767] = 32767 ## integer limit
    }
    return(x)
  })
  
  return(result)
}
