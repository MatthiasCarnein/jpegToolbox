#' write jpeg Image
#' 
#' write a jpeg image to a file or memory location. If a filename is provided, it is written to a file, else to memory
#' 
#' @param image list of Integer Matrices as  pixel values
#' @param quality jpeg quality as integer between 0 and 100. If negative number is provided, the quantization tables from the original image are used.
#' @param DCTmethod DCT calculation method. Either slow, fast or float
#' @param subsampling Subsampling method as a string. Available options are 4:4:4 (1x1, 1x1, 1x1, full colour), 4:2:2 (2x1, 1x1, 1x1, 1/2 colour), 4:4:0 (1x2 1x1 1x1, 1/2 colour) and 4:2:0 (2x2, 1x1, 1x1, 1/4 colour), 4:1:1 (4x1 1x1 1x1, 1/4 colour) and 4:1:0 (4x2 1x1 1x1, 1/8 colour). Colons are optional and can be ommited. Alternatively a vector of the form c(H1, V1, H2, V2, H3, V3) to provide the vertical and horizontal subsampling factors can be used.
#' @param force_baseline if TRUE quantization table entries are constrained to 0...255 for full JPEG baseline compatibility
#' @param optimize_coding boolean whether parameters for entropy encoding should be optimization (smaller filesize, computationally more expensive)
#' @param progressive write progressive jpeg. Image chunks are stored in the file in an order that allows progressive image refinement but file size is slightly larger.
#' @param smoothing smoothing factor between 0 and 100 to smooth image artifacts. Image will become more blurry. Default 0.
#' @param fancyDownsampling boolean whether fancy (DCT) downscaling should be used
#' @param filename optional filename if file should be written to a file
#' 
#' @return If a filename is provided returns nothing. If image is written to memory, returns a raw vector.
#'   
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @export
writeJpeg = function(image, quality=100, DCTmethod="slow", subsampling="4:4:4", force_baseline=T, optimize_coding=T , progressive=T, smoothing=0, fancyDownsampling=T, filename){
  
  if(class(image)!="jpegImage"){
    image = toJpegImage(image)
  }
  
  if(!is.numeric(DCTmethod)){
    DCTmethod = which(DCTmethod==c("slow","fast","float"))
  }
  
  subsampling = getSubsampling(subsampling)
  
  if(!missing(filename)){
    ## write to file
    return(writeJpeg_file(image = image, quality = quality, DCTmethod = DCTmethod, subsampling = subsampling, force_baseline = force_baseline, optimize_coding = optimize_coding, progressive = progressive, smoothing = smoothing, fancyDownsampling = fancyDownsampling,  filename = filename))
    
  } else {
    ## write to memory
    return(writeJpeg_memory(image = image, quality = quality, DCTmethod = DCTmethod, subsampling = subsampling, force_baseline = force_baseline, optimize_coding = optimize_coding, progressive = progressive, smoothing = smoothing, fancyDownsampling = fancyDownsampling))
  }
  
}


getSubsampling = function(subsampling){
  
  if(is.character(subsampling)){
    subsampling = gsub(":", "", subsampling) ## remove dots
    
    if(subsampling=="444"){ ## none
      return(c(1,1,1,1,1,1))
    } else if(subsampling == "422"){ ## 1/2 colour
      return(c(2,1,1,1,1,1))
    } else if(subsampling == "440"){ ## 1/2 colour
      return(c(1,2,1,1,1,1))
    } else if(subsampling == "420"){ ## 1/4 colour
      return(c(2,2,1,1,1,1))
    } else if(subsampling == "411"){ ## 1/4 colour
      return(c(4,1,1,1,1,1))
    } else if(subsampling == "410"){ ## 1/8 colour
      return(c(4,2,1,1,1,1))
    } else { ## default 420
      return(c(2,2,1,1,1,1))
    }
  } else {
    return(subsampling)
  }
  
}




