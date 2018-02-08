#' Print Function for jpegImages
#' 
#' prints a summary of an object of class jpegImage
#' 
#' @param image image object of class jpegImage
#' @param ... additional parameters
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @export
print.jpegImage <- function(image,...){
  cat("Jpeg Image \n----------\n")
  if(!is.null(image$Channel)){
    cat("Channel     : ", image$Channel, "\n", sep="");
    cat("Colour      : ", sep="")
    cat(names(image)[1:image$Channel], sep=", ")
    cat("\n", sep="")
    
  }
  if(!is.null(image$Tables)){
    cat("Tables      : ", sep="")
    cat(names(image$Tables), sep=", ")
    cat("\n", sep="")
  }
  if(!is.null(image$Resolution)){
    cat("Resolution  : ", image$Resolution["width"],"x", image$Resolution["height"], "\n", sep="");
  }
  if(!is.null(image$Subsampling) & !is.null(image$Channel)){
    cat("Subsampling : ", sep="");
    if(image$Channel==3){
      J=4
      a=1/(image$Subsampling[1] / image$Subsampling[3])*4
      b=ifelse(image$Subsampling[2] / image$Subsampling[4]==1, a, 0)
      
      cat(c(J,a,b), sep=":")
      cat(" (")
      cat(sapply(seq(1,2*image$Channel,2), function(x){paste(image$Subsampling[x:(x+1)], collapse = "x")}), sep=" ")
      cat(")")
    } else {
      cat("1x1")
    }
    cat("\n")
  }
  if(!is.null(image$Blocks)){
    cat("Blocks      : ", sep="");
    cat(paste(names(image$Blocks), sapply(seq(1,2*image$Channel,2), function(x){paste(unlist(image$Blocks)[x:(x+1)], collapse = "x")}), sep=": "), sep=", ")
    cat("\n")
  }
}



#' Plot Function for jpegImages
#' 
#' plots an object of class jpegImage
#' 
#' @param image object of class jpegImage
#' @param xlab optional label of x axis
#' @param ylab optional label of y axis
#' @param ... additional plot parameters
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @export
plot.jpegImage = function(image, xlab="", ylab="",...){
  plotImage(image, xlab="", ylab="", ...)
}




#' Constructor for jpegImage
#' 
#' Constructs and object of class jpegImage
#' 
#' @param grey matrix of greyscale pixel values
#' @param red matrix of red channel of  pixel values
#' @param green matrix of green channel of  pixel values
#' @param blue matrix of blue channel of  pixel values
#' @param Table Quantization Tables
#' @param Channel Number of Channels
#' @param Resolution Resolution of image
#' @param Subsampling subsampling method
#' 
#' @return object of class jpegImage with the provided information
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @export
jpegImage = function(grey, red, green, blue, Table, Channel, Resolution, Subsampling){
  image = list()
  
  if(!missing(grey)){
    image$grey = grey
    image$Resolution = c(width=ncol(grey), height=nrow(grey))
    image$Channel=1
  }
  
  if(!missing(red) && !missing(green) && !missing(blue)){
    image$red=red
    image$green=green
    image$blue=blue
    image$Resolution = c(width=ncol(red), height=nrow(red))
    image$Channel=3
  }
  
  if(!missing(Table)){
    image$Table = Table
  } else{
    image$Table = NA
  }
  
  if(!missing(Channel)){
    image$Channel = Channel
  }
  
  if(!missing(Resolution)){
    image$Resolution = Resolution
  }
  
  if(!missing(Subsampling)){
    image$Subsampling = Subsampling
  } else{
    image$Subsampling = NA
  }
  
  class(image)="jpegImage"
  return(image)
}



#' to Jpeg Image
#' 
#' Converts a list or matrix into an object of class jpegImage
#' 
#' @param image either a list with color values grey or red green and blue or a single matrix
#' 
#' @return image as a jpegImage object
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @export
toJpegImage = function(image){
  if(is.matrix(image)){
    image = jpegImage(grey=image)
  } else if(is.list(image)){
    if(!is.null(image$grey)){
      image = jpegImage(grey=image$grey)
    } else if (!is.null(image$red) && !is.null(image$green) && !is.null(image$blue)){
      image = jpegImage(red = image$red, green = image$green, blue = image$blue)
    }
  }
}


#' Convert PNG to JpegImage
#' 
#' Converts a loaded PNG file using the png package into the internal class jpegImage
#' 
#' @param image loaded image as returned from the png package
#' 
#' @return image as a jpegImage object
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @examples
#' \dontrun{
#' library(png)
#' image = readPng("file.png")
#' image = PNGtoJpegImage(image)
#' }
#' 
#' @export
PNGtoJpegImage = function(image){
  image = image*255
  if(length(dim(image))==3){
    image = jpegImage(red=image[,,1], green=image[,,2], blue=image[,,3])
  } else{
    image = jpegImage(grey=image)
  }
  return (image)
}


#' Convert JpegImage to PNG
#' 
#' Converts an object of the class jpegImage to an object processable by the png package
#' 
#' @param image object of class jpegImage
#' 
#' @return image as a PNG object
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @export
JpegImagetoPNG = function(image){
  if(image$Channel==3){
    png = array(dim = c(image$Resolution["height"],image$Resolution["width"],3))
    png[,,1] = image$red/255
    png[,,2] = image$green/255
    png[,,3] = image$blue/255
  } else{
    png = image$grey/255
  }
  return(png)
}


#' Convert PMM to JpegImage
#' 
#' Converts a loaded PMM file using the pixmap package into the internal class jpegImage
#' 
#' @param image loaded image as returned from the pixmap package
#' 
#' @return image as a jpegImage object
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @examples
#' \dontrun{
#' library(pixmap)
#' image = getChannels(read.pnm(file = "file.PPM"), colors="all")
#' image = PMMtoJpegImage(image)
#' }
#' 
#' @export
PMMtoJpegImage = function(image){
  image = image*255
  if(length(dim(image))==3){
    image = jpegImage(red=image[,,1], green=image[,,2], blue=image[,,3])
  } else{
    image = jpegImage(grey=image)
  }
  return (image)
}


#' Convert TIFF to JpegImage
#' 
#' Converts a loaded TIFF file using the tiff package into the internal class jpegImage
#' 
#' @param image loaded image as returned from the tiff package
#' 
#' @return image as a jpegImage object
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @examples
#' \dontrun{
#' library(tiff)
#' image = readTIFF("file.tif")
#' image = TIFFtoJpegImage(image)
#' }
#' 
#' @export
TIFFtoJpegImage = function(image){
  image=image*255
  if(length(dim(image))==3){
    image = jpegImage(red=image[,,1], green=image[,,2], blue=image[,,3])
  } else{
    image = jpegImage(grey=image)
  }
  return (image)
}


#' @export
JpegImagetoTIFF = function(image){
  if(image$Channel==3){
    tiff = array(dim = c(image$Resolution["height"],image$Resolution["width"],3))
    tiff[,,1] = image$red/255
    tiff[,,2] = image$green/255
    tiff[,,3] = image$blue/255
  } else{
    tiff = image$grey/255
  }
  return(tiff)
}