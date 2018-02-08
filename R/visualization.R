#' Plot Image
#' 
#' Plots an image form greyscale matrix or rgb matrices
#' 
#' @param img list with either img$grey for greyscale images or img$red, img$green, img$blue for rgb image
#' @param xlab x label 
#' @param ylab y label
#' @param ... Additional graphical parameters
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
plotImage = function(img, xlab="", ylab="", ...){
  if(class(img)=="jpegImage"){
    height=img$Resolution["height"]
    width= img$Resolution["width"]
    if(img$Channel==1){
      col = grey(img$grey/255)
    } else if ((img$Channel==3)) {
      col = rgb(img$red/255,img$green/255,img$blue/255)
    }
  } else{
    colourspace = findColourspace(img)
    if(colourspace=="grey"){
      height = nrow(img$grey)
      width = ncol(img$grey)
      col = grey(img$grey/255)
    } else if(colourspace == "rgb"){
      height = nrow(img$red)
      width = ncol(img$red)
      col = rgb(img$red/255,img$green/255,img$blue/255)
    } else if(colourspace =="matrix"){
      height = nrow(img)
      width = ncol(img)
      col = grey(img/255)
    }
  }
  asp = height/width
  image(x=0:width,y=0:height,z=t(matrix(1:(width*height),ncol=width)), ylim=c(height,0), xlim=c(0,width), xlab=xlab, ylab=ylab, axes = FALSE, asp=asp, col=col, useRaster = T, ...)
}


#' Plot Image
#' 
#' Plots an image form greyscale matrix or rgb matrices
#' 
#' @param highlight matrix of logical values indicating the highlighted area
#' @param col colour of the highlighted area
#' @param blockheight height of the highlighted block
#' @param blockwidth width of the highlighted block
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @examples
#' \dontrun{
#' src="D:/Desktop/Uni-Muenster/Master/4. Semester/BossBase-1.01-cover/png/"
#' files = dir(path=src, full.names=T, pattern=".png")
#' image = readPNG(files[1])
#' image = PNGtoJpegImage(image)
#' 
#' temp = matrix(F, 64,64)
#' temp[10, 10]=T
#' highlightArea(temp)
#' }
#' 
#' @export
highlightArea = function(highlight, col=rgb(0,.5,.5), blockheight=8, blockwidth=8){
  indices = which(highlight, arr.ind=T)
  rect((indices[,2]-1)*blockwidth, (indices[,1]-1)*blockheight, indices[,2]*blockwidth, indices[,1]*blockheight, border = col)
}



#' Find Colourspace
#' 
#' Determine colorsapce based on the available channels
#' 
#' @param img image as list or jpegImage object
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
findColourspace = function(img){
  if (is.matrix(img)){
    ## if just a matrix
    return("matrix")
  } else if(!is.null(img$grey)){
    ## if greyscale
    return("grey")
  } else if(!is.null(img$red) && !is.null(img$green) && !is.null(img$blue)){
    ## if rgb
    return("rgb")
  } 
}



#' Plot Average and Range of Values
#' 
#' Plots the average of a given data frame as well as its range (min and max values) as a shadow.
#' 
#' @param data data frame with values to plot
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @examples
#' criticalValues = getCriticalValuesFromRandom_colour(10, 512, 512)
#' plotRange(criticalValues)
#' 
#' @export
plotRange = function(data){
  
  means = colMeans(data)
  maxs = apply(data,2,max)
  mins = apply(data,2,min)
    
  plot(x=c(1,ncol(data))-1,y=c(min(mins),max(maxs)), type="n", main="",xlab="number of compressions",ylab="ratio of stable blocks", ylim=c(0,1))
  
  rangecolor = rgb(30,144,255,alpha=80,maxColorValue=255)
  polygon(x=c(1:ncol(data)-1,rev((1:ncol(data))-1)),y=c(maxs,rev(means)),col=rangecolor,border=NA)
  polygon(x=c(1:ncol(data)-1,rev((1:ncol(data))-1)),y=c(mins,rev(means)),col=rangecolor,border=NA)
  
  lines(x=c(1:ncol(data))-1,y=means,col="black")
}