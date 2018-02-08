
#' Discrete Cosine Transformation
#' 
#' Performs a Discrete Cosine Transformation on a vector or matrix.
#' 
#' @param x vector or matrix to be transformed
#' @param inverted boolean whether the inverse operation should be performed
#' @param DCTscaling whether DCT scaling should be applied
#' 
#' @return transformed values
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @examples
#' x = c(0,64,128,191,255,255,128,128)
#' round(DCT(DCT(x), inverted=TRUE)) == x
#' 
#' matrix = matrix(1:64,8,8)
#' round(IDCT(DCT(matrix)))==matrix
#' 
#' @export
DCT = function(x, inverted=F, DCTscaling=F){
  if(class(x)=="jpegImage"){
    DCTimage_cpp(image=x, inverted = inverted, DCTscaling = DCTscaling)
  } else if(is.matrix(x) || is.data.frame(x)){
    DCT2d_cpp(x = x, inverted = inverted)
  } else if(is.vector(x)){
    DCT1d_cpp(x = x, inverted = inverted)
  } 
}


#' Inverse Discrete Cosine Transformation
#' 
#' Performs the inverse of a Discrete Cosine Transformation on a vector or matrix. 
#' Serves as a simple wrapper to the DCT function, but with the inverse parameter set to TRUE.
#' 
#' @param x vector or matrix to be transformed
#' @param DCTscaling whether DCT scaling should be applied
#' 
#' @return transformed values
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @examples
#' x = c(0,64,128,191,255,255,128,128)
#' round(IDCT(DCT(x))) == x
#' 
#' matrix = matrix(1:64,8,8)
#' round(IDCT(DCT(matrix)))==matrix
#' 
#' @export
IDCT = function(x, DCTscaling=F){
  DCT(x = x, inverted=T, DCTscaling = DCTscaling)
}


#' Discrete Cosine Transformation using serialized notation
#' 
#' Performs the 2D Discrete Cosine Transformation on a matrix where the blocks are serialized as columns
#' 
#' @param x vector or matrix where columns are the serialized blocks
#' @param inverted whether the inverse operation should be performed
#' 
#' @return transformed values
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @examples
#' x = matrix(c(1:64),8)
#' 
#' DCT_serialized(x)
#' 
#' @export
DCT_serialized = function(x, inverted=F){
  
  n = nrow(x)
  
  a2d = (kronecker(kronecker(matrix(1,n,1), DCT1dmatrix_cpp(n)), matrix(1,1,n)) * kronecker(kronecker(matrix(1,1,n), DCT1dmatrix_cpp(n)), matrix(1,n,1)))
  
  if(inverted){
    a2d = t(a2d)
  }
  
  result = a2d %*% matrix(as.vector(x), ncol=1)
  
  return(matrix(result, nrow = n, byrow = T))
}



#' Inverse Discrete Cosine Transformation using serialized notation
#' 
#' Wrapper for the DCT_serialized method, with inverse set to true. 
#' Performs the nverse 2D Discrete Cosine Transformation on a matrix where the blocks are serialized as columns
#' 
#' @param x vector or matrix where columns are the serialized DCT coefficients
#' 
#' @return values in the spatial domain
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @examples
#' x = matrix(c(1:64),8)
#' x = DCT_serialized(x)
#' 
#' IDCT_serialized(x)
#' 
#' @export
IDCT_serialized = function(x){
  DCT_serialized(x, inverted = T)
}






DCT_serialized_coefficients = function(x, inverted=F){
  
  n = nrow(x)
  
  a2d = (kronecker(kronecker(matrix(1,n,1), DCT1dmatrix_cpp(n)), matrix(1,1,n)) * kronecker(kronecker(matrix(1,1,n), DCT1dmatrix_cpp(n)), matrix(1,n,1)))
  
  if(inverted){
    a2d = t(a2d)
  }
  
  do.call(rbind, lapply(1:nrow(a2d), function(y){
      a2d[y,] * as.vector(x)
  }))
}


IDCT_serialized_coefficients = function(x){
  DCT_serialized_coefficients(x = x, inverted = T)
}


#' 2D DCT matrix using serialized notation
#' 
#' Returns the 2D DCT matrix used for the serialized notation.
#' 
#' @param n number of samples
#' @param inverse logical whether inverse matrix should be returned
#' 
#' @return 2D DCT matrix
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
#' @examples
#' 
#' a2d_serialized(8)
#' 
#' @export
a2d_serialized = function(n=8, inverse=F){

  a2d = (kronecker(kronecker(matrix(1,n,1), DCT1dmatrix_cpp(n)), matrix(1,1,n)) * kronecker(kronecker(matrix(1,1,n), DCT1dmatrix_cpp(n)), matrix(1,n,1)))
  
  if(inverse){
    a2d = t(a2d)
  }
  
  return(a2d)
}




## from Advanced Statistical Steganalysis
a1d_R = function(N, i, j){
  sqrt(2/N) * cos( ((2*j-1)*(i-1)*pi) / (2*N) ) * (1 + ((i==1) / 2) * (sqrt(2) - 2))
}


## from lecture
Dij_R = function(n, i, j){
  ifelse(j==1, 1/sqrt(2), 1) * sqrt(2/n) * cos(((2*i-1)*(j-1)*pi) / (2*n))
}


## assemble DCT matrix
DCT1dmatrix_R = function(N=8){
  M = matrix(0, N,N)
  for(i in 1:N){
    for(j in 1:N){
      M[i,j]=a1d_R(N,i,j) ## Dij(N,i,j)
    }
  }
  return(M)
}


## get DCT matrix as in Steganal package
DCT1dmatrix2_R <- function(N=8,offset=0){
  r <- matrix((1:(N-1)*pi)/(2*N),(N-1),N) * matrix(0:(N-1)*2+1,(N-1),N,byrow=T)
  sqrt(2/N) * rbind(rep(1/sqrt(2),N),cos(r+offset))
}


#' 1-Dimensional DCT
#' 
#' Performs a Discrete Cosine Transformation on a vector.
#' Inverted operation performs the same multiplication but with the transposed D matrix (same as inverted matrix).
#' 
#' @param x vector to be transformed
#' @param inverted boolean whether the inverse operation should be performed
#' 
#' @return transformed values
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
DCT1d_R = function(x, inverted=F){
  if(!inverted){
    return( as.vector(DCT1dmatrix_R(N = length(x))  %*% x))
  } else{
    return( as.vector(t(DCT1dmatrix_R(N = length(x)))  %*% x))
  }
}


#' 2-Dimensional DCT
#' 
#' Performs a Discrete Cosine Transformation on a matrix. First performs the one dimensional DCT row-wise, then column-wise
#' 
#' @param x matrix to be transformed
#' @param inverted boolean whether the inverse operation should be performed
#' 
#' @return transformed values
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
DCT2d_R = function(x, inverted=F){
  x = t(apply(x, 1, function(row){
    DCT1d_R(x = row, inverted = inverted)
  }))
  x = apply(x, 2, function(col){
    DCT1d_R(x = col, inverted = inverted)
  })
  return(x)
}




#' Discrete Cosine Transformation for Images
#' 
#' Performs a Discrete Cosine Transformation on each block of an image.
#' 
#' @param image and object of class jpegImage
#' @param inverted boolean whether the inverse operation should be performed
#' 
#' @return transformed image
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
#' 
DCTimage_R = function(image, inverted=F){
  index = which(names(image) %in% c("grey", "red", "green", "blue", "Y", "Cb", "Cr"))
  
  image[index] = lapply(image[index], function(channel){
    ## for every row list
    lapply(channel, function(outer){
      ## for every column block
      lapply(outer, function(inner){
        DCT2d_R(x=inner, inverted=inverted)
      })
    })
  })
  return(image)
}











#' dtt Package Discrete Cosine Transformation
#' 
#' Performs a Discrete Cosine Transformation on a vector using the implementation from the dtt package with an optional scaling
#' 
#' @param x vector to be transformed
#' @param scale whether a general scaling should be applied
#' @param inverted boolean whether the inverse operation should be performed
#' 
#' @return transformed values
#' 
#' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}, adapted from dtt package
#' 
DCT_dtt_R = function(x, scale=T, inverted=F){
  n = length(x)
  res=x
  if(!inverted){
    for (k in 0:(n - 1)) {
      res[k + 1] <- ifelse(scale, ifelse(k==0, 1/sqrt(2), 1) * sqrt(2/n), 1) * sum(x * cos(pi/n * ((0:(n - 1)) + 0.5) * k)) 
    }
  } else{
    for (k in 0:(n - 1)) {
      res[k + 1] <- (x[1]/ifelse(scale, sqrt(2), 2) + sum(x[2:n] * cos(pi/n * (1:(n - 1)) * (k + 0.5)))) * ifelse(scale,sqrt(2/n),(2/n))
    }
  }
  return(res)
}



