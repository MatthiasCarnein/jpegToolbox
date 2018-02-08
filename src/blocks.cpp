#include <Rcpp.h>
using namespace Rcpp;



//' Image To Block Matrix
//' 
//' Splits an image into its blocks of a given size (typically 8x8) and returns them as a nested list.
//' The list contains the blocks in a [[row]][[column]] pattern, i.e. the inner list contains the rows, the outer list the columns.
//' 
//' @param image list of integer matrix of pixel values
//' @param blockwidths vector containing width of the blocks per channel
//' @param blockheights vector containing height of the blocks per channel
//' 
//' @return the image  containing all 8x8 blocks in a matrix form
//' 
//' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List imageToBlocks(Rcpp::List image, Rcpp::IntegerVector blockheights=Rcpp::IntegerVector::create(8,8,8), Rcpp::IntegerVector blockwidths=Rcpp::IntegerVector::create(8,8,8)) {
  
  // copy image to leave R object untouched
  Rcpp::List modifiedImage(clone(image));
  
  int numChannel = modifiedImage["Channel"];
  
  // determine colour space
  Rcpp::CharacterVector channelNames;
  if( modifiedImage.containsElementNamed("Y")){
    channelNames = CharacterVector::create("Y", "Cb", "Cr");
  } else if ( modifiedImage.containsElementNamed("red")) {
    channelNames = CharacterVector::create("red", "green", "blue");
  } else if ( modifiedImage.containsElementNamed("grey")){
    channelNames = CharacterVector::create("grey");
  }
  
  
  // for every channel
  for(int c=0; c<numChannel; c++){
    
    Rcpp::IntegerMatrix channel = modifiedImage[Rcpp::as<std::string>(channelNames[c])];
    
    int blockheight = blockheights[c];
    int blockwidth = blockwidths[c];
    
    int block_rows = std::ceil((double)channel.nrow()/blockheight);
    int block_cols = std::ceil((double)channel.ncol()/blockwidth);
    
    Rcpp::List blocks(block_rows);
    
    // for every row
    for(int y=0; y<block_rows; y++){
      
      Rcpp::List columnBlocks(block_cols);
      
      // for every column
      for(int x=0; x<block_cols; x++){
        
        // iterate block and copy
        Rcpp::IntegerMatrix block(blockheight, blockwidth);
        for(int yi =0; yi < blockheight; yi++){
          for(int xi=0; xi < blockwidth; xi++){
            // padding
            if(y*blockheight+yi >= channel.nrow() && x*blockwidth+xi >= channel.ncol()){
              block(yi, xi) = channel(channel.nrow()-1, channel.ncol()-1); // if width and height exceeded, copy last value
            } else if(y*blockheight+yi >= channel.nrow()){
              block(yi, xi) = channel(channel.nrow()-1, xi); // if height exceeded, copy last value
            } else if (x*blockwidth+xi >= channel.ncol()){
              block(yi, xi) = channel(yi, channel.ncol()-1); // if width exceeded, copy last value
            } else{
              // copy
              block(yi, xi) = channel(y*blockheight+yi, x*blockwidth+xi);
            }
          }
        }
        // Alternatively use submatrix
        // Rcpp::IntegerMatrix block = channel(Range(y*blockheight, std::min(((y+1)*blockheight), channel.nrow())-1), Range(x*blockwidth, std::min(((x+1)*blockwidth), channel.ncol())-1));
        
        columnBlocks[x] = block; 
        
      }
      blocks[y] = columnBlocks;
    }
    modifiedImage[Rcpp::as<std::string>(channelNames[c])] = blocks;
  }
  
  return modifiedImage;
}



//' Block Matrix to Image Wrapper
//' 
//' Joins the blocks of an image to form the image again.
//' 
//' @param image list of integer matrix of pixel values
//' 
//' @return the merged image
//' 
//' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List blocksToImage(Rcpp::List image) {
  
  // copy image to leave R object untouched
  Rcpp::List modifiedImage(clone(image));
  
  int numChannel = modifiedImage["Channel"];
  
  Rcpp::IntegerVector resolution = modifiedImage["Resolution"];
  int width = resolution["width"];
  int height= resolution["height"];
  
  // determine colour space
  Rcpp::CharacterVector channelNames;
  if( modifiedImage.containsElementNamed("Y")){
    channelNames = CharacterVector::create("Y", "Cb", "Cr");
  } else if ( modifiedImage.containsElementNamed("red")) {
    channelNames = CharacterVector::create("red", "green", "blue");
  } else if ( modifiedImage.containsElementNamed("grey")){
    channelNames = CharacterVector::create("grey");
  }
  
  // for every channel
  for(int c=0; c<numChannel; c++){
    
    Rcpp::IntegerMatrix channel(height, width);
    Rcpp::List blocks = modifiedImage[Rcpp::as<std::string>(channelNames[c])];
    
    // for every row
    for(int y=0; y<blocks.size(); y++){
      
      Rcpp::List columnBlocks = blocks[y];
      
      // for every column
      for(int x=0; x<columnBlocks.size(); x++){
        
        Rcpp::IntegerMatrix block = columnBlocks[x];
        
        // iterate block and copy
        // padding is reversed here due to indexing
        for(int yi = 0; yi < block.nrow() && y*block.nrow()+yi < height; yi++){
          for(int xi= 0; xi < block.ncol() && x*block.ncol()+xi < width; xi++){
            channel(y*block.nrow()+yi, x*block.ncol()+xi) = block(yi, xi);
          }
        }        
      }
    }
    modifiedImage[Rcpp::as<std::string>(channelNames[c])] = channel;
  }
  
  return modifiedImage;
}




//' matrix of blocks to list of blocks
//' 
//' Transforms matrix of blocks to list of blocks
//' 
//' @param image list of list of integer matrices of pixel values
//' 
//' @return list of integer matrices
//' 
//' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List imageMatrixToList(Rcpp::List image){
  
  // copy image to leave R object untouched
  Rcpp::List modifiedImage(clone(image));
  
  int numChannel = modifiedImage["Channel"];
  
  // determine colour space
  Rcpp::CharacterVector channelNames;
  if( modifiedImage.containsElementNamed("Y")){
    channelNames = CharacterVector::create("Y", "Cb", "Cr");
  } else if ( modifiedImage.containsElementNamed("red")) {
    channelNames = CharacterVector::create("red", "green", "blue");
  } else if ( modifiedImage.containsElementNamed("grey")){
    channelNames = CharacterVector::create("grey");
  }
  
  // for every channel
  for(int c=0; c<numChannel; c++){
    
    Rcpp::List temp = modifiedImage[Rcpp::as<std::string>(channelNames[c])];
    int block_rows = temp.size();
    temp = temp[0];
    int block_cols = temp.size();
    
    Rcpp::List newBlocks(block_rows * block_cols);
    Rcpp::List blocks = modifiedImage[Rcpp::as<std::string>(channelNames[c])];
    
    // for every row
    for(int y=0; y<blocks.size(); y++){
      
      Rcpp::List columnBlocks = blocks[y];
      
      // for every column
      for(int x=0; x<columnBlocks.size(); x++){
        
        Rcpp::IntegerMatrix block = columnBlocks[x];
        newBlocks[y*columnBlocks.size()+x] = block;
        
      }
    }
    
    modifiedImage[Rcpp::as<std::string>(channelNames[c])] = newBlocks;
    
  }
  return(modifiedImage);
}


//' To Block List
//' 
//' Splits an image into its blocks of a given size (typically 8x8) and returns them as a list.
//' The list contains the blocks column-wise, i.e. the first column of blocks is outputted first.
//' 
//' @param image list of integer matrix of pixel values
//' @param blockwidth width of a block
//' @param blockheight height of a block
//' 
//' @return the image  containing all 8x8 blocks in a list form
//' 
//' @author Matthias Carnein \email{Matthias.Carnein@@uni-muenster.de}
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List imageToBlocks_list(Rcpp::List image, int blockheight=8, int blockwidth=8) {
  
  // copy image to leave R object untouched
  Rcpp::List modifiedImage(clone(image));
  
  int numChannel = modifiedImage["Channel"];
  
  // determine colour space
  Rcpp::CharacterVector channelNames;
  if( modifiedImage.containsElementNamed("Y")){
    channelNames = CharacterVector::create("Y", "Cb", "Cr");
  } else if ( modifiedImage.containsElementNamed("red")) {
    channelNames = CharacterVector::create("red", "green", "blue");
  } else if ( modifiedImage.containsElementNamed("grey")){
    channelNames = CharacterVector::create("grey");
  }
  
  // for every channel
  for(int c=0; c<numChannel; c++){
    
    Rcpp::IntegerMatrix channel = modifiedImage[Rcpp::as<std::string>(channelNames[c])];
    
    int block_rows = std::ceil((double)channel.nrow()/blockheight);
    int block_cols = std::ceil((double)channel.ncol()/blockwidth);
    
    Rcpp::List blocks(block_rows * block_cols);
    
    
    // for every block column
    for(int x=0; x<block_cols; x++){
      // for every block row
      for(int y=0; y<block_rows; y++){
        
        // iterate block and copy
        Rcpp::IntegerMatrix block(blockheight, blockwidth);
        for(int yi =0; yi < blockheight; yi++){
          for(int xi=0; xi < blockwidth; xi++){
            // padding
            if(y*blockheight+yi >= channel.nrow() && x*blockwidth+xi >= channel.ncol()){
              block(yi, xi) = channel(channel.nrow()-1, channel.ncol()-1); // if width and height exceeded, copy last value
            } else if(y*blockheight+yi >= channel.nrow()){
              block(yi, xi) = channel(channel.nrow()-1, xi); // if height exceeded, copy last value
            } else if (x*blockwidth+xi >= channel.ncol()){
              block(yi, xi) = channel(yi, channel.ncol()-1); // if width exceeded, copy last value
            } else{
              // copy
              block(yi, xi) = channel(y*blockheight+yi, x*blockwidth+xi);
            }
          }
        }
        blocks[x*block_rows + y] = block; 
        
      }  
    }
    modifiedImage[Rcpp::as<std::string>(channelNames[c])] = blocks;
  }
  
  return modifiedImage;
}