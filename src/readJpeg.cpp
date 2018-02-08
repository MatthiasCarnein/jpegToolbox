#include <Rcpp.h>

using namespace Rcpp;

//#define HAVE_BOOLEAN
//#ifndef HAVE_BOOLEAN
//typedef int boolean;
//#endif
//#ifndef FALSE    	/* in case these macros already exist */
//#define FALSE	0		/* values of boolean */
//#endif
//#ifndef TRUE
//#define TRUE	1
//#endif

extern "C" {
  #include <jpeglib.h> 
  #include <jerror.h> 
}

#if JPEG_LIB_VERSION < 70
#include "readMemory.h"
#endif



Rcpp::List readJpeg(jpeg_decompress_struct cinfo, int fancyUpsampling){
  
  /* Step 3: read file parameters with jpeg_read_header() */
  jpeg_read_header(&cinfo, TRUE);
  
  /* Step 4: set parameters for decompression */
  
  // use fancy upsampling
  if (fancyUpsampling){
    cinfo.do_fancy_upsampling = TRUE;
  } else {
    cinfo.do_fancy_upsampling = FALSE;
  }
  
  /* Step 5: Start decompressor */
  jpeg_start_decompress( &cinfo );
  
  // get image parameters
  int width = cinfo.output_width;
  int height = cinfo.output_height;
  int numChannel = cinfo.num_components; // 1 = greyscale, 3 = RGB, 4 = RGBA
  
  
  // read quantization tables
  int numQuantTables = numChannel == 3 ? 2 : 1; 
  Rcpp::List quantizationTables;
  Rcpp::CharacterVector tableNames = CharacterVector::create("luminance", "chrominance");
  
  for (int t = 0; t < numQuantTables; t++){
    Rcpp::IntegerMatrix table(DCTSIZE, DCTSIZE);
    for (int y = 0; y < DCTSIZE; y++){
      for (int x = 0; x < DCTSIZE; x++){
        table(y,x) = cinfo.quant_tbl_ptrs[t]->quantval[y*DCTSIZE + x];
      }
    }
    quantizationTables[Rcpp::as<std::string>(tableNames[t])] = table;
  }
  
  
  /* Step 6: while (scan lines remain to be read) */
  unsigned long dataSize = width * height * numChannel;
  unsigned char *raw_image = (unsigned char *)malloc( dataSize );
  unsigned char *row_pointer[1];
  
  while(cinfo.output_scanline < cinfo.output_height){
    row_pointer[ 0 ] = raw_image + cinfo.output_scanline * width * numChannel;
    jpeg_read_scanlines( &cinfo, row_pointer, 1 );
  }
  
  
  // read pixel values
  Rcpp::List result;
  Rcpp::IntegerVector subsampling(2*numChannel);
  Rcpp::List blocks;
  Rcpp::CharacterVector channelNames;
  
  if(numChannel==1){
      channelNames = CharacterVector::create("grey");
  } else if(numChannel==3){
      channelNames = CharacterVector::create("red", "green", "blue");
  }
  
  Rcpp::CharacterVector blockNames = CharacterVector::create("Y", "Cb", "Cr");
  
  // for every channel
  for(int c=0; c< numChannel; c++){
    Rcpp::IntegerMatrix channel(height, width);    
    
    jpeg_component_info* compptr = cinfo.comp_info + c;
    
    subsampling[2*c] = compptr->h_samp_factor;
    subsampling[2*c+1] = compptr->v_samp_factor; 
    
    blocks[Rcpp::as<std::string>(blockNames[c])] = Rcpp::IntegerVector::create(Rcpp::Named("width") = compptr->width_in_blocks, Rcpp::Named("height") =  compptr->height_in_blocks);
    
    // for every row
    for( int y = 0 ; y < height ; y++ ){
      // for every column
      for( int x = 0 ; x < width ; x++ ) {
        channel(y, x) = raw_image[y * width * numChannel + x * numChannel + c];
      }
    }
    result[Rcpp::as<std::string>(channelNames[c])] = channel; 
  }
  
  // compose result
  result["Table"] = quantizationTables;
  result["Channel"] = numChannel;
  result["Resolution"] = Rcpp::IntegerVector::create(Rcpp::Named("width") = width, Rcpp::Named("height") =  height);
  result["Subsampling"] = subsampling;
  result["Blocks"] = blocks;
  
  result.attr("class") = "jpegImage";
  
  /* Step 7: Finish decompression */
  jpeg_finish_decompress(&cinfo);  
  
  /* Step 8: Release JPEG decompression object */
  jpeg_destroy_decompress(&cinfo);
  
  free(raw_image);
  
  return result;
}





// [[Rcpp::export]]
Rcpp::List readJpeg_file(std::string filename, int fancyUpsampling){
  
  
  /* Step 1: allocate and initialize JPEG decompression object */
  struct jpeg_decompress_struct cinfo;
  struct jpeg_error_mgr err;
  
  // set up the error handler
  cinfo.err = jpeg_std_error( &err );
  
  // initialize the JPEG decompression object
  jpeg_create_decompress( &cinfo );
  
  
  /* Step 2: specify data source */
  const char *_filename = filename.c_str();
  FILE *file = fopen( _filename, "rb" );
  
  // if file empty, return
  if ( file == NULL ){
    printf("Cannot open file!");
    return 0;
  }
  
  // use file as source
  jpeg_stdio_src( &cinfo, file );
  
  Rcpp::List result = readJpeg(cinfo, fancyUpsampling);
  
  fclose(file); 
  
  return result;
}





// [[Rcpp::export]]
Rcpp::List readJpeg_memory(Rcpp::RawVector rawImage, int fancyUpsampling) {
  
  /* Step 1: allocate and initialize JPEG decompression object */
  struct jpeg_decompress_struct cinfo;
  struct jpeg_error_mgr err;
  
  // set up the error handler
  cinfo.err = jpeg_std_error( &err );
  
  // initialize the JPEG decompression object
  jpeg_create_decompress( &cinfo );
  
  /* Step 2: specify data source */
  unsigned char* mem = new unsigned char[rawImage.size()];
  std::copy( rawImage.begin(), rawImage.end(), mem) ;
  unsigned long mem_size = rawImage.size();
  
  // use memory as source
  jpeg_mem_src(&cinfo, mem, mem_size);
  
  
  Rcpp::List result = readJpeg(cinfo, fancyUpsampling);
  
  delete[] mem;

  return result;
}



