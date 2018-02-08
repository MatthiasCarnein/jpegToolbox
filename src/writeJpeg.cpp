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
#include "writeMemory.h"
#endif




void writeJpeg(jpeg_compress_struct cinfo, Rcpp::List image, int quality, int DCTmethod, Rcpp::IntegerVector subsampling, int force_baseline, int optimize_coding, int progressive, int smoothing, int fancyDownsampling){
  
  /* Step 3: set parameters for compression */
  
  // image width and height
  Rcpp::IntegerVector resolution = image["Resolution"];
  int width = resolution["width"];
  int height= resolution["height"];
  cinfo.image_width = width;  
  cinfo.image_height = height;
  
  // colorspace
  int numChannel = image["Channel"];
  cinfo.input_components = numChannel;
  if(numChannel==1){  
    cinfo.in_color_space = JCS_GRAYSCALE;
  } else if(numChannel==3){  
    cinfo.in_color_space = JCS_RGB;
  }
  
  
  // set default compression parameters
  jpeg_set_defaults( &cinfo );
  
  
  // set quality
  jpeg_set_quality(&cinfo,quality,force_baseline);
  
  // overwrite generated quality tables with provided ones
  if (quality < 0){
    Rcpp::List quantizationTables = image["Table"];
    unsigned int quant_tables[DCTSIZE2];
    for (int t = 0; t < quantizationTables.size(); t++){
      Rcpp::IntegerMatrix table = quantizationTables[t];
      for (int y = 0; y < DCTSIZE; y++){
        for (int x = 0; x < DCTSIZE; x++){
          quant_tables[y*DCTSIZE + x] = table(y,x);
        }
      }
      jpeg_add_quant_table (&cinfo, t, quant_tables, 100, force_baseline);
    }  
  }
  
  // optimize coding
  cinfo.optimize_coding = optimize_coding;
  
  // set smoothing factor (0...100)
  cinfo.smoothing_factor = smoothing;
  
  // write progressive jpeg file
  if (progressive){
    jpeg_simple_progression (&cinfo);
  }
  
  // fancy downsampling
  // fancy downsampling is not supported until libjpeg 7
  #if JPEG_LIB_VERSION >= 70
  cinfo.do_fancy_downsampling=fancyDownsampling;
  #endif
  
  // set subsampling method
  if(numChannel==3){
    cinfo.comp_info[0].h_samp_factor = subsampling[0];
    cinfo.comp_info[0].v_samp_factor = subsampling[1];
    cinfo.comp_info[1].h_samp_factor = subsampling[2];;
    cinfo.comp_info[1].v_samp_factor = subsampling[3];
    cinfo.comp_info[2].h_samp_factor = subsampling[4];
    cinfo.comp_info[2].v_samp_factor = subsampling[5];
  }
  
  // set DCT method
  switch (DCTmethod){
    case 1: cinfo.dct_method = JDCT_ISLOW; // Slow
    break;
    case 2: cinfo.dct_method = JDCT_IFAST; // Fast
    break;
    case 3: cinfo.dct_method = JDCT_FLOAT; // Float
    break;
    default:
    printf("DCT method unknown. I will use the default slow.");
  }
  
  /* Step 4: Start compressor */
  jpeg_start_compress( &cinfo, TRUE );
  
  unsigned long dataSize = width * height * numChannel;
  unsigned char *raw_image = (unsigned char *)malloc( dataSize );
  
  Rcpp::CharacterVector channelNames;
  
  if(numChannel==1){
    channelNames = CharacterVector::create("grey");
  } else if(numChannel==3){
    channelNames = CharacterVector::create("red", "green", "blue");
  }
  
  // write channels to vector
  for(int c=0; c < numChannel; c++){
    
    Rcpp::IntegerMatrix channel = image[Rcpp::as<std::string>(channelNames[c])];
    
    for( int y = 0 ; y < height ; y++ ){
      for( int x = 0 ; x < width ; x++ ) {
        raw_image[y * width * numChannel + x * numChannel + c] = channel(y, x);
        
      }
    }
  }  
  
  
  /* Step 5: while (scan lines remain to be written) */
  unsigned char *row_pointer[1];
  while(cinfo.next_scanline < cinfo.image_height){
    row_pointer[0] = &raw_image[ cinfo.next_scanline * width *  numChannel];
    jpeg_write_scanlines( &cinfo, row_pointer, 1 );
  }
  
  /* Step 6: Finish compression */
  jpeg_finish_compress( &cinfo );
  
  /* Step 7: release JPEG compression object */
  jpeg_destroy_compress( &cinfo );
  
  free(raw_image);
}




// [[Rcpp::export]]
void writeJpeg_file(Rcpp::List image, int quality, int DCTmethod, Rcpp::IntegerVector subsampling, int force_baseline, int optimize_coding, int progressive, int smoothing, int fancyDownsampling, std::string filename) {
  
  
  /* Step 1: allocate and initialize JPEG compression object */
  struct jpeg_compress_struct cinfo;
  struct jpeg_error_mgr err;
  
  // set up the error handler
  cinfo.err = jpeg_std_error(&err);
  
  // initialize the JPEG compression object
  jpeg_create_compress(&cinfo);
  
  
  
  /* Step 2: specify data destination */
  const char *_filename = filename.c_str();
  FILE *outfile = fopen( _filename, "wb" );
  
  if (!outfile){
    printf("Cannot open file!");
    return;
  }
  
  // use file
  jpeg_stdio_dest(&cinfo, outfile);
  
  
  writeJpeg(cinfo, image, quality, DCTmethod, subsampling, force_baseline, optimize_coding, progressive, smoothing, fancyDownsampling);
  
  // close file
  fclose( outfile );
  
}


// [[Rcpp::export]]
Rcpp::RawVector writeJpeg_memory(Rcpp::List image, int quality, int DCTmethod, Rcpp::IntegerVector subsampling, int force_baseline, int optimize_coding, int progressive, int smoothing, int fancyDownsampling) {
  
  /* Step 1: allocate and initialize JPEG compression object */
  struct jpeg_compress_struct cinfo;
  struct jpeg_error_mgr err;
  
  // set up the error handler
  cinfo.err = jpeg_std_error(&err);
  
  // initialize the JPEG compression object
  jpeg_create_compress(&cinfo);
  
  
  /* Step 2: specify data destination */
  unsigned char *mem = NULL;
  unsigned long mem_size = 0;
  
  // use memorylocation
  jpeg_mem_dest(&cinfo, &mem, &mem_size);
  
  writeJpeg(cinfo, image, quality, DCTmethod, subsampling, force_baseline, optimize_coding, progressive, smoothing, fancyDownsampling);
  
  // return result as raw vector
  Rcpp::RawVector raw( mem_size ) ;
  std::copy( mem, mem+mem_size, raw.begin()) ;
  
  delete mem;
  
  return raw;
  
}




