#include <Rcpp.h>

using namespace Rcpp;

//#define HAVE_BOOLEAN
//#ifndef HAVE_BOOLEAN
//typedef int boolean;
//#endif
//#ifndef FALSE  		/* in case these macros already exist */
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




Rcpp::List readDCT(jpeg_decompress_struct cinfo, int dequantize){
  
  /* Step 3: read file parameters with jpeg_read_header() */
  jpeg_read_header(&cinfo, TRUE);
  
  /* Step 4: set parameters for decompression */
  /* not necessary */
  
  /* Step 5: Start reading coefficients */
  jvirt_barray_ptr *coeffs_array = jpeg_read_coefficients(&cinfo);
  
  // get image parameters
  int width = cinfo.output_width;
  int height = cinfo.output_height;
  int numChannels = cinfo.num_components; // 1 = greyscale, 3 = RGB, 4 = RGBA
  
  // read quantization tables
  int numQuantTables = numChannels == 3 ? 2 : 1; 
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
  
  Rcpp::List result;
  Rcpp::IntegerVector subsampling(2*numChannels);
  Rcpp::List blocks;
  Rcpp::CharacterVector channelNames = CharacterVector::create("Y", "Cb", "Cr");
  int cval;
  
  // for every channel
  for (int c = 0; c < numChannels; c++){
    JBLOCKARRAY buffer;
    JCOEFPTR blockpointer;
    jpeg_component_info* compptr = cinfo.comp_info + c;
    JQUANT_TBL * qtable = compptr->quant_table;
    
    subsampling[2*c] = compptr->h_samp_factor;
    subsampling[2*c+1] = compptr->v_samp_factor; 
    
    Rcpp::List DCTrow(compptr->height_in_blocks);
    
    blocks[Rcpp::as<std::string>(channelNames[c])] = Rcpp::IntegerVector::create(Rcpp::Named("width") = compptr->width_in_blocks, Rcpp::Named("height") =  compptr->height_in_blocks);
    
    // for every block row
    for (int block_row = 0; block_row < compptr->height_in_blocks; block_row++){
      buffer = (cinfo.mem->access_virt_barray)((j_common_ptr)&cinfo, coeffs_array[c], block_row, (JDIMENSION)1, FALSE);
      Rcpp::List DCTcolumn(compptr->width_in_blocks);
      
      // for every block column
      for (int block_col = 0; block_col < compptr->width_in_blocks; block_col++){
        
        Rcpp::IntegerMatrix DCTmatrix(DCTSIZE, DCTSIZE);
        blockpointer = buffer[0][block_col];
        
        // iterate block (8x8 matrix)
        for (int coefy = 0; coefy < DCTSIZE; coefy++){
          for (int coefx = 0; coefx < DCTSIZE; coefx++){
            cval = blockpointer[coefy * DCTSIZE + coefx];
            if (dequantize){
              cval *= qtable->quantval[coefy * DCTSIZE + coefx];
            }
            DCTmatrix(coefy, coefx) = cval;
            
          }
        }
        DCTcolumn[block_col] = DCTmatrix;
      }
      DCTrow[block_row] = DCTcolumn;
      
    }
    result[Rcpp::as<std::string>(channelNames[c])] = DCTrow;
  }
  
  // compose result
  result["Table"] = quantizationTables;
  result["Channel"] = cinfo.num_components;
  result["Resolution"] = Rcpp::IntegerVector::create(Rcpp::Named("width") = width, Rcpp::Named("height") =  height);
  result["Subsampling"] = subsampling;
  result["Blocks"] = blocks;
  
  result.attr("class") = "jpegImage";

  /* Step 7: Finish decompression */
  jpeg_finish_decompress(&cinfo); 
  
  /* Step 8: Release JPEG decompression object */
  jpeg_destroy_decompress(&cinfo);
  
  return result;
}



// List[[width]][[height]] of DCT matrices
// [[Rcpp::export]]
Rcpp::List readDCT_file(std::string filename, int dequantize){
  
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
  
  Rcpp::List result = readDCT(cinfo, dequantize);
  
  // close file
  fclose( file );
  
  return result;
}


// [[Rcpp::export]]
Rcpp::List readDCT_memory(Rcpp::RawVector rawImage, int dequantize){
  
  
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
    
  Rcpp::List result = readDCT(cinfo, dequantize);
  
  delete[] mem;
  
  return result;
  
}