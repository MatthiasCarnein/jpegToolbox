// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// imageToBlocks
Rcpp::List imageToBlocks(Rcpp::List image, Rcpp::IntegerVector blockheights, Rcpp::IntegerVector blockwidths);
RcppExport SEXP _jpegToolbox_imageToBlocks(SEXP imageSEXP, SEXP blockheightsSEXP, SEXP blockwidthsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type image(imageSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type blockheights(blockheightsSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type blockwidths(blockwidthsSEXP);
    rcpp_result_gen = Rcpp::wrap(imageToBlocks(image, blockheights, blockwidths));
    return rcpp_result_gen;
END_RCPP
}
// blocksToImage
Rcpp::List blocksToImage(Rcpp::List image);
RcppExport SEXP _jpegToolbox_blocksToImage(SEXP imageSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type image(imageSEXP);
    rcpp_result_gen = Rcpp::wrap(blocksToImage(image));
    return rcpp_result_gen;
END_RCPP
}
// imageMatrixToList
Rcpp::List imageMatrixToList(Rcpp::List image);
RcppExport SEXP _jpegToolbox_imageMatrixToList(SEXP imageSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type image(imageSEXP);
    rcpp_result_gen = Rcpp::wrap(imageMatrixToList(image));
    return rcpp_result_gen;
END_RCPP
}
// imageToBlocks_list
Rcpp::List imageToBlocks_list(Rcpp::List image, int blockheight, int blockwidth);
RcppExport SEXP _jpegToolbox_imageToBlocks_list(SEXP imageSEXP, SEXP blockheightSEXP, SEXP blockwidthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type image(imageSEXP);
    Rcpp::traits::input_parameter< int >::type blockheight(blockheightSEXP);
    Rcpp::traits::input_parameter< int >::type blockwidth(blockwidthSEXP);
    rcpp_result_gen = Rcpp::wrap(imageToBlocks_list(image, blockheight, blockwidth));
    return rcpp_result_gen;
END_RCPP
}
// RGBtoYCbCr_cpp
Rcpp::IntegerVector RGBtoYCbCr_cpp(int R, int G, int B);
RcppExport SEXP _jpegToolbox_RGBtoYCbCr_cpp(SEXP RSEXP, SEXP GSEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type R(RSEXP);
    Rcpp::traits::input_parameter< int >::type G(GSEXP);
    Rcpp::traits::input_parameter< int >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(RGBtoYCbCr_cpp(R, G, B));
    return rcpp_result_gen;
END_RCPP
}
// YCbCrtoRGB_cpp
Rcpp::IntegerVector YCbCrtoRGB_cpp(int Y, int Cb, int Cr);
RcppExport SEXP _jpegToolbox_YCbCrtoRGB_cpp(SEXP YSEXP, SEXP CbSEXP, SEXP CrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type Y(YSEXP);
    Rcpp::traits::input_parameter< int >::type Cb(CbSEXP);
    Rcpp::traits::input_parameter< int >::type Cr(CrSEXP);
    rcpp_result_gen = Rcpp::wrap(YCbCrtoRGB_cpp(Y, Cb, Cr));
    return rcpp_result_gen;
END_RCPP
}
// RGBtoYCbCr_image_cpp
Rcpp::List RGBtoYCbCr_image_cpp(Rcpp::List image);
RcppExport SEXP _jpegToolbox_RGBtoYCbCr_image_cpp(SEXP imageSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type image(imageSEXP);
    rcpp_result_gen = Rcpp::wrap(RGBtoYCbCr_image_cpp(image));
    return rcpp_result_gen;
END_RCPP
}
// YCbCrtoRGB_image_cpp
Rcpp::List YCbCrtoRGB_image_cpp(Rcpp::List image);
RcppExport SEXP _jpegToolbox_YCbCrtoRGB_image_cpp(SEXP imageSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type image(imageSEXP);
    rcpp_result_gen = Rcpp::wrap(YCbCrtoRGB_image_cpp(image));
    return rcpp_result_gen;
END_RCPP
}
// kroneckerDelta
int kroneckerDelta(int i, int j);
RcppExport SEXP _jpegToolbox_kroneckerDelta(SEXP iSEXP, SEXP jSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    Rcpp::traits::input_parameter< int >::type j(jSEXP);
    rcpp_result_gen = Rcpp::wrap(kroneckerDelta(i, j));
    return rcpp_result_gen;
END_RCPP
}
// DCT1dmatrix_cpp
Rcpp::NumericMatrix DCT1dmatrix_cpp(int N);
RcppExport SEXP _jpegToolbox_DCT1dmatrix_cpp(SEXP NSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    rcpp_result_gen = Rcpp::wrap(DCT1dmatrix_cpp(N));
    return rcpp_result_gen;
END_RCPP
}
// DCT1d_cpp
Rcpp::NumericVector DCT1d_cpp(Rcpp::NumericVector x, int inverted);
RcppExport SEXP _jpegToolbox_DCT1d_cpp(SEXP xSEXP, SEXP invertedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type inverted(invertedSEXP);
    rcpp_result_gen = Rcpp::wrap(DCT1d_cpp(x, inverted));
    return rcpp_result_gen;
END_RCPP
}
// DCT2d_cpp
Rcpp::NumericMatrix DCT2d_cpp(Rcpp::NumericMatrix x, int inverted);
RcppExport SEXP _jpegToolbox_DCT2d_cpp(SEXP xSEXP, SEXP invertedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type inverted(invertedSEXP);
    rcpp_result_gen = Rcpp::wrap(DCT2d_cpp(x, inverted));
    return rcpp_result_gen;
END_RCPP
}
// DCTimage_cpp
Rcpp::List DCTimage_cpp(Rcpp::List image, int inverted, int DCTscaling);
RcppExport SEXP _jpegToolbox_DCTimage_cpp(SEXP imageSEXP, SEXP invertedSEXP, SEXP DCTscalingSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type image(imageSEXP);
    Rcpp::traits::input_parameter< int >::type inverted(invertedSEXP);
    Rcpp::traits::input_parameter< int >::type DCTscaling(DCTscalingSEXP);
    rcpp_result_gen = Rcpp::wrap(DCTimage_cpp(image, inverted, DCTscaling));
    return rcpp_result_gen;
END_RCPP
}
// DCTimage_cpp_plain
Rcpp::List DCTimage_cpp_plain(Rcpp::List image, int inverted);
RcppExport SEXP _jpegToolbox_DCTimage_cpp_plain(SEXP imageSEXP, SEXP invertedSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type image(imageSEXP);
    Rcpp::traits::input_parameter< int >::type inverted(invertedSEXP);
    rcpp_result_gen = Rcpp::wrap(DCTimage_cpp_plain(image, inverted));
    return rcpp_result_gen;
END_RCPP
}
// clipToJpegRange
int clipToJpegRange(int x);
RcppExport SEXP _jpegToolbox_clipToJpegRange(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(clipToJpegRange(x));
    return rcpp_result_gen;
END_RCPP
}
// getLibjpegVersion
int getLibjpegVersion();
RcppExport SEXP _jpegToolbox_getLibjpegVersion() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(getLibjpegVersion());
    return rcpp_result_gen;
END_RCPP
}
// roundC_wrapper
int roundC_wrapper(double x);
RcppExport SEXP _jpegToolbox_roundC_wrapper(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(roundC_wrapper(x));
    return rcpp_result_gen;
END_RCPP
}
// quantize
Rcpp::List quantize(Rcpp::List image, Rcpp::List quantizationTable);
RcppExport SEXP _jpegToolbox_quantize(SEXP imageSEXP, SEXP quantizationTableSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type image(imageSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type quantizationTable(quantizationTableSEXP);
    rcpp_result_gen = Rcpp::wrap(quantize(image, quantizationTable));
    return rcpp_result_gen;
END_RCPP
}
// dequantize
Rcpp::List dequantize(Rcpp::List image);
RcppExport SEXP _jpegToolbox_dequantize(SEXP imageSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type image(imageSEXP);
    rcpp_result_gen = Rcpp::wrap(dequantize(image));
    return rcpp_result_gen;
END_RCPP
}
// readDCT_file
Rcpp::List readDCT_file(std::string filename, int dequantize);
RcppExport SEXP _jpegToolbox_readDCT_file(SEXP filenameSEXP, SEXP dequantizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< int >::type dequantize(dequantizeSEXP);
    rcpp_result_gen = Rcpp::wrap(readDCT_file(filename, dequantize));
    return rcpp_result_gen;
END_RCPP
}
// readDCT_memory
Rcpp::List readDCT_memory(Rcpp::RawVector rawImage, int dequantize);
RcppExport SEXP _jpegToolbox_readDCT_memory(SEXP rawImageSEXP, SEXP dequantizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::RawVector >::type rawImage(rawImageSEXP);
    Rcpp::traits::input_parameter< int >::type dequantize(dequantizeSEXP);
    rcpp_result_gen = Rcpp::wrap(readDCT_memory(rawImage, dequantize));
    return rcpp_result_gen;
END_RCPP
}
// readJpeg_file
Rcpp::List readJpeg_file(std::string filename, int fancyUpsampling);
RcppExport SEXP _jpegToolbox_readJpeg_file(SEXP filenameSEXP, SEXP fancyUpsamplingSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type filename(filenameSEXP);
    Rcpp::traits::input_parameter< int >::type fancyUpsampling(fancyUpsamplingSEXP);
    rcpp_result_gen = Rcpp::wrap(readJpeg_file(filename, fancyUpsampling));
    return rcpp_result_gen;
END_RCPP
}
// readJpeg_memory
Rcpp::List readJpeg_memory(Rcpp::RawVector rawImage, int fancyUpsampling);
RcppExport SEXP _jpegToolbox_readJpeg_memory(SEXP rawImageSEXP, SEXP fancyUpsamplingSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::RawVector >::type rawImage(rawImageSEXP);
    Rcpp::traits::input_parameter< int >::type fancyUpsampling(fancyUpsamplingSEXP);
    rcpp_result_gen = Rcpp::wrap(readJpeg_memory(rawImage, fancyUpsampling));
    return rcpp_result_gen;
END_RCPP
}
// subsampling_simple
Rcpp::List subsampling_simple(Rcpp::List image, Rcpp::IntegerVector subsampling);
RcppExport SEXP _jpegToolbox_subsampling_simple(SEXP imageSEXP, SEXP subsamplingSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type image(imageSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type subsampling(subsamplingSEXP);
    rcpp_result_gen = Rcpp::wrap(subsampling_simple(image, subsampling));
    return rcpp_result_gen;
END_RCPP
}
// upsampling_simple
Rcpp::List upsampling_simple(Rcpp::List image);
RcppExport SEXP _jpegToolbox_upsampling_simple(SEXP imageSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type image(imageSEXP);
    rcpp_result_gen = Rcpp::wrap(upsampling_simple(image));
    return rcpp_result_gen;
END_RCPP
}
// subsampling_fancy8
Rcpp::List subsampling_fancy8(Rcpp::List image, Rcpp::IntegerVector subsampling);
RcppExport SEXP _jpegToolbox_subsampling_fancy8(SEXP imageSEXP, SEXP subsamplingSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type image(imageSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type subsampling(subsamplingSEXP);
    rcpp_result_gen = Rcpp::wrap(subsampling_fancy8(image, subsampling));
    return rcpp_result_gen;
END_RCPP
}
// upsampling_fancy8
Rcpp::List upsampling_fancy8(Rcpp::List image);
RcppExport SEXP _jpegToolbox_upsampling_fancy8(SEXP imageSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type image(imageSEXP);
    rcpp_result_gen = Rcpp::wrap(upsampling_fancy8(image));
    return rcpp_result_gen;
END_RCPP
}
// upsampling_fancy6
Rcpp::List upsampling_fancy6(Rcpp::List image);
RcppExport SEXP _jpegToolbox_upsampling_fancy6(SEXP imageSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type image(imageSEXP);
    rcpp_result_gen = Rcpp::wrap(upsampling_fancy6(image));
    return rcpp_result_gen;
END_RCPP
}
// writeJpeg_file
void writeJpeg_file(Rcpp::List image, int quality, int DCTmethod, Rcpp::IntegerVector subsampling, int force_baseline, int optimize_coding, int progressive, int smoothing, int fancyDownsampling, std::string filename);
RcppExport SEXP _jpegToolbox_writeJpeg_file(SEXP imageSEXP, SEXP qualitySEXP, SEXP DCTmethodSEXP, SEXP subsamplingSEXP, SEXP force_baselineSEXP, SEXP optimize_codingSEXP, SEXP progressiveSEXP, SEXP smoothingSEXP, SEXP fancyDownsamplingSEXP, SEXP filenameSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type image(imageSEXP);
    Rcpp::traits::input_parameter< int >::type quality(qualitySEXP);
    Rcpp::traits::input_parameter< int >::type DCTmethod(DCTmethodSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type subsampling(subsamplingSEXP);
    Rcpp::traits::input_parameter< int >::type force_baseline(force_baselineSEXP);
    Rcpp::traits::input_parameter< int >::type optimize_coding(optimize_codingSEXP);
    Rcpp::traits::input_parameter< int >::type progressive(progressiveSEXP);
    Rcpp::traits::input_parameter< int >::type smoothing(smoothingSEXP);
    Rcpp::traits::input_parameter< int >::type fancyDownsampling(fancyDownsamplingSEXP);
    Rcpp::traits::input_parameter< std::string >::type filename(filenameSEXP);
    writeJpeg_file(image, quality, DCTmethod, subsampling, force_baseline, optimize_coding, progressive, smoothing, fancyDownsampling, filename);
    return R_NilValue;
END_RCPP
}
// writeJpeg_memory
Rcpp::RawVector writeJpeg_memory(Rcpp::List image, int quality, int DCTmethod, Rcpp::IntegerVector subsampling, int force_baseline, int optimize_coding, int progressive, int smoothing, int fancyDownsampling);
RcppExport SEXP _jpegToolbox_writeJpeg_memory(SEXP imageSEXP, SEXP qualitySEXP, SEXP DCTmethodSEXP, SEXP subsamplingSEXP, SEXP force_baselineSEXP, SEXP optimize_codingSEXP, SEXP progressiveSEXP, SEXP smoothingSEXP, SEXP fancyDownsamplingSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type image(imageSEXP);
    Rcpp::traits::input_parameter< int >::type quality(qualitySEXP);
    Rcpp::traits::input_parameter< int >::type DCTmethod(DCTmethodSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type subsampling(subsamplingSEXP);
    Rcpp::traits::input_parameter< int >::type force_baseline(force_baselineSEXP);
    Rcpp::traits::input_parameter< int >::type optimize_coding(optimize_codingSEXP);
    Rcpp::traits::input_parameter< int >::type progressive(progressiveSEXP);
    Rcpp::traits::input_parameter< int >::type smoothing(smoothingSEXP);
    Rcpp::traits::input_parameter< int >::type fancyDownsampling(fancyDownsamplingSEXP);
    rcpp_result_gen = Rcpp::wrap(writeJpeg_memory(image, quality, DCTmethod, subsampling, force_baseline, optimize_coding, progressive, smoothing, fancyDownsampling));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_jpegToolbox_imageToBlocks", (DL_FUNC) &_jpegToolbox_imageToBlocks, 3},
    {"_jpegToolbox_blocksToImage", (DL_FUNC) &_jpegToolbox_blocksToImage, 1},
    {"_jpegToolbox_imageMatrixToList", (DL_FUNC) &_jpegToolbox_imageMatrixToList, 1},
    {"_jpegToolbox_imageToBlocks_list", (DL_FUNC) &_jpegToolbox_imageToBlocks_list, 3},
    {"_jpegToolbox_RGBtoYCbCr_cpp", (DL_FUNC) &_jpegToolbox_RGBtoYCbCr_cpp, 3},
    {"_jpegToolbox_YCbCrtoRGB_cpp", (DL_FUNC) &_jpegToolbox_YCbCrtoRGB_cpp, 3},
    {"_jpegToolbox_RGBtoYCbCr_image_cpp", (DL_FUNC) &_jpegToolbox_RGBtoYCbCr_image_cpp, 1},
    {"_jpegToolbox_YCbCrtoRGB_image_cpp", (DL_FUNC) &_jpegToolbox_YCbCrtoRGB_image_cpp, 1},
    {"_jpegToolbox_kroneckerDelta", (DL_FUNC) &_jpegToolbox_kroneckerDelta, 2},
    {"_jpegToolbox_DCT1dmatrix_cpp", (DL_FUNC) &_jpegToolbox_DCT1dmatrix_cpp, 1},
    {"_jpegToolbox_DCT1d_cpp", (DL_FUNC) &_jpegToolbox_DCT1d_cpp, 2},
    {"_jpegToolbox_DCT2d_cpp", (DL_FUNC) &_jpegToolbox_DCT2d_cpp, 2},
    {"_jpegToolbox_DCTimage_cpp", (DL_FUNC) &_jpegToolbox_DCTimage_cpp, 3},
    {"_jpegToolbox_DCTimage_cpp_plain", (DL_FUNC) &_jpegToolbox_DCTimage_cpp_plain, 2},
    {"_jpegToolbox_clipToJpegRange", (DL_FUNC) &_jpegToolbox_clipToJpegRange, 1},
    {"_jpegToolbox_getLibjpegVersion", (DL_FUNC) &_jpegToolbox_getLibjpegVersion, 0},
    {"_jpegToolbox_roundC_wrapper", (DL_FUNC) &_jpegToolbox_roundC_wrapper, 1},
    {"_jpegToolbox_quantize", (DL_FUNC) &_jpegToolbox_quantize, 2},
    {"_jpegToolbox_dequantize", (DL_FUNC) &_jpegToolbox_dequantize, 1},
    {"_jpegToolbox_readDCT_file", (DL_FUNC) &_jpegToolbox_readDCT_file, 2},
    {"_jpegToolbox_readDCT_memory", (DL_FUNC) &_jpegToolbox_readDCT_memory, 2},
    {"_jpegToolbox_readJpeg_file", (DL_FUNC) &_jpegToolbox_readJpeg_file, 2},
    {"_jpegToolbox_readJpeg_memory", (DL_FUNC) &_jpegToolbox_readJpeg_memory, 2},
    {"_jpegToolbox_subsampling_simple", (DL_FUNC) &_jpegToolbox_subsampling_simple, 2},
    {"_jpegToolbox_upsampling_simple", (DL_FUNC) &_jpegToolbox_upsampling_simple, 1},
    {"_jpegToolbox_subsampling_fancy8", (DL_FUNC) &_jpegToolbox_subsampling_fancy8, 2},
    {"_jpegToolbox_upsampling_fancy8", (DL_FUNC) &_jpegToolbox_upsampling_fancy8, 1},
    {"_jpegToolbox_upsampling_fancy6", (DL_FUNC) &_jpegToolbox_upsampling_fancy6, 1},
    {"_jpegToolbox_writeJpeg_file", (DL_FUNC) &_jpegToolbox_writeJpeg_file, 10},
    {"_jpegToolbox_writeJpeg_memory", (DL_FUNC) &_jpegToolbox_writeJpeg_memory, 9},
    {NULL, NULL, 0}
};

RcppExport void R_init_jpegToolbox(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}