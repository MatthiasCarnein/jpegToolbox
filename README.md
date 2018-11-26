# jpegToolbox


This R-package provides various implementations of the JPEG image compression algorithm.
 Most importantly, an interface to the popular libjpeg library is available. The package ships with pre-compiled libraries of libjpeg versions 6b, 8d and 9a for Windows. 
 In addition, the entire lossy compression pipeline is implemented in R for easy debugging as well as in C++ with interfaces to R for faster computation speed.


## Installation

The easiest way to install the package is by using devtools:

```R
devtools::install_git("https://wiwi-gitlab.uni-muenster.de/m_carn01/jpegToolbox")
```

Alternatively, the package can be build from source.


## libjpeg Interface

The package provides an interface to the popular libjpeg (and libjpeg-turbo) libraries.
The interface is similar to the R-package 'jpeg' but allows to change more compression settings.

The path to libjpeg needs to be specified in the src/makevars.win (Windows) or src/makevars (unix) files.
A number of pre-compiled libraries for version 6b, 8d and 9a are available. Default is libjpeg 6b.
A change of library requires recompilation of the package.

Images can be read from file, plotted and recompressed to a file again. Various decompression and compression settings are available.

```R
library(jpegToolbox)

## read image from file
image = readJpeg("image.jpg", fancyUpsampling = T)

## plot image
plot(image)

## compress and write to file
writeJpeg(image, filename="image2.jpg", quality = 100, DCTmethod = "slow", subsampling="4:4:4", force_baseline=T, optimize_coding=T , progressive=T, smoothing=0, fancyDownsampling=T)
```

Images can also be written to memory rather than a file for faster experimenting.
In contrast to the original libraries, this is also available for older libjpeg versions.

```R
## write to memory
compressed = writeJpeg(image, quality = 100, DCTmethod = "slow", subsampling="4:4:4", force_baseline=T, optimize_coding=T , progressive=T, smoothing=0, fancyDownsampling=T)

## read from memory
decompressed = readJpeg(compressed, fancyUpsampling = T)
plot(image)
```



## Custom Implementation

The library also provides its own implementations for all lossy compression steps using similar syntax:

```R
compressed = compressJpeg(image, quality = 100, subsampling = "444", force_baseline=T, linear=F, fancyDownsampling=T, libjpeg=6)

decompressed = decompressJpeg(compressed, fancyUpsampling = T)

plot(decompressed)
```


The above wrapper functions call the individual compression and decompression steps one by one. This can also be done manually:

```R
## color conversion to YCbCr
compressed = RGBtoYCbCr(RGB = image)

## subsample
compressed = subsampling(image = compressed, subsampling = "4:4:4", fancyDownsampling = T, libjpeg = 6)

## block
compressed = imageToBlocks(image = compressed, blockheights = c(8,8,8), blockwidths = c(8,8,8))

## DCT
compressed = DCT(x = compressed)

## quantization
table = quantizationTable(quality = 100, force_baseline = T, linear = F)
compressed = quantize(image = compressed, quantizationTable = table)


## decompression

## dequantize
decompressed = dequantize(image = compressed)

## IDCT
decompressed = IDCT(x = decompressed, DCTscaling = T)

## unblock
decompressed = blocksToImage(image = decompressed)

## upsample
decompressed = upsampling(image = decompressed, fancyUpsampling = T)

## color conversion to RGB
decompressed = YCbCrtoRGB(YCbCr = decompressed)
plot(decompressed)
```

Internally all functions interface c++, however most functions also provide a corresponding R implementation which is considerably slower but easier for debugging.

## Acknowledgement

This software is based in part on the work of the Independent JPEG Group.
