library(jpeg)
# library(phenora)
# library(gridExtra)
library(exifr)
library(doBy)

source('r/function_get_gcc.R')

# calculate GCC for site
cal.gcc.site.func('qp',ROI = c(.2,.8,.2,.8))
cal.gcc.site.func('ng',ROI = c(.2,.8,.2,.8))
cal.gcc.site.func('mp',ROI = c(.2,.8,.2,.8))
cal.gcc.site.func('ym',ROI = c(.2,.8,.2,.8))