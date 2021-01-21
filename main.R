library(jpeg)
# library(phenora)
# library(gridExtra)
library(exifr)
library(doBy)

source('r/function_get_gcc.R')
source('r/plot_func.R')
pic.path <- 'pic'
# calculate GCC for site
cal.gcc.site.func(site.nm='qp',ROI = c(.2,.8,.2,.8),pic.path = pic.path)

cal.gcc.site.func('ng',ROI = c(.2,.8,.2,.8))
cal.gcc.site.func('mp',ROI = c(.2,.8,.2,.8))
cal.gcc.site.func('ym',ROI = c(.2,.8,.2,.8))

# make plot####

pdf('qp.gcc.pdf',width = 10,height = 10 * 0.618)

plot.gcc.dn.func(list.files('pic/qp/'),site.nm = 'qp')


dev.off()


