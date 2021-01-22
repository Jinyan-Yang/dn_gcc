library(jpeg)
# library(phenora)
# library(gridExtra)
library(exifr)
library(doBy)
library(imager)
library(lubridate)

source('r/function_get_gcc.R')
source('r/plot_func.R')
pic.path <- 'pic'
ROI.in <- c(.2,.8,.2,.8)
# calculate GCC for site
cal.gcc.site.func(site.nm='qp',ROI = ROI.in,pic.path = pic.path)

cal.gcc.site.func('ng',ROI = ROI.in,pic.path = pic.path)
cal.gcc.site.func('mp',ROI = ROI.in,pic.path = pic.path)
cal.gcc.site.func('ym',ROI = ROI.in,pic.path = pic.path)

# make plot####

pdf('figures/qp.gcc.pdf',width = 10,height = 10 * 0.618)
plot.gcc.dn.func(list.files('pic/qp/'),site.nm = 'qp',plot.photo = T)
dev.off()

pdf('figures/ym.gcc.pdf',width = 10,height = 10 * 0.618)
plot.gcc.dn.func(folder.nm = list.files('pic/ym/'),site.nm = 'ym',plot.photo = T,
                 treat.vec = c('-','c','-','c','-'))
dev.off()

pdf('figures/ng.gcc.pdf',width = 10,height = 10 * 0.618)
plot.gcc.dn.func(folder.nm = list.files('pic/ng/'),site.nm = 'ng',plot.photo = T)
dev.off()

pdf('figures/mp.gcc.pdf',width = 10,height = 10 * 0.618)
plot.gcc.dn.func(folder.nm = list.files('pic/mp/'),site.nm = 'mp',plot.photo = T)
dev.off()