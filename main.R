library(jpeg)
# library(phenora)
# library(gridExtra)
library(exifr)
library(doBy)

source('r/function_get_gcc.R')
pic.path <- 'pic'
# calculate GCC for site
cal.gcc.site.func('qp',ROI = c(.2,.8,.2,.8),pic.path = pic.path)

cal.gcc.site.func('ng',ROI = c(.2,.8,.2,.8))
cal.gcc.site.func('mp',ROI = c(.2,.8,.2,.8))
cal.gcc.site.func('ym',ROI = c(.2,.8,.2,.8))

# make plot####
plot.gcc.dn.func <- function(folder.nm,
                             site.nm,
                             pic.dim = c(1,1),
                             # plot.photo = FALSE,
                             treat.vec = c('c','-',#'c',
                                           '-','+','-',
                                           '+','c','+','landscape')
                             ){
  par(mfrow=pic.dim)
  # treat.vec <- c('c','-',#'c',
  #                '-','+','-',
  #                '+','c','+','landscape')
  for(i in seq_along(folder.nm)){
    
    fn.out <- sprintf('cache/gcc_%s_%s.rds',site.nm,folder.nm[i])
    
    if(file.exists(fn.out)){
      
      gcc.df <- readRDS(fn.out)
      gcc.df <- gcc.df[gcc.df$RGBtot > 200,]
      
      if(site.nm=='qp'){
        gcc.df <- gcc.df[gcc.df$Date > as.Date('2019-5-27'),]
      }
      
      # # change date
      # if(folder.nm[i] %in%c(4,5)){
      #   gcc.df$Date[gcc.df$Date<as.Date('2019-1-1')] <- gcc.df$Date[gcc.df$Date<as.Date('2019-1-1')] + 1168
      # }
      # 
      # 
      gcc.df <- summaryBy(.~Date,data = gcc.df,FUN = mean,na.rm=T,keep.names = T)
      
      gcc.df$gcc.smooth <- get.smooth.gcc.func(Date.vec = gcc.df$Date,gcc.vec = gcc.df$GCC)
      
      # hist(gcc.df$RGBtot)
      plot(GCC~Date,data = gcc.df,main = folder.nm[i],
           pch=16,ylim=c(0.25,0.4),xlab='')
      
      points(gcc.smooth~Date,data = gcc.df,type='l',
             col='grey',lwd=2)
      legend('topleft',legend =treat.vec[i],bty='n')
      abline(h=0.3,col='salmon')
      
      
      
    }
  }
}
# folder.nm <- list.files('pic/qp/')

pdf('qp.gcc.pdf',width = 10,height = 10 * 0.618)

plot.gcc.dn.func(list.files('pic/qp/'),site.nm = 'qp')


dev.off()


