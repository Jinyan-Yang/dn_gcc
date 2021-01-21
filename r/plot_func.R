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
           pch=16,ylim=c(0.25,0.4),xlab='',xaxt='n')
      
      # plot date
      date.range = range(gcc.df$Date,na.rm=T)
      mons.vec =  seq(date.range[1],date.range[2],by='mon')
      
      mon.c <- format(mons.vec,'%m')
      axis(1,at = mons.vec,labels = mon.c)
      # mtext('2018',side = 1,adj=0,line = 3)
      # mtext('2019',side = 1,adj=0.5,line = 3)
      yr.vec <- unique(year(gcc.df$Date))
      where.c <-which(mon.c =='01') / length(mon.c)
      num.yr <- length(where.c)
      if(num.yr>0){
        mtext(yr.vec[(length(yr.vec) - num.yr + 1):length(yr.vec)],side = 1,adj = where.c,line = 3)
      }
      
      # 
      points(gcc.smooth~Date,data = gcc.df,type='l',
             col='grey',lwd=2)
      legend('topleft',legend =treat.vec[i],bty='n')
      abline(h=0.3,col='salmon')
      
      # plot insert
      # if(plot.photo == TRUE)
      
      
    }
  }
}