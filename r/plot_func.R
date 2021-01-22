devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")

plot.gcc.dn.func <- function(folder.nm,
                             site.nm,
                             pic.dim = c(1,1),
                             plot.photo = FALSE,
                             roi = c(.2,.8,.2,.8),
                             treat.vec = c('c','-',#'c',
                                           '-','+','-',
                                           '+','c','+','landscape')
){
  par(mfrow=pic.dim,mar=c(5,5,1,1))
  # treat.vec <- c('c','-',#'c',
  #                '-','+','-',
  #                '+','c','+','landscape')
  for(i in seq_along(folder.nm)){
    
    fn.out <- sprintf('cache/gcc_%s_%s.rds',site.nm,folder.nm[i])
    
    if(file.exists(fn.out)){
      # gcc.df <- readRDS("C:/repo/dn_gcc/cache/gcc_ng_up.rds")
      gcc.df <- readRDS(fn.out)
      print(fn.out)
      # remove photos that are too dark
      gcc.df <- gcc.df[gcc.df$RGBtot > 300,]
      gcc.df <- gcc.df[order(gcc.df$Date),]
      # remove photos that are white
      gcc.df[which(gcc.df$GCC == gcc.df$RCC & gcc.df$RCC== gcc.df$BCC),c('GCC','RCC','BCC')] <- NA
      
      gcc.df <- gcc.df[!is.na(gcc.df$GCC),]
      
      if(site.nm=='qp'){
        gcc.df <- gcc.df[gcc.df$Date > as.Date('2019-5-27'),]
      }
      
      if(site.nm=='ng'){
        
        if(folder.nm[i]=='up'){
          diff.dates <- as.Date('2019-12-22') - min(gcc.df$Date,na.rm=T )
          gcc.df$Date[gcc.df$Date < as.Date('2019-5-1')] <-  gcc.df$Date[gcc.df$Date < as.Date('2019-5-1')]+ diff.dates
        }
        
        gcc.df <- gcc.df[gcc.df$Date > as.Date('2019-5-1'),]
      }
      
      last.photo.nm <- gcc.df$filename[nrow(gcc.df)]
      first.photo.nm <- gcc.df$filename[1]
      

      
      # # change date
      # if(folder.nm[i] %in%c(4,5)){
      #   gcc.df$Date[gcc.df$Date<as.Date('2019-1-1')] <- gcc.df$Date[gcc.df$Date<as.Date('2019-1-1')] + 1168
      # }
      # 
      # 
      gcc.df <- summaryBy(.~Date,data = gcc.df,FUN = mean,na.rm=T,keep.names = T)
      
      gcc.df$gcc.smooth <- get.smooth.gcc.func(Date.vec = gcc.df$Date,gcc.vec = gcc.df$GCC)
      
      # hist(gcc.df$RGBtot)
      plot(GCC~Date,data = gcc.df,main = paste0(folder.nm[i],' ',treat.vec[i]),
           pch=16,ylim=c(0.25,0.45),xlab='',xaxt='n',col=col.df$auLandscape[2])
      
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
             col=col.df$iris[2],lwd=2)
      # legend('top',legend =treat.vec[i],bty='n',cex=1.5,text.font=2)
      abline(h=0.3,col=col.df$daisy[5])
      
      # plot insert
      if(plot.photo == TRUE){
        # calculate position of inset
        # plot last photo
        plotdim <- par("plt")
        xleft    = plotdim[2] - (plotdim[2] - plotdim[1]) * 0.25
        xright   = plotdim[2]  #
        ybottom  = plotdim[4] - (plotdim[4] - plotdim[3]) * 0.25  #
        ytop     = plotdim[4]  #
        
        # set position for inset
        par(
          fig = c(xleft, xright, ybottom, ytop)
          , mar=c(0,0,0,0)
          , new=TRUE
        )
        
        # add inset
        print(last.photo.nm)
        last.photo <- load.image(last.photo.nm)
        plot(last.photo,ann=F,axes=F)
        
        # define aoi
        xmin <- roi[1]
        xmax <- roi[2]
        ymin <- roi[3]
        ymax <- roi[4]
        
        x.coords <- c(xmin,xmax,xmax,xmin) * nrow(last.photo)
        y.coords <- c(ymin,ymin,ymax,ymax) * ncol(last.photo)
        
        polygon(x.coords,y.coords,border = 'red')
        # 
        # plot 1st photo
        xleft    = plotdim[1] 
        xright   = plotdim[1] + (plotdim[2] - plotdim[1]) * 0.25 #
        ybottom  = plotdim[4] - (plotdim[4] - plotdim[3]) * 0.25  #
        ytop     = plotdim[4]  #
        
        # set position for inset
        par(
          fig = c(xleft, xright, ybottom, ytop)
          , mar=c(0,0,0,0)
          , new=TRUE
        )
        
        # add inset
        print(first.photo.nm)
        first.photo <- load.image(first.photo.nm)
        plot(first.photo,ann=F,axes=F)
        polygon(x.coords,y.coords,border = 'red')
        # reset par
        par(fig=c(0,1,0,1),new=F,mar=c(5,5,1,1))
      }
      
      
    }
  }
}
