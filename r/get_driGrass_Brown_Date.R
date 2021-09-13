drigrass.df <- readRDS('cache/driGrass/gcc_driGrass_10.rds')

get.brown.date.func <- function(fn){
  # read the gcc file
  drigrass.df <- readRDS(fn)
  # fileter out dark photos
  drigrass.df <- drigrass.df[drigrass.df$RGBtot>270 & 
                               drigrass.df$RGBtot < 350,]

  
  # make a numberic vecor of date
  drigrass.df <- drigrass.df[!is.na(drigrass.df$Date),]
  drigrass.df$x <- drigrass.df$Date - drigrass.df$Date[1]
  drigrass.df$x <- as.numeric(drigrass.df$x)
  
  #fit to data 
  library(mgcv)
  fit.gam <- gam(GCC~s(x,k=15),data = drigrass.df)
  
  # make a plot to visually check fitting
  
  plot(GCC~x,data = drigrass.df,pch=16,col='grey',xaxt='n',xlab='')
  points(fit.gam$fitted.values~fit.gam$model$x,ylim=c(0.3,0.5),pch=16)
  
# add date
  date.range = range(drigrass.df$Date,na.rm=T)
  date.1st <- as.Date(paste0(substr(as.character(date.range[1]),1,7),'-1'))
  date.last <- as.Date(paste0(substr(as.character(date.range[2]),1,7),'-1'))
  
  mons.vec =  seq(date.1st,date.last,by='mon')
  # 
  x.vec <- drigrass.df$x[drigrass.df$Date %in% mons.vec]
  x.vec <- unique(x.vec)
  # 
  mon.c <- format(mons.vec,'%m')
  axis(1,at = x.vec,labels = unique(drigrass.df$Date[drigrass.df$x %in% x.vec]))
  
  # yr.vec <- unique(year(drigrass.df$Date))
  # where.c <-which(mon.c =='01') / length(mon.c)
  # num.yr <- length(where.c)
  # mtext(yr.vec[(length(yr.vec) - num.yr + 1):length(yr.vec)],side = 1,adj = where.c,line = 3)
  
  
  # get plot num
  plot.no <- gsub('cache/driGrass/gcc_driGrass_','',fn)
  plot.no <- gsub('.rds','',plot.no)
  
  legend('topleft',legend = plot.no,bty='n')
  
  # get the date of max GCC which is the onset of brown down
  date.i <- drigrass.df$x[which.max(fit.gam$fitted.values)]
  
  date.max <- unique(drigrass.df$Date[drigrass.df$x == date.i])
  
  return(date.max)
}

# example
# fn <- 'cache/driGrass/gcc_driGrass_10.rds'
# get.brown.date.func(fn)

# read all files#####
fn.vec <- list.files('cache/driGrass/',full.names = T)

pdf('figures/gcc.plot.pdf',width = 8,height = 8*.618)

out.df <- data.frame(fn = fn.vec,
                     date = as.Date(sapply(fn.vec,get.brown.date.func),
                                    origin = "1970-01-01"))
dev.off()

# save dates
out.df$shelter.no <- gsub('cache/driGrass/gcc_driGrass_','',out.df$fn)
out.df$shelter.no <- gsub('.rds','',out.df$shelter.no)

write.csv(out.df,'cache/driGrassBrownDate.csv',row.names = F)
