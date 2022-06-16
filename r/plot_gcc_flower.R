dg.26 <- readRDS('cache/driGrass/gcc_driGrass_26.rds')
dg.28 <- readRDS('cache/driGrass/gcc_driGrass_28.rds')

plot(GCC~Date,data = dg.26[1:500,],pch=3)
points(GCC~Date,data = dg.28[1:500,],pch=16,col='red')

# 
treat.info.df <- read.csv('data/TreatmentMaster.csv')
# 
treat.info.df <- read.csv('E:/repo/get_drigrass_met/TreatmentMaster.csv')
# 
flower.df <- read.csv('cache/driGrass/DRI grass pollinator observatio.csv')
flower.df$Date <- strptime(flower.df$Date,'%m/%d/%Y')
# flower.df$Plot.number
library(doBy)

flower.daily.plot <- summaryBy(X..flowers~Date+Plot.number,data = flower.df,
                               FUN = sum,na.rm=T,keep.names = T)
flower.daily.plot$Date <- as.Date(flower.daily.plot$Date)

flower.daily.plot.trt <- merge(flower.daily.plot,treat.info.df,
                               by.x='Plot.number',by.y='Plot',all.x=T)

flower.daily.plot.mean <- summaryBy(X..flowers~Date + Treatment,data = flower.daily.plot.trt,
                                    FUN = c(mean,sd,length),keep.names = T)

flower.daily.plot.mean <- flower.daily.plot.mean[flower.daily.plot.mean$Treatment %in% 
                                                   c('Ambient', 'Drought', 'Frequency'),]
flower.daily.plot.mean$plot.fac <- as.factor(flower.daily.plot.mean$Treatment)

# read GCC

tmp.ls <- list()
plot.vec <- unique(flower.daily.plot.trt$Plot.number)
for(plot.i in seq_along(plot.vec)){
  
  fn <- sprintf('cache/driGrass/gcc_driGrass_%s.rds',plot.vec[plot.i])
  
  df <- try(readRDS(fn))
  
if(class(df) == 'try-error'){
  tmp.ls[[plot.i]] <- NULL
}else{
  df$plot <- plot.vec[plot.i]
  
  tmp.ls[[plot.i]] <- df
}
  
}

# 
gcc.df <- do.call(rbind,tmp.ls)

gcc.df.trt <- merge(gcc.df,treat.info.df,by.x = 'plot',by.y = 'Plot',all.x=T)
gcc.df.trt <- gcc.df.trt[complete.cases(gcc.df.trt$GCC),]
gcc.df.trt.mean <- summaryBy(GCC~Date + Treatment,data = gcc.df.trt,
                           FUN=c(mean,sd,length),keep.names=T)

gcc.df.trt.mean <- gcc.df.trt.mean[gcc.df.trt.mean$Treatment %in% c('Ambient','Drought','Frequency'),]


# 
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")

pdf('gcc_flower.pdf',width = 8,height = 8*2*.618)
# palette(c('forestgreen','coral','navy'))
palette(col.df$auLandscape[c(2,4,5,1)])
par(mfrow=c(2,1))
par(mar=c(1,5,5,1))
plot(GCC.mean~Date,
     data = gcc.df.trt.mean[gcc.df.trt.mean$Treatment=='Ambient',], 
     type = 'l',lty='solid',col=1,
     xlim=c(as.Date('2019-9-1'),as.Date('2020-12-31')),lwd=3,
     xlab='',ylab='Greenness',xaxt='n')

points(GCC.mean~Date,
       data = gcc.df.trt.mean[gcc.df.trt.mean$Treatment=='Drought',],
       type = 'l',lty='solid',
       col=2,lwd=3)

points(GCC.mean~Date,
       data = gcc.df.trt.mean[gcc.df.trt.mean$Treatment=='Frequency',],
       type = 'l',lty='solid',
       col=3,lwd=3)

# 
legend('topright',legend = unique(gcc.df.trt.mean$Treatment),col=palette(),
       lty='solid',bty='n')

# 
# mons.vec =  seq(as.Date('2019-9-1'),as.Date('2020-12-31'),by='mon')

# mon.c <- format(mons.vec,'%m')
# axis(1,at = mons.vec,labels = mon.c)
# mtext('2019',side = 1,line=2.5,adj=0.1)
# mtext('2020',side = 1,line=2.5,adj=0.3)
# par(new=TRUE)
par(mar=c(5,5,1,1))
plot(X..flowers.mean~Date,
     data = flower.daily.plot.mean[flower.daily.plot.mean$plot.fac=='Ambient',],
     type='b',col=1,pch=16,ylim=c(0,250),xlab='',
     xlim=c(as.Date('2019-9-1'),as.Date('2020-12-31')),lwd=3,
     ylab='Number of flowers',xaxt='n')

points(X..flowers.mean~Date,
            data = flower.daily.plot.mean[flower.daily.plot.mean$plot.fac=='Drought',],
            type='b',col=2,pch=16,lwd=3)

points(X..flowers.mean~Date,
       data = flower.daily.plot.mean[flower.daily.plot.mean$plot.fac=='Frequency',],
       type='b',col=3,pch=16,lwd=3)

mons.vec =  seq(as.Date('2019-9-1'),as.Date('2020-12-31'),by='mon')

mon.c <- format(mons.vec,'%m')
axis(1,at = mons.vec,labels = mon.c)
mtext('2019',side = 1,line=2.5,adj=0.1)
mtext('2020',side = 1,line=2.5,adj=0.3)

dev.off()