# read brown dates####
b.date.df <- read.csv('cache/driGrass/driGrassBrownDate.csv')
# read flower dates
flower.1st.date.df <- read.csv('download/DateOfFirstFlower.csv')
flower.1st.date.df$XAxis[flower.1st.date.df$XAxis=='Open'] <- 'ShelterControl'
names(flower.1st.date.df)[names(flower.1st.date.df) == 'Date2'] <- 'date.1st.flower'
names(b.date.df)[names(b.date.df) == 'date'] <- 'date.peak.green'
# merge the two
brown.flower.df <- merge(flower.1st.date.df,b.date.df,
                         by.x = 'Plot',by.y = 'shelter.no',
                         all=T)
# convert dates to doy
brown.flower.df$Treatment <- as.factor(brown.flower.df$XAxis)
brown.flower.df$date.1st.flower <- as.Date(brown.flower.df$date.1st.flower)
brown.flower.df$date.peak.green <- as.Date(brown.flower.df$date.peak.green)
brown.flower.df <- brown.flower.df[brown.flower.df$date.peak.green>as.Date('2020-1-1'),]
# 
brown.flower.df$date.1st.flower.d.since <- as.numeric(brown.flower.df$date.1st.flower - as.Date('2019-7-2'))
brown.flower.df$date.peak.green.d.since <- as.numeric(brown.flower.df$date.peak.green - as.Date('2019-7-2'))

# read gcc$$$$#####
# read GCC

tmp.ls <- list()
plot.vec <- unique(flower.daily.plot.trt$Plot)
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
# get gcc
treat.info.df <- read.csv('data/TreatmentMaster.csv')
# 
treat.info.df <- read.csv('E:/repo/get_drigrass_met/TreatmentMaster.csv')
# 
gcc.df.trt <- merge(gcc.df,treat.info.df,by.x = 'plot',by.y = 'Plot',all.x=T)
gcc.df.trt <- gcc.df.trt[complete.cases(gcc.df.trt$GCC),]
gcc.df.trt.mean <- summaryBy(GCC~Date + Treatment,data = gcc.df.trt,
                             FUN=c(mean,sd,length),keep.names=T)

gcc.df.trt.mean <- gcc.df.trt.mean[gcc.df.trt.mean$Treatment %in% c('Ambient','Drought','Frequency'),]




# make plots####
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
palette(col.df$auLandscape[c(2,4,5,1)])
library(lubridate)
pdf('gcc_flower_dates.pdf',width = 6,height = 2*6*.618)
# plot gcc ts####
palette(c("green3","goldenrod4","blue"))

par(mfrow=c(2,1))
par(mar=c(5,5,1,1))
plot(GCC.mean~Date,
     data = gcc.df.trt.mean[gcc.df.trt.mean$Treatment=='Ambient',], 
     type = 'l',lty='solid',col=1,xaxt='n',
     xlim=c(as.Date('2019-9-1'),as.Date('2020-12-31')),lwd=3,
     xlab='',ylab='Greenness')

points(GCC.mean~Date,
       data = gcc.df.trt.mean[gcc.df.trt.mean$Treatment=='Drought',],
       type = 'l',lty='solid',
       col=2,lwd=3)

points(GCC.mean~Date,
       data = gcc.df.trt.mean[gcc.df.trt.mean$Treatment=='Frequency',],
       type = 'l',lty='solid',
       col=3,lwd=3)
# 
mons.vec =  seq(as.Date('2019-9-1'),as.Date('2020-12-31'),by='mon')

mon.c <- format(mons.vec,'%m')
axis(1,at = mons.vec,labels = mon.c)
mtext('2020',side = 1,line=2.5,adj=0.3)
# 
legend('topright',legend = unique(gcc.df.trt.mean$Treatment),col=palette(),
       lty='solid',bty='n')

legend('topleft',legend = '(a)',bty='n')

# plot date####
plot.date.df <- brown.flower.df[brown.flower.df$Treatment != 'ShelterControl',]
plot.date.df$Treatment<- droplevels(plot.date.df$Treatment )
plot(date.1st.flower.d.since~date.peak.green.d.since,
     data = plot.date.df,
     pch=16,col=Treatment,
     xlab='Greeness peak',ylab='Flower present')
legend('topleft',
       legend = levels(plot.date.df$Treatment),
       col=palette(),
       pch=16,bty='n')

fit.lm <- lm(date.1st.flower.d.since~date.peak.green.d.since,
             data = plot.date.df[
               plot.date.df$date.peak.green>as.Date('2020-2-15') & 
                 plot.date.df$date.1st.flower<as.Date('2020-1-1'),])

mylabel = bquote(italic(R)^2 == .(format(summary(fit.lm)$r.squared, digits = 2)))
text(x = 235, y = 140, labels = mylabel)

clip(238,275,80,300)
abline(fit.lm,col='grey',lwd=3)
legend('topleft',legend = '(b)',bty='n')
dev.off()