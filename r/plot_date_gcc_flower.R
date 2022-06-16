
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

# make plots####
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")
palette(col.df$auLandscape[c(2,4,5,1)])
library(lubridate)
pdf('gcc_flower_dates.pdf',width = 6,height = 6*.618)
plot(date.1st.flower.d.since~date.peak.green.d.since,data = brown.flower.df,
     pch=16,col=Treatment,
     xlab='Greeness peak',ylab='Flower present')
legend('topleft',legend = levels(brown.flower.df$Treatment),col=palette(),
       pch=16,bty='n')

fit.lm <- lm(date.1st.flower.d.since~date.peak.green.d.since,
           data = brown.flower.df[
             brown.flower.df$date.peak.green>as.Date('2020-2-15') & 
               brown.flower.df$date.1st.flower<as.Date('2020-1-1'),])

mylabel = bquote(italic(R)^2 == .(format(summary(fit.lm)$r.squared, digits = 2)))
text(x = 235, y = 140, labels = mylabel)

clip(238,275,80,300)
abline(fit.lm,col='grey',lwd=3)

dev.off()






# not to use ####
library(lubridate)
b.date.df.info <- b.date.df.info[year(b.date.df.info$date)>=2020,]
b.date.df.info$Treatment <- as.factor(b.date.df.info$Treatment)
b.date.df.info$date <- as.Date(b.date.df.info$date)
plot(date~Treatment,data = b.date.df.info)
b.date.df.info$Treatment

plot(shelter.no~date,data = b.date.df.info,col=Treatment,pch=16)

boxplot(date~Treatment,data = b.date.df.info)


# ########
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
flower.daily.plot.trt$Treatment
# get 1st and last flower data for each plot(
flower.1st.date.df <- read.csv('download/DateOfFirstFlower.csv')
flower.1st.date.df$XAxis[flower.1st.date.df$XAxis=='Open'] <- 'ShelterControl'

get.flower.date.func <- function(plot.num){
  df = flower.daily.plot.trt[flower.daily.plot.trt$Plot.number==plot.num,]
  
  
  
  
}

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
# make sure white photos are not included
gcc.df.trt <- gcc.df.trt[abs(gcc.df.trt$GCC -1/3)>0.001,]
gcc.df.trt.mean <- summaryBy(GCC~Date + Treatment,data = gcc.df.trt,
                             FUN=c(mean,sd,length),keep.names=T)

gcc.df.trt.mean <- gcc.df.trt.mean[gcc.df.trt.mean$Treatment %in% c('Ambient','Drought','Frequency'),]

# plot(GCC~Date,data = gcc.df.trt[gcc.df.trt$plot==4,])
# par(new=T)
# points((X..flowers)/500~Date,
#        data = flower.daily.plot.trt[flower.daily.plot.trt$Plot.number==4,],
#        pch=16,col='red')
# 
devtools::source_url("https://github.com/Jinyan-Yang/colors/blob/master/R/col.R?raw=TRUE")

