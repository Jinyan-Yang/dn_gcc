
jpeg('test.plot.jpg')
plot(1,pch=NA,ann=F,xaxt='n')

par(mar=c(0,0,0,0),oma=rep(0,4))
plot(1,pch=NA,ann=F,axes=F,
     xlim=c(0,1),ylim=c(0,1))



polygon(x = c(0,0.5,0.5,0,0),y=c(0.5,0.5,1,1,0.5),col= rgb(0,1,0),border = F)

polygon(x = c(0,0.5,0.5,0,0),y=c(0,0,0.5,0.5,0),col= rgb(0,0.5,0),border = F)

polygon(x = c(0.5,1,1,0.5,0.5),y=c(0.5,0.5,1,1,0.5),col= rgb(1,0,0),border = F)
dev.off()

jpeg('test.plot.green.jpg')
plot(1,pch=NA,ann=F,xaxt='n')

par(mar=c(0,0,0,0),oma=rep(0,4))
plot(1,pch=NA,ann=F,axes=F,
     xlim=c(0,1),ylim=c(0,1))



polygon(x = c(0,0.5,0.5,0,0),y=c(0.5,0.5,1,1,0.5),col= rgb(0,1,0),border = F)

polygon(x = c(0,0.5,0.5,0,0),y=c(0,0,0.5,0.5,0),col= rgb(0,0.75,0),border = F)

polygon(x = c(0.5,1,1,0.5,0.5),y=c(0.5,0.5,1,1,0.5),col= rgb(0,0.5,0),border = F)

polygon(x = c(0.5,1,1,0.5,0.5),y=c(0,0,0.5,0.5,0),col= rgb(0,.25,0),border = F)
dev.off()

processImage.new('test.plot.green.jpg')
processImage.new('pic/ym/18/MFDC1997.JPG')
processImage.new('halfgreen.jpg')
jpg.df <- read_and_crop('halfgreen.jpg',ROI=NULL)
jpg.m <- t(as.matrix(jpg.df[,3:5]))
# x <- aperm(jpg[,,1,],c(3,1,2))

# x.m <- t(apply(x,1,cbind))
# rgb2hsv(matrix(rep(0.5,15),nrow=3),maxColorValue = 1)

hsv.m <- rgb2hsv(jpg.m*255)
h.vec <- hsv.m[1,]


length(which(h.vec>31/360 & h.vec<160/360)) / length(h.vec)

range(h.vec)
hist(h.vec*360)
