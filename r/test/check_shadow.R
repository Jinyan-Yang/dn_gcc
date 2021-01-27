gcc.df.8 <- gcc_qp_8[gcc_qp_8$RGBtot >300,]
gcc.df.5 <- gcc_qp_5[gcc_qp_5$RGBtot >300,]

# fn.bare <- 'pic/qp/up/WSCT0004.JPG'
# fn.bare <- 'pic/ng/1/WSCT0002.JPG'
fn.bare <- 'pic/qp/8/WSCT9499.JPG'
tmp.bare <- readJPEG(fn.bare)

library(raster)
plot(raster(tmp.bare[,,1]))
plot(raster(tmp.bare[,,2]))
plot(raster(tmp.bare[,,3]))

m.tot <- tmp.bare[,,1] + tmp.bare[,,2] + tmp.bare[,,3]

plot(raster(m.tot),breaks = c(0,1,1.5,2,4),col=c('grey','yellow','coral','red'))

# get GCC

RDN <- mean(as.vector(tmp.bare[,,1])[which(m.tot<1.5)])
GDN <- mean(as.vector(tmp.bare[,,2])[which(m.tot<1.5)])
BDN <- mean(as.vector(tmp.bare[,,3])[which(m.tot<1.5)])
bn <- RDN+GDN+BDN

RI.filtered <- RDN/bn
GI.filtered <- GDN/bn
BI.filtered <- BDN/bn

# 

RDN <- mean(tmp.bare[,,1])
GDN <- mean(tmp.bare[,,2])
BDN <- mean(tmp.bare[,,3])
bn <- RDN+GDN+BDN

RI <- RDN/bn
GI <- GDN/bn
BI <- BDN/bn


# find a picture with vege
fn.veg <- 'pic/qp/5/WSCT2636.JPG'

tmp.veg <- readJPEG(fn.veg)

library(raster)
plot(raster(tmp.veg[,,1]))
plot(raster(tmp.veg[,,2]))
plot(raster(tmp.veg[,,3]))

m.tot <- tmp.bare[,,1] + tmp.bare[,,2] + tmp.bare[,,3]

plot(raster(m.tot),breaks = c(0,1,1.5,2,4),col=c('grey','yellow','coral','red'))

# get GCC

RDN <- mean(as.vector(tmp.bare[,,1])[which(m.tot<1.5)])
GDN <- mean(as.vector(tmp.bare[,,2])[which(m.tot<1.5)])
BDN <- mean(as.vector(tmp.bare[,,3])[which(m.tot<1.5)])
bn <- RDN+GDN+BDN

RI.filtered <- RDN/bn
GI.filtered <- GDN/bn
BI.filtered <- BDN/bn

# 

RDN <- mean(tmp.bare[,,1])
GDN <- mean(tmp.bare[,,2])
BDN <- mean(tmp.bare[,,3])
bn <- RDN+GDN+BDN

RI <- RDN/bn
GI <- GDN/bn
BI <- BDN/bn
