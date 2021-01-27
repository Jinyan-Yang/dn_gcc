# read a normal image
fn <- 'pic/qp/2/WSCT2951.jpg'
phot <- read_and_crop(fn,NULL)

# fileter for white pixels
phot[which(phot$R>.9&phot$G>.9&phot$B>.9),c('R','G','B')] <- 0

# get gcc
RDN <- mean(phot$R)
GDN <- mean(phot$G)
BDN <- mean(phot$B)
bn <- RDN+GDN+BDN

GI <- GDN/bn

GI.old =  0.2861862

# make a copy 
phot.white <- phot
# make certain proportin of the picture white
proportion.white <- .2
row.nm <- proportion.white*nrow(phot.white)
phot.white$G[1:row.nm] <- 0.9
phot.white$R[1:row.nm] <- 0.9
phot.white$B[1:row.nm] <- 0.9

# get GCC again
RDN.white <- mean(phot.white$R)
GDN.white <- mean(phot.white$G)
BDN.white <- mean(phot.white$B)
bn.white <- RDN.white+GDN.white+BDN.white

GI.white <- GDN.white/bn.white

GI.white / GI

# 
hist(phot$G)
hist(phot$R)
hist(phot$B)
