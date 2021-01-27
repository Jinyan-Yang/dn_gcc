tran.func <- function(cols,alpha = 0.5){
  targe.vec <- col2rgb(cols)
  out.col <- apply(targe.vec,2,FUN = function(x){
    rgb(red = x["red"]/255, green=x["green"]/255, blue=x["blue"]/255,alpha=alpha)})
  return(out.col)
}
# 
folder.nm = list.files('pic/qp/')
site.nm = 'qp'
fn.out <- sprintf('cache/gcc_%s_%s.rds',site.nm,folder.nm[i])

fn.out <- 'cache/gcc_ng_9.rds'
for(i in seq_along(folder.nm)){
  
  gcc.df <- readRDS(fn.out)

  gcc.df <- gcc.df[gcc.df$RGBtot > 300,]
  gcc.df <- gcc.df[order(gcc.df$Date),]
  gcc.df <- gcc.df[!is.na(gcc.df$GCC),]
  gcc.df$DateTime <- as.POSIXct(gcc.df$DateTime)
  
  
  gcc.df[which(gcc.df$GCC == gcc.df$RCC & gcc.df$RCC== gcc.df$BCC),c('GCC','RCC','BCC')] <- NA
  
  gcc.df$DateTime <- as.POSIXct(gcc.df$DateTime)
  
  gcc.df$hour <- as.factor(hour(gcc.df$DateTime))
  
  # plot(GCC~hour,data = gcc.df[gcc.df$RCC<0.5,],col=col.df$beauty,pch=16)
  
  # plot(GCC~DateTime,data = gcc.df[gcc.df$RCC<0.5,],col=col.df$beauty,pch=16)
  
  plot(GCC~DateTime,data = gcc.df,col=col.df$beauty,pch=16,ylim=c(0.25,0.45))
  legend('bottom',legend = levels(gcc.df$hour),col=col.df$beauty,pch=16,horiz = T)
  
  x.range <- range(gcc.df$DateTime)
  
  # abline(h=0.33,col='grey50')
  polygon(x = c(x.range[1],x.range[2],x.range[2],x.range[1]),y = c(0,0,.33,.33),col=tran.func('grey',alpha = .25),border = NA)
  
  sd(gcc.df$GCC,na.rm = T) / mean(gcc.df$GCC,na.rm = T)
  # 
  
  plot(RCC~DateTime,data = gcc.df,col=col.df$beauty,pch=16)
  
  points(RCC~DateTime,data = gcc.df,col='red',pch=1)
  
  sd(gcc.df$RCC,na.rm = T) / mean(gcc.df$RCC,na.rm = T)
}

(0.34-.3) / (0.45-0.3)

tmp.df <- gcc.df[hour(gcc.df$DateTime)>=6,]
