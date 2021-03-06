source('r/functions.R')
# modifed from Remko's function

# fn <- 'pic/qp/2/WSCT2951.jpg'

processImage.new <- function(fn, ROI=NULL){
  
  phot <- read_and_crop(fn,NULL)

  
  if(is.null(phot)){
    return(data.frame(filename=NA, GCC=NA, RCC=NA, BCC=NA, RGBtot=NA))
  }
  
  if(!is.null(ROI)){
    xmin <- ceiling(max(phot$x) * ROI[1])
    xmax <- ceiling((max(phot$x) * ROI[2]))
    
    ymin <- ceiling(max(phot$y) * ROI[3])
    ymax <- ceiling(max(phot$y) * ROI[4])
    
    phot <- phot[phot$x >= xmin & phot$x <= xmax &
                   phot$y >= ymin & phot$y <= ymax,]
  }
  
  # filter out white pixels
  phot[which(phot$R>.9&phot$G>.9&phot$B>.9),c('R','G','B')] <- 0
  
  # get GCC
  RDN <- mean(phot$R)
  GDN <- mean(phot$G)
  BDN <- mean(phot$B)
  bn <- RDN+GDN+BDN
  
  RI <- RDN/bn
  GI <- GDN/bn
  BI <- BDN/bn
  
  #GEI <- 2*GDN - (RDN + BDN)
  
  # Convention
  rgbtot <- bn * 255
  
  return(data.frame(filename=fn, GCC=GI, RCC=RI, BCC=BI, RGBtot=rgbtot))
}
# 
get_gcc_func <- function(fn, ROI=NULL){
  # path.vec='pic/ng/5/'

  # reread picture names
  # fn <- list.files(path.vec,pattern = '.JPG',full.names = T)
  # fn = c('download/irrigated/WSCT7365.JPG','download/irrigated/WSCT7366.JPG')
  # fn = 'pic/mp/1/MFDC0940.JPG'
  
  # get the date
  date.vec <- read_exif(fn)$CreateDate

  dateTime.corrected <- strptime(as.character(date.vec),'%Y:%m:%d %H:%M:%S')
  date.corrected <-  as.Date(dateTime.corrected,'%Y:%m:%d')
  
  if(hour(dateTime.corrected)>6){
    # read and calculate gcc
    temp.ls <- list()
    for(i in seq_along(fn)){
      temp.ls[[i]] <- processImage.new(fn[i], ROI=ROI)
    }
    
    # put gcc into a data frame
    gcc.day.df <- do.call(rbind,temp.ls)
    
    # gcc.day.df$DateTime <- as.Date(as.character(date.vec),'%Y%m%d')
    gcc.day.df$DateTime <- strptime(as.character(date.vec),'%Y:%m:%d %H:%M:%S')
    gcc.day.df$DateTime <- as.POSIXct(gcc.day.df$DateTime)
    gcc.day.df$Date <-  as.Date(gcc.day.df$DateTime,'%Y:%m:%d')
    
    return(gcc.day.df)
  }else{
    return(data.frame(filename = fn,
                      GCC=NA,
                      RCC = NA,
                      BCC = NA,
                      RGBtot = NA,
                      DateTime = NA,
                      Date = NA)
           )
  }
  
}

# 
get.smooth.gcc.func = function(Date.vec,gcc.vec){
  library(mgcv)
  library(lubridate)
  gam.frdm = round(length(Date.vec)/3)
  
  gam.in.df = data.frame(x = as.numeric(Date.vec),
                         y = gcc.vec)
  fit.gam <- gam(y~s(x,k = gam.frdm),data = gam.in.df)
  
  out.df = predict(fit.gam,gam.in.df)
  return(out.df)
}

# 
cal.gcc.site.func <- function(site.nm,ROI,pic.path = 'pic'){
  site.folders <- list.files(file.path(pic.path,site.nm))
  
  # loop through plots
  for(i in seq_along(site.folders)){
    # define the nm of output file
    out.nm <- sprintf('cache/gcc_%s_%s.rds',site.nm,site.folders[i])
    
    # check if the output file exists
    if(!file.exists(out.nm)){
      gcc.old.df <- data.frame(filename = NULL,
                               GCC=NULL,
                               RCC = NULL,
                               BCC = NULL,
                               RGBtot = NULL,
                               DateTime = NULL,
                               Date = NULL)
      saveRDS(gcc.old.df,out.nm)
    }
    
    # list all photos of the plot
    pic.vec <- list.files(sprintf('%s/%s/%s',pic.path,site.nm,site.folders[i]),full.names = T)
    
    # get old gcc
    gcc.old.df <- readRDS(out.nm)
    
    # take only those are not prcessed yet
    unprocessed.vec <- setdiff(pic.vec, gcc.old.df$filename)

    if(length(unprocessed.vec)>0){
      
      # loop throught the photos to get gcc
      for(j in seq_along(unprocessed.vec)){
        gcc.old.df <- readRDS(out.nm)
        # calculated GCC
        gcc.new.df <- try(get_gcc_func(unprocessed.vec[j],ROI = ROI))
        # see <-  get_gcc_func(pic.vec[1])
        if(class(gcc.new.df) == 'try-error'){
          gcc.new.df <- data.frame(filename = unprocessed.vec[j],
                                  GCC=NA,
                                  RCC = NA,
                                  BCC = NA,
                                  RGBtot = NA,
                                  DateTime = NA,
                                  Date = NA)
        }
        
        # put old and new gcc together
        gcc.out.df <- rbind(gcc.old.df,gcc.new.df)
 
        saveRDS(gcc.out.df,out.nm)
        
        # do a print to check prograss 
        print(unprocessed.vec[j])
      }
      
    }
    
  }
  
}

