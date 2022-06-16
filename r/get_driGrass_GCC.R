library(jpeg)
# library(phenora)
# library(gridExtra)
library(exifr)
library(doBy)
library(imager)
library(lubridate)

source('r/function_get_gcc.R')
cal.gcc.dg.func <- function(site.nm,ROI,pic.path){
  site.folders <- list.files(file.path(pic.path))
  # site.nm <- 'driGrass'
  # loop through plots
  for(i in seq_along(site.folders)){
    # define the nm of output file
    out.nm <- sprintf('cache/driGrass/gcc_%s_%s.rds',
                      site.nm,
                      site.folders[i])
    
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
    pic.vec <- list.files(sprintf('%s/%s',
                                  pic.path,site.folders[i]),
                          full.names = T)
    
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

# 
pic.path <- 'E:/storage/cloudStar/Shared/DriGrass photo/'
ROI.in <- c(.3,.7,.3,.7)
# calculate GCC for site
cal.gcc.dg.func(site.nm='driGrass',ROI = ROI.in,pic.path = pic.path)

# drigrass.10.df <- readRDS('cache/driGrass/gcc_driGrass_10.rds')
# drigrass.10.df <- drigrass.10.df[complete.cases(drigrass.10.df),]
# 
# drigrass.10.df <- drigrass.10.df[drigrass.10.df$RGBtot>300,]
# 
# plot(GCC~Date,data = drigrass.10.df,pch=16,col='darkseagreen')
