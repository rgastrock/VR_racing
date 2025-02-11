source('ana/shared.R')

getBehaviorData <- function(){
  
  Reach::downloadOSFdata(repository = 'w5d4u',
                         filelist = list('data/' =c('T-RACING_0.zip', 'T-RACING_180.zip', 'T-RACING_90.zip', 'T-RACING_270.zip')),
                         folder = 'data/',
                         overwrite = TRUE,
                         unzip = TRUE,
                         removezips = TRUE)
}