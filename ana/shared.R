library(Reach)
library(svglite)
library(scales)

getConfidenceInterval <- function(data, variance = var(data), conf.level = 0.95, method='t-distr', resamples=1000, FUN=mean, returndist=FALSE) {
  
  if (method %in% c('t-distr','t')) {
    
    z = qt((1 - conf.level)/2, df = length(data) - 1, lower.tail = FALSE)
    
    xbar = mean(data)
    sdx = sqrt(variance/length(data))
    
    return(c(xbar - z * sdx, xbar, xbar + z * sdx))
    
  }
  
  # add sample z-distribution?
  
  # for bootstrapping:
  
  if (method %in% c('bootstrap','b')) {
    
    data <- data[which(is.finite(data))] #need is.finite due to NA values
    
    samplematrix <- matrix(sample(data, size = resamples*length(data), replace = TRUE), nrow = resamples)
    
    BS <- apply(samplematrix, c(1), FUN=FUN) 
    
    lo <- (1-conf.level)/2.
    hi <- 1 - lo
    mid <- .50
    
    if (returndist) {
      percentiles <- data.frame(percentile=seq(.01,.99,.01),value=quantile(BS, probs=seq(.01,.99,.01)))
      densdist <- density(BS, bw='SJ', from=min(percentiles$value), to=max(percentiles$value))  
      return(list('percentiles'=percentiles, 'density'=densdist, 'CI95'=quantile(BS, probs = c(lo,hi))))
    } else {
      return(quantile(BS, probs = c(lo,mid,hi)))
    }
    
  }
  
}

getColourScheme <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270')){
  #create a list containing the colourscheme per group
  for (group in groups){
    colourscheme <- list()
    
    colourscheme[['T-RACING_0']] <- list('S'='#ff8200ff', # pure orange
                               'T'='#ff82002f')    #2f gives a lighter shade of the color
    
    colourscheme[['T-RACING_180']] <- list('S'='#e51636ff', #vivid/york red
                               'T'='#e516362f')
    
    colourscheme[['T-RACING_90']] <- list('S'='#c400c4ff', #strong magenta
                                        'T'='#c400c42f')
    
    colourscheme[['T-RACING_270']] <-   list('S'='#005de4ff', #pure blue
                                 'T'='#005de42f')
    
    #colourscheme[['ALIGNED']] <-   list('S'='#A9A9A9ff', #dark grey
    #                               'T'='#A9A9A92f')
    
  }
  return(colourscheme)
}

getAllTrackSession1ColourScheme <- function(blocks = c(1,2,3,4,5,6)){
  #create a list containing the colourscheme per group
  
  for(block in blocks){
    colourscheme <- list()
    
    #create a list containing the colourscheme per group
    colourscheme[[1]] <- list('S'=alpha('#c400c4ff', 0.5), #strong magenta
                         'T'='#c400c42f')
    
    colourscheme[[2]] <- list('S'=alpha('#c400c4ff', 0.55), #strong magenta
                         'T'='#c400c42f') 
    
    colourscheme[[3]] <- list('S'=alpha('#c400c4ff', 0.6), #strong magenta
                         'T'='#c400c42f') 
    
    colourscheme[[4]] <- list('S'=alpha('#c400c4ff', 0.65), #strong magenta
                         'T'='#c400c42f') 
    
    colourscheme[[5]] <- list('S'=alpha('#c400c4ff', 0.7), #strong magenta
                         'T'='#c400c42f') 
    
    colourscheme[[6]] <- list('S'='#c400c4ff', #strong magenta
                         'T'='#c400c42f') 
    
  }
  
  return(colourscheme)
}

getAllTrackSession2ColourScheme <- function(blocks = c(1,2,3,4)){
  
  for(block in blocks){
    if(block == 1 | block == 4){
      #create a list containing the colourscheme per group
      colourscheme <- list('S'='#e51636ff', #vivid/york red
                           'T'='#e516362f')
    } else if (block == 2){
      colourscheme <- list('S'='#ff8200ff', # pure orange
                           'T'='#ff82002f') 
    } else if (block == 3){
      colourscheme <- list('S'='#005de4ff', #pure blue
                           'T'='#005de42f') 
    }
  }
  return(colourscheme)
}

getAllTrackDayOneColourScheme <- function(){
  #create a list containing the colourscheme per group
  
  colourscheme <- list('S'='#e51636ff', #vivid/ york red
                       'T'='#e516362f')
  
  return(colourscheme)
}

getSAFS2ColourScheme <- function(blocks = c(1,2,3,4,5,6,7,8)){
  
  for(block in blocks){
    colourscheme <- list()
    
    #create a list containing the colourscheme per group
    colourscheme[[1]] <- list('S'=alpha('#e51636ff', 0.5), #vivid/york red
                         'T'='#e516362f')
    
    #create a list containing the colourscheme per group
    colourscheme[[2]] <- list('S'=alpha('#e51636ff', 0.5), #vivid/york red
                         'T'='#e516362f')
    
    #create a list containing the colourscheme per group
    colourscheme[[3]] <- list('S'=alpha('#ff8200ff', 1), #orange
                         'T'='#ff82002f')
    
    #create a list containing the colourscheme per group
    colourscheme[[4]] <- list('S'=alpha('#ff8200ff', 1), #orange
                         'T'='#ff82002f')
    
    #create a list containing the colourscheme per group
    colourscheme[[5]] <- list('S'=alpha('#005de4ff', 1), #blue
                         'T'='#005de42f')
    
    #create a list containing the colourscheme per group
    colourscheme[[6]] <- list('S'=alpha('#005de4ff', 1), #blue
                         'T'='#005de42f')
    
    #create a list containing the colourscheme per group
    colourscheme[[7]] <- list('S'=alpha('#e51636ff', 1), #vivid/york red
                         'T'='#e516362f')
    
    #create a list containing the colourscheme per group
    colourscheme[[8]] <- list('S'=alpha('#e51636ff', 1), #vivid/york red
                         'T'='#e516362f')
    
  }
  return(colourscheme)
}
