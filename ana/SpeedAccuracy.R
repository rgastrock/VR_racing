source('ana/shared.R')
source('ana/lapTime.R')
source('ana/percentOnTrack.R')

#get all track orientations (group) data for MT and accuracy
#per trial: have a mean MT and accuracy measure (mean across participants)
#sort MTs from lowest to highest
#plotting data this way is noisy, so we can bin and take averages per bin - say 5 bins (every 6 trials)
#repeat for session 2 then plot

#Session 1 (bin by MT)-----
getAverageMTandAccuracy <- function(){
  #start with movement time
  mtdat <- getAllTrackGroupLap()
  
  #get the first and last block of 30 trials, remove first trial of every block
  b1trials <- c(2:30)
  b2trials <- c(272:300)
  mtb1 <- mtdat[b1trials,]
  mtb2 <- mtdat[b2trials,]
  
  b1dat <- c()
  for(trialno in c(1:nrow(mtb1))){
    ndat <- as.numeric(mtb1[trialno, 2:ncol(mtb1)])
    ndat <- mean(ndat, na.rm = T)
    b1dat <- c(b1dat, ndat)
  }
  
  b2dat <- c()
  for(trialno in c(1:nrow(mtb2))){
    ndat <- as.numeric(mtb2[trialno, 2:ncol(mtb2)])
    ndat <- mean(ndat, na.rm = T)
    b2dat <- c(b2dat, ndat)
  }
  
  laptime <- c(b1dat, b2dat)
  trial <- c(b1trials, b2trials)
  
  outdat <- data.frame(trial, laptime)
  
  #then we add accuracy to the data frame
  accdat <- getAllTrackGroupAccuracy()
  accb1 <- accdat[b1trials,]
  accb2 <- accdat[b2trials,]
  
  b1dat <- c()
  for(trialno in c(1:nrow(accb1))){
    ndat <- as.numeric(accb1[trialno, 2:ncol(accb1)])
    ndat <- mean(ndat, na.rm = T)
    b1dat <- c(b1dat, ndat)
  }
  
  b2dat <- c()
  for(trialno in c(1:nrow(accb2))){
    ndat <- as.numeric(accb2[trialno, 2:ncol(accb2)])
    ndat <- mean(ndat, na.rm = T)
    b2dat <- c(b2dat, ndat)
  }
  
  accuracy <- c(b1dat, b2dat)
  outdat$accuracy <- accuracy
  return(outdat)
  
}

getBinnedSpeedAccCI <- function(block, type= 'b'){
  
  if(block == 'first'){
    b1trials <- c(2:30)
  } else if(block == 'last'){
    b1trials <- c(272:300)
  }
  
  data <- getAverageMTandAccuracy()
  
  b1data <- data[which(data$trial %in% b1trials),]
  ndat <- b1data[order(b1data$laptime, decreasing = T),]
  n <- 5
  subgroups <- c(1:ceiling(nrow(ndat)/ n))
  binmt_low <- c()
  binmt_mid <- c()
  binmt_upper <- c()
  binacc_low <- c()
  binacc_mid <- c()
  binacc_upper <- c()
  for(sub in c(1:n)){
    
    subdat <- ndat[subgroups,]
    mt <- getConfidenceInterval(data = subdat$laptime, variance = var(subdat$laptime), method = type)
    acc <- getConfidenceInterval(data = subdat$accuracy, variance = var(subdat$accuracy), method = type)
    
    binmt_low <- c(binmt_low, mt[1])
    binmt_mid <- c(binmt_mid, mt[2])
    binmt_upper <- c(binmt_upper, mt[3])
    binacc_low <- c(binacc_low, acc[1])
    binacc_mid <- c(binacc_mid, acc[2])
    binacc_upper <- c(binacc_upper, acc[3])
    
    subgroups <- subgroups +6
  }
  s1b1 <- data.frame(binmt_low, binmt_mid, binmt_upper, binacc_low, binacc_mid, binacc_upper)
  return(s1b1)
}

#Session 2 (bin by MT)-----
getS2AverageMTandAccuracy <- function(){
  #start with movement time
  mtdat <- getS2AllTrackGroupLap()
  
  #get blocks of 30 trials, remove first trial of every block
  b1trials <- c(2:30)
  b2trials <- c(32:60)
  b3trials <- c(62:90)
  b4trials <- c(92:120)
  
  mtb1 <- mtdat[b1trials,]
  mtb2 <- mtdat[b2trials,]
  mtb3 <- mtdat[b3trials,]
  mtb4 <- mtdat[b4trials,]
  
  b1dat <- c()
  for(trialno in c(1:nrow(mtb1))){
    ndat <- as.numeric(mtb1[trialno, 2:ncol(mtb1)])
    ndat <- mean(ndat, na.rm = T)
    b1dat <- c(b1dat, ndat)
  }
  
  b2dat <- c()
  for(trialno in c(1:nrow(mtb2))){
    ndat <- as.numeric(mtb2[trialno, 2:ncol(mtb2)])
    ndat <- mean(ndat, na.rm = T)
    b2dat <- c(b2dat, ndat)
  }
  
  b3dat <- c()
  for(trialno in c(1:nrow(mtb3))){
    ndat <- as.numeric(mtb3[trialno, 2:ncol(mtb3)])
    ndat <- mean(ndat, na.rm = T)
    b3dat <- c(b3dat, ndat)
  }
  
  b4dat <- c()
  for(trialno in c(1:nrow(mtb4))){
    ndat <- as.numeric(mtb4[trialno, 2:ncol(mtb4)])
    ndat <- mean(ndat, na.rm = T)
    b4dat <- c(b4dat, ndat)
  }
  
  laptime <- c(b1dat, b2dat, b3dat, b4dat)
  trial <- c(b1trials, b2trials, b3trials, b4trials)
  
  outdat <- data.frame(trial, laptime)
  
  #then we add accuracy to the data frame
  accdat <- getS2AllTrackGroupAccuracy()
  accb1 <- accdat[b1trials,]
  accb2 <- accdat[b2trials,]
  accb3 <- accdat[b3trials,]
  accb4 <- accdat[b4trials,]
  
  b1dat <- c()
  for(trialno in c(1:nrow(accb1))){
    ndat <- as.numeric(accb1[trialno, 2:ncol(accb1)])
    ndat <- mean(ndat, na.rm = T)
    b1dat <- c(b1dat, ndat)
  }
  
  b2dat <- c()
  for(trialno in c(1:nrow(accb2))){
    ndat <- as.numeric(accb2[trialno, 2:ncol(accb2)])
    ndat <- mean(ndat, na.rm = T)
    b2dat <- c(b2dat, ndat)
  }
  
  b3dat <- c()
  for(trialno in c(1:nrow(accb3))){
    ndat <- as.numeric(accb3[trialno, 2:ncol(accb3)])
    ndat <- mean(ndat, na.rm = T)
    b3dat <- c(b3dat, ndat)
  }
  
  b4dat <- c()
  for(trialno in c(1:nrow(accb4))){
    ndat <- as.numeric(accb4[trialno, 2:ncol(accb4)])
    ndat <- mean(ndat, na.rm = T)
    b4dat <- c(b4dat, ndat)
  }
  
  accuracy <- c(b1dat, b2dat, b3dat, b4dat)
  outdat$accuracy <- accuracy
  return(outdat)
  
}

getS2BinnedSpeedAccCI <- function(block, type = 'b'){
  
  if(block == 1){
    b1trials <- c(2:30)
  } else if(block == 2){
    b1trials <- c(32:60)
  } else if(block == 3){
    b1trials <- c(62:90)
  } else if(block == 4){
    b1trials <- c(92:120)
  }
  
  data <- getS2AverageMTandAccuracy()
  
  b1data <- data[which(data$trial %in% b1trials),]
  ndat <- b1data[order(b1data$laptime, decreasing = T),]
  n <- 5
  subgroups <- c(1:ceiling(nrow(ndat)/ n))
  binmt_low <- c()
  binmt_mid <- c()
  binmt_upper <- c()
  binacc_low <- c()
  binacc_mid <- c()
  binacc_upper <- c()
  for(sub in c(1:n)){
    
    subdat <- ndat[subgroups,]
    mt <- getConfidenceInterval(data = subdat$laptime, variance = var(subdat$laptime), method = type)
    acc <- getConfidenceInterval(data = subdat$accuracy, variance = var(subdat$accuracy), method = type)
    
    binmt_low <- c(binmt_low, mt[1])
    binmt_mid <- c(binmt_mid, mt[2])
    binmt_upper <- c(binmt_upper, mt[3])
    binacc_low <- c(binacc_low, acc[1])
    binacc_mid <- c(binacc_mid, acc[2])
    binacc_upper <- c(binacc_upper, acc[3])
    
    subgroups <- subgroups +6
  }
  s1b1 <- data.frame(binmt_low, binmt_mid, binmt_upper, binacc_low, binacc_mid, binacc_upper)
  return(s1b1)
}

#Plot Speed-Accuracy tradeoffs (bin by MT)----

plotSpeedAccuracyTradeoffs <- function(target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig10_SAF_AllTrack.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  plot(NA, NA, xlim = c(2,7), ylim = c(90, 101), 
       xlab = "Lap time (s)", ylab = "Accuracy (% on track)", frame.plot = FALSE, #frame.plot takes away borders
       main = 'Speed-accuracy tradeoffs', xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  #abline(v = c(30, 60, 90, 120, 150, 180, 210, 240, 270), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5)) #tick marks for x axis
  axis(2, at = c(90, 92, 93, 94, 95, 96, 97, 98, 99, 100), las=2) #tick marks for y axis
  
  
  #read in files created by CI function
  s1b1 <- getBinnedSpeedAccCI(block = 'first')
  s1b2 <- getBinnedSpeedAccCI(block = 'last')
  s2b1 <- getS2BinnedSpeedAccCI(block = 1)
  s2b2 <- getS2BinnedSpeedAccCI(block = 2)
  s2b3 <- getS2BinnedSpeedAccCI(block = 3)
  s2b4 <- getS2BinnedSpeedAccCI(block = 4)
  
  colourscheme <- getAllTrackSession1ColourScheme()
  #session 1, first block
  for(i in c(1:nrow(s1b1))){
    subdat <- s1b1[i,]
    col <- colourscheme[['S']]
    points(subdat$binmt_mid, subdat$binacc_mid,pch=16,cex=1.5,col=col)
    col <- colourscheme[['T']]
    lines(rep(subdat$binmt_mid,2), c(subdat$binacc_low, subdat$binacc_upper),col=col)
  }
  col <- colourscheme[['S']]
  lines(s1b1$binmt_mid, s1b1$binacc_mid, col=col, lty = 1, lwd = 2)
  
  #session 1, last block
  colourscheme <- getAllTrackSessionEndColourScheme()
  for(i in c(1:nrow(s1b2))){
    subdat <- s1b2[i,]
    col <- colourscheme[['S']]
    points(subdat$binmt_mid, subdat$binacc_mid,pch=16,cex=1.5,col=col)
    col <- colourscheme[['T']]
    lines(rep(subdat$binmt_mid,2), c(subdat$binacc_low, subdat$binacc_upper),col=col)
  }
  col <- colourscheme[['S']]
  lines(s1b2$binmt_mid, s1b2$binacc_mid, col=col, lty=1, lwd = 2)
  
  #session 2, block 1
  colourscheme <- getAllTrackSession2ColourScheme(blocks = 1)
  for(i in c(1:nrow(s2b1))){
    subdat <- s2b1[i,]
    col <- colourscheme[['S']]
    points(subdat$binmt_mid, subdat$binacc_mid,pch=16,cex=1.5,col=col)
    col <- colourscheme[['T']]
    lines(rep(subdat$binmt_mid,2), c(subdat$binacc_low, subdat$binacc_upper),col=col)
  }
  col <- colourscheme[['S']]
  lines(s2b1$binmt_mid, s2b1$binacc_mid, col=col, lty=1, lwd = 2)
  
  #session 2, block 2
  colourscheme <- getAllTrackSession2ColourScheme(blocks = 2)
  for(i in c(1:nrow(s2b2))){
    subdat <- s2b2[i,]
    col <- colourscheme[['S']]
    points(subdat$binmt_mid, subdat$binacc_mid,pch=16,cex=1.5,col=col)
    col <- colourscheme[['T']]
    lines(rep(subdat$binmt_mid,2), c(subdat$binacc_low, subdat$binacc_upper),col=col)
  }
  col <- colourscheme[['S']]
  lines(s2b2$binmt_mid, s2b2$binacc_mid, col=col, lty=1, lwd = 2)
  
  #session 2, block 3
  colourscheme <- getAllTrackSession2ColourScheme(blocks = 3)
  for(i in c(1:nrow(s2b3))){
    subdat <- s2b3[i,]
    col <- colourscheme[['S']]
    points(subdat$binmt_mid, subdat$binacc_mid,pch=16,cex=1.5,col=col)
    col <- colourscheme[['T']]
    lines(rep(subdat$binmt_mid,2), c(subdat$binacc_low, subdat$binacc_upper),col=col)
  }
  col <- colourscheme[['S']]
  lines(s2b3$binmt_mid, s2b3$binacc_mid, col=col, lty=1, lwd = 2)
  
  #session 2, block 4
  colourscheme <- getAllTrackSessionEndColourScheme()
  for(i in c(1:nrow(s2b4))){
    subdat <- s2b4[i,]
    col <- colourscheme[['S']]
    points(subdat$binmt_mid, subdat$binacc_mid,pch=16,cex=1.5,col=col)
    col <- colourscheme[['T']]
    lines(rep(subdat$binmt_mid,2), c(subdat$binacc_low, subdat$binacc_upper),col=col)
  }
  col <- colourscheme[['S']]
  lines(s2b4$binmt_mid, s2b4$binacc_mid, col=col, lty=1, lwd = 2)
  
  
  #add legend
  # legend(200,18,legend=c('track_0°', 'track_90°', 'track_180°', 'track_270°'),
  #        col=c(colourscheme[['T-RACING_0']][['S']],colourscheme[['T-RACING_90']][['S']],colourscheme[['T-RACING_180']][['S']],colourscheme[['T-RACING_270']][['S']]),
  #        lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Session 1-----
getLapTimeandAccuracy <- function(){
  #start with movement time
  mtdat <- getAllTrackGroupLap()
  
  #get the first and last block of 30 trials, remove first trial of every block
  b1trials <- c(2:30)
  b2trials <- c(272:300)
  mtb1 <- mtdat[b1trials,]
  mtb2 <- mtdat[b2trials,]
  
  #block 1 lap times, day 1
  blockdefs <- list('b1' = c(2:6), 'b2' = c(7:12), 'b3' = c(13:18), 'b4' = c(19:24), 'b5' = c(25:30))
  
  mtb1_trialave <- data.frame()
  for(block in c(1:length(blockdefs))){
    trials <- blockdefs[[block]]
    ndat <- mtb1[which(mtb1$trial %in% trials),]
    ndat <- ndat[,2:ncol(ndat)]
    trialave <- as.numeric(colMeans(ndat)) #mean across trials per participant
    
    if (prod(dim(mtb1_trialave)) == 0){
      mtb1_trialave <- trialave
    } else {
      mtb1_trialave <- rbind(mtb1_trialave, trialave)
    }
  }
  mtb1_trialave <- as.data.frame(mtb1_trialave)
  
  
  #block last lap times, day 1
  blockdefs <- list('b1' = c(272:276), 'b2' = c(277:282), 'b3' = c(283:288), 'b4' = c(289:294), 'b5' = c(295:300))
  
  mtb2_trialave <- data.frame()
  for(block in c(1:length(blockdefs))){
    trials <- blockdefs[[block]]
    ndat <- mtb2[which(mtb2$trial %in% trials),]
    ndat <- ndat[,2:ncol(ndat)]
    trialave <- as.numeric(colMeans(ndat)) #mean across trials per participant
    
    if (prod(dim(mtb2_trialave)) == 0){
      mtb2_trialave <- trialave
    } else {
      mtb2_trialave <- rbind(mtb2_trialave, trialave)
    }
  }
  mtb2_trialave <- as.data.frame(mtb2_trialave)
  
  #then we add accuracy to the data frame
  accdat <- getAllTrackGroupAccuracy()
  accb1 <- accdat[b1trials,]
  accb2 <- accdat[b2trials,]
  
  #block 1 lap times, day 1
  blockdefs <- list('b1' = c(2:6), 'b2' = c(7:12), 'b3' = c(13:18), 'b4' = c(19:24), 'b5' = c(25:30))
  
  accb1_trialave <- data.frame()
  for(block in c(1:length(blockdefs))){
    trials <- blockdefs[[block]]
    ndat <- accb1[which(accb1$trial %in% trials),]
    ndat <- ndat[,2:ncol(ndat)]
    trialave <- as.numeric(colMeans(ndat)) #mean across trials per participant
    
    if (prod(dim(accb1_trialave)) == 0){
      accb1_trialave <- trialave
    } else {
      accb1_trialave <- rbind(accb1_trialave, trialave)
    }
  }
  accb1_trialave <- as.data.frame(accb1_trialave)
  
  #block last lap times, day 1
  blockdefs <- list('b1' = c(272:276), 'b2' = c(277:282), 'b3' = c(283:288), 'b4' = c(289:294), 'b5' = c(295:300))
  
  accb2_trialave <- data.frame()
  for(block in c(1:length(blockdefs))){
    trials <- blockdefs[[block]]
    ndat <- accb2[which(accb2$trial %in% trials),]
    ndat <- ndat[,2:ncol(ndat)]
    trialave <- as.numeric(colMeans(ndat)) #mean across trials per participant
    
    if (prod(dim(accb2_trialave)) == 0){
      accb2_trialave <- trialave
    } else {
      accb2_trialave <- rbind(accb2_trialave, trialave)
    }
  }
  accb2_trialave <- as.data.frame(accb2_trialave)
  
  outdat <- list('mtb1'=mtb1_trialave, 'mtb2'=mtb2_trialave, 'accb1'=accb1_trialave, 'accb2'=accb2_trialave)
  
  return(outdat)
  
}

getLapTimeandAccuracyCI <- function(type = 'b'){
  
  data <- getLapTimeandAccuracy()
  
  for(subdat in c(1:length(data))){
    confidence <- data.frame()
    blocksubdat <- data[[subdat]]
    for(t in c(1:nrow(data[[subdat]]))){
      block <- as.numeric(blocksubdat[t, ])
      if (type == "t"){
        block <- block[!is.na(block)]
        citrial <- getConfidenceInterval(data = block, variance = var(block), method = type)
      } else if(type == "b"){
        citrial <- getConfidenceInterval(data = block, variance = var(block), method = type)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
      
    }
    if(subdat == 1){
      write.csv(confidence, file='data/SAFCI_LapTime_FirstBlock_S001.csv', row.names = F) 
    } else if (subdat == 2){
      write.csv(confidence, file='data/SAFCI_LapTime_LastBlock_S001.csv', row.names = F) 
    } else if (subdat == 3){
      write.csv(confidence, file='data/SAFCI_Accuracy_FirstBlock_S001.csv', row.names = F) 
    } else if (subdat == 4){
      write.csv(confidence, file='data/SAFCI_Accuracy_LastBlock_S001.csv', row.names = F) 
    }
    
  }
}














# kernelRegression <- function(x,y,width,interpoints) {
#   
#   output <- c()
#   
#   for (interpoint in interpoints) {
#     
#     w <- exp(-1 * ((x-interpoint)^2/(2 * width^2)))
#     output <- c(output, sum(w*y)/sum(w))
#     
#   }
#   
#   return(output)
#   
# }
# 
# interpoints <- c(3.5, 4.0, 4.5, 5, 5.5, 6, 6.5)
# ab <- kernelRegression(x=ndat$laptime,y=ndat$accuracy,width=.5,interpoints=interpoints)









