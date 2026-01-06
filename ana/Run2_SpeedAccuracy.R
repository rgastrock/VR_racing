source('ana/shared.R')
source('ana/Run2_lapTime.R')
source('ana/Run2_percentOnTrack.R')

#get all track orientations (group) data for MT and accuracy
#per trial: have a mean MT and accuracy measure (mean across participants)
#sort MTs from lowest to highest
#plotting data this way is noisy, so we can bin and take averages per bin - say 5 bins (every 6 trials)
#repeat for session 2 then plot

# #Session 1 (bin by MT)-----
# getR2AverageMTandAccuracy <- function(){
#   #start with movement time
#   mtdat <- getAllTrackGroupLap()
#   
#   #get the first and last block of 30 trials, remove first trial of every block
#   b1trials <- c(2:30)
#   b2trials <- c(272:300)
#   mtb1 <- mtdat[b1trials,]
#   mtb2 <- mtdat[b2trials,]
#   
#   b1dat <- c()
#   for(trialno in c(1:nrow(mtb1))){
#     ndat <- as.numeric(mtb1[trialno, 2:ncol(mtb1)])
#     ndat <- mean(ndat, na.rm = T)
#     b1dat <- c(b1dat, ndat)
#   }
#   
#   b2dat <- c()
#   for(trialno in c(1:nrow(mtb2))){
#     ndat <- as.numeric(mtb2[trialno, 2:ncol(mtb2)])
#     ndat <- mean(ndat, na.rm = T)
#     b2dat <- c(b2dat, ndat)
#   }
#   
#   laptime <- c(b1dat, b2dat)
#   trial <- c(b1trials, b2trials)
#   
#   outdat <- data.frame(trial, laptime)
#   
#   #then we add accuracy to the data frame
#   accdat <- getAllTrackGroupAccuracy()
#   accb1 <- accdat[b1trials,]
#   accb2 <- accdat[b2trials,]
#   
#   b1dat <- c()
#   for(trialno in c(1:nrow(accb1))){
#     ndat <- as.numeric(accb1[trialno, 2:ncol(accb1)])
#     ndat <- mean(ndat, na.rm = T)
#     b1dat <- c(b1dat, ndat)
#   }
#   
#   b2dat <- c()
#   for(trialno in c(1:nrow(accb2))){
#     ndat <- as.numeric(accb2[trialno, 2:ncol(accb2)])
#     ndat <- mean(ndat, na.rm = T)
#     b2dat <- c(b2dat, ndat)
#   }
#   
#   accuracy <- c(b1dat, b2dat)
#   outdat$accuracy <- accuracy
#   return(outdat)
#   
# }
# 
# getR2BinnedSpeedAccCI <- function(block, type= 'b'){
#   
#   if(block == 'first'){
#     b1trials <- c(2:30)
#   } else if(block == 'last'){
#     b1trials <- c(272:300)
#   }
#   
#   data <- getAverageMTandAccuracy()
#   
#   b1data <- data[which(data$trial %in% b1trials),]
#   ndat <- b1data[order(b1data$laptime, decreasing = T),]
#   n <- 5
#   subgroups <- c(1:ceiling(nrow(ndat)/ n))
#   binmt_low <- c()
#   binmt_mid <- c()
#   binmt_upper <- c()
#   binacc_low <- c()
#   binacc_mid <- c()
#   binacc_upper <- c()
#   for(sub in c(1:n)){
#     
#     subdat <- ndat[subgroups,]
#     mt <- getConfidenceInterval(data = subdat$laptime, variance = var(subdat$laptime), method = type)
#     acc <- getConfidenceInterval(data = subdat$accuracy, variance = var(subdat$accuracy), method = type)
#     
#     binmt_low <- c(binmt_low, mt[1])
#     binmt_mid <- c(binmt_mid, mt[2])
#     binmt_upper <- c(binmt_upper, mt[3])
#     binacc_low <- c(binacc_low, acc[1])
#     binacc_mid <- c(binacc_mid, acc[2])
#     binacc_upper <- c(binacc_upper, acc[3])
#     
#     subgroups <- subgroups +6
#   }
#   s1b1 <- data.frame(binmt_low, binmt_mid, binmt_upper, binacc_low, binacc_mid, binacc_upper)
#   return(s1b1)
# }
# 
# #Session 2 (bin by MT)-----
# getR2S2AverageMTandAccuracy <- function(){
#   #start with movement time
#   mtdat <- getS2AllTrackGroupLap()
#   
#   #get blocks of 30 trials, remove first trial of every block
#   b1trials <- c(2:30)
#   b2trials <- c(32:60)
#   b3trials <- c(62:90)
#   b4trials <- c(92:120)
#   
#   mtb1 <- mtdat[b1trials,]
#   mtb2 <- mtdat[b2trials,]
#   mtb3 <- mtdat[b3trials,]
#   mtb4 <- mtdat[b4trials,]
#   
#   b1dat <- c()
#   for(trialno in c(1:nrow(mtb1))){
#     ndat <- as.numeric(mtb1[trialno, 2:ncol(mtb1)])
#     ndat <- mean(ndat, na.rm = T)
#     b1dat <- c(b1dat, ndat)
#   }
#   
#   b2dat <- c()
#   for(trialno in c(1:nrow(mtb2))){
#     ndat <- as.numeric(mtb2[trialno, 2:ncol(mtb2)])
#     ndat <- mean(ndat, na.rm = T)
#     b2dat <- c(b2dat, ndat)
#   }
#   
#   b3dat <- c()
#   for(trialno in c(1:nrow(mtb3))){
#     ndat <- as.numeric(mtb3[trialno, 2:ncol(mtb3)])
#     ndat <- mean(ndat, na.rm = T)
#     b3dat <- c(b3dat, ndat)
#   }
#   
#   b4dat <- c()
#   for(trialno in c(1:nrow(mtb4))){
#     ndat <- as.numeric(mtb4[trialno, 2:ncol(mtb4)])
#     ndat <- mean(ndat, na.rm = T)
#     b4dat <- c(b4dat, ndat)
#   }
#   
#   laptime <- c(b1dat, b2dat, b3dat, b4dat)
#   trial <- c(b1trials, b2trials, b3trials, b4trials)
#   
#   outdat <- data.frame(trial, laptime)
#   
#   #then we add accuracy to the data frame
#   accdat <- getS2AllTrackGroupAccuracy()
#   accb1 <- accdat[b1trials,]
#   accb2 <- accdat[b2trials,]
#   accb3 <- accdat[b3trials,]
#   accb4 <- accdat[b4trials,]
#   
#   b1dat <- c()
#   for(trialno in c(1:nrow(accb1))){
#     ndat <- as.numeric(accb1[trialno, 2:ncol(accb1)])
#     ndat <- mean(ndat, na.rm = T)
#     b1dat <- c(b1dat, ndat)
#   }
#   
#   b2dat <- c()
#   for(trialno in c(1:nrow(accb2))){
#     ndat <- as.numeric(accb2[trialno, 2:ncol(accb2)])
#     ndat <- mean(ndat, na.rm = T)
#     b2dat <- c(b2dat, ndat)
#   }
#   
#   b3dat <- c()
#   for(trialno in c(1:nrow(accb3))){
#     ndat <- as.numeric(accb3[trialno, 2:ncol(accb3)])
#     ndat <- mean(ndat, na.rm = T)
#     b3dat <- c(b3dat, ndat)
#   }
#   
#   b4dat <- c()
#   for(trialno in c(1:nrow(accb4))){
#     ndat <- as.numeric(accb4[trialno, 2:ncol(accb4)])
#     ndat <- mean(ndat, na.rm = T)
#     b4dat <- c(b4dat, ndat)
#   }
#   
#   accuracy <- c(b1dat, b2dat, b3dat, b4dat)
#   outdat$accuracy <- accuracy
#   return(outdat)
#   
# }
# 
# getR2S2BinnedSpeedAccCI <- function(block, type = 'b'){
#   
#   if(block == 1){
#     b1trials <- c(2:30)
#   } else if(block == 2){
#     b1trials <- c(32:60)
#   } else if(block == 3){
#     b1trials <- c(62:90)
#   } else if(block == 4){
#     b1trials <- c(92:120)
#   }
#   
#   data <- getS2AverageMTandAccuracy()
#   
#   b1data <- data[which(data$trial %in% b1trials),]
#   ndat <- b1data[order(b1data$laptime, decreasing = T),]
#   n <- 5
#   subgroups <- c(1:ceiling(nrow(ndat)/ n))
#   binmt_low <- c()
#   binmt_mid <- c()
#   binmt_upper <- c()
#   binacc_low <- c()
#   binacc_mid <- c()
#   binacc_upper <- c()
#   for(sub in c(1:n)){
#     
#     subdat <- ndat[subgroups,]
#     mt <- getConfidenceInterval(data = subdat$laptime, variance = var(subdat$laptime), method = type)
#     acc <- getConfidenceInterval(data = subdat$accuracy, variance = var(subdat$accuracy), method = type)
#     
#     binmt_low <- c(binmt_low, mt[1])
#     binmt_mid <- c(binmt_mid, mt[2])
#     binmt_upper <- c(binmt_upper, mt[3])
#     binacc_low <- c(binacc_low, acc[1])
#     binacc_mid <- c(binacc_mid, acc[2])
#     binacc_upper <- c(binacc_upper, acc[3])
#     
#     subgroups <- subgroups +6
#   }
#   s1b1 <- data.frame(binmt_low, binmt_mid, binmt_upper, binacc_low, binacc_mid, binacc_upper)
#   return(s1b1)
# }
# 
# #Plot Speed-Accuracy tradeoffs (bin by MT)----
# 
# plotR2SpeedAccuracyTradeoffs <- function(target='inline') {
#   
#   
#   #but we can save plot as svg file
#   if (target=='svg') {
#     svglite(file='doc/fig/Fig10_SAF_AllTrack.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
#   }
#   
#   # create plot
#   #NA to create empty plot
#   plot(NA, NA, xlim = c(2,7), ylim = c(90, 101), 
#        xlab = "Lap time (s)", ylab = "Accuracy (% on track)", frame.plot = FALSE, #frame.plot takes away borders
#        main = 'Speed-accuracy tradeoffs', xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
#   #abline(v = c(30, 60, 90, 120, 150, 180, 210, 240, 270), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
#   axis(1, at = c(2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5)) #tick marks for x axis
#   axis(2, at = c(90, 92, 93, 94, 95, 96, 97, 98, 99, 100), las=2) #tick marks for y axis
#   
#   
#   #read in files created by CI function
#   s1b1 <- getBinnedSpeedAccCI(block = 'first')
#   s1b2 <- getBinnedSpeedAccCI(block = 'last')
#   s2b1 <- getS2BinnedSpeedAccCI(block = 1)
#   s2b2 <- getS2BinnedSpeedAccCI(block = 2)
#   s2b3 <- getS2BinnedSpeedAccCI(block = 3)
#   s2b4 <- getS2BinnedSpeedAccCI(block = 4)
#   
#   colourscheme <- getAllTrackSession1ColourScheme()
#   #session 1, first block
#   for(i in c(1:nrow(s1b1))){
#     subdat <- s1b1[i,]
#     col <- colourscheme[['S']]
#     points(subdat$binmt_mid, subdat$binacc_mid,pch=16,cex=1.5,col=col)
#     col <- colourscheme[['T']]
#     lines(rep(subdat$binmt_mid,2), c(subdat$binacc_low, subdat$binacc_upper),col=col)
#   }
#   col <- colourscheme[['S']]
#   lines(s1b1$binmt_mid, s1b1$binacc_mid, col=col, lty = 1, lwd = 2)
#   
#   #session 1, last block
#   colourscheme <- getAllTrackSessionEndColourScheme()
#   for(i in c(1:nrow(s1b2))){
#     subdat <- s1b2[i,]
#     col <- colourscheme[['S']]
#     points(subdat$binmt_mid, subdat$binacc_mid,pch=16,cex=1.5,col=col)
#     col <- colourscheme[['T']]
#     lines(rep(subdat$binmt_mid,2), c(subdat$binacc_low, subdat$binacc_upper),col=col)
#   }
#   col <- colourscheme[['S']]
#   lines(s1b2$binmt_mid, s1b2$binacc_mid, col=col, lty=1, lwd = 2)
#   
#   #session 2, block 1
#   colourscheme <- getAllTrackSession2ColourScheme(blocks = 1)
#   for(i in c(1:nrow(s2b1))){
#     subdat <- s2b1[i,]
#     col <- colourscheme[['S']]
#     points(subdat$binmt_mid, subdat$binacc_mid,pch=16,cex=1.5,col=col)
#     col <- colourscheme[['T']]
#     lines(rep(subdat$binmt_mid,2), c(subdat$binacc_low, subdat$binacc_upper),col=col)
#   }
#   col <- colourscheme[['S']]
#   lines(s2b1$binmt_mid, s2b1$binacc_mid, col=col, lty=1, lwd = 2)
#   
#   #session 2, block 2
#   colourscheme <- getAllTrackSession2ColourScheme(blocks = 2)
#   for(i in c(1:nrow(s2b2))){
#     subdat <- s2b2[i,]
#     col <- colourscheme[['S']]
#     points(subdat$binmt_mid, subdat$binacc_mid,pch=16,cex=1.5,col=col)
#     col <- colourscheme[['T']]
#     lines(rep(subdat$binmt_mid,2), c(subdat$binacc_low, subdat$binacc_upper),col=col)
#   }
#   col <- colourscheme[['S']]
#   lines(s2b2$binmt_mid, s2b2$binacc_mid, col=col, lty=1, lwd = 2)
#   
#   #session 2, block 3
#   colourscheme <- getAllTrackSession2ColourScheme(blocks = 3)
#   for(i in c(1:nrow(s2b3))){
#     subdat <- s2b3[i,]
#     col <- colourscheme[['S']]
#     points(subdat$binmt_mid, subdat$binacc_mid,pch=16,cex=1.5,col=col)
#     col <- colourscheme[['T']]
#     lines(rep(subdat$binmt_mid,2), c(subdat$binacc_low, subdat$binacc_upper),col=col)
#   }
#   col <- colourscheme[['S']]
#   lines(s2b3$binmt_mid, s2b3$binacc_mid, col=col, lty=1, lwd = 2)
#   
#   #session 2, block 4
#   colourscheme <- getAllTrackSessionEndColourScheme()
#   for(i in c(1:nrow(s2b4))){
#     subdat <- s2b4[i,]
#     col <- colourscheme[['S']]
#     points(subdat$binmt_mid, subdat$binacc_mid,pch=16,cex=1.5,col=col)
#     col <- colourscheme[['T']]
#     lines(rep(subdat$binmt_mid,2), c(subdat$binacc_low, subdat$binacc_upper),col=col)
#   }
#   col <- colourscheme[['S']]
#   lines(s2b4$binmt_mid, s2b4$binacc_mid, col=col, lty=1, lwd = 2)
#   
#   
#   #add legend
#   # legend(200,18,legend=c('track_0°', 'track_90°', 'track_180°', 'track_270°'),
#   #        col=c(colourscheme[['T-RACING_0']][['S']],colourscheme[['T-RACING_90']][['S']],colourscheme[['T-RACING_180']][['S']],colourscheme[['T-RACING_270']][['S']]),
#   #        lty=1,bty='n',cex=1,lwd=2)
#   
#   #close everything if you saved plot as svg
#   if (target=='svg') {
#     dev.off()
#   }
#   
# }

#Session 1-----
getR2LapTimeandAccuracy <- function(){
  #start with movement time
  mtdat <- getR2AllTrackGroupLap()
  
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
  accdat <- getR2AllTrackGroupAccuracy()
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

getR2LapTimeandAccuracyCI <- function(type = 'b'){
  
  data <- getR2LapTimeandAccuracy()
  
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
      write.csv(confidence, file='data/Run2_SAFCI_LapTime_FirstBlock_S001.csv', row.names = F) 
    } else if (subdat == 2){
      write.csv(confidence, file='data/Run2_SAFCI_LapTime_LastBlock_S001.csv', row.names = F) 
    } else if (subdat == 3){
      write.csv(confidence, file='data/Run2_SAFCI_Accuracy_FirstBlock_S001.csv', row.names = F) 
    } else if (subdat == 4){
      write.csv(confidence, file='data/Run2_SAFCI_Accuracy_LastBlock_S001.csv', row.names = F) 
    }
    
  }
}

#Session 2----
getR2S2LapTimeandAccuracy <- function(){
  #start with movement time
  mtdat <- getR2S2AllTrackGroupLap()
  
  #get the first and last block of 30 trials, remove first trial of every block
  # b1trials <- c(2:30)
  # b2trials <- c(272:300)
  # mtb1 <- mtdat[b1trials,]
  # mtb2 <- mtdat[b2trials,]
  
  #block lap times, day 2
  blockdefs <- list('b1' = c(2:6), 'b2' = c(25:30), 
                    'b3' = c(32:36), 'b4' = c(55:60), 
                    'b5' = c(62:66), 'b6' = c(85:90), 
                    'b7' = c(92:96), 'b8' = c(115:120))
  
  mtb1_trialave <- data.frame()
  for(block in c(1:length(blockdefs))){
    trials <- blockdefs[[block]]
    ndat <- mtdat[which(mtdat$trial %in% trials),]
    ndat <- ndat[,2:ncol(ndat)]
    trialave <- as.numeric(colMeans(ndat)) #mean across trials per participant
    
    if (prod(dim(mtb1_trialave)) == 0){
      mtb1_trialave <- trialave
    } else {
      mtb1_trialave <- rbind(mtb1_trialave, trialave)
    }
  }
  mtb1_trialave <- as.data.frame(mtb1_trialave)
  
  
  
  #then we add accuracy to the data frame
  accdat <- getR2S2AllTrackGroupAccuracy()
  
  #block accuracy, day 2
  blockdefs <- list('b1' = c(2:6), 'b2' = c(25:30), 
                    'b3' = c(32:36), 'b4' = c(55:60), 
                    'b5' = c(62:66), 'b6' = c(85:90), 
                    'b7' = c(92:96), 'b8' = c(115:120))
  
  accb1_trialave <- data.frame()
  for(block in c(1:length(blockdefs))){
    trials <- blockdefs[[block]]
    ndat <- accdat[which(accdat$trial %in% trials),]
    ndat <- ndat[,2:ncol(ndat)]
    trialave <- as.numeric(colMeans(ndat)) #mean across trials per participant
    
    if (prod(dim(accb1_trialave)) == 0){
      accb1_trialave <- trialave
    } else {
      accb1_trialave <- rbind(accb1_trialave, trialave)
    }
  }
  accb1_trialave <- as.data.frame(accb1_trialave)
  
  outdat <- list('mtS2'=mtb1_trialave, 'accS2'=accb1_trialave)
  
  return(outdat)
  
}

getR2S2LapTimeandAccuracyCI <- function(type = 'b'){
  
  data <- getR2S2LapTimeandAccuracy()
  
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
      write.csv(confidence, file='data/Run2_SAFCI_LapTime_AllBlocks_S002.csv', row.names = F) 
    } else if (subdat == 2){
      write.csv(confidence, file='data/Run2_SAFCI_Accuracy_AllBlocks_S002.csv', row.names = F)
      
    }
  }
}

#Plot Speed-Accuracy tradeoffs (bin by trials/blocks)----

plotR2SAF <- function(target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig11_SAFbyTrial_AllTrack.svg', width=7, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  plot(NA, NA, xlim = c(2,9), ylim = c(90, 101), 
       xlab = "Lap time (s)", ylab = "Accuracy (% on track)", frame.plot = FALSE, #frame.plot takes away borders
       main = 'Speed-accuracy across training', xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  #abline(v = c(30, 60, 90, 120, 150, 180, 210, 240, 270), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 8.5)) #tick marks for x axis
  axis(2, at = c(90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100), las=2) #tick marks for y axis
  
  
  #read in files created by CI function
  s1b1_laptime <- read.csv(file='data/Run2_SAFCI_LapTime_FirstBlock_S001.csv')
  s1b1_accuracy <- read.csv(file='data/Run2_SAFCI_Accuracy_FirstBlock_S001.csv')
  
  s1b2_laptime <- read.csv(file='data/Run2_SAFCI_LapTime_LastBlock_S001.csv')
  s1b2_accuracy <- read.csv(file='data/Run2_SAFCI_Accuracy_LastBlock_S001.csv')
  
  s2ball_laptime <- read.csv(file='data/Run2_SAFCI_LapTime_AllBlocks_S002.csv')
  s2ball_accuracy <- read.csv(file='data/Run2_SAFCI_Accuracy_AllBlocks_S002.csv')
  
  
  #session 1, first block
  for(i in c(1:nrow(s1b1_laptime))){
    colourscheme <- getAllTrackSession1ColourScheme(blocks=1)
    subdat_laptime <- s1b1_laptime[i,]
    subdat_accuracy <- s1b1_accuracy[i,]
    col <- colourscheme[[i]][['S']]
    points(subdat_laptime$X50., subdat_accuracy$X50.,pch=16,cex=1.5,col=col)
    
    col <- colourscheme[[i]][['T']]
    #accuracy CI
    lines(rep(subdat_laptime$X50.,2), c(subdat_accuracy$X2.5., subdat_accuracy$X97.5.),col=col)
    #laptime CI
    lines(c(subdat_laptime$X2.5., subdat_laptime$X97.5.), rep(subdat_accuracy$X50.,2),col=col)
    #text(subdat_laptime$X50., subdat_accuracy$X50.)
    #connect blocks in session 1
    if(i == 2){
      col <- colourscheme[[i]][['S']]
      lines(c(s1b1_laptime$X50.[1], s1b1_laptime$X50.[2]),c(s1b1_accuracy$X50.[1], s1b1_accuracy$X50.[2]), col=col, lty = 3)
    } else if (i == 3){
      col <- colourscheme[[i]][['S']]
      lines(c(s1b1_laptime$X50.[2], s1b1_laptime$X50.[3]),c(s1b1_accuracy$X50.[2], s1b1_accuracy$X50.[3]), col=col, lty = 3)
    } else if (i == 4){
      col <- colourscheme[[i]][['S']]
      lines(c(s1b1_laptime$X50.[3], s1b1_laptime$X50.[4]),c(s1b1_accuracy$X50.[3], s1b1_accuracy$X50.[4]), col=col, lty = 3)
    } else if (i == 5){
      col <- colourscheme[[i]][['S']]
      lines(c(s1b1_laptime$X50.[4], s1b1_laptime$X50.[5]),c(s1b1_accuracy$X50.[4], s1b1_accuracy$X50.[5]), col=col, lty = 3)
    }
    text(subdat_laptime$X50., subdat_accuracy$X50.,labels=i,cex=0.6,col='white')
  }
  
  #session 1, last block
  colourscheme <- getAllTrackSession1ColourScheme(blocks = 6)
  i <- nrow(s1b2_laptime)
  subdat_laptime <- s1b2_laptime[i,]
  subdat_accuracy <- s1b2_accuracy[i,]
  col <- colourscheme[[6]][['S']]
  points(subdat_laptime$X50., subdat_accuracy$X50.,pch=16,cex=1.5,col=col)
  lines(c(s1b1_laptime$X50.[5], s1b2_laptime$X50.[5]),c(s1b1_accuracy$X50.[5], s1b2_accuracy$X50.[5]), col=col, lty = 3)
  text(subdat_laptime$X50., subdat_accuracy$X50.,labels=1,cex=0.6,col='white')
  col <- colourscheme[[6]][['T']]
  #accuracy CI
  lines(rep(subdat_laptime$X50.,2), c(subdat_accuracy$X2.5., subdat_accuracy$X97.5.),col=col)
  #laptime CI
  lines(c(subdat_laptime$X2.5., subdat_laptime$X97.5.), rep(subdat_accuracy$X50.,2),col=col)
  
  
  #session 2
  for(i in c(1:nrow(s2ball_laptime))){
    colourscheme <- getSAFS2ColourScheme(blocks=i)
    subdat_laptime <- s2ball_laptime[i,]
    subdat_accuracy <- s2ball_accuracy[i,]
    col <- colourscheme[[i]][['S']]
    points(subdat_laptime$X50., subdat_accuracy$X50.,pch=15,cex=1.5,col=col)
    
    
    col <- colourscheme[[i]][['T']]
    #accuracy CI
    lines(rep(subdat_laptime$X50.,2), c(subdat_accuracy$X2.5., subdat_accuracy$X97.5.),col=col)
    #laptime CI
    lines(c(subdat_laptime$X2.5., subdat_laptime$X97.5.), rep(subdat_accuracy$X50.,2),col=col)
    
    #connect blocks in session 2
    if(i == 2){
      col <- colourscheme[[i]][['S']]
      lines(c(s2ball_laptime$X50.[1], s2ball_laptime$X50.[2]),c(s2ball_accuracy$X50.[1], s2ball_accuracy$X50.[2]), col=col, lty = 3)
    } else if (i == 4){
      col <- colourscheme[[i]][['S']]
      lines(c(s2ball_laptime$X50.[3], s2ball_laptime$X50.[4]),c(s2ball_accuracy$X50.[3], s2ball_accuracy$X50.[4]), col=col, lty = 3)
    } else if (i == 6){
      col <- colourscheme[[i]][['S']]
      lines(c(s2ball_laptime$X50.[5], s2ball_laptime$X50.[6]),c(s2ball_accuracy$X50.[5], s2ball_accuracy$X50.[6]), col=col, lty = 3)
    } else if (i == 8){
      col <- colourscheme[[i]][['S']]
      lines(c(s2ball_laptime$X50.[7], s2ball_laptime$X50.[8]),c(s2ball_accuracy$X50.[7], s2ball_accuracy$X50.[8]), col=col, lty = 3)
    }
    if(i %% 2 == 1){
      text(subdat_laptime$X50., subdat_accuracy$X50.,labels=1,cex=0.6,col='white')
    } else if(i %% 2 == 0){
      text(subdat_laptime$X50., subdat_accuracy$X50.,labels=2,cex=0.6,col='white')
    }
  }
  
  
  
  
  #add legend
  colourscheme <- getAllTrackSession1ColourScheme()
  legend(2.5,101.5,
         legend=c('1st block', 'last block'),
         col=c(colourscheme[[1]][['S']],colourscheme[[6]][['S']]),
         bty='n',cex=1,pch = 16, title='day 1')
  
  
  colourscheme <- getSAFS2ColourScheme()
  legend(4.15,101.5,
         legend=c('1st block',
                  '2nd block',
                  '3rd block',
                  'last block'),
         col=c(colourscheme[[1]][['S']],
               colourscheme[[3]][['S']],
               colourscheme[[5]][['S']],
               colourscheme[[7]][['S']]),
         bty='n',cex=1,pch = 15, ncol=2, title = 'day 2')
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Relationship between within session improvement and offline gains-----

getR2WithinImprovementOfflineGainsAcc <- function(){
  
  #Session 1
  data <- getR2LapTimeandAccuracy()
  
  set1block1 <- data[[3]][1,] #indexing lists of lists in data: 3rd list is accuracy for first 30 trials, we get the first set 5 trials (first trial removed)
  set5block2 <- data[[4]][5,] #same but now we want acc for last 30 trials, we get last set
  
  D1set1block1 <- set1block1[,-c(9,29)]#remove participant 044 and 007 as they did not do session 2 (9 and 29 are their idx in this df)
  D1set5block2 <- set5block2[,-c(9,29)]
  
  #Session 2
  data2 <- getR2S2LapTimeandAccuracy()
  D2set1block1 <- data2[[2]][1,] #same but for day 2
  
  ndat <- list('D1Set1Block1'=D1set1block1, 'D1Set5Block2'=D1set5block2, 'D2Set1Block1'=D2set1block1)
  
  #calculate normalized within session improvements
  WithinImprovement <- as.numeric(ndat[[2]][1,]) - as.numeric(ndat[[1]][1,]) #subtract last set last block and first set first block
  WImin <- min(WithinImprovement)
  WImax <- max(WithinImprovement)
  WInorm <- c()
  for(i in c(WithinImprovement)){
    normval <- 2 * ((i - WImin)/(WImax - WImin)) - 1
    WInorm <- c(WInorm, normval)
  }
  
  #calculate normalized offline gains
  OfflineGains <- as.numeric(ndat[[3]][1,]) - as.numeric(ndat[[2]][1,]) #subtract last set last block from first set first block day 2
  OGmin <- min(OfflineGains)
  OGmax <- max(OfflineGains)
  OGnorm <- c()
  for(i in c(OfflineGains)){
    normval <- 2 * ((i - OGmin)/(OGmax - OGmin)) - 1
    OGnorm <- c(OGnorm, normval)
  }
  
  plot(WInorm, OGnorm)
  corstats <- cor.test(WInorm, OGnorm)
  return(corstats)
}

#plot trial 2 and trial 300 for day 1 (individual data)----
plotR2IndividualLapTimeandAccuracy <- function(target='inline'){
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig12_SAFDay1_firstvslasttrial_AllTrack.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  #start with movement time
  mtdat <- getR2AllTrackGroupLap()
  
  #get trials 2 and 300
  trial2mt <- as.numeric(mtdat[2,c(2:ncol(mtdat))])
  trial300mt <- as.numeric(mtdat[300,c(2:ncol(mtdat))])
  
  #then we add accuracy
  accdat <- getR2AllTrackGroupAccuracy()
  
  trial2acc <- as.numeric(accdat[2,c(2:ncol(accdat))])
  trial300acc <- as.numeric(accdat[300,c(2:ncol(accdat))])
  
  plot(NA, NA, xlim = c(2,14), ylim = c(70, 101), 
       xlab = "Lap time (s)", ylab = "Accuracy (% on track)", frame.plot = FALSE, #frame.plot takes away borders
       main = 'Speed-accuracy tradeoffs', xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  #abline(v = c(30, 60, 90, 120, 150, 180, 210, 240, 270), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(2, 4, 6, 8, 10, 12, 14)) #tick marks for x axis
  axis(2, at = c(70, 75, 80, 85, 90, 92, 93, 94, 95, 96, 97, 98, 99, 100), las=2) #tick marks for y axis
  points(trial2mt, trial2acc, col = 'red', pch=16)
  points(trial300mt, trial300acc, col='blue', pch=16)
  
  legend(10,80,
         legend=c('day 1: trial 2', 'day 1: trial 300'),
         col=c('red','blue'),
         bty='n',cex=1,pch = 16)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
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









