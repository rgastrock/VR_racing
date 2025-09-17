source('ana/shared.R')

# participant exclusion----
getR2SessionOneSampleSize <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 1){
  
  # exlude participants due to experiment problems
  pp_exclude <- c('p002', 'p005', 'p012', 'p017', 'p021', 'p032')
  
  condition <- c()
  day <- c()
  N <- c()
  
  for(group in groups){
    pp_group <- unique(list.files(sprintf('data/data_run2/%s', group)))
    pp_group <- pp_group[which(!pp_group %in% pp_exclude)]
    
    condition <- c(condition, group)
    day <- c(day, session)
    N <- c(N, length(pp_group))
  }
  data_sample <- data.frame(condition, day, N)
  return(data_sample)
}

getR2SessionTwoSampleSize <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 2){
  
  # exlude participants due to experiment problems
  pp_exclude <- c('p002', 'p005', 'p012', 'p017', 'p021', 'p032')
  
  condition <- c()
  day <- c()
  N <- c()
  
  for(group in groups){
    pp_group <- unique(list.files(sprintf('data/data_run2/%s', group)))
    pp_group <- pp_group[which(!pp_group %in% pp_exclude)]
    
    noS2 <- c()
    for(pp in pp_group){
      pp_session <- unique(list.files(sprintf('data/data_run2/%s/%s', group, pp)))
      if(length(pp_session) < 2){
        noS2 <- c(noS2, pp)
      }
    }
    
    pp_group <- pp_group[which(!pp_group %in% noS2)]
    
    condition <- c(condition, group)
    day <- c(day, session)
    N <- c(N, length(pp_group))
  }
  data_sample <- data.frame(condition, day, N)
  return(data_sample)
}

#Lap times ----
getR2ParticipantLap <- function(group, id, session) {
  
  filepath <- sprintf('data/data_run2/%s/%s/S%03d/trial_results.csv', group, id, session)
  df <- read.csv(filepath, stringsAsFactors = F)
  
  #setup relevant vectors
  trialno <- c()
  participant <- c()
  session <- c()
  group <- c()
  track_orientation <- c()
  laptime <- c()
  
  for (trial in c(1:dim(df)[1])) {
    trial_num <- df$trial_num[trial]
    pp <- df$ppid[trial]
    session_num <- df$session_num[trial]
    group_cond <- df$experiment[trial]
    orientation <- df$Track_orientation[trial]
    time <- df$lap_time[trial]
    
    trialno <- c(trialno, trial_num)
    participant <- c(participant, pp)
    session <- c(session, session_num)
    group <- c(group, group_cond)
    track_orientation <- c(track_orientation, orientation)
    laptime <- c(laptime, time)    
    
  }
  
  ppdata <- data.frame(trialno, participant, session, group, track_orientation, laptime)
  return(ppdata)
  
}

#Session 1----
getR2GroupLap <- function(group, session){
  
  # exlude participants due to experiment problems
  pp_exclude <- c('p002', 'p005', 'p012', 'p017', 'p021', 'p032')
  pp_group <- unique(list.files(sprintf('data/data_run2/%s', group)))
  pp_group <- pp_group[which(!pp_group %in% pp_exclude)]
  
  dataoutput <- data.frame()
  for(pp in pp_group){
    ppdat <- getR2ParticipantLap(group = group, id = pp, session = session)
    trial <- ppdat$trialno
    time <- ppdat$laptime
    ppdat <- data.frame(trial, time)
    names(ppdat)[names(ppdat) == 'time'] <- pp
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, time)
      names(dataoutput)[names(dataoutput) == 'time'] <- pp
    }
    
  }
  return(dataoutput)
}

getR2GroupLapCI <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 1, type = 'b'){
  
  for(group in groups){
    data <- getR2GroupLap(group = group, session = session)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:(dim(data)[2])])
    
    confidence <- data.frame()
    
    
    for (t in trialno){
      laptime <- data1[t, ]
      
      if (type == "t"){
        laptime <- laptime[!is.na(laptime)]
        citrial <- getConfidenceInterval(data = laptime, variance = var(laptime), method = type)
      } else if(type == "b"){
        citrial <- getConfidenceInterval(data = laptime, variance = var(laptime), method = type)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
    }
    write.csv(confidence, file=sprintf('data/Run2_LapTimeCI_%s_S%03d.csv', group, session), row.names = F) 
  }
  
}

plotR2LapTime <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 1, target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig1_LapTimes.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  plot(NA, NA, xlim = c(0,301), ylim = c(0, 23), 
       xlab = "Trial", ylab = "Lap time (s)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Lap time across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  #abline(v = c(30, 60, 90, 120, 150, 180, 210, 240, 270), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 15, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300)) #tick marks for x axis
  axis(2, at = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22), las=2) #tick marks for y axis
  
  for(group in groups){
    #read in files created by CI function
    groupconfidence <- read.csv(file=sprintf('data/Run2_LapTimeCI_%s_S%03d.csv', group, session))
    
    colourscheme <- getColourScheme(groups = group)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:300), rev(c(1:300))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (group in groups) {
    # plot mean reaches for each group
    col <- colourscheme[[group]][['S']]
    lines(meanGroupReaches[[group]],col=col,lty=1, lwd=2)
  }
  
  #add legend
  legend(200,18,legend=c('track_0°', 'track_90°', 'track_180°', 'track_270°'),
         col=c(colourscheme[['T-RACING_0']][['S']],colourscheme[['T-RACING_90']][['S']],colourscheme[['T-RACING_180']][['S']],colourscheme[['T-RACING_270']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# Session 1: Combine track orientations----

getR2AllTrackGroupLap <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 1){
  
  alldata <- data.frame()
  
  for(group in groups){
    data <- getR2GroupLap(group = group, session = session)
    trial <- data$trial
    data1 <- as.matrix(data[,2:(dim(data)[2])])
    
    if (prod(dim(alldata)) == 0){
      alldata <- data1
    } else {
      alldata <- cbind(alldata, data1)
    }
  }
  ndat <- data.frame(trial, alldata)
  return(ndat)
}

getR2AllTrackGroupLapCI <- function(session = 1, type = 'b'){
  
  data <- getR2AllTrackGroupLap(session = session)
  trialno <- data$trial
  data1 <- as.matrix(data[,2:(dim(data)[2])])
  
  confidence <- data.frame()
  
  
  for (t in trialno){
    laptime <- data1[t, ]
    
    if (type == "t"){
      laptime <- laptime[!is.na(laptime)]
      citrial <- getConfidenceInterval(data = laptime, variance = var(laptime), method = type)
    } else if(type == "b"){
      citrial <- getConfidenceInterval(data = laptime, variance = var(laptime), method = type)
    }
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
  }
  write.csv(confidence, file=sprintf('data/Run2_LapTimeCI_AllTrack_S%03d.csv', session), row.names = F) 
  
}

plotR2AllTrackLapTime <- function(session = 1, target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig4_LapTimes_AllTrack.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  plot(NA, NA, xlim = c(0,301), ylim = c(0, 13), 
       xlab = "Trial", ylab = "Lap time (s)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Lap time across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  #abline(v = c(30, 60, 90, 120, 150, 180, 210, 240, 270), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300)) #tick marks for x axis
  axis(2, at = c(0, 2, 4, 6, 8, 10, 12), las=2) #tick marks for y axis
  
  
  #read in files created by CI function
  groupconfidence <- read.csv(file=sprintf('data/Run2_LapTimeCI_AllTrack_S%03d.csv', session))
  
  colourscheme <- getAllTrackDayOneColourScheme()
  #take only first, last and middle columns of file
  lower <- groupconfidence[,1]
  upper <- groupconfidence[,3]
  mid <- groupconfidence[,2]
  
  col <- colourscheme[['T']]
  
  #upper and lower bounds create a polygon
  #polygon creates it from low left to low right, then up right to up left -> use rev
  #x is just trial nnumber, y depends on values of bounds
  polygon(x = c(c(1:300), rev(c(1:300))), y = c(lower, rev(upper)), border=NA, col=col)
  # plot mean
  col <- colourscheme[['S']]
  lines(mid,col=col,lty=1, lwd=2)
  
  
  #add legend
  # legend(200,18,legend=c('track_0°', 'track_90°', 'track_180°', 'track_270°'),
  #        col=c(colourscheme[['T-RACING_0']][['S']],colourscheme[['T-RACING_90']][['S']],colourscheme[['T-RACING_180']][['S']],colourscheme[['T-RACING_270']][['S']]),
  #        lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}


#Session 2----

getR2S2GroupLap <- function(group, session = 2){
  
  # exlude participants due to experiment problems/ attrition
  pp_exclude <- c('p002', 'p005', 'p012', 'p017', 'p021', 'p032')
  pp_group <- unique(list.files(sprintf('data/data_run2/%s', group)))
  pp_group <- pp_group[which(!pp_group %in% pp_exclude)]
  
  noS2 <- c()
  for(pp in pp_group){
    pp_session <- unique(list.files(sprintf('data/data_run2/%s/%s', group, pp)))
    if(length(pp_session) < 2){
      noS2 <- c(noS2, pp)
    }
  }
  pp_group <- pp_group[which(!pp_group %in% noS2)]
  
  dataoutput <- data.frame()
  for(pp in pp_group){
    ppdat <- getR2ParticipantLap(group = group, id = pp, session = session)
    trial <- ppdat$trialno
    time <- ppdat$laptime
    ppdat <- data.frame(trial, time)
    names(ppdat)[names(ppdat) == 'time'] <- pp
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, time)
      names(dataoutput)[names(dataoutput) == 'time'] <- pp
    }
    
  }
  return(dataoutput)
}

getR2S2GroupLapCI <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 2, type = 'b'){
  
  for(group in groups){
    data <- getR2S2GroupLap(group = group, session = session)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:(dim(data)[2])])
    
    confidence <- data.frame()
    
    
    for (t in trialno){
      laptime <- data1[t, ]
      
      if (type == "t"){
        laptime <- laptime[!is.na(laptime)]
        citrial <- getConfidenceInterval(data = laptime, variance = var(laptime), method = type)
      } else if(type == "b"){
        citrial <- getConfidenceInterval(data = laptime, variance = var(laptime), method = type)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
    }
    write.csv(confidence, file=sprintf('data/Run2_LapTimeCI_%s_S%03d.csv', group, session), row.names = F) 
  }
  
}

plotR2S2LapTime <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 2, target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig1A_LapTimes_Session2.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  plot(NA, NA, xlim = c(0,121), ylim = c(0, 23), 
       xlab = "Trial", ylab = "Lap time (s)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Lap time across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(v = c(30, 60, 90), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 15, 30, 60, 90, 120)) #tick marks for x axis
  axis(2, at = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22), las=2) #tick marks for y axis
  
  for(group in groups){
    #read in files created by CI function
    groupconfidence <- read.csv(file=sprintf('data/Run2_LapTimeCI_%s_S%03d.csv', group, session))
    
    colourscheme <- getColourScheme(groups = group)
    #take only first, last and middle columns of file
    lower <- groupconfidence[,1]
    upper <- groupconfidence[,3]
    mid <- groupconfidence[,2]
    
    col <- colourscheme[[group]][['T']] #use colour scheme according to group
    
    #upper and lower bounds create a polygon
    #polygon creates it from low left to low right, then up right to up left -> use rev
    #x is just trial nnumber, y depends on values of bounds
    polygon(x = c(c(1:120), rev(c(1:120))), y = c(lower, rev(upper)), border=NA, col=col)
    
    meanGroupReaches[[group]] <- mid #use mean to fill in empty list for each group
  }
  
  
  for (group in groups) {
    # plot mean reaches for each group
    col <- colourscheme[[group]][['S']]
    lines(meanGroupReaches[[group]],col=col,lty=1, lwd=2)
  }
  
  #add legend
  legend(80,18,legend=c('track_0°', 'track_90°', 'track_180°', 'track_270°'),
         col=c(colourscheme[['T-RACING_0']][['S']],colourscheme[['T-RACING_90']][['S']],colourscheme[['T-RACING_180']][['S']],colourscheme[['T-RACING_270']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# Session 2: Combine track orientations----

getR2S2AllTrackGroupLap <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 2){
  
  alldata <- data.frame()
  
  for(group in groups){
    data <- getR2S2GroupLap(group = group, session = session)
    trial <- data$trial
    data1 <- as.matrix(data[,2:(dim(data)[2])])
    
    if (prod(dim(alldata)) == 0){
      alldata <- data1
    } else {
      alldata <- cbind(alldata, data1)
    }
  }
  ndat <- data.frame(trial, alldata)
  return(ndat)
}

getR2S2AllTrackGroupLapCI <- function(session = 2, type = 'b'){
  
  data <- getR2S2AllTrackGroupLap(session = session)
  trialno <- data$trial
  data1 <- as.matrix(data[,2:(dim(data)[2])])
  
  confidence <- data.frame()
  
  
  for (t in trialno){
    laptime <- data1[t, ]
    
    if (type == "t"){
      laptime <- laptime[!is.na(laptime)]
      citrial <- getConfidenceInterval(data = laptime, variance = var(laptime), method = type)
    } else if(type == "b"){
      citrial <- getConfidenceInterval(data = laptime, variance = var(laptime), method = type)
    }
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
  }
  write.csv(confidence, file=sprintf('data/Run2_LapTimeCI_AllTrack_S%03d.csv', session), row.names = F) 
  
}

plotR2S2AllTrackLapTime <- function(session = 2, blocks = c(1,2,3,4), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig4A_LapTimes_AllTrack_Session2.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  plot(NA, NA, xlim = c(0,121), ylim = c(0, 13), 
       xlab = "Trial", ylab = "Lap time (s)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Lap time across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(v = c(30, 60, 90), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90, 120)) #tick marks for x axis
  axis(2, at = c(0, 2, 4, 6, 8, 10, 12), las=2) #tick marks for y axis
  
  
  #read in files created by CI function
  groupconfidence <- read.csv(file=sprintf('data/Run2_LapTimeCI_AllTrack_S%03d.csv', session))
  
  #take only first, last and middle columns of file
  lower <- groupconfidence[,1]
  upper <- groupconfidence[,3]
  mid <- groupconfidence[,2]
  
  for(block in blocks){
    if(block == 1){
      b1Trials <- c(1:30)
      lowCI <- lower[b1Trials]
      upCI <- upper[b1Trials]
      midCI <- mid[b1Trials]
      
      colourscheme <- getAllTrackSession2ColourScheme(blocks=block)
      col <- colourscheme[['T']]
      polygon(x = c(b1Trials, rev(b1Trials)), y = c(lowCI, rev(upCI)), border=NA, col=col)
      # plot mean
      col <- colourscheme[['S']]
      lines(x = b1Trials,y = midCI,col=col,lty=1, lwd=2)
    } else if (block == 2){
      b1Trials <- c(31:60)
      lowCI <- lower[b1Trials]
      upCI <- upper[b1Trials]
      midCI <- mid[b1Trials]
      
      colourscheme <- getAllTrackSession2ColourScheme(blocks=block)
      col <- colourscheme[['T']]
      polygon(x = c(b1Trials, rev(b1Trials)), y = c(lowCI, rev(upCI)), border=NA, col=col)
      # plot mean
      col <- colourscheme[['S']]
      lines(x = b1Trials,y = midCI,col=col,lty=1, lwd=2)
    } else if (block == 3){
      b1Trials <- c(61:90)
      lowCI <- lower[b1Trials]
      upCI <- upper[b1Trials]
      midCI <- mid[b1Trials]
      
      colourscheme <- getAllTrackSession2ColourScheme(blocks=block)
      col <- colourscheme[['T']]
      polygon(x = c(b1Trials, rev(b1Trials)), y = c(lowCI, rev(upCI)), border=NA, col=col)
      # plot mean
      col <- colourscheme[['S']]
      lines(x = b1Trials,y = midCI,col=col,lty=1, lwd=2)
    } else if (block == 4){
      b1Trials <- c(91:120)
      lowCI <- lower[b1Trials]
      upCI <- upper[b1Trials]
      midCI <- mid[b1Trials]
      
      colourscheme <- getAllTrackSession2ColourScheme(blocks=block)
      col <- colourscheme[['T']]
      polygon(x = c(b1Trials, rev(b1Trials)), y = c(lowCI, rev(upCI)), border=NA, col=col)
      # plot mean
      col <- colourscheme[['S']]
      lines(x = b1Trials,y = midCI,col=col,lty=1, lwd=2)
    }
  }
  
  #add legend
  colb1 <- getAllTrackSession2ColourScheme(blocks=1)
  colb2 <- getAllTrackSession2ColourScheme(blocks=2)
  colb3 <- getAllTrackSession2ColourScheme(blocks=3)
  colb4 <- getAllTrackSession2ColourScheme(blocks=4)
  legend(70,12,legend=c('trained track', 'flipped track: 180°', 'reverse track'),
         col=c(colb1[['S']],colb2[['S']],colb3[['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#plotting sessions together----

plotR2AcrossSessionLapTime <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig1B_LapTimes_AllSessions.svg', width=16, height=6, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(3,3), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotR2LapTime()
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotR2S2LapTime()
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotR2AllTrackAcrossSessionLapTime <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig4B_LapTimes_AllTrack_AllSessions.svg', width=16, height=6, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(3,3), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotR2AllTrackLapTime()
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotR2S2AllTrackLapTime()
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# Remove 1st trial: All tracks----

plotR2S1FirstLastLapTime <- function(session = 1, target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig5_LapTimes_AllTrack.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  plot(NA, NA, xlim = c(1,61), ylim = c(0, 7), 
       xlab = "Trial", ylab = "Lap time (s)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Lap time across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(v = c(30), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(side=1, at=c(2, 15, 30, 32, 45, 60), labels=c('2', '15', '30', '272', '285', '300'))
  axis(2, at = c(0, 2, 4, 6), las=2) #tick marks for y axis
  
  
  #read in files created by CI function
  groupconfidence <- read.csv(file=sprintf('data/Run2_LapTimeCI_AllTrack_S%03d.csv', session))
  
  colourscheme <- getAllTrackDayOneColourScheme()
  #take only first, last and middle columns of file
  lower <- groupconfidence[,1]
  b1lower <- lower[2:30]
  b2lower <- lower[272:300]
  
  upper <- groupconfidence[,3]
  b1upper <- upper[2:30]
  b2upper <- upper[272:300]
  
  mid <- groupconfidence[,2]
  b1mid <- mid[2:30]
  b2mid <- mid[272:300]
  
  col <- colourscheme[['T']]
  
  #first block
  polygon(x = c(c(2:30), rev(c(2:30))), y = c(b1lower, rev(b1upper)), border=NA, col=col)
  # plot mean
  col <- colourscheme[['S']]
  lines(x=c(2:30),y=b1mid,col=col,lty=1, lwd=2)
  
  col <- colourscheme[['T']]
  #last block
  polygon(x = c(c(32:60), rev(c(32:60))), y = c(b2lower, rev(b2upper)), border=NA, col=col)
  # plot mean
  col <- colourscheme[['S']]
  lines(x=c(32:60),y=b2mid,col=col,lty=1, lwd=2)
  
  
  #add legend
  # legend(200,18,legend=c('track_0°', 'track_90°', 'track_180°', 'track_270°'),
  #        col=c(colourscheme[['T-RACING_0']][['S']],colourscheme[['T-RACING_90']][['S']],colourscheme[['T-RACING_180']][['S']],colourscheme[['T-RACING_270']][['S']]),
  #        lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotR2S2FirstLastLapTime <- function(session = 2, blocks = c(1,2,3,4), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig5A_LapTimes_AllTrack_Session2.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  plot(NA, NA, xlim = c(0,121), ylim = c(0, 7), 
       xlab = "Trial", ylab = "Lap time (s)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Lap time across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(v = c(30, 60, 90), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(2, 32, 62, 92, 120)) #tick marks for x axis
  axis(2, at = c(0, 2, 4, 6), las=2) #tick marks for y axis
  
  
  #read in files created by CI function
  groupconfidence <- read.csv(file=sprintf('data/Run2_LapTimeCI_AllTrack_S%03d.csv', session))
  
  #take only first, last and middle columns of file
  lower <- groupconfidence[,1]
  upper <- groupconfidence[,3]
  mid <- groupconfidence[,2]
  
  for(block in blocks){
    if(block == 1){
      b1Trials <- c(2:30)
      lowCI <- lower[b1Trials]
      upCI <- upper[b1Trials]
      midCI <- mid[b1Trials]
      
      colourscheme <- getAllTrackSession2ColourScheme(blocks=block)
      col <- colourscheme[['T']]
      polygon(x = c(b1Trials, rev(b1Trials)), y = c(lowCI, rev(upCI)), border=NA, col=col)
      # plot mean
      col <- colourscheme[['S']]
      lines(x = b1Trials,y = midCI,col=col,lty=1, lwd=2)
    } else if (block == 2){
      b1Trials <- c(32:60)
      lowCI <- lower[b1Trials]
      upCI <- upper[b1Trials]
      midCI <- mid[b1Trials]
      
      colourscheme <- getAllTrackSession2ColourScheme(blocks=block)
      col <- colourscheme[['T']]
      polygon(x = c(b1Trials, rev(b1Trials)), y = c(lowCI, rev(upCI)), border=NA, col=col)
      # plot mean
      col <- colourscheme[['S']]
      lines(x = b1Trials,y = midCI,col=col,lty=1, lwd=2)
    } else if (block == 3){
      b1Trials <- c(62:90)
      lowCI <- lower[b1Trials]
      upCI <- upper[b1Trials]
      midCI <- mid[b1Trials]
      
      colourscheme <- getAllTrackSession2ColourScheme(blocks=block)
      col <- colourscheme[['T']]
      polygon(x = c(b1Trials, rev(b1Trials)), y = c(lowCI, rev(upCI)), border=NA, col=col)
      # plot mean
      col <- colourscheme[['S']]
      lines(x = b1Trials,y = midCI,col=col,lty=1, lwd=2)
    } else if (block == 4){
      b1Trials <- c(92:120)
      lowCI <- lower[b1Trials]
      upCI <- upper[b1Trials]
      midCI <- mid[b1Trials]
      
      colourscheme <- getAllTrackSession2ColourScheme(blocks=block)
      col <- colourscheme[['T']]
      polygon(x = c(b1Trials, rev(b1Trials)), y = c(lowCI, rev(upCI)), border=NA, col=col)
      # plot mean
      col <- colourscheme[['S']]
      lines(x = b1Trials,y = midCI,col=col,lty=1, lwd=2)
    }
  }
  
  #add legend
  colb1 <- getAllTrackSession2ColourScheme(blocks=1)
  colb2 <- getAllTrackSession2ColourScheme(blocks=2)
  colb3 <- getAllTrackSession2ColourScheme(blocks=3)
  colb4 <- getAllTrackSession2ColourScheme(blocks=4)
  legend(2,2,legend=c('trained track', 'flipped track: 180°', 'reverse track'),
         col=c(colb1[['S']],colb2[['S']],colb3[['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotR2FirstLastAllTrackAcrossSessionLapTime <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig5B_LapTimes_AllTrack_AllSessions.svg', width=12, height=6, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(3,3), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotR2S1FirstLastLapTime()
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotR2S2FirstLastLapTime()
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}



