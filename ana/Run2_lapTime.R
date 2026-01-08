source('ana/shared.R')
source('ana/lapTime.R')

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

getR2AllTrackGroupLap <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 1, target=0){
  
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
  if(target == 1){
    write.csv(ndat, file=sprintf('data/Run2_LapTime_AllTrack_S%03d.csv', session), row.names = F) 
  } else{
    return(ndat)
  }
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

getR2S2AllTrackGroupLap <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 2, target=0){
  
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
  if(target == 1){
    write.csv(ndat, file=sprintf('data/Run2_LapTime_AllTrack_S%03d.csv', session), row.names = F) 
  } else{
    return(ndat)
  }
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

# Individual data - Remove 1st trial: All tracks----

plotR2S1IndFirstLastLapTime <- function(session = 1, target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig5C_IndividualLapTimes_AllTrack.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  plot(NA, NA, xlim = c(1,61), ylim = c(0, 7), 
       xlab = "Trial", ylab = "Lap time (s)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Lap time across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(v = c(30), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(side=1, at=c(2, 15, 30, 32, 45, 60), labels=c('2', '15', '30', '272', '285', '300'))
  axis(2, at = c(0, 2, 4, 6), las=2) #tick marks for y axis
  
  #read in individual data
  alldat <- read.csv(file=sprintf('data/Run2_LapTime_AllTrack_S%03d.csv', session))
  alldat <- alldat[,2:ncol(alldat)]
  for (pp in 1:ncol(alldat)){
    subdat <- alldat[,pp]
    b1subdat <- subdat[2:30]
    b2subdat <- subdat[272:300]
    
    col <- '#A9A9A9ff'
    lines(x=c(2:30),y=b1subdat,col=col,lty=1, lwd=1)
    lines(x=c(32:60),y=b2subdat,col=col,lty=1, lwd=1)
  }
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

plotR2S2IndFirstLastLapTime <- function(session = 2, blocks = c(1,2,3,4), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig5D_IndividualLapTimes_AllTrack_Session2.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  plot(NA, NA, xlim = c(0,121), ylim = c(0, 7), 
       xlab = "Trial", ylab = "Lap time (s)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Lap time across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(v = c(30, 60, 90), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(2, 32, 62, 92, 120)) #tick marks for x axis
  axis(2, at = c(0, 2, 4, 6), las=2) #tick marks for y axis
  
  #read in individual data
  alldat <- read.csv(file=sprintf('data/Run2_LapTime_AllTrack_S%03d.csv', session))
  alldat <- alldat[,2:ncol(alldat)]
  for (pp in 1:ncol(alldat)){
    subdat <- alldat[,pp]
    b1subdat <- subdat[2:30]
    b2subdat <- subdat[32:60]
    b3subdat <- subdat[62:90]
    b4subdat <- subdat[92:120]
    
    col <- '#A9A9A9ff'
    lines(x=c(2:30),y=b1subdat,col=col,lty=1, lwd=1)
    lines(x=c(32:60),y=b2subdat,col=col,lty=1, lwd=1)
    lines(x=c(62:90),y=b3subdat,col=col,lty=1, lwd=1)
    lines(x=c(92:120),y=b4subdat,col=col,lty=1, lwd=1)
  }
  
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

plotR2IndFirstLastAllTrackAcrossSessionLapTime <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig5E_IndividualLapTimes_AllTrack_AllSessions.svg', width=12, height=6, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(3,3), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotR2S1IndFirstLastLapTime()
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotR2S2IndFirstLastLapTime()
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# Statistics (Preprocess data) ----

getR2BlockedLapTime <- function(session, blockdefs) {
  
  dat <- read.csv(file=sprintf('data/Run2_LapTime_AllTrack_S%03d.csv', session))
  dat <- dat[,-1] #remove trial rows
  participants <- colnames(dat)
  N <- length(participants)
  
  participant <- c()
  set <- c()
  dv <- c()
  
  for (ppno in c(1:N)) {
    
    pp <- participants[ppno]
    
    for (blockno in c(1:length(blockdefs))) {
      #for each participant, and every 9 trials, get the mean
      blockdef <- blockdefs[[blockno]]
      blockstart <- blockdef[1]
      blockend <- blockstart + blockdef[2] - 1
      samples <- dat[blockstart:blockend,ppno]
      samples <- mean(samples, na.rm=TRUE)
      
      participant <- c(participant, pp)
      set <- c(set, names(blockdefs)[blockno])
      dv <- c(dv, samples)
    }
  }
  LCaov <- data.frame(participant, set, dv)
  
  #need to make some columns as factors for ANOVA
  #LCaov$participant <- as.factor(LCaov$participant)
  LCaov$set <- as.factor(LCaov$set)
  if(session == 1){
    LCaov$set <- factor(LCaov$set, levels = c('S1_first','S1_last'))
  } else if(session == 2){
    LCaov$set <- factor(LCaov$set, levels = c('S2_1','S2_2','S2_3','S2_4','S2_5','S2_6','S2_7','S2_8'))
  }
  
  return(LCaov)
  
}

#Statistics (Retention; Frequentist) ----

retentionR2LapTimeANOVA <- function() {
  
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getR2BlockedLapTime(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC_part2 <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2'),]
  
  #but we only want to analyze participants with data in both
  LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_1','S2_2'))
  
  #ANOVA's
  # for ez, case ID should be a factor:
  
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=dv, within= c(set), type=3, return_aov = TRUE) #df is k-1 or 3 levels minus 1; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Lap times during first and last set in session 1 and first and second set in session 2:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#follow up on significant interaction
retentionR2LapTimeComparisonMeans <- function(){
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getR2BlockedLapTime(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC_part2 <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2'),]
  
  #but we only want to analyze participants with data in both
  LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_1','S2_2'))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
  
  cellmeans <- emmeans(secondAOV,specs=c('set'))
  print(cellmeans)
  
}

retentionR2LapTimeComparisons <- function(method='bonferroni'){
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getR2BlockedLapTime(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC_part2 <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2'),]
  
  #but we only want to analyze participants with data in both
  LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_1','S2_2'))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
  
  #specify contrasts
  S1_firstvsS1_last <- c(-1,1,0,0)
  S1_firstvsS2_1 <- c(-1,0,1,0)
  S1_lastvsS2_1 <- c(0,-1,1,0)
  S1_lastvsS2_2 <- c(0,-1,0,1)
  
  contrastList <- list('Session 1 Set 1 vs Session 1 Set Last' = S1_firstvsS1_last, 
                       'Session 1 Set 1 vs Session 2 Set 1' = S1_firstvsS2_1, 
                       'Session 1 Set Last vs Session 2 Set 1' = S1_lastvsS2_1,
                       'Session 1 Set Last vs Session 2 Set 2' = S1_lastvsS2_2)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('set')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
retentionR2LapTimeComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- retentionR2LapTimeComparisons(method=method)
  #we can use eta-squared as effect size
  #% of variance in DV(percentcomp) accounted for 
  #by the difference between target1 and target2
  comparisonsdf <- as.data.frame(comparisons)
  etasq <- ((comparisonsdf$t.ratio)^2)/(((comparisonsdf$t.ratio)^2)+(comparisonsdf$df))
  comparisons1 <- cbind(comparisonsdf,etasq)
  
  effectsize <- data.frame(comparisons1$contrast, comparisons1$etasq)
  colnames(effectsize) <- c('contrast', 'etasquared')
  #print(comparisons)
  print(effectsize)
}

#Statistics (Retention; Bayesian) ----
retentionR2LapTimeBayesANOVA <- function() {
  
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getR2BlockedLapTime(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC_part2 <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2'),]
  
  #but we only want to analyze participants with data in both
  LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_1','S2_2'))
  
  cat('Lap times during first and last set in session 1 and first and second set in session 2:\n')
  bfLC<- anovaBF(dv ~ set + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  #bfinteraction <- bfLC[4]/bfLC[3]
  
  #bfinclude to compare model with interactions against all other models
  bfinteraction <- bayesfactor_inclusion(bfLC)
  
  print(bfLC)
  print(bfinteraction)
  
}

retentionR2LapTimeComparisonsBayesfollowup <- function() {
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getR2BlockedLapTime(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC_part2 <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2'),]
  
  #but we only want to analyze participants with data in both
  LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_1','S2_2'))
  
  
  S1_first <- LC4aov[which(LC4aov$set == 'S1_first'),]
  S1_last <- LC4aov[which(LC4aov$set == 'S1_last'),]
  S2_1 <- LC4aov[which(LC4aov$set == 'S2_1'),]
  S2_2 <- LC4aov[which(LC4aov$set == 'S2_2'),]
  
  
  cat('Bayesian t-test - Session 1 Set 1 vs Session 1 Set Last:\n')
  print(ttestBF(S1_first$dv, S1_last$dv, paired = TRUE))
  
  cat('Bayesian t-test - Session 1 Set 1 vs Session 2 Set 1:\n')
  print(ttestBF(S1_first$dv, S2_1$dv, paired = TRUE))
  
  cat('Bayesian t-test - Session 1 Set Last vs Session 2 Set 1:\n')
  print(ttestBF(S1_last$dv, S2_1$dv, paired = TRUE))
  
  cat('Bayesian t-test - Session 1 Set Last vs Session 2 Set 2:\n')
  print(ttestBF(S1_last$dv, S2_2$dv, paired = TRUE))
  
}

#Statistics (Generalization; Frequentist) ----

genR2LapTimeANOVA <- function() {
  
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_3' | LC_part2$set == 'S2_4'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_3','S2_4'))
  
  #ANOVA's
  # for ez, case ID should be a factor:
  
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=dv, within= c(set), type=3, return_aov = TRUE) #df is k-1 or 3 levels minus 1; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Lap times during first block in session 2 and second block in session 2:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#follow up on significant interaction
genR2LapTimeComparisonMeans <- function(){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_3' | LC_part2$set == 'S2_4'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_3','S2_4'))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
  
  cellmeans <- emmeans(secondAOV,specs=c('set'))
  print(cellmeans)
  
}

genR2LapTimeComparisons <- function(method='bonferroni'){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_3' | LC_part2$set == 'S2_4'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_3','S2_4'))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
  
  #specify contrasts
  S2_1vsS2_3 <- c(-1,0,1,0)
  S2_2vsS2_3 <- c(0,-1,1,0)
  S2_2vsS2_4 <- c(0,-1,0,1)
  
  contrastList <- list('Session 2 Set 1 vs Session 2 Set 3' = S2_1vsS2_3, 
                       'Session 2 Set 2 vs Session 2 Set 3' = S2_2vsS2_3, 
                       'Session 2 Set 2 vs Session 2 Set 4' = S2_2vsS2_4)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('set')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
genR2LapTimeComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- genR2LapTimeComparisons(method=method)
  #we can use eta-squared as effect size
  #% of variance in DV(percentcomp) accounted for 
  #by the difference between target1 and target2
  comparisonsdf <- as.data.frame(comparisons)
  etasq <- ((comparisonsdf$t.ratio)^2)/(((comparisonsdf$t.ratio)^2)+(comparisonsdf$df))
  comparisons1 <- cbind(comparisonsdf,etasq)
  
  effectsize <- data.frame(comparisons1$contrast, comparisons1$etasq)
  colnames(effectsize) <- c('contrast', 'etasquared')
  #print(comparisons)
  print(effectsize)
}

#Statistics (Generalization; Bayesian) ----
genR2LapTimeBayesANOVA <- function() {
  
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_3' | LC_part2$set == 'S2_4'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_3','S2_4'))
  
  cat('Lap times during first block in session 2 and second block in session 2:\n')
  bfLC<- anovaBF(dv ~ set + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  #bfinteraction <- bfLC[4]/bfLC[3]
  
  #bfinclude to compare model with interactions against all other models
  bfinteraction <- bayesfactor_inclusion(bfLC)
  
  print(bfLC)
  print(bfinteraction)
  
}

genR2LapTimeComparisonsBayesfollowup <- function() {
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_3' | LC_part2$set == 'S2_4'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_3','S2_4'))
  
  
  S2_1 <- LC4aov[which(LC4aov$set == 'S2_1'),]
  S2_2 <- LC4aov[which(LC4aov$set == 'S2_2'),]
  S2_3 <- LC4aov[which(LC4aov$set == 'S2_3'),]
  S2_4 <- LC4aov[which(LC4aov$set == 'S2_4'),]
  
  
  cat('Bayesian t-test - Session 2 Set 1 vs Set 3:\n')
  print(ttestBF(S2_1$dv, S2_3$dv, paired = TRUE))
  
  cat('Bayesian t-test - Session 2 Set 2 vs Set 3:\n')
  print(ttestBF(S2_2$dv, S2_3$dv, paired = TRUE))
  
  cat('Bayesian t-test - Session 2 Set 2 vs Set 4:\n')
  print(ttestBF(S2_2$dv, S2_4$dv, paired = TRUE))
  
}

#Statistics (Reverse direction; Frequentist) ----

revR2LapTimeANOVA <- function() {
  
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_5' | LC_part2$set == 'S2_6'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_5','S2_6'))
  
  #ANOVA's
  # for ez, case ID should be a factor:
  
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=dv, within= c(set), type=3, return_aov = TRUE) #df is k-1 or 3 levels minus 1; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Lap times during first block in session 2 and third block in session 2:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#follow up on significant interaction
revR2LapTimeComparisonMeans <- function(){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_5' | LC_part2$set == 'S2_6'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_5','S2_6'))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
  
  cellmeans <- emmeans(secondAOV,specs=c('set'))
  print(cellmeans)
  
}

revR2LapTimeComparisons <- function(method='bonferroni'){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_5' | LC_part2$set == 'S2_6'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_5','S2_6'))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
  
  #specify contrasts
  S2_1vsS2_5 <- c(-1,0,1,0)
  S2_2vsS2_5 <- c(0,-1,1,0)
  S2_2vsS2_6 <- c(0,-1,0,1)
  
  contrastList <- list('Session 2 Set 1 vs Session 2 Set 5' = S2_1vsS2_5, 
                       'Session 2 Set 2 vs Session 2 Set 5' = S2_2vsS2_5, 
                       'Session 2 Set 2 vs Session 2 Set 6' = S2_2vsS2_6)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('set')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
revR2LapTimeComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- revR2LapTimeComparisons(method=method)
  #we can use eta-squared as effect size
  #% of variance in DV(percentcomp) accounted for 
  #by the difference between target1 and target2
  comparisonsdf <- as.data.frame(comparisons)
  etasq <- ((comparisonsdf$t.ratio)^2)/(((comparisonsdf$t.ratio)^2)+(comparisonsdf$df))
  comparisons1 <- cbind(comparisonsdf,etasq)
  
  effectsize <- data.frame(comparisons1$contrast, comparisons1$etasq)
  colnames(effectsize) <- c('contrast', 'etasquared')
  #print(comparisons)
  print(effectsize)
}

#Statistics (Reverse direction; Bayesian) ----
revR2LapTimeBayesANOVA <- function() {
  
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_5' | LC_part2$set == 'S2_6'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_5','S2_6'))
  
  cat('Lap times during first block in session 2 and third block in session 2:\n')
  bfLC<- anovaBF(dv ~ set + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  #bfinteraction <- bfLC[4]/bfLC[3]
  
  #bfinclude to compare model with interactions against all other models
  bfinteraction <- bayesfactor_inclusion(bfLC)
  
  print(bfLC)
  print(bfinteraction)
  
}

revR2LapTimeComparisonsBayesfollowup <- function() {
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_5' | LC_part2$set == 'S2_6'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_5','S2_6'))
  
  
  S2_1 <- LC4aov[which(LC4aov$set == 'S2_1'),]
  S2_2 <- LC4aov[which(LC4aov$set == 'S2_2'),]
  S2_5 <- LC4aov[which(LC4aov$set == 'S2_5'),]
  S2_6 <- LC4aov[which(LC4aov$set == 'S2_6'),]
  
  
  cat('Bayesian t-test - Session 2 Set 1 vs Set 5:\n')
  print(ttestBF(S2_1$dv, S2_5$dv, paired = TRUE))
  
  cat('Bayesian t-test - Session 2 Set 2 vs Set 5:\n')
  print(ttestBF(S2_2$dv, S2_5$dv, paired = TRUE))
  
  cat('Bayesian t-test - Session 2 Set 2 vs Set 6:\n')
  print(ttestBF(S2_2$dv, S2_6$dv, paired = TRUE))
  
}

#Statistics (Trained direction; Frequentist) ----

trainR2LapTimeANOVA <- function() {
  
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_7' | LC_part2$set == 'S2_8'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_7','S2_8'))
  
  #ANOVA's
  # for ez, case ID should be a factor:
  
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=dv, within= c(set), type=3, return_aov = TRUE) #df is k-1 or 3 levels minus 1; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Lap times during first block in session 2 and last block in session 2:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#follow up on significant interaction
trainR2LapTimeComparisonMeans <- function(){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_7' | LC_part2$set == 'S2_8'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_7','S2_8'))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
  
  cellmeans <- emmeans(secondAOV,specs=c('set'))
  print(cellmeans)
  
}

trainR2LapTimeComparisons <- function(method='bonferroni'){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_7' | LC_part2$set == 'S2_8'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_7','S2_8'))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
  
  #specify contrasts
  S2_1vsS2_7 <- c(-1,0,1,0)
  S2_2vsS2_7 <- c(0,-1,1,0)
  S2_2vsS2_8 <- c(0,-1,0,1)
  
  contrastList <- list('Session 2 Set 1 vs Session 2 Set 7' = S2_1vsS2_7, 
                       'Session 2 Set 2 vs Session 2 Set 7' = S2_2vsS2_7, 
                       'Session 2 Set 2 vs Session 2 Set 8' = S2_2vsS2_8)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('set')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
trainR2LapTimeComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- trainR2LapTimeComparisons(method=method)
  #we can use eta-squared as effect size
  #% of variance in DV(percentcomp) accounted for 
  #by the difference between target1 and target2
  comparisonsdf <- as.data.frame(comparisons)
  etasq <- ((comparisonsdf$t.ratio)^2)/(((comparisonsdf$t.ratio)^2)+(comparisonsdf$df))
  comparisons1 <- cbind(comparisonsdf,etasq)
  
  effectsize <- data.frame(comparisons1$contrast, comparisons1$etasq)
  colnames(effectsize) <- c('contrast', 'etasquared')
  #print(comparisons)
  print(effectsize)
}

#Statistics (Trained direction; Bayesian) ----
trainR2LapTimeBayesANOVA <- function() {
  
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_7' | LC_part2$set == 'S2_8'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_7','S2_8'))
  
  cat('Lap times during first block in session 2 and last block in session 2:\n')
  bfLC<- anovaBF(dv ~ set + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  #bfinteraction <- bfLC[4]/bfLC[3]
  
  #bfinclude to compare model with interactions against all other models
  bfinteraction <- bayesfactor_inclusion(bfLC)
  
  print(bfLC)
  print(bfinteraction)
  
}

trainR2LapTimeComparisonsBayesfollowup <- function() {
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_7' | LC_part2$set == 'S2_8'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_7','S2_8'))
  
  
  S2_1 <- LC4aov[which(LC4aov$set == 'S2_1'),]
  S2_2 <- LC4aov[which(LC4aov$set == 'S2_2'),]
  S2_7 <- LC4aov[which(LC4aov$set == 'S2_7'),]
  S2_8 <- LC4aov[which(LC4aov$set == 'S2_8'),]
  
  
  cat('Bayesian t-test - Session 2 Set 1 vs Set 7:\n')
  print(ttestBF(S2_1$dv, S2_7$dv, paired = TRUE))
  
  cat('Bayesian t-test - Session 2 Set 2 vs Set 7:\n')
  print(ttestBF(S2_2$dv, S2_7$dv, paired = TRUE))
  
  cat('Bayesian t-test - Session 2 Set 2 vs Set 8:\n')
  print(ttestBF(S2_2$dv, S2_8$dv, paired = TRUE))
  
}

#Statistics (Compare studies: Session 1; Frequentist) ----

compareStudiesLapTimeANOVA <- function() {
  
  #study1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedLapTime(session = 1, blockdefs=blockdefs)
  LC_part1$study <- rep(1, nrow(LC_part1))
  #study2
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part2 <- getR2BlockedLapTime(session = 1, blockdefs=blockdefs)
  LC_part2$study <- rep(2, nrow(LC_part2))
  
  #between subjects comparison
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last'))
  LC4aov$study <- factor(LC4aov$study, levels = c(1,2))
  
  #ANOVA's
  # for ez, case ID should be a factor:
  
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=dv, within= c(set), between = c(study), type=3, return_aov = TRUE) #df is k-1 or 3 levels minus 1; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Lap times during first and last set in session 1 between studies:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#main effect of block and studyxblock interaction is expected given learning across blocks
#we can test for differences in main effect of study
studiesLapTimeComparisonMeans <- function(){
  #study1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedLapTime(session = 1, blockdefs=blockdefs)
  LC_part1$study <- rep(1, nrow(LC_part1))
  #study2
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part2 <- getR2BlockedLapTime(session = 1, blockdefs=blockdefs)
  LC_part2$study <- rep(2, nrow(LC_part2))
  
  #between subjects comparison
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last'))
  LC4aov$study <- factor(LC4aov$study, levels = c(1,2))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"),between=c("study"))
  
  cellmeans <- emmeans(secondAOV,specs=c('set','study'))
  print(cellmeans)
  
}

studiesLapTimeComparisons <- function(method='bonferroni'){
  #study1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedLapTime(session = 1, blockdefs=blockdefs)
  LC_part1$study <- rep(1, nrow(LC_part1))
  #study2
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part2 <- getR2BlockedLapTime(session = 1, blockdefs=blockdefs)
  LC_part2$study <- rep(2, nrow(LC_part2))
  
  #between subjects comparison
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last'))
  LC4aov$study <- factor(LC4aov$study, levels = c(1,2))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"),between=c("study"))
  
  #specify contrasts
  S1B1vsS2B1 <- c(-1,0,1,0)
  S1B2vsS2B2 <- c(0,-1,0,1)

  
  contrastList <- list('Study 1 Set 1 vs Study 2 Set 1' = S1B1vsS2B1, 
                       'Study 1 Set 2 vs Study 2 Set 2' = S1B2vsS2B2)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('set','study')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
studiesLapTimeComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- studiesLapTimeComparisons(method=method)
  #we can use eta-squared as effect size
  #% of variance in DV(percentcomp) accounted for 
  #by the difference between target1 and target2
  comparisonsdf <- as.data.frame(comparisons)
  etasq <- ((comparisonsdf$t.ratio)^2)/(((comparisonsdf$t.ratio)^2)+(comparisonsdf$df))
  comparisons1 <- cbind(comparisonsdf,etasq)
  
  effectsize <- data.frame(comparisons1$contrast, comparisons1$etasq)
  colnames(effectsize) <- c('contrast', 'etasquared')
  #print(comparisons)
  print(effectsize)
}

#Statistics (Compare studies: Session 1; Bayesian) ----
studiesLapTimeBayesANOVA <- function() {
  
  #study1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedLapTime(session = 1, blockdefs=blockdefs)
  LC_part1$study <- rep(1, nrow(LC_part1))
  #study2
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part2 <- getR2BlockedLapTime(session = 1, blockdefs=blockdefs)
  LC_part2$study <- rep(2, nrow(LC_part2))
  
  #between subjects comparison
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last'))
  LC4aov$study <- factor(LC4aov$study, levels = c(1,2))
  
  cat('Lap times during first and last set in session 1 between studies:\n')
  bfLC<- anovaBF(dv ~ set*study + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  #bfinteraction <- bfLC[4]/bfLC[3]
  
  #bfinclude to compare model with interactions against all other models
  bfinteraction <- bayesfactor_inclusion(bfLC)
  
  print(bfLC)
  print(bfinteraction)
  
}

studiesLapTimeComparisonsBayesfollowup <- function() {
  #study1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedLapTime(session = 1, blockdefs=blockdefs)
  LC_part1$study <- rep(1, nrow(LC_part1))
  #study2
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part2 <- getR2BlockedLapTime(session = 1, blockdefs=blockdefs)
  LC_part2$study <- rep(2, nrow(LC_part2))
  
  #between subjects comparison
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last'))
  LC4aov$study <- factor(LC4aov$study, levels = c(1,2))
  
  S1B1 <- LC4aov[which(LC4aov$set == 'S1_first' & LC4aov$study == 1),]
  S1B2 <- LC4aov[which(LC4aov$set == 'S1_last' & LC4aov$study == 1),]
  S2B1 <- LC4aov[which(LC4aov$set == 'S1_first' & LC4aov$study == 2),]
  S2B2 <- LC4aov[which(LC4aov$set == 'S1_last' & LC4aov$study == 2),]
  
  
  cat('Bayesian t-test - Study 1 Set 1 vs Study 2 Set 1:\n')
  print(ttestBF(S1B1$dv, S2B1$dv))
  
  cat('Bayesian t-test - Study 1 Set 2 vs Study 2 Set 2:\n')
  print(ttestBF(S1B2$dv, S2B2$dv))
  
  
}

#Statistics (Compare studies: Session 2; Frequentist) ----

compareStudiesS2LapTimeANOVA <- function() {
  
  #study1
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part1 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
  #LC_part1 <- LC_part1[which(LC_part1$set == 'S2_1' | LC_part1$set == 'S2_2'),]
  LC_part1$study <- rep(1, nrow(LC_part1))
  
  #study2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  #LC_part2 <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2'),]
  LC_part2$study <- rep(2, nrow(LC_part2))
  
  #between subjects comparison
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- as.factor(LC4aov$set)
  LC4aov$study <- factor(LC4aov$study, levels = c(1,2))
  
  #ANOVA's
  # for ez, case ID should be a factor:
  
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=dv, within= c(set), between = c(study), type=3, return_aov = TRUE) #df is k-1 or 3 levels minus 1; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Lap times in session 2 between studies:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#we can test for differences in main effect of study
studiesS2LapTimeComparisonMeans <- function(){
  #study1
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part1 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
  #LC_part1 <- LC_part1[which(LC_part1$set == 'S2_1' | LC_part1$set == 'S2_2'),]
  LC_part1$study <- rep(1, nrow(LC_part1))
  
  #study2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  #LC_part2 <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2'),]
  LC_part2$study <- rep(2, nrow(LC_part2))
  
  #between subjects comparison
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- as.factor(LC4aov$set)
  LC4aov$study <- factor(LC4aov$study, levels = c(1,2))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"),between=c("study"))
  
  cellmeans <- emmeans(secondAOV,specs=c('set','study'))
  print(cellmeans)
  
}

studiesS2LapTimeComparisons <- function(method='bonferroni'){
  #study1
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part1 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
  #LC_part1 <- LC_part1[which(LC_part1$set == 'S2_1' | LC_part1$set == 'S2_2'),]
  LC_part1$study <- rep(1, nrow(LC_part1))
  
  #study2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  #LC_part2 <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2'),]
  LC_part2$study <- rep(2, nrow(LC_part2))
  
  #between subjects comparison
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- as.factor(LC4aov$set)
  LC4aov$study <- factor(LC4aov$study, levels = c(1,2))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"),between=c("study"))
  
  #specify contrasts
  S1B1vsS2B1 <- c(-1,0,0,0,0,0,0,0,
                  1,0,0,0,0,0,0,0)
  S1B2vsS2B2 <- c(0,-1,0,0,0,0,0,0,
                  0,1,0,0,0,0,0,0)
  S1B3vsS2B3 <- c(0,0,-1,0,0,0,0,0,
                  0,0,1,0,0,0,0,0)
  S1B4vsS2B4 <- c(0,0,0,-1,0,0,0,0,
                  0,0,0,1,0,0,0,0)
  S1B5vsS2B5 <- c(0,0,0,0,-1,0,0,0,
                  0,0,0,0,1,0,0,0)
  S1B6vsS2B6 <- c(0,0,0,0,0,-1,0,0,
                  0,0,0,0,0,1,0,0)
  S1B7vsS2B7 <- c(0,0,0,0,0,0,-1,0,
                  0,0,0,0,0,0,1,0)
  S1B8vsS2B8 <- c(0,0,0,0,0,0,0,-1,
                  0,0,0,0,0,0,0,1)
  
  
  contrastList <- list('Study 1 Set 1 vs Study 2 Set 1' = S1B1vsS2B1, 
                       'Study 1 Set 2 vs Study 2 Set 2' = S1B2vsS2B2,
                       'Study 1 Set 3 vs Study 2 Set 3' = S1B3vsS2B3,
                       'Study 1 Set 4 vs Study 2 Set 4' = S1B4vsS2B4,
                       'Study 1 Set 5 vs Study 2 Set 5' = S1B5vsS2B5,
                       'Study 1 Set 6 vs Study 2 Set 6' = S1B6vsS2B6,
                       'Study 1 Set 7 vs Study 2 Set 7' = S1B7vsS2B7,
                       'Study 1 Set 8 vs Study 2 Set 8' = S1B8vsS2B8)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('set','study')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
studiesS2LapTimeComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- studiesS2LapTimeComparisons(method=method)
  #we can use eta-squared as effect size
  #% of variance in DV(percentcomp) accounted for 
  #by the difference between target1 and target2
  comparisonsdf <- as.data.frame(comparisons)
  etasq <- ((comparisonsdf$t.ratio)^2)/(((comparisonsdf$t.ratio)^2)+(comparisonsdf$df))
  comparisons1 <- cbind(comparisonsdf,etasq)
  
  effectsize <- data.frame(comparisons1$contrast, comparisons1$etasq)
  colnames(effectsize) <- c('contrast', 'etasquared')
  #print(comparisons)
  print(effectsize)
}

#Statistics (Compare studies: Session 2, Block 1; Bayesian) ----
studiesS2LapTimeBayesANOVA <- function() {
  
  #study1
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part1 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
  #LC_part1 <- LC_part1[which(LC_part1$set == 'S2_1' | LC_part1$set == 'S2_2'),]
  LC_part1$study <- rep(1, nrow(LC_part1))
  
  #study2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  #LC_part2 <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2'),]
  LC_part2$study <- rep(2, nrow(LC_part2))
  
  #between subjects comparison
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- as.factor(LC4aov$set)
  LC4aov$study <- factor(LC4aov$study, levels = c(1,2))
  
  cat('Lap times in session 2 between studies:\n')
  bfLC<- anovaBF(dv ~ set*study + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  #bfinteraction <- bfLC[4]/bfLC[3]
  
  #bfinclude to compare model with interactions against all other models
  bfinteraction <- bayesfactor_inclusion(bfLC)
  
  print(bfLC)
  print(bfinteraction)
  
}

studiesS2LapTimeComparisonsBayesfollowup <- function() {
  #study1
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part1 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
  #LC_part1 <- LC_part1[which(LC_part1$set == 'S2_1' | LC_part1$set == 'S2_2'),]
  LC_part1$study <- rep(1, nrow(LC_part1))
  
  #study2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedLapTime(session = 2, blockdefs=blockdefs) 
  #LC_part2 <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2'),]
  LC_part2$study <- rep(2, nrow(LC_part2))
  
  #between subjects comparison
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- as.factor(LC4aov$set)
  LC4aov$study <- factor(LC4aov$study, levels = c(1,2))
  
  
  S1B1 <- LC4aov[which(LC4aov$set == 'S2_1' & LC4aov$study == 1),]
  S1B2 <- LC4aov[which(LC4aov$set == 'S2_2' & LC4aov$study == 1),]
  S1B3 <- LC4aov[which(LC4aov$set == 'S2_3' & LC4aov$study == 1),]
  S1B4 <- LC4aov[which(LC4aov$set == 'S2_4' & LC4aov$study == 1),]
  S1B5 <- LC4aov[which(LC4aov$set == 'S2_5' & LC4aov$study == 1),]
  S1B6 <- LC4aov[which(LC4aov$set == 'S2_6' & LC4aov$study == 1),]
  S1B7 <- LC4aov[which(LC4aov$set == 'S2_7' & LC4aov$study == 1),]
  S1B8 <- LC4aov[which(LC4aov$set == 'S2_8' & LC4aov$study == 1),]
  
  
  S2B1 <- LC4aov[which(LC4aov$set == 'S2_1' & LC4aov$study == 2),]
  S2B2 <- LC4aov[which(LC4aov$set == 'S2_2' & LC4aov$study == 2),]
  S2B3 <- LC4aov[which(LC4aov$set == 'S2_3' & LC4aov$study == 2),]
  S2B4 <- LC4aov[which(LC4aov$set == 'S2_4' & LC4aov$study == 2),]
  S2B5 <- LC4aov[which(LC4aov$set == 'S2_5' & LC4aov$study == 2),]
  S2B6 <- LC4aov[which(LC4aov$set == 'S2_6' & LC4aov$study == 2),]
  S2B7 <- LC4aov[which(LC4aov$set == 'S2_7' & LC4aov$study == 2),]
  S2B8 <- LC4aov[which(LC4aov$set == 'S2_8' & LC4aov$study == 2),]
  
  
  cat('Bayesian t-test - Study 1 Set 1 vs Study 2 Set 1:\n')
  print(ttestBF(S1B1$dv, S2B1$dv))
  
  cat('Bayesian t-test - Study 1 Set 2 vs Study 2 Set 2:\n')
  print(ttestBF(S1B2$dv, S2B2$dv))
  
  cat('Bayesian t-test - Study 1 Set 3 vs Study 2 Set 3:\n')
  print(ttestBF(S1B3$dv, S2B3$dv))
  
  cat('Bayesian t-test - Study 1 Set 4 vs Study 2 Set 4:\n')
  print(ttestBF(S1B4$dv, S2B4$dv))
  
  cat('Bayesian t-test - Study 1 Set 5 vs Study 2 Set 5:\n')
  print(ttestBF(S1B5$dv, S2B5$dv))
  
  cat('Bayesian t-test - Study 1 Set 6 vs Study 2 Set 6:\n')
  print(ttestBF(S1B6$dv, S2B6$dv))
  
  cat('Bayesian t-test - Study 1 Set 7 vs Study 2 Set 7:\n')
  print(ttestBF(S1B7$dv, S2B7$dv))
  
  cat('Bayesian t-test - Study 1 Set 8 vs Study 2 Set 8:\n')
  print(ttestBF(S1B8$dv, S2B8$dv))
  
}
