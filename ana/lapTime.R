source('ana/shared.R')

# participant exclusion----
getSessionOneSampleSize <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 1){
  
  # exlude participants due to experiment problems
  pp_exclude <- c('01', '02', '03', '04', '05', '20', '22', '25', '44', '46')
  
  condition <- c()
  day <- c()
  N <- c()
  
  for(group in groups){
    pp_group <- unique(list.files(sprintf('data/%s', group)))
    pp_group <- pp_group[which(!pp_group %in% pp_exclude)]
    
    condition <- c(condition, group)
    day <- c(day, session)
    N <- c(N, length(pp_group))
  }
  data_sample <- data.frame(condition, day, N)
  return(data_sample)
}

getSessionTwoSampleSize <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 2){
  
  # exlude participants due to experiment problems
  pp_exclude <- c('01', '02', '03', '04', '05', '20', '22', '25', '44', '46')
  
  condition <- c()
  day <- c()
  N <- c()
  
  for(group in groups){
    pp_group <- unique(list.files(sprintf('data/%s', group)))
    pp_group <- pp_group[which(!pp_group %in% pp_exclude)]
    
    noS2 <- c()
    for(pp in pp_group){
      pp_session <- unique(list.files(sprintf('data/%s/%s', group, pp)))
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
getParticipantLap <- function(group, id, session) {
  
  filepath <- sprintf('data/%s/%s/S%03d/trial_results.csv', group, id, session)
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
getGroupLap <- function(group, session){
  
  # exlude participants due to experiment problems
  pp_exclude <- c('01', '02', '03', '04', '05', '20', '22', '25', '44', '46')
  pp_group <- unique(list.files(sprintf('data/%s', group)))
  pp_group <- pp_group[which(!pp_group %in% pp_exclude)]
  
  dataoutput <- data.frame()
  for(pp in pp_group){
    ppdat <- getParticipantLap(group = group, id = pp, session = session)
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

getGroupLapCI <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 1, type = 'b'){
  
  for(group in groups){
    data <- getGroupLap(group = group, session = session)
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
    write.csv(confidence, file=sprintf('data/LapTimeCI_%s_S%03d.csv', group, session), row.names = F) 
  }
  
}

plotLapTime <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 1, target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig1_LapTimes.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
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
    groupconfidence <- read.csv(file=sprintf('data/LapTimeCI_%s_S%03d.csv', group, session))
    
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

getAllTrackGroupLap <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 1, target = 0){
  
  alldata <- data.frame()
  
  for(group in groups){
    data <- getGroupLap(group = group, session = session)
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
    write.csv(ndat, file=sprintf('data/LapTime_AllTrack_S%03d.csv', session), row.names = F) 
  } else {
    return(ndat)
  }
}

getAllTrackGroupLapCI <- function(session = 1, type = 'b'){
  
  data <- getAllTrackGroupLap(session = session)
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
  write.csv(confidence, file=sprintf('data/LapTimeCI_AllTrack_S%03d.csv', session), row.names = F) 
  
}

plotAllTrackLapTime <- function(session = 1, target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig4_LapTimes_AllTrack.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
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
  groupconfidence <- read.csv(file=sprintf('data/LapTimeCI_AllTrack_S%03d.csv', session))
  
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

getS2GroupLap <- function(group, session = 2){
  
  # exlude participants due to experiment problems/ attrition
  pp_exclude <- c('01', '02', '03', '04', '05', '20', '22', '25', '44', '46')
  pp_group <- unique(list.files(sprintf('data/%s', group)))
  pp_group <- pp_group[which(!pp_group %in% pp_exclude)]
  
  noS2 <- c()
  for(pp in pp_group){
    pp_session <- unique(list.files(sprintf('data/%s/%s', group, pp)))
    if(length(pp_session) < 2){
      noS2 <- c(noS2, pp)
    }
  }
  pp_group <- pp_group[which(!pp_group %in% noS2)]
  
  dataoutput <- data.frame()
  for(pp in pp_group){
    ppdat <- getParticipantLap(group = group, id = pp, session = session)
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

getS2GroupLapCI <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 2, type = 'b'){
  
  for(group in groups){
    data <- getS2GroupLap(group = group, session = session)
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
    write.csv(confidence, file=sprintf('data/LapTimeCI_%s_S%03d.csv', group, session), row.names = F) 
  }
  
}

plotS2LapTime <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 2, target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig1A_LapTimes_Session2.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
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
    groupconfidence <- read.csv(file=sprintf('data/LapTimeCI_%s_S%03d.csv', group, session))
    
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

getS2AllTrackGroupLap <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 2, target = 0){
  
  alldata <- data.frame()
  
  for(group in groups){
    data <- getS2GroupLap(group = group, session = session)
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
    write.csv(ndat, file=sprintf('data/LapTime_AllTrack_S%03d.csv', session), row.names = F) 
  } else {
    return(ndat)
  }
}

getS2AllTrackGroupLapCI <- function(session = 2, type = 'b'){
  
  data <- getS2AllTrackGroupLap(session = session)
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
  write.csv(confidence, file=sprintf('data/LapTimeCI_AllTrack_S%03d.csv', session), row.names = F) 
  
}

plotS2AllTrackLapTime <- function(session = 2, blocks = c(1,2,3,4), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig4A_LapTimes_AllTrack_Session2.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
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
  groupconfidence <- read.csv(file=sprintf('data/LapTimeCI_AllTrack_S%03d.csv', session))
  
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

plotAcrossSessionLapTime <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig1B_LapTimes_AllSessions.svg', width=16, height=6, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(3,3), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotLapTime()
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotS2LapTime()
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  

  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotAllTrackAcrossSessionLapTime <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig4B_LapTimes_AllTrack_AllSessions.svg', width=16, height=6, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(3,3), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotAllTrackLapTime()
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotS2AllTrackLapTime()
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# Remove 1st trial: All tracks----

plotS1FirstLastLapTime <- function(session = 1, target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig5_LapTimes_AllTrack.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  plot(NA, NA, xlim = c(1,61), ylim = c(0, 10), 
       xlab = "Trial", ylab = "Lap time (s)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Lap time across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  
  # include shaded regions to highlight trial set 1
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(2, lim[3], 6, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 1
  colourscheme <- getAllTrackSession1ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(4, 10,pch=16,cex=2,col=col)
  text(4, 10,labels=set,cex=0.6,col='white')
  
  # trial set 2
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(7, lim[3], 12, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 2
  colourscheme <- getAllTrackSession1ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(9.5, 10,pch=16,cex=2,col=col)
  text(9.5, 10,labels=set,cex=0.6,col='white')
  
  # trial set 3
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(13, lim[3], 18, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 3
  colourscheme <- getAllTrackSession1ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(15.5, 10,pch=16,cex=2,col=col)
  text(15.5, 10,labels=set,cex=0.6,col='white')
  
  # trial set 4
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(19, lim[3], 24, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 4
  colourscheme <- getAllTrackSession1ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(21.5, 10,pch=16,cex=2,col=col)
  text(21.5, 10,labels=set,cex=0.6,col='white')
  
  # trial set 5
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(25, lim[3], 30, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 5
  colourscheme <- getAllTrackSession1ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(27.5, 10,pch=16,cex=2,col=col)
  text(27.5, 10,labels=set,cex=0.6,col='white')
  
  # trial set 50
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(55, lim[3], 60, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 6
  colourscheme <- getAllTrackSession1ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(57.5, 10,pch=16,cex=2,col=col)
  text(57.5, 10,labels=50,cex=0.6,col='white')
  
  abline(v = c(30), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(side=1, at=c(2, 15, 30, 32, 45, 60), labels=c('2', '15', '30', '272', '285', '300'))
  axis(2, at = c(0, 2, 4, 6, 8, 10), las=2) #tick marks for y axis
  
  #read in files created by CI function
  groupconfidence <- read.csv(file=sprintf('data/LapTimeCI_AllTrack_S%03d.csv', session))
  
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

plotS2FirstLastLapTime <- function(session = 2, blocks = c(1,2,3,4), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig5A_LapTimes_AllTrack_Session2.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  plot(NA, NA, xlim = c(0,121), ylim = c(0, 10), 
       xlab = "Trial", ylab = "Lap time (s)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Lap time across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  
  # include shaded regions to highlight trial set 1
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(2, lim[3], 6, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 1
  colourscheme <- getSAFS2ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(4, 10,pch=15,cex=2,col=col)
  text(4, 10,labels=1,cex=0.6,col='white')
  
  # include shaded regions to highlight trial set 2
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(25, lim[3], 30, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 2
  colourscheme <- getSAFS2ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(27.5, 10,pch=15,cex=2,col=col)
  text(27.5, 10,labels=5,cex=0.6,col='white')
  
  # include shaded regions to highlight trial set 3
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(32, lim[3], 36, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 3
  colourscheme <- getSAFS2ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(34, 10,pch=15,cex=2,col=col)
  text(34, 10,labels=1,cex=0.6,col='white')
  
  # include shaded regions to highlight trial set 4
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(55, lim[3], 60, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 4
  colourscheme <- getSAFS2ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(57.5, 10,pch=15,cex=2,col=col)
  text(57.5, 10,labels=5,cex=0.6,col='white')
  
  # include shaded regions to highlight trial set 5
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(62, lim[3], 66, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 5
  colourscheme <- getSAFS2ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(64, 10,pch=15,cex=2,col=col)
  text(64, 10,labels=1,cex=0.6,col='white')
  
  # include shaded regions to highlight trial set 6
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(85, lim[3], 90, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 6
  colourscheme <- getSAFS2ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(87.5, 10,pch=15,cex=2,col=col)
  text(87.5, 10,labels=5,cex=0.6,col='white')
  
  # include shaded regions to highlight trial set 7
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(92, lim[3], 96, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 7
  colourscheme <- getSAFS2ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(94, 10,pch=15,cex=2,col=col)
  text(94, 10,labels=1,cex=0.6,col='white')
  
  # include shaded regions to highlight trial set 8
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(115, lim[3], 120, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 8
  colourscheme <- getSAFS2ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(117.5, 10,pch=15,cex=2,col=col)
  text(117.5, 10,labels=5,cex=0.6,col='white')
  
  
  abline(v = c(30, 60, 90), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(2, 32, 62, 92, 120)) #tick marks for x axis
  axis(2, at = c(0, 2, 4, 6, 8, 10), las=2) #tick marks for y axis
  
  
  #read in files created by CI function
  groupconfidence <- read.csv(file=sprintf('data/LapTimeCI_AllTrack_S%03d.csv', session))
  
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

plotFirstLastAllTrackAcrossSessionLapTime <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig5B_LapTimes_AllTrack_AllSessions.svg', width=12, height=6, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(3,3), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotS1FirstLastLapTime()
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotS2FirstLastLapTime()
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# Individual data - Remove 1st trial: All tracks----

plotS1IndFirstLastLapTime <- function(session = 1, target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig5C_IndividualLapTimes_AllTrack.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  plot(NA, NA, xlim = c(1,61), ylim = c(0, 10), 
       xlab = "Trial", ylab = "Lap time (s)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Lap time across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(v = c(30), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(side=1, at=c(2, 15, 30, 32, 45, 60), labels=c('2', '15', '30', '272', '285', '300'))
  axis(2, at = c(0, 2, 4, 6, 8, 10), las=2) #tick marks for y axis
  
  #read in individual data
  alldat <- read.csv(file=sprintf('data/LapTime_AllTrack_S%03d.csv', session))
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
  groupconfidence <- read.csv(file=sprintf('data/LapTimeCI_AllTrack_S%03d.csv', session))
  
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

plotS2IndFirstLastLapTime <- function(session = 2, blocks = c(1,2,3,4), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig5D_IndividualLapTimes_AllTrack_Session2.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  plot(NA, NA, xlim = c(0,121), ylim = c(0, 10), 
       xlab = "Trial", ylab = "Lap time (s)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Lap time across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(v = c(30, 60, 90), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(2, 32, 62, 92, 120)) #tick marks for x axis
  axis(2, at = c(0, 2, 4, 6, 8, 10), las=2) #tick marks for y axis
  
  #read in individual data
  alldat <- read.csv(file=sprintf('data/LapTime_AllTrack_S%03d.csv', session))
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
  groupconfidence <- read.csv(file=sprintf('data/LapTimeCI_AllTrack_S%03d.csv', session))
  
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

plotIndFirstLastAllTrackAcrossSessionLapTime <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig5E_IndividualLapTimes_AllTrack_AllSessions.svg', width=12, height=6, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(3,3), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotS1IndFirstLastLapTime()
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotS2IndFirstLastLapTime()
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# Statistics (Preprocess data) ----

getBlockedLapTime <- function(session, blockdefs) {
  
  dat <- read.csv(file=sprintf('data/LapTime_AllTrack_S%03d.csv', session))
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

retentionLapTimeANOVA <- function() {
  
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedLapTime(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC_part2 <- LC_part2[which(LC_part2$set == 'S2_1'),]
  
  #but we only want to analyze participants with data in both
  LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_1'))
  
  #ANOVA's
  # for ez, case ID should be a factor:
  
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=dv, within= c(set), type=3, return_aov = TRUE) #df is k-1 or 3 levels minus 1; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Lap times during first and last set in session 1 and first set in session 2:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#follow up on significant interaction
retentionLapTimeComparisonMeans <- function(){
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedLapTime(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC_part2 <- LC_part2[which(LC_part2$set == 'S2_1'| LC_part2$set == 'S2_2'),] #keep for mean values
  
  #but we only want to analyze participants with data in both
  LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_1','S2_2'))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
  
  cellmeans <- emmeans(secondAOV,specs=c('set'))
  print(cellmeans)
  
}

retentionLapTimeComparisons <- function(method='bonferroni'){
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedLapTime(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC_part2 <- LC_part2[which(LC_part2$set == 'S2_1'),]
  
  #but we only want to analyze participants with data in both
  LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_1'))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
  
  #specify contrasts
  S1_firstvsS1_last <- c(-1,1,0)
  S1_firstvsS2_1 <- c(-1,0,1)
  S1_lastvsS2_1 <- c(0,-1,1)
  #S1_lastvsS2_2 <- c(0,-1,0,1)
  
  contrastList <- list('Session 1 Set 1 vs Session 1 Set Last' = S1_firstvsS1_last, 
                       'Session 1 Set 1 vs Session 2 Set 1' = S1_firstvsS2_1, 
                       'Session 1 Set Last vs Session 2 Set 1' = S1_lastvsS2_1)#,
                       # 'Session 1 Set Last vs Session 2 Set 2' = S1_lastvsS2_2)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('set')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
retentionLapTimeComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- retentionLapTimeComparisons(method=method)
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
retentionLapTimeBayesANOVA <- function() {
  
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedLapTime(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC_part2 <- LC_part2[which(LC_part2$set == 'S2_1'),]
  
  #but we only want to analyze participants with data in both
  LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_1'))
  
  cat('Lap times during first and last set in session 1 and first and set in session 2:\n')
  bfLC<- anovaBF(dv ~ set + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  #bfinteraction <- bfLC[4]/bfLC[3]
  
  #bfinclude to compare model with interactions against all other models
  bfinteraction <- bayesfactor_inclusion(bfLC)
  
  print(bfLC)
  print(bfinteraction)
  
}

retentionLapTimeComparisonsBayesfollowup <- function() {
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedLapTime(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
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

#Top Up Statistics (Generalization; Frequentist) ----

genTopUpLapTimeANOVA <- function() {

  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs)
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
genTopUpLapTimeComparisonMeans <- function(){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs)
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_3' | LC_part2$set == 'S2_4'),]

  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_3','S2_4'))

  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))

  cellmeans <- emmeans(secondAOV,specs=c('set'))
  print(cellmeans)

}

genTopUpLapTimeComparisons <- function(method='bonferroni'){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs)
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
genTopUpLapTimeComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- genLapTimeComparisons(method=method)
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

# Top Up Statistics (Generalization; Bayesian) ----
genTopUpLapTimeBayesANOVA <- function() {
  
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
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

genTopUpLapTimeComparisonsBayesfollowup <- function() {
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
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

#Statistics (Generalization; Frequentist) ----

genLapTimeANOVA <- function() {
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedLapTime(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC_part2 <- LC_part2[which(LC_part2$set == 'S2_3'),]
  
  #but we only want to analyze participants with data in both
  LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_3'))
  
  #ANOVA's
  # for ez, case ID should be a factor:
  
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=dv, within= c(set), type=3, return_aov = TRUE) #df is k-1 or 3 levels minus 1; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Lap times during first and last set in session 1 and first set of rotated track in session 2:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#follow up on significant interaction
genLapTimeComparisonMeans <- function(){
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedLapTime(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC_part2 <- LC_part2[which(LC_part2$set == 'S2_3'),]
  
  #but we only want to analyze participants with data in both
  LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_3'))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
  
  cellmeans <- emmeans(secondAOV,specs=c('set'))
  print(cellmeans)
  
}

genLapTimeComparisons <- function(method='bonferroni'){
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedLapTime(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC_part2 <- LC_part2[which(LC_part2$set == 'S2_3'),]
  
  #but we only want to analyze participants with data in both
  LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_3'))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
  
  #specify contrasts
  S1_1vsS2_3 <- c(-1,0,1)
  S1_2vsS2_3 <- c(0,-1,1)
  
  contrastList <- list('Session 1 Set 1 vs Session 2 Set 3' = S1_1vsS2_3, 
                       'Session 1 Set 2 vs Session 2 Set 3' = S1_2vsS2_3)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('set')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
genLapTimeComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- genLapTimeComparisons(method=method)
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
genLapTimeBayesANOVA <- function() {
  
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedLapTime(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC_part2 <- LC_part2[which(LC_part2$set == 'S2_3'),]
  
  #but we only want to analyze participants with data in both
  LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_3'))
  
  cat('Lap times during first and last set in session 1 and first set of rotated track in session 2:\n')
  bfLC<- anovaBF(dv ~ set + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  #bfinteraction <- bfLC[4]/bfLC[3]
  
  #bfinclude to compare model with interactions against all other models
  bfinteraction <- bayesfactor_inclusion(bfLC)
  
  print(bfLC)
  print(bfinteraction)
  
}

genLapTimeComparisonsBayesfollowup <- function() {
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedLapTime(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC_part2 <- LC_part2[which(LC_part2$set == 'S2_3'),]
  
  #but we only want to analyze participants with data in both
  LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_3'))
  
  
  S1_1 <- LC4aov[which(LC4aov$set == 'S1_first'),]
  S1_2 <- LC4aov[which(LC4aov$set == 'S1_last'),]
  S2_3 <- LC4aov[which(LC4aov$set == 'S2_3'),]
  
  
  cat('Bayesian t-test - Session 1 Set 1 vs Session 2 Set 3:\n')
  print(ttestBF(S1_1$dv, S2_3$dv, paired = TRUE))
  
  cat('Bayesian t-test - Session 1 Set 2 vs Session 2 Set 3:\n')
  print(ttestBF(S1_2$dv, S2_3$dv, paired = TRUE))
  
}

# Top Up Statistics (Reverse direction; Frequentist) ----

revTopUpLapTimeANOVA <- function() {
  
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
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
revTopUpLapTimeComparisonMeans <- function(){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_5' | LC_part2$set == 'S2_6'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_5','S2_6'))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
  
  cellmeans <- emmeans(secondAOV,specs=c('set'))
  print(cellmeans)
  
}

revTopUpLapTimeComparisons <- function(method='bonferroni'){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
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
revTopUpLapTimeComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- revLapTimeComparisons(method=method)
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

#Top Up Statistics (Reverse direction; Bayesian) ----
revTopUpLapTimeBayesANOVA <- function() {
  
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
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

revTopUpLapTimeComparisonsBayesfollowup <- function() {
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
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

#Statistics (Reverse direction; Frequentist) ----

revLapTimeANOVA <- function() {
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedLapTime(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC_part2 <- LC_part2[which(LC_part2$set == 'S2_5'),]
  
  #but we only want to analyze participants with data in both
  LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_5'))
  
  #ANOVA's
  # for ez, case ID should be a factor:
  
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=dv, within= c(set), type=3, return_aov = TRUE) #df is k-1 or 3 levels minus 1; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Lap times during first and last set in session 1 and first set of reverse track in session 2:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#follow up on significant interaction
revLapTimeComparisonMeans <- function(){
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedLapTime(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC_part2 <- LC_part2[which(LC_part2$set == 'S2_5'),]
  
  #but we only want to analyze participants with data in both
  LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_5'))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
  
  cellmeans <- emmeans(secondAOV,specs=c('set'))
  print(cellmeans)
  
}

revLapTimeComparisons <- function(method='bonferroni'){
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedLapTime(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC_part2 <- LC_part2[which(LC_part2$set == 'S2_5'),]
  
  #but we only want to analyze participants with data in both
  LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_5'))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
  
  #specify contrasts
  S1_1vsS2_5 <- c(-1,0,1)
  S1_2vsS2_5 <- c(0,-1,1)
  
  contrastList <- list('Session 1 Set 1 vs Session 2 Set 5' = S1_1vsS2_5, 
                       'Session 1 Set 2 vs Session 2 Set 5' = S1_2vsS2_5)
  
  comparisons<- contrast(emmeans(secondAOV,specs=c('set')), contrastList, adjust=method)
  
  print(comparisons)
  
}

#effect size
revLapTimeComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- revLapTimeComparisons(method=method)
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
revLapTimeBayesANOVA <- function() {
  
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedLapTime(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC_part2 <- LC_part2[which(LC_part2$set == 'S2_5'),]
  
  #but we only want to analyze participants with data in both
  LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_5'))
  
  cat('Lap times during first and last set in session 1 and first set of reverse track in session 2:\n')
  bfLC<- anovaBF(dv ~ set + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  #bfinteraction <- bfLC[4]/bfLC[3]
  
  #bfinclude to compare model with interactions against all other models
  bfinteraction <- bayesfactor_inclusion(bfLC)
  
  print(bfLC)
  print(bfinteraction)
  
}

revLapTimeComparisonsBayesfollowup <- function() {
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedLapTime(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC_part2 <- LC_part2[which(LC_part2$set == 'S2_5'),]
  
  #but we only want to analyze participants with data in both
  LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_5'))
  
  
  S1_1 <- LC4aov[which(LC4aov$set == 'S1_first'),]
  S1_2 <- LC4aov[which(LC4aov$set == 'S1_last'),]
  S2_5 <- LC4aov[which(LC4aov$set == 'S2_5'),]
  
  
  cat('Bayesian t-test - Session 1 Set 1 vs Session 2 Set 5:\n')
  print(ttestBF(S1_1$dv, S2_5$dv, paired = TRUE))
  
  cat('Bayesian t-test - Session 1 Set 2 vs Session 2 Set 5:\n')
  print(ttestBF(S1_2$dv, S2_5$dv, paired = TRUE))
  
}

#Statistics (Trained direction; Frequentist) ----

trainLapTimeANOVA <- function() {
  
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
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
trainLapTimeComparisonMeans <- function(){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_7' | LC_part2$set == 'S2_8'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_7','S2_8'))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
  
  cellmeans <- emmeans(secondAOV,specs=c('set'))
  print(cellmeans)
  
}

trainLapTimeComparisons <- function(method='bonferroni'){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
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
trainLapTimeComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- trainLapTimeComparisons(method=method)
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

# T-tests to compare last set in block 1 and first set in block 4 (both trained track in Session 2)
trainLapTimeTtest <- function(){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_2' | LC_part2$set == 'S2_7'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_2','S2_7'))
  
  subdat1 <- LC4aov[which(LC4aov$set == 'S2_2'),]
  subdat2 <- LC4aov[which(LC4aov$set == 'S2_7'),]
  
  cat('Session 2 Set 2 vs. Session 2 Set 7:\n')
  print(t.test(subdat1$dv, subdat2$dv, paired = T))
  cat('Effect Size - Cohen d:\n')
  print(cohensD(subdat1$dv, subdat2$dv))

}

#Statistics (Trained direction; Bayesian) ----
trainLapTimeBayesANOVA <- function() {
  
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
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

trainLapTimeComparisonsBayesfollowup <- function() {
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedLapTime(session = 2, blockdefs=blockdefs) 
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


#Exponential Model Plots----
getLapTimePars <- function(bootstraps = 1000){
  
  data <- getAllTrackGroupLap()
  subdat <- data[2:nrow(data),2:ncol(data)]
  lambda <- c()
  N0 <- c()
  C <- c()
  for(bs in c(1:bootstraps)){
    cat(sprintf('iteration: %s \n', bs))
    bs_mat <- subdat[,sample(ncol(subdat),ncol(subdat), replace = TRUE)]
    bs_dat <- rowMeans(bs_mat, na.rm = TRUE)
    
    par <- decayFit(signal = bs_dat)
    lambda <- c(lambda, par['lambda'])
    N0 <- c(N0, par['N0'])
    C <- c(C, par['C'])
  }
  
  write.csv(data.frame(lambda, N0, C), file='data/LapTime_exponentialPars.csv', quote=F, row.names=F)
}
  


plotLapTimeModel <- function(session = 1, target='inline'){
  
  #but we can save plot as svg file
  if(target=='svg'){
    svglite(file='doc/fig/pilot/Fig17_LapTime_exponentialFit.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  
  
  #par(mfrow = c(1,2))
  
  
  plot(NA, NA, xlim = c(0,300), ylim = c(0,7), 
       xlab = "Trial", ylab = "Lap time (s)", frame.plot = FALSE, #frame.plot takes away borders
       main = "Exponential fit, lap time", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  #abline(h = c(-100,0, 100), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(side=1, at=c(1, 49, 99, 149, 199, 249, 299), labels=c('2', '50', '100', '150', '200', '250', '300'))
  axis(2, at = c(0,2,4,6)) #tick marks for y axis
  
  
  #show the percent compensation from data
  groupconfidence <- read.csv(file=sprintf('data/LapTimeCI_AllTrack_S%03d.csv', session))
  mid <- groupconfidence[2:nrow(groupconfidence),2]
  x <- c(1:299)
  col <- '#A9A9A9ff'
  lines(x, mid, lty=1, col=col)
  
  #get model parameters from data - no bootstrapping
  dat <- getAllTrackGroupLap()
  subdat <- dat[2:nrow(dat),2:ncol(dat)]
  
  bs_dat <- rowMeans(subdat, na.rm = TRUE)
  par <- decayFit(signal = bs_dat)
  
  #get CIs for rate of change, asymptote will just be 50%, then solid line is based from pars of data (no bootstrapping)
  #bootstrapped pars are used for lower and upper bounds
  data <- read.csv(file='data/LapTime_exponentialPars.csv')
  
  qs_lambda <- quantile(data$lambda, probs = c(0.025, 0.500, 0.975))
  qs_N0 <- quantile(data$N0, probs = c(0.025, 0.500, 0.975))
  qs_C <- quantile(data$C, probs = c(0.025, 0.500, 0.975))
  
  lwr <- setNames(c(qs_lambda[['2.5%']], qs_N0[['50%']],qs_C[['2.5%']]), c('lambda', 'N0', 'C'))
  mid <- setNames(c(par[['lambda']], qs_N0[['50%']],par[['C']]), c('lambda', 'N0', 'C'))
  upr <- setNames(c(qs_lambda[['97.5%']], qs_N0[['50%']],qs_C[['97.5%']]), c('lambda', 'N0','C'))
  
  xcoords <- c(1:299)
  dfit <- decayModel(par=lwr, timepoints=xcoords)
  y_lwr <- dfit$output
  dfit <- decayModel(par=mid, timepoints=xcoords)
  y_mid <- dfit$output
  dfit <- decayModel(par=upr, timepoints=xcoords)
  y_upr <- dfit$output
  
  colourscheme <- getAllTrackDayOneColourScheme()
  col <- colourscheme[['T']] #use colour scheme according to group
  #upper and lower bounds create a polygon
  #polygon creates it from low left to low right, then up right to up left -> use rev
  #x is just trial nnumber, y depends on values of bounds
  polygon(x = c(xcoords, rev(xcoords)), y = c(y_lwr, rev(y_upr)), border=NA, col=col)
  #add CIs for asymptote
  abline(h = c(qs_N0[['2.5%']], qs_N0[['97.5%']]), col = col, lty = 2, lwd=2)
  col <- colourscheme[['S']]
  lines(xcoords, y_mid,col=col,lty=1,lwd=2)
  
  #add legend
  legend(2,100,legend=c('lap time','model (rate of change)','asymptote 95% CI'),
         col=c('#A9A9A9ff',colourscheme[['S']],colourscheme[['T']]),
         lty=c(1,1,2),bty='n',cex=.65,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
}
  

