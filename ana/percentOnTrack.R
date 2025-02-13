source('ana/shared.R')

#Percent on Track (accuracy) ----
getParticipantAccuracy <- function(group, id, session) {
  
  filepath <- sprintf('data/%s/%s/S%03d/trial_results.csv', group, id, session)
  df <- read.csv(filepath, stringsAsFactors = F)
  
  #setup relevant vectors
  trialno <- c()
  participant <- c()
  session <- c()
  group <- c()
  track_orientation <- c()
  accuracy<- c()
  
  for (trial in c(1:dim(df)[1])) {
    trial_num <- df$trial_num[trial]
    pp <- df$ppid[trial]
    session_num <- df$session_num[trial]
    group_cond <- df$experiment[trial]
    orientation <- df$Track_orientation[trial]
    perc_track <- df$percent_on_track[trial]
    
    trialno <- c(trialno, trial_num)
    participant <- c(participant, pp)
    session <- c(session, session_num)
    group <- c(group, group_cond)
    track_orientation <- c(track_orientation, orientation)
    accuracy <- c(accuracy, perc_track)    
    
  }
  
  ppdata <- data.frame(trialno, participant, session, group, track_orientation, accuracy)
  return(ppdata)
  
}

#Session 1----
getGroupAccuracy <- function(group, session){
  
  # exlude participants due to experiment problems
  pp_exclude <- c('01', '02', '03', '04', '05', '20', '22', '25', '44', '46')
  pp_group <- unique(list.files(sprintf('data/%s', group)))
  pp_group <- pp_group[which(!pp_group %in% pp_exclude)]
  
  dataoutput <- data.frame()
  for(pp in pp_group){
    ppdat <- getParticipantAccuracy(group = group, id = pp, session = session)
    trial <- ppdat$trialno
    accuracy <- ppdat$accuracy
    ppdat <- data.frame(trial, accuracy)
    names(ppdat)[names(ppdat) == 'accuracy'] <- pp
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, accuracy)
      names(dataoutput)[names(dataoutput) == 'accuracy'] <- pp
    }
    
  }
  return(dataoutput)
}

getGroupAccuracyCI <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 1, type = 'b'){
  
  for(group in groups){
    data <- getGroupAccuracy(group = group, session = session)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:(dim(data)[2])])
    
    confidence <- data.frame()
    
    
    for (t in trialno){
      accuracy <- data1[t, ]
      
      if (type == "t"){
        accuracy <- accuracy[!is.na(accuracy)]
        citrial <- getConfidenceInterval(data = accuracy, variance = var(accuracy), method = type)
      } else if(type == "b"){
        citrial <- getConfidenceInterval(data = accuracy, variance = var(accuracy), method = type)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
    }
    write.csv(confidence, file=sprintf('data/PercentOnTrackCI_%s_S%03d.csv', group, session), row.names = F) 
  }
  
}

plotAccuracy <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 1, target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig2_PercentOnTrack.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  plot(NA, NA, xlim = c(0,301), ylim = c(70, 101), 
       xlab = "Trial", ylab = "Percent on track (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Accuracy across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  #abline(v = c(30, 60, 90, 120, 150, 180, 210, 240, 270), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 15, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300)) #tick marks for x axis
  axis(2, at = c(70, 80, 90, 100), las=2) #tick marks for y axis
  
  for(group in groups){
    #read in files created by CI function
    groupconfidence <- read.csv(file=sprintf('data/PercentOnTrackCI_%s_S%03d.csv', group, session))
    
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
  legend(200,80,legend=c('track_0°', 'track_90°', 'track_180°', 'track_270°'),
         col=c(colourscheme[['T-RACING_0']][['S']],colourscheme[['T-RACING_90']][['S']],colourscheme[['T-RACING_180']][['S']],colourscheme[['T-RACING_270']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Session 2----

getS2GroupAccuracy <- function(group, session = 2){
  
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
    ppdat <- getParticipantAccuracy(group = group, id = pp, session = session)
    trial <- ppdat$trialno
    accuracy <- ppdat$accuracy
    ppdat <- data.frame(trial, accuracy)
    names(ppdat)[names(ppdat) == 'accuracy'] <- pp
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, accuracy)
      names(dataoutput)[names(dataoutput) == 'accuracy'] <- pp
    }
    
  }
  return(dataoutput)
}

getS2GroupAccuracyCI <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 2, type = 'b'){
  
  for(group in groups){
    data <- getS2GroupAccuracy(group = group, session = session)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:(dim(data)[2])])
    
    confidence <- data.frame()
    
    
    for (t in trialno){
      accuracy <- data1[t, ]
      
      if (type == "t"){
        accuracy <- accuracy[!is.na(accuracy)]
        citrial <- getConfidenceInterval(data = accuracy, variance = var(accuracy), method = type)
      } else if(type == "b"){
        citrial <- getConfidenceInterval(data = accuracy, variance = var(accuracy), method = type)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
    }
    write.csv(confidence, file=sprintf('data/PercentOnTrackCI_%s_S%03d.csv', group, session), row.names = F) 
  }
  
}

plotS2Accuracy <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 2, target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig2A_PercentOnTrack_Session2.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  plot(NA, NA, xlim = c(0,121), ylim = c(70, 101), 
       xlab = "Trial", ylab = "Percent on track (%)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Accuracy across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(v = c(30, 60, 90), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 15, 30, 60, 90, 120)) #tick marks for x axis
  axis(2, at = c(70, 80, 90, 100), las=2) #tick marks for y axis
  
  for(group in groups){
    #read in files created by CI function
    groupconfidence <- read.csv(file=sprintf('data/PercentOnTrackCI_%s_S%03d.csv', group, session))
    
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
  legend(80,80,legend=c('track_0°', 'track_90°', 'track_180°', 'track_270°'),
         col=c(colourscheme[['T-RACING_0']][['S']],colourscheme[['T-RACING_90']][['S']],colourscheme[['T-RACING_180']][['S']],colourscheme[['T-RACING_270']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#plotting sessions together----

plotAcrossSessionAccuracy <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig2B_PercentOnTrack_AllSessions.svg', width=16, height=6, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(3,3), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotAccuracy()
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotS2Accuracy()
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}