source('ana/shared.R')
source('ana/pathLength.R')

# Details----
# Every sample has (x,z) coordinate from Unity output.
# First sample will have distance from origin as sqrt(x^2 + z^2) - called absolute vector
# Path length is total distance (given x and z trajectories) traveled between movement onset and offset.
# One may think that simply calculating absolute vector from endpoint will measure this, but trajectories
# may curve or go in different directions. So we need to account for every sample.But we simply can't
# add absolute vectors across samples (this will include lengths accounted for by previous sample). So,
# every new sample's absolute vector will be calculated using the previous sample as its origin. Then all
# these values are added to come up with a total path length.
# Repeat this process for all trials within one participant. Then show mean measures across participants for every trial.
# Unity sample output is in units of meters. So we have to convert this to centimeters on screen.

# Screen dimensions (WxH): 52.7 cm X 29.6 cm
# Tablet dimensions (WXH): 31.1 cm X 21.6 cm
# Horizontal (x) dimension ratio: 31.1/ 52.7 = 0.59
# Vertical (z) dimension ratio: 21.6/ 29.6 = 0.73

# Horizontal dimension:
# 1m distance in Unity = 62 pixels
# Screen has 36 pixels/ cm
# 62/ 36 = 1.72 cm on screen
# 1.72 cm on screen (*0.59) = 1.01 cm on tablet

# Vertical dimension:
# 1m distance in Unity = 57 pixels
# 57/ 36 = 1.58 cm on screen
# 1.58 cm on screen (*0.73) = 1.15 cm on tablet


# Path Length----
getR2ParticipantTrajectory <- function(group, id, session) {
  
  filepath <- sprintf('data/data_run2/%s/%s/S%03d/trial_results.csv', group, id, session)
  df <- read.csv(filepath, stringsAsFactors = F)
  
  #setup relevant vectors
  trialno <- c()
  participant <- c()
  session <- c()
  group <- c()
  track_orientation <- c()
  path_length <- c()
  
  for (trial in c(1:dim(df)[1])) {
    trial_num <- df$trial_num[trial]
    pp <- df$ppid[trial]
    session_num <- df$session_num[trial]
    group_cond <- df$experiment[trial]
    orientation <- df$Track_orientation[trial]
    
    #calculate path length for trial
    x_m <- convertCellToNumVector(df$cursor_path_x[trial])
    z_m <- convertCellToNumVector(df$cursor_path_z[trial])
    #convert meters to screen cm
    x <- (x_m * 62)/ 36
    z <- (z_m * 62)/ 36
    
    
    ndat <- data.frame()
    for (idx in c(2:length(x))){ #start with second sample, since first will calculate a difference from origin
      sampx <- x[idx] - x[idx-1]
      sampz <- z[idx] - z[idx-1]
      absvec <- sqrt(((sampx)^2)+((sampz)^2))
      
      if (prod(dim(ndat)) == 0){
        ndat <- absvec
      } else {
        ndat <- rbind(ndat, absvec)
      }
      
    }
    pathlength <- sum(ndat[1:length(ndat)])
    
    trialno <- c(trialno, trial_num)
    participant <- c(participant, pp)
    session <- c(session, session_num)
    group <- c(group, group_cond)
    track_orientation <- c(track_orientation, orientation)
    path_length <- c(path_length, pathlength)    
    
  }
  
  ppdata <- data.frame(trialno, participant, session, group, track_orientation, path_length)
  return(ppdata)
  
}

getR2ParticipantInOutPL <- function(group, id, session) {
  
  filepath <- sprintf('data/data_run2/%s/%s/S%03d/trial_results.csv', group, id, session)
  df <- read.csv(filepath, stringsAsFactors = F)
  
  #setup relevant vectors
  trialno <- c()
  participant <- c()
  session <- c()
  group <- c()
  track_orientation <- c()
  path_length_in <- c()
  path_length_out <- c()
  
  for (trial in c(1:dim(df)[1])) {
    #print(trial)
    trial_num <- df$trial_num[trial]
    pp <- df$ppid[trial]
    session_num <- df$session_num[trial]
    group_cond <- df$experiment[trial]
    orientation <- df$Track_orientation[trial]
    
    #unpack data into its own cell
    x_m <- convertCellToNumVector(df$cursor_path_x[trial])
    z_m <- convertCellToNumVector(df$cursor_path_z[trial])
    t_m <- convertCellToNumVector(df$cursor_pathTime[trial])
    in_m <- convertCellToNumVector(df$Enter_track_Time[trial])
    out_m <- convertCellToNumVector(df$Exit_track_time[trial])
    
    #convert meters to screen cm for x and z positions
    x <- (x_m * 62)/ 36
    z <- (z_m * 62)/ 36
    trackloc <- 'in'
    idx <- c(1:length(x))
    
    trialdat <- data.frame(idx, x, z, t_m, trackloc)
    
    #match timepts they go out of track
    if(length(out_m) == 0){
      #get total path length
      
      ndat_all <- data.frame()
      for (samp in c(2:nrow(trialdat))){ #start with second sample, since first will calculate a difference from origin
        sampx <- trialdat$x[samp] - trialdat$x[samp-1]
        sampz <- trialdat$z[samp] - trialdat$z[samp-1]
        absvec <- sqrt(((sampx)^2)+((sampz)^2))
        
        if (prod(dim(ndat_all)) == 0){
          ndat_all <- absvec
        } else {
          ndat_all <- rbind(ndat_all, absvec)
        }
        
      }
      PL_all<- sum(ndat_all[1:length(ndat_all)])
      PL_in <- PL_all
      PL_out <- 0
      
    } else{
      ndat_trackloc <- data.frame()
      for(trackloc in c(1:length(out_m))){
        
        timepts <- c(out_m[trackloc], in_m[trackloc])
        if(is.na(timepts[2])){
          timepts[2] <- trialdat$t_m[length(trialdat$t_m)]
        }
        timepts <- as.data.frame(matrix(data=timepts, ncol=2, byrow=T))
        if (prod(dim(ndat_trackloc)) == 0){
          ndat_trackloc <- timepts
        } else {
          ndat_trackloc <- rbind(ndat_trackloc, timepts)
        }
      }
      
      #assign in or out labels for each timept
      for(n in c(1:nrow(ndat_trackloc))){
        
        limits <- c(ndat_trackloc[n,])
        #can raise warnings:
        #In at least one participant, many samples have the same time recorded (T-RACING_270, pp 47)
        #line below will raise warnings, since time indexed will have mutliple values
        #but it always chooses the first sample, which is what is correct
        #add [1] to remove warnings
        time_idx <- trialdat[which(trialdat$t_m == limits[1])[1]:which(trialdat$t_m == limits[2])[1],]
        time_idx$trackloc <- 'out'
        trialdat[which(trialdat$t_m == limits[1])[1]:which(trialdat$t_m == limits[2])[1],] <- time_idx
      }
      
      #get path length for out
      ndat_out <- data.frame()
      subdat <- trialdat[which(trialdat$trackloc == 'out'),]
      out_idx <- subdat$idx
      breaks <- c(1,which(diff(out_idx) != 1))
      
      for(i in c(1:length(breaks))){
        if(i == 1 && i == length(breaks)){
          start_idx <- breaks[i]
          end_idx <- length(out_idx)
        } else if (i == 1) {
          start_idx <- breaks[i]
          end_idx <- breaks[i+1]
        } else if (i == length(breaks)) {
          start_idx <- breaks[i] + 1
          end_idx <- length(out_idx)
        } else {
          start_idx <- breaks[i] + 1
          end_idx <- breaks[i+1]
        }
        
        
        newdf<- subdat[start_idx:end_idx,]
        if(nrow(newdf) == 1){
          absvec <- NA #since it is just a single sample
          if (prod(dim(ndat_out)) == 0){
            ndat_out <- absvec
          } else {
            ndat_out <- rbind(ndat_out, absvec)
          }
        } else {
          for(samp in c(2:nrow(newdf))){ #start with second sample, since first will calculate a difference from origin
            sampx <- newdf$x[samp] - newdf$x[samp-1]
            sampz <- newdf$z[samp] - newdf$z[samp-1]
            absvec <- sqrt(((sampx)^2)+((sampz)^2))
            
            if (prod(dim(ndat_out)) == 0){
              ndat_out <- absvec
            } else {
              ndat_out <- rbind(ndat_out, absvec)
            }
          }
        }
        
      }
      
      PL_out <- sum(ndat_out[1:length(ndat_out)])
      
      #get total path length
      
      ndat_all <- data.frame()
      for (samp in c(2:nrow(trialdat))){ #start with second sample, since first will calculate a difference from origin
        sampx <- trialdat$x[samp] - trialdat$x[samp-1]
        sampz <- trialdat$z[samp] - trialdat$z[samp-1]
        absvec <- sqrt(((sampx)^2)+((sampz)^2))
        
        if (prod(dim(ndat_all)) == 0){
          ndat_all <- absvec
        } else {
          ndat_all <- rbind(ndat_all, absvec)
        }
        
      }
      PL_all<- sum(ndat_all[1:length(ndat_all)])
      PL_in <- PL_all - PL_out #get in track path length
      
    }
    
    trialno <- c(trialno, trial_num)
    participant <- c(participant, pp)
    session <- c(session, session_num)
    group <- c(group, group_cond)
    track_orientation <- c(track_orientation, orientation)
    path_length_in <- c(path_length_in, PL_in)    
    path_length_out <- c(path_length_out, PL_out) 
    
  }
  
  ppdata <- data.frame(trialno, participant, session, group, track_orientation, path_length_in, path_length_out)
  return(ppdata)
  
}

#Session 1----
getR2GroupPL <- function(group, session){
  
  # exlude participants due to experiment problems
  pp_exclude <- c('p002', 'p005', 'p012', 'p017', 'p021', 'p032')
  pp_group <- unique(list.files(sprintf('data/data_run2/%s', group)))
  pp_group <- pp_group[which(!pp_group %in% pp_exclude)]
  
  dataoutput <- data.frame()
  for(pp in pp_group){
    ppdat <- getR2ParticipantTrajectory(group = group, id = pp, session = session)
    trial <- ppdat$trialno
    pathlength <- ppdat$path_length
    ppdat <- data.frame(trial, pathlength)
    names(ppdat)[names(ppdat) == 'pathlength'] <- pp
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, pathlength)
      names(dataoutput)[names(dataoutput) == 'pathlength'] <- pp
    }
    
  }
  return(dataoutput)
}

getR2GroupPLCI <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 1, type = 'b'){
  
  for(group in groups){
    data <- getR2GroupPL(group = group, session = session)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:(dim(data)[2])])
    
    confidence <- data.frame()
    
    
    for (t in trialno){
      pathlength <- data1[t, ]
      
      if (type == "t"){
        pathlength <- pathlength[!is.na(pathlength)]
        citrial <- getConfidenceInterval(data = pathlength, variance = var(pathlength), method = type)
      } else if(type == "b"){
        citrial <- getConfidenceInterval(data = pathlength, variance = var(pathlength), method = type)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
    }
    write.csv(confidence, file=sprintf('data/Run2_PathLengthCI_%s_S%03d.csv', group, session), row.names = F) 
  }
  
}

plotR2PathLength <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 1, target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig3_PathLength.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  plot(NA, NA, xlim = c(0,301), ylim = c(45, 71), 
       xlab = "Trial", ylab = "Path length (cm on screen)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Path length across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  #abline(v = c(30, 60, 90, 120, 150, 180, 210, 240, 270), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 15, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300)) #tick marks for x axis
  axis(2, at = c(45, 50, 55, 60, 65, 70), las=2) #tick marks for y axis
  
  for(group in groups){
    #read in files created by CI function
    groupconfidence <- read.csv(file=sprintf('data/Run2_PathLengthCI_%s_S%03d.csv', group, session))
    
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
  legend(200,68,legend=c('track_0°', 'track_90°', 'track_180°', 'track_270°'),
         col=c(colourscheme[['T-RACING_0']][['S']],colourscheme[['T-RACING_90']][['S']],colourscheme[['T-RACING_180']][['S']],colourscheme[['T-RACING_270']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# Session 1: Combine track orientations----

getR2AllTrackGroupPL <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 1, target=0){
  
  alldata <- data.frame()
  
  for(group in groups){
    data <- getR2GroupPL(group = group, session = session)
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
    write.csv(ndat, file=sprintf('data/Run2_PathLength_AllTrack_S%03d.csv', session), row.names = F) 
  } else{
    return(ndat)
  }
}

getR2AllTrackGroupPLCI <- function(session = 1, type = 'b'){
  
  data <- getR2AllTrackGroupPL(session = session)
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
  write.csv(confidence, file=sprintf('data/Run2_PathLengthCI_AllTrack_S%03d.csv', session), row.names = F) 
  
}

plotR2AllTrackPL <- function(session = 1, target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig8_PathLength_AllTrack.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  plot(NA, NA, xlim = c(0,301), ylim = c(45, 61), 
       xlab = "Trial", ylab = "Path length (cm on screen)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Path length across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  #abline(v = c(30, 60, 90, 120, 150, 180, 210, 240, 270), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300)) #tick marks for x axis
  axis(2, at = c(45, 50, 55, 60), las=2) #tick marks for y axis
  
  
  #read in files created by CI function
  groupconfidence <- read.csv(file=sprintf('data/Run2_PathLengthCI_AllTrack_S%03d.csv', session))
  
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

# Session 1: PL IN vs OUT----

getR2GroupInOutPL <- function(group, session, trackloc){
  
  # exlude participants due to experiment problems
  pp_exclude <- c('p002', 'p005', 'p012', 'p017', 'p021', 'p032')
  pp_group <- unique(list.files(sprintf('data/data_run2/%s', group)))
  pp_group <- pp_group[which(!pp_group %in% pp_exclude)]
  
  dataoutput <- data.frame()
  for(pp in pp_group){
    ppdat <- getR2ParticipantInOutPL(group = group, id = pp, session = session)
    trial <- ppdat$trialno
    
    if(trackloc == 'in'){
      pathlength_in <- ppdat$path_length_in
      ppdat_in <- data.frame(trial, pathlength_in)
      names(ppdat_in)[names(ppdat_in) == 'pathlength_in'] <- pp
      if (prod(dim(dataoutput)) == 0){
        dataoutput <- ppdat_in
      } else {
        dataoutput <- cbind(dataoutput, pathlength_in)
        names(dataoutput)[names(dataoutput) == 'pathlength_in'] <- pp
      }
    } else if (trackloc == 'out'){
      pathlength_out <- ppdat$path_length_out
      ppdat_out <- data.frame(trial, pathlength_out)
      names(ppdat_out)[names(ppdat_out) == 'pathlength_out'] <- pp
      if (prod(dim(dataoutput)) == 0){
        dataoutput <- ppdat_out
      } else {
        dataoutput <- cbind(dataoutput, pathlength_out)
        names(dataoutput)[names(dataoutput) == 'pathlength_out'] <- pp
      }
    }
  }
  return(dataoutput)
}

getR2AllTrackGroupInOutPL <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 1, trackloc = 'out'){
  
  alldata <- data.frame()
  
  for(group in groups){
    data <- getR2GroupInOutPL(group = group, session = session, trackloc = trackloc)
    trial <- data$trial
    data1 <- as.matrix(data[,2:(dim(data)[2])])
    
    if (prod(dim(alldata)) == 0){
      alldata <- data1
    } else {
      alldata <- cbind(alldata, data1)
    }
  }
  ndat <- data.frame(trial, alldata)
  if(trackloc == 'in'){
    write.csv(ndat, file=sprintf('data/Run2_PathLength_in_AllTrack_S%03d.csv', session), row.names = F) 
  } else if (trackloc == 'out'){
    write.csv(ndat, file=sprintf('data/Run2_PathLength_out_AllTrack_S%03d.csv', session), row.names = F) 
  }
  
}

getR2AllTrackGroupInOutPLCI <- function(session = 1, type = 'b', trackloc = 'out'){
  
  #data <- getAllTrackGroupInOutPL(session = session, trackloc = trackloc)
  data <- read.csv(file=sprintf('data/Run2_PathLength_%s_AllTrack_S%03d.csv', trackloc, session))
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
  write.csv(confidence, file=sprintf('data/Run2_PathLengthCI_%s_AllTrack_S%03d.csv', trackloc, session), row.names = F) 
  
}

plotR2AllTrackInOutPL <- function(session = 1, target='inline', trackloc = 'out') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file=sprintf('doc/fig_run2/Fig13_PathLength_%s_AllTrack.svg', trackloc), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  if(trackloc == 'out'){
    plot(NA, NA, xlim = c(0,301), ylim = c(0, 12), 
         xlab = "Trial", ylab = "Path length (cm on screen)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Path length %sside track: Session %s", trackloc, session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    #abline(v = c(30, 60, 90, 120, 150, 180, 210, 240, 270), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    axis(1, at = c(1, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300)) #tick marks for x axis
    axis(2, at = c(1, 2,4,6,8,10,12), las=2) #tick marks for y axis
  } else if(trackloc == 'in'){
    plot(NA, NA, xlim = c(0,301), ylim = c(40, 71), 
         xlab = "Trial", ylab = "Path length (cm on screen)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Path length %sside track: Session %s", trackloc, session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    #abline(v = c(30, 60, 90, 120, 150, 180, 210, 240, 270), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    axis(1, at = c(1, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300)) #tick marks for x axis
    axis(2, at = c(40, 45, 50, 55, 60), las=2) #tick marks for y axis
  }
  
  
  
  #read in files created by CI function
  groupconfidence <- read.csv(file=sprintf('data/Run2_PathLengthCI_%s_AllTrack_S%03d.csv', trackloc, session))
  
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

getR2S2GroupPL <- function(group, session = 2){
  
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
    ppdat <- getR2ParticipantTrajectory(group = group, id = pp, session = session)
    trial <- ppdat$trialno
    pathlength <- ppdat$path_length
    ppdat <- data.frame(trial,pathlength)
    names(ppdat)[names(ppdat) == 'pathlength'] <- pp
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, pathlength)
      names(dataoutput)[names(dataoutput) == 'pathlength'] <- pp
    }
    
  }
  return(dataoutput)
}

getR2S2GroupPLCI <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 2, type = 'b'){
  
  for(group in groups){
    data <- getR2S2GroupPL(group = group, session = session)
    trialno <- data$trial
    data1 <- as.matrix(data[,2:(dim(data)[2])])
    
    confidence <- data.frame()
    
    
    for (t in trialno){
      pathlength <- data1[t, ]
      
      if (type == "t"){
        pathlength <- pathlength[!is.na(pathlength)]
        citrial <- getConfidenceInterval(data = pathlength, variance = var(pathlength), method = type)
      } else if(type == "b"){
        citrial <- getConfidenceInterval(data = pathlength, variance = var(pathlength), method = type)
      }
      
      if (prod(dim(confidence)) == 0){
        confidence <- citrial
      } else {
        confidence <- rbind(confidence, citrial)
      }
    }
    write.csv(confidence, file=sprintf('data/Run2_PathLengthCI_%s_S%03d.csv', group, session), row.names = F) 
  }
  
}

plotR2S2PathLength <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 2, target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig3A_PathLength_Session2.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  meanGroupReaches <- list() #empty list so that it plots the means last
  
  #NA to create empty plot
  plot(NA, NA, xlim = c(0,121), ylim = c(45, 71), 
       xlab = "Trial", ylab = "Path length (cm on screen)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Path length across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(v = c(30, 60, 90), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 15, 30, 60, 90, 120)) #tick marks for x axis
  axis(2, at = c(45, 50, 55, 60, 65, 70), las=2) #tick marks for y axis
  
  for(group in groups){
    #read in files created by CI function
    groupconfidence <- read.csv(file=sprintf('data/Run2_PathLengthCI_%s_S%03d.csv', group, session))
    
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
  legend(80,68,legend=c('track_0°', 'track_90°', 'track_180°', 'track_270°'),
         col=c(colourscheme[['T-RACING_0']][['S']],colourscheme[['T-RACING_90']][['S']],colourscheme[['T-RACING_180']][['S']],colourscheme[['T-RACING_270']][['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# Session 2: Combine track orientations----

getR2S2AllTrackGroupPL <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 2, target=0){
  
  alldata <- data.frame()
  
  for(group in groups){
    data <- getR2S2GroupPL(group = group, session = session)
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
    write.csv(ndat, file=sprintf('data/Run2_PathLength_AllTrack_S%03d.csv', session), row.names = F) 
  } else{
    return(ndat)
  }
}

getR2S2AllTrackGroupPLCI <- function(session = 2, type = 'b'){
  
  data <- getR2S2AllTrackGroupPL(session = session)
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
  write.csv(confidence, file=sprintf('data/Run2_PathLengthCI_AllTrack_S%03d.csv', session), row.names = F) 
  
}

plotR2S2AllTrackPL <- function(session = 2, blocks = c(1,2,3,4), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig8A_PathLength_AllTrack_Session2.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  plot(NA, NA, xlim = c(0,121), ylim = c(45, 61), 
       xlab = "Trial", ylab = "Path length (cm on screen)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Path length across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(v = c(30, 60, 90), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90, 120)) #tick marks for x axis
  axis(2, at = c(45, 50, 55, 60), las=2) #tick marks for y axis
  
  
  #read in files created by CI function
  groupconfidence <- read.csv(file=sprintf('data/Run2_PathLengthCI_AllTrack_S%03d.csv', session))
  
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
  legend(70,60,legend=c('trained track', 'flipped track: 180°', 'reverse track'),
         col=c(colb1[['S']],colb2[['S']],colb3[['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

#Session 2: PL IN vs OUT----

getR2S2GroupInOutPL <- function(group, session = 2, trackloc){
  
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
    ppdat <- getR2ParticipantInOutPL(group = group, id = pp, session = session)
    trial <- ppdat$trialno
    
    if(trackloc == 'in'){
      pathlength_in <- ppdat$path_length_in
      ppdat_in <- data.frame(trial, pathlength_in)
      names(ppdat_in)[names(ppdat_in) == 'pathlength_in'] <- pp
      if (prod(dim(dataoutput)) == 0){
        dataoutput <- ppdat_in
      } else {
        dataoutput <- cbind(dataoutput, pathlength_in)
        names(dataoutput)[names(dataoutput) == 'pathlength_in'] <- pp
      }
    } else if (trackloc == 'out'){
      pathlength_out <- ppdat$path_length_out
      ppdat_out <- data.frame(trial, pathlength_out)
      names(ppdat_out)[names(ppdat_out) == 'pathlength_out'] <- pp
      if (prod(dim(dataoutput)) == 0){
        dataoutput <- ppdat_out
      } else {
        dataoutput <- cbind(dataoutput, pathlength_out)
        names(dataoutput)[names(dataoutput) == 'pathlength_out'] <- pp
      }
    }
    
  }
  return(dataoutput)
}

getR2S2AllTrackGroupInOutPL <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 2, trackloc = 'out'){
  
  alldata <- data.frame()
  
  for(group in groups){
    data <- getR2S2GroupInOutPL(group = group, session = session, trackloc = trackloc)
    trial <- data$trial
    data1 <- as.matrix(data[,2:(dim(data)[2])])
    
    if (prod(dim(alldata)) == 0){
      alldata <- data1
    } else {
      alldata <- cbind(alldata, data1)
    }
  }
  ndat <- data.frame(trial, alldata)
  if(trackloc == 'in'){
    write.csv(ndat, file=sprintf('data/Run2_PathLength_in_AllTrack_S%03d.csv', session), row.names = F) 
  } else if (trackloc == 'out'){
    write.csv(ndat, file=sprintf('data/Run2_PathLength_out_AllTrack_S%03d.csv', session), row.names = F) 
  }
  
}

getR2S2AllTrackGroupInOutPLCI <- function(session = 2, type = 'b', trackloc = 'out'){
  
  #data <- getAllTrackGroupInOutPL(session = session, trackloc = trackloc)
  data <- read.csv(file=sprintf('data/Run2_PathLength_%s_AllTrack_S%03d.csv', trackloc, session))
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
  write.csv(confidence, file=sprintf('data/Run2_PathLengthCI_%s_AllTrack_S%03d.csv', trackloc, session), row.names = F) 
  
}

plotR2S2AllTrackInOutPL <- function(session = 2, target='inline', blocks = c(1,2,3,4), trackloc = 'out') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file=sprintf('doc/fig_run2/Fig14_PathLength_%s_AllTrack_S2.svg', trackloc), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  if(trackloc == 'out'){
    plot(NA, NA, xlim = c(0,121), ylim = c(0, 12), 
         xlab = "Trial", ylab = "Path length (cm on screen)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Path length %sside track: Session %s", trackloc, session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    #abline(v = c(30, 60, 90, 120, 150, 180, 210, 240, 270), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(v = c(30, 60, 90), col = 8, lty = 2)
    axis(1, at = c(1, 30, 60, 90, 120)) #tick marks for x axis
    axis(2, at = c(1, 2,4,6,8,10,12), las=2) #tick marks for y axis
  } else if(trackloc == 'in'){
    plot(NA, NA, xlim = c(0,121), ylim = c(40, 71), 
         xlab = "Trial", ylab = "Path length (cm on screen)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Path length %sside track: Session %s", trackloc, session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    #abline(v = c(30, 60, 90, 120, 150, 180, 210, 240, 270), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(v = c(30, 60, 90), col = 8, lty = 2)
    axis(1, at = c(1, 30, 60, 90, 120)) #tick marks for x axis
    axis(2, at = c(40, 45, 50, 55, 60), las=2) #tick marks for y axis
  }
  
  #read in files created by CI function
  groupconfidence <- read.csv(file=sprintf('data/Run2_PathLengthCI_%s_AllTrack_S%03d.csv', trackloc, session))
  
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
  if(trackloc == 'out'){
    legend(70,12,legend=c('trained track', 'flipped track: 180°', 'reverse track'),
           col=c(colb1[['S']],colb2[['S']],colb3[['S']]),
           lty=1,bty='n',cex=1,lwd=2)
  } else {
    legend(70,60,legend=c('trained track', 'flipped track: 180°', 'reverse track'),
           col=c(colb1[['S']],colb2[['S']],colb3[['S']]),
           lty=1,bty='n',cex=1,lwd=2)
  }
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# CHECK: plot individual data----
plotIndividualR2S2PL <- function(group, session = 2){
  
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
    ppdat <- getR2ParticipantTrajectory(group = group, id = pp, session = session)
    trial <- ppdat$trialno
    pathlength <- ppdat$path_length
    ppdat <- data.frame(trial,pathlength)
    names(ppdat)[names(ppdat) == 'pathlength'] <- pp
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, pathlength)
      names(dataoutput)[names(dataoutput) == 'pathlength'] <- pp
    }
    
  }
  plot(NA, NA, xlim = c(0,121), ylim = c(45, 71), 
       xlab = "Trial", ylab = "Path length (cm on screen)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Path length across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(v = c(30, 60, 90), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 15, 30, 60, 90, 120)) #tick marks for x axis
  axis(2, at = c(45, 50, 55, 60, 65, 70), las=2) #tick marks for y axis
  
  for(pp in c(2:dim(dataoutput)[2])){
    lines(dataoutput[pp], col = 'grey')
  }
  
}

plotR2ParticipantTrajectory <- function(group, id, session=2) {
  
  filepath <- sprintf('data/data_run2/%s/%s/S%03d/trial_results.csv', group, id, session)
  df <- read.csv(filepath, stringsAsFactors = F)
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mfrow=c(3,5))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  # layout(matrix(c(1,2,3,4,5,
  #                 6,7,8,9,10,
  #                 11,12,13,14,15), 6, 5, byrow = TRUE), widths=c(3,3,3,3,3), heights=c(1,1,1,1,1,1))
  
  for (trial in c(61:90)) {
    #calculate path length for trial
    x_m <- convertCellToNumVector(df$cursor_path_x[trial])
    z_m <- convertCellToNumVector(df$cursor_path_z[trial])
    #convert meters to screen cm
    x <- (x_m * 62)/ 36
    z <- (z_m * 62)/ 36
    
    plot(x,z)
  }
  
}

#plotting sessions together----

plotR2AcrossSessionPathLength <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig3B_PathLength_AllSessions.svg', width=16, height=6, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(3,3), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotR2PathLength()
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotR2S2PathLength()
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotR2AllTrackAcrossSessionPL<- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig8B_PathLength_AllTrack_AllSessions.svg', width=16, height=6, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(3,3), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotR2AllTrackPL()
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotR2S2AllTrackPL()
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# Remove 1st trial: All tracks----

plotR2S1FirstLastPL <- function(session = 1, target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig9_PathLength_AllTrack.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  plot(NA, NA, xlim = c(1,61), ylim = c(45, 56), 
       xlab = "Trial", ylab = "Path length (cm on screen)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Path length across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  
  # include shaded regions to highlight trial set 1
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(2, lim[3], 6, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 1
  colourscheme <- getAllTrackSession1ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(4, 55,pch=16,cex=2,col=col)
  text(4, 55,labels=set,cex=0.6,col='white')
  
  # trial set 2
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(7, lim[3], 12, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 2
  colourscheme <- getAllTrackSession1ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(9.5, 55,pch=16,cex=2,col=col)
  text(9.5, 55,labels=set,cex=0.6,col='white')
  
  # trial set 3
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(13, lim[3], 18, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 3
  colourscheme <- getAllTrackSession1ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(15.5, 55,pch=16,cex=2,col=col)
  text(15.5, 55,labels=set,cex=0.6,col='white')
  
  # trial set 4
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(19, lim[3], 24, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 4
  colourscheme <- getAllTrackSession1ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(21.5, 55,pch=16,cex=2,col=col)
  text(21.5, 55,labels=set,cex=0.6,col='white')
  
  # trial set 5
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(25, lim[3], 30, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 5
  colourscheme <- getAllTrackSession1ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(27.5, 55,pch=16,cex=2,col=col)
  text(27.5, 55,labels=set,cex=0.6,col='white')
  
  # trial set 50
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(55, lim[3], 60, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 6
  colourscheme <- getAllTrackSession1ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(57.5, 55,pch=16,cex=2,col=col)
  text(57.5, 55,labels=50,cex=0.6,col='white')
  
  abline(v = c(30), h = c(49.68), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(side=1, at=c(2, 15, 30, 32, 45, 60), labels=c('2', '15', '30', '272', '285', '300'))
  axis(2, at = c(45, 47, 49, 51, 53, 55), las=2) #tick marks for y axis
  
  #read in files created by CI function
  groupconfidence <- read.csv(file=sprintf('data/Run2_PathLengthCI_AllTrack_S%03d.csv', session))
  
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

plotR2S1InOutFirstLastPL <- function(session = 1, target='inline', trackloc = 'out') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file=sprintf('doc/fig_run2/Fig15_PathLength_%s_AllTrack_FirstLast.svg', trackloc), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  if(trackloc == 'in'){
    plot(NA, NA, xlim = c(1,61), ylim = c(41, 51), 
         xlab = "Trial", ylab = "Path length (cm on screen)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Path length %sside trials: Session %s", trackloc, session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    # include shaded regions to highlight trial set 1
    lim <- par('usr')
    col <- "#ededed"
    col <- alpha(col, .5)
    rect(2, lim[3], 6, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
    
    set <- 1
    colourscheme <- getAllTrackSession1ColourScheme(blocks=set)
    col <- colourscheme[[set]][['S']]
    points(4, 51,pch=16,cex=2,col=col)
    text(4, 51,labels=set,cex=0.6,col='white')
    
    # trial set 2
    col <- "#ededed"
    col <- alpha(col, .5)
    rect(7, lim[3], 12, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
    
    set <- 2
    colourscheme <- getAllTrackSession1ColourScheme(blocks=set)
    col <- colourscheme[[set]][['S']]
    points(9.5, 51,pch=16,cex=2,col=col)
    text(9.5, 51,labels=set,cex=0.6,col='white')
    
    # trial set 3
    col <- "#ededed"
    col <- alpha(col, .5)
    rect(13, lim[3], 18, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
    
    set <- 3
    colourscheme <- getAllTrackSession1ColourScheme(blocks=set)
    col <- colourscheme[[set]][['S']]
    points(15.5, 51,pch=16,cex=2,col=col)
    text(15.5, 51,labels=set,cex=0.6,col='white')
    
    # trial set 4
    col <- "#ededed"
    col <- alpha(col, .5)
    rect(19, lim[3], 24, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
    
    set <- 4
    colourscheme <- getAllTrackSession1ColourScheme(blocks=set)
    col <- colourscheme[[set]][['S']]
    points(21.5, 51,pch=16,cex=2,col=col)
    text(21.5, 51,labels=set,cex=0.6,col='white')
    
    # trial set 5
    col <- "#ededed"
    col <- alpha(col, .5)
    rect(25, lim[3], 30, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
    
    set <- 5
    colourscheme <- getAllTrackSession1ColourScheme(blocks=set)
    col <- colourscheme[[set]][['S']]
    points(27.5, 51,pch=16,cex=2,col=col)
    text(27.5, 51,labels=set,cex=0.6,col='white')
    
    # trial set 50
    col <- "#ededed"
    col <- alpha(col, .5)
    rect(55, lim[3], 60, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
    
    set <- 6
    colourscheme <- getAllTrackSession1ColourScheme(blocks=set)
    col <- colourscheme[[set]][['S']]
    points(57.5, 51,pch=16,cex=2,col=col)
    text(57.5, 51,labels=50,cex=0.6,col='white')
    
    abline(v = c(30), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    axis(side=1, at=c(2, 15, 30, 32, 45, 60), labels=c('2', '15', '30', '272', '285', '300'))
    axis(2, at = c(41, 43, 45, 47, 49, 51), las=2) #tick marks for y axis
  } else if (trackloc == 'out'){
    plot(NA, NA, xlim = c(1,61), ylim = c(0, 11), 
         xlab = "Trial", ylab = "Path length (cm on screen)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Path length %sside trials: Session %s", trackloc, session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(v = c(30), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    axis(side=1, at=c(2, 15, 30, 32, 45, 60), labels=c('2', '15', '30', '272', '285', '300'))
    axis(2, at = c(0, 2, 4, 6, 8, 10), las=2) #tick marks for y axis
  }
  
  
  #read in files created by CI function
  groupconfidence <- read.csv(file=sprintf('data/Run2_PathLengthCI_%s_AllTrack_S%03d.csv', trackloc, session))
  
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

plotR2S2FirstLastPL <- function(session = 2, blocks = c(1,2,3,4), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig9A_PathLength_AllTrack_Session2.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  plot(NA, NA, xlim = c(0,121), ylim = c(45, 56), 
       xlab = "Trial", ylab = "Path length (cm on screen)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Path length across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  
  # include shaded regions to highlight trial set 1
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(2, lim[3], 6, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 1
  colourscheme <- getSAFS2ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(4, 55,pch=15,cex=2,col=col)
  text(4, 55,labels=1,cex=0.6,col='white')
  
  # include shaded regions to highlight trial set 2
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(25, lim[3], 30, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 2
  colourscheme <- getSAFS2ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(27.5, 55,pch=15,cex=2,col=col)
  text(27.5, 55,labels=5,cex=0.6,col='white')
  
  # include shaded regions to highlight trial set 3
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(32, lim[3], 36, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 3
  colourscheme <- getSAFS2ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(34, 55,pch=15,cex=2,col=col)
  text(34, 55,labels=1,cex=0.6,col='white')
  
  # include shaded regions to highlight trial set 4
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(55, lim[3], 60, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 4
  colourscheme <- getSAFS2ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(57.5, 55,pch=15,cex=2,col=col)
  text(57.5, 55,labels=5,cex=0.6,col='white')
  
  # include shaded regions to highlight trial set 5
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(62, lim[3], 66, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 5
  colourscheme <- getSAFS2ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(64, 55,pch=15,cex=2,col=col)
  text(64, 55,labels=1,cex=0.6,col='white')
  
  # include shaded regions to highlight trial set 6
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(85, lim[3], 90, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 6
  colourscheme <- getSAFS2ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(87.5, 55,pch=15,cex=2,col=col)
  text(87.5, 55,labels=5,cex=0.6,col='white')
  
  # include shaded regions to highlight trial set 7
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(92, lim[3], 96, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 7
  colourscheme <- getSAFS2ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(94, 55,pch=15,cex=2,col=col)
  text(94, 55,labels=1,cex=0.6,col='white')
  
  # include shaded regions to highlight trial set 8
  lim <- par('usr')
  col <- "#ededed"
  col <- alpha(col, .5)
  rect(115, lim[3], 120, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
  
  set <- 8
  colourscheme <- getSAFS2ColourScheme(blocks=set)
  col <- colourscheme[[set]][['S']]
  points(117.5, 55,pch=15,cex=2,col=col)
  text(117.5, 55,labels=5,cex=0.6,col='white')
  
  abline(v = c(30, 60, 90), h = c(49.68), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  #axis(1, at = c(1, 15, 30, 60, 90, 120)) #tick marks for x axis
  #axis(2, at = c(45, 47, 49, 51, 53, 55), las=2) #tick marks for y axis
  axis(1, at = c(2, 32, 62, 92, 120)) #tick marks for x axis
  axis(2, at = c(45, 47, 49, 51, 53, 55), las=2) #tick marks for y axis
  
  
  #read in files created by CI function
  groupconfidence <- read.csv(file=sprintf('data/Run2_PathLengthCI_AllTrack_S%03d.csv', session))
  
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
  legend(70,55,legend=c('trained track', 'flipped track: 180°', 'reverse track'),
         col=c(colb1[['S']],colb2[['S']],colb3[['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotR2S2InOutFirstLastPL <- function(session = 2, blocks = c(1,2,3,4), target='inline', trackloc = 'out') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file=sprintf('doc/fig_run2/Fig15_PathLength_%s_AllTrack_FirstLast_Session2.svg', trackloc), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  if(trackloc == 'out'){
    plot(NA, NA, xlim = c(0,121), ylim = c(0, 11), 
         xlab = "Trial", ylab = "Path length (cm on screen)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Path length %sside track: Session %s", trackloc, session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    #abline(v = c(30, 60, 90, 120, 150, 180, 210, 240, 270), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(v = c(30, 60, 90), col = 8, lty = 2)
    axis(1, at = c(2, 30, 60, 90, 120)) #tick marks for x axis
    axis(2, at = c(0,2,4,6,8,10), las=2) #tick marks for y axis
  } else if(trackloc == 'in'){
    plot(NA, NA, xlim = c(0,121), ylim = c(41, 51), 
         xlab = "Trial", ylab = "Path length (cm on screen)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Path length %sside track: Session %s", trackloc, session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    
    # include shaded regions to highlight trial set 1
    lim <- par('usr')
    col <- "#ededed"
    col <- alpha(col, .5)
    rect(2, lim[3], 6, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
    
    set <- 1
    colourscheme <- getSAFS2ColourScheme(blocks=set)
    col <- colourscheme[[set]][['S']]
    points(4, 51,pch=15,cex=2,col=col)
    text(4, 51,labels=1,cex=0.6,col='white')
    
    # include shaded regions to highlight trial set 2
    lim <- par('usr')
    col <- "#ededed"
    col <- alpha(col, .5)
    rect(25, lim[3], 30, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
    
    set <- 2
    colourscheme <- getSAFS2ColourScheme(blocks=set)
    col <- colourscheme[[set]][['S']]
    points(27.5, 51,pch=15,cex=2,col=col)
    text(27.5, 51,labels=5,cex=0.6,col='white')
    
    # include shaded regions to highlight trial set 3
    lim <- par('usr')
    col <- "#ededed"
    col <- alpha(col, .5)
    rect(32, lim[3], 36, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
    
    set <- 3
    colourscheme <- getSAFS2ColourScheme(blocks=set)
    col <- colourscheme[[set]][['S']]
    points(34, 51,pch=15,cex=2,col=col)
    text(34, 51,labels=1,cex=0.6,col='white')
    
    # include shaded regions to highlight trial set 4
    lim <- par('usr')
    col <- "#ededed"
    col <- alpha(col, .5)
    rect(55, lim[3], 60, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
    
    set <- 4
    colourscheme <- getSAFS2ColourScheme(blocks=set)
    col <- colourscheme[[set]][['S']]
    points(57.5, 51,pch=15,cex=2,col=col)
    text(57.5, 51,labels=5,cex=0.6,col='white')
    
    # include shaded regions to highlight trial set 5
    lim <- par('usr')
    col <- "#ededed"
    col <- alpha(col, .5)
    rect(62, lim[3], 66, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
    
    set <- 5
    colourscheme <- getSAFS2ColourScheme(blocks=set)
    col <- colourscheme[[set]][['S']]
    points(64, 51,pch=15,cex=2,col=col)
    text(64, 51,labels=1,cex=0.6,col='white')
    
    # include shaded regions to highlight trial set 6
    lim <- par('usr')
    col <- "#ededed"
    col <- alpha(col, .5)
    rect(85, lim[3], 90, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
    
    set <- 6
    colourscheme <- getSAFS2ColourScheme(blocks=set)
    col <- colourscheme[[set]][['S']]
    points(87.5, 51,pch=15,cex=2,col=col)
    text(87.5, 51,labels=5,cex=0.6,col='white')
    
    # include shaded regions to highlight trial set 7
    lim <- par('usr')
    col <- "#ededed"
    col <- alpha(col, .5)
    rect(92, lim[3], 96, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
    
    set <- 7
    colourscheme <- getSAFS2ColourScheme(blocks=set)
    col <- colourscheme[[set]][['S']]
    points(94, 51,pch=15,cex=2,col=col)
    text(94, 51,labels=1,cex=0.6,col='white')
    
    # include shaded regions to highlight trial set 8
    lim <- par('usr')
    col <- "#ededed"
    col <- alpha(col, .5)
    rect(115, lim[3], 120, lim[4]+2, border = col, col = col) #xleft, ybottom, x right, ytop; light grey hex code
    
    set <- 8
    colourscheme <- getSAFS2ColourScheme(blocks=set)
    col <- colourscheme[[set]][['S']]
    points(117.5, 51,pch=15,cex=2,col=col)
    text(117.5, 51,labels=5,cex=0.6,col='white')
    
    #abline(v = c(30, 60, 90, 120, 150, 180, 210, 240, 270), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(v = c(30, 60, 90), col = 8, lty = 2)
    axis(1, at = c(2, 30, 60, 90, 120)) #tick marks for x axis
    axis(2, at = c(41, 43, 45, 47, 49, 51), las=2) #tick marks for y axis
  }
  
  #read in files created by CI function
  groupconfidence <- read.csv(file=sprintf('data/Run2_PathLengthCI_%s_AllTrack_S%03d.csv', trackloc, session))
  
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
  if(trackloc == 'out'){
    legend(70,11,legend=c('trained track', 'flipped track: 180°', 'reverse track'),
           col=c(colb1[['S']],colb2[['S']],colb3[['S']]),
           lty=1,bty='n',cex=1,lwd=2)
  } else {
    legend(70,51,legend=c('trained track', 'flipped track: 180°', 'reverse track'),
           col=c(colb1[['S']],colb2[['S']],colb3[['S']]),
           lty=1,bty='n',cex=1,lwd=2)
  }
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotR2FirstLastAllTrackAcrossSessionPL <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig9B_PathLength_AllTrack_AllSessions.svg', width=12, height=6, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(3,3), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotR2S1FirstLastPL()
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotR2S2FirstLastPL()
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotR2InOutFirstLastAllTrackAcrossSessionPL <- function(target='inline', trackloc= 'out'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file=sprintf('doc/fig_run2/Fig16_PathLength_%s_AllTrack_AllSessions.svg', trackloc), width=16, height=6, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(3,3), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotR2S1InOutFirstLastPL(trackloc = trackloc)
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotR2S2InOutFirstLastPL(trackloc = trackloc)
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# Individual data - Remove 1st trial: All tracks----

plotR2S1IndFirstLastPL <- function(session = 1, target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig9C_IndividualPathLength_AllTrack.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  plot(NA, NA, xlim = c(1,61), ylim = c(42, 56), 
       xlab = "Trial", ylab = "Path length (cm on screen)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Path length across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(v = c(30), h = c(49.68), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(side=1, at=c(2, 15, 30, 32, 45, 60), labels=c('2', '15', '30', '272', '285', '300'))
  axis(2, at = c(42, 45, 47, 49, 51, 53, 55), las=2) #tick marks for y axis
  
  #read in individual data
  alldat <- read.csv(file=sprintf('data/Run2_PathLength_AllTrack_S%03d.csv', session))
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
  groupconfidence <- read.csv(file=sprintf('data/Run2_PathLengthCI_AllTrack_S%03d.csv', session))
  
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

plotR2S1IndInOutFirstLastPL <- function(session = 1, target='inline', trackloc = 'out') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file=sprintf('doc/fig_run2/Fig15B_IndividualPathLength_%s_AllTrack_FirstLast.svg', trackloc), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  if(trackloc == 'in'){
    plot(NA, NA, xlim = c(1,61), ylim = c(41, 51), 
         xlab = "Trial", ylab = "Path length (cm on screen)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Path length %sside trials: Session %s", trackloc, session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(v = c(30), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    axis(side=1, at=c(2, 15, 30, 32, 45, 60), labels=c('2', '15', '30', '272', '285', '300'))
    axis(2, at = c(41, 43, 45, 47, 49, 51), las=2) #tick marks for y axis
  } else if (trackloc == 'out'){
    plot(NA, NA, xlim = c(1,61), ylim = c(0, 11), 
         xlab = "Trial", ylab = "Path length (cm on screen)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Path length %sside trials: Session %s", trackloc, session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    abline(v = c(30), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    axis(side=1, at=c(2, 15, 30, 32, 45, 60), labels=c('2', '15', '30', '272', '285', '300'))
    axis(2, at = c(0, 2, 4, 6, 8, 10), las=2) #tick marks for y axis
  }
  
  #read in individual data
  alldat <- read.csv(file=sprintf('data/Run2_PathLength_%s_AllTrack_S%03d.csv', trackloc, session))
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
  groupconfidence <- read.csv(file=sprintf('data/Run2_PathLengthCI_%s_AllTrack_S%03d.csv', trackloc, session))
  
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

plotR2S2IndFirstLastPL <- function(session = 2, blocks = c(1,2,3,4), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig9D_IndividualPathLength_AllTrack_Session2.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  plot(NA, NA, xlim = c(0,121), ylim = c(42, 56), 
       xlab = "Trial", ylab = "Path length (cm on screen)", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Path length across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(v = c(30, 60, 90), h = c(49.68), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  #axis(1, at = c(1, 15, 30, 60, 90, 120)) #tick marks for x axis
  #axis(2, at = c(45, 47, 49, 51, 53, 55), las=2) #tick marks for y axis
  axis(1, at = c(2, 32, 62, 92, 120)) #tick marks for x axis
  axis(2, at = c(42, 45, 47, 49, 51, 53, 55), las=2) #tick marks for y axis
  
  #read in individual data
  alldat <- read.csv(file=sprintf('data/Run2_PathLength_AllTrack_S%03d.csv', session))
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
  groupconfidence <- read.csv(file=sprintf('data/Run2_PathLengthCI_AllTrack_S%03d.csv', session))
  
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
  legend(70,55,legend=c('trained track', 'flipped track: 180°', 'reverse track'),
         col=c(colb1[['S']],colb2[['S']],colb3[['S']]),
         lty=1,bty='n',cex=1,lwd=2)
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotR2S2IndInOutFirstLastPL <- function(session = 2, blocks = c(1,2,3,4), target='inline', trackloc = 'out') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file=sprintf('doc/fig_run2/Fig15C_IndividualPathLength_%s_AllTrack_FirstLast_Session2.svg', trackloc), width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  if(trackloc == 'out'){
    plot(NA, NA, xlim = c(0,121), ylim = c(0, 11), 
         xlab = "Trial", ylab = "Path length (cm on screen)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Path length %sside track: Session %s", trackloc, session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    #abline(v = c(30, 60, 90, 120, 150, 180, 210, 240, 270), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(v = c(30, 60, 90), col = 8, lty = 2)
    axis(1, at = c(2, 30, 60, 90, 120)) #tick marks for x axis
    axis(2, at = c(0,2,4,6,8,10), las=2) #tick marks for y axis
  } else if(trackloc == 'in'){
    plot(NA, NA, xlim = c(0,121), ylim = c(41, 51), 
         xlab = "Trial", ylab = "Path length (cm on screen)", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Path length %sside track: Session %s", trackloc, session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    #abline(v = c(30, 60, 90, 120, 150, 180, 210, 240, 270), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    abline(v = c(30, 60, 90), col = 8, lty = 2)
    axis(1, at = c(2, 30, 60, 90, 120)) #tick marks for x axis
    axis(2, at = c(41, 43, 45, 47, 49, 51), las=2) #tick marks for y axis
  }
  
  #read in individual data
  alldat <- read.csv(file=sprintf('data/Run2_PathLength_%s_AllTrack_S%03d.csv', trackloc, session))
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
  groupconfidence <- read.csv(file=sprintf('data/Run2_PathLengthCI_%s_AllTrack_S%03d.csv', trackloc, session))
  
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
  if(trackloc == 'out'){
    legend(70,11,legend=c('trained track', 'flipped track: 180°', 'reverse track'),
           col=c(colb1[['S']],colb2[['S']],colb3[['S']]),
           lty=1,bty='n',cex=1,lwd=2)
  } else {
    legend(70,51,legend=c('trained track', 'flipped track: 180°', 'reverse track'),
           col=c(colb1[['S']],colb2[['S']],colb3[['S']]),
           lty=1,bty='n',cex=1,lwd=2)
  }
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotR2IndFirstLastAllTrackAcrossSessionPL <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig_run2/Fig9E_IndividualPathLength_AllTrack_AllSessions.svg', width=12, height=6, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(3,3), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotR2S1IndFirstLastPL()
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotR2S2IndFirstLastPL()
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotR2IndInOutFirstLastAllTrackAcrossSessionPL <- function(target='inline', trackloc= 'out'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file=sprintf('doc/fig_run2/Fig16B_PathLength_%s_AllTrack_AllSessions.svg', trackloc), width=16, height=6, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(3,3), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotR2S1IndInOutFirstLastPL(trackloc = trackloc)
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotR2S2IndInOutFirstLastPL(trackloc = trackloc)
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

# Statistics (Preprocess data) ----

getR2BlockedPL<- function(session, blockdefs) {
  
  dat <- read.csv(file=sprintf('data/Run2_PathLength_AllTrack_S%03d.csv', session))
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

retentionR2PLANOVA <- function() {
  
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getR2BlockedPL(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
  LC_part2 <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2'),]
  
  #but we only want to analyze participants with data in both
  LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_1','S2_2'))
  
  #ANOVA's
  # for ez, case ID should be a factor:
  
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=dv, within= c(set), type=3, return_aov = TRUE) #df is k-1 or 3 levels minus 1; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Path Length during first and last set in session 1 and first and second set in session 2:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#follow up on significant interaction
retentionR2PLComparisonMeans <- function(){
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getR2BlockedPL(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
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

retentionR2PLComparisons <- function(method='bonferroni'){
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getR2BlockedPL(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
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
retentionR2PLComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- retentionR2PLComparisons(method=method)
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
retentionR2PLBayesANOVA <- function() {
  
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getR2BlockedPL(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
  LC_part2 <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2'),]
  
  #but we only want to analyze participants with data in both
  LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_1','S2_2'))
  
  cat('Path Length during first and last set in session 1 and first and second set in session 2:\n')
  bfLC<- anovaBF(dv ~ set + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  #bfinteraction <- bfLC[4]/bfLC[3]
  
  #bfinclude to compare model with interactions against all other models
  bfinteraction <- bayesfactor_inclusion(bfLC)
  
  print(bfLC)
  print(bfinteraction)
  
}

retentionR2PLComparisonsBayesfollowup <- function() {
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getR2BlockedPL(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
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

genR2PLANOVA <- function() {
  
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_3' | LC_part2$set == 'S2_4'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_3','S2_4'))
  
  #ANOVA's
  # for ez, case ID should be a factor:
  
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=dv, within= c(set), type=3, return_aov = TRUE) #df is k-1 or 3 levels minus 1; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Path Length during first block in session 2 and second block in session 2:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#follow up 
#means below
genR2PLComparisonMeans <- function(){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_3' | LC_part2$set == 'S2_4'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_3','S2_4'))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
  
  cellmeans <- emmeans(secondAOV,specs=c('set'))
  print(cellmeans)
  
}

genR2PLComparisons <- function(method='bonferroni'){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs)
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
genR2PLComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- genR2PLComparisons(method=method)
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
genR2PLBayesANOVA <- function() {
  
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_3' | LC_part2$set == 'S2_4'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_3','S2_4'))
  
  cat('Path Length during first block in session 2 and second block in session 2:\n')
  bfLC<- anovaBF(dv ~ set + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  #bfinteraction <- bfLC[4]/bfLC[3]
  
  #bfinclude to compare model with interactions against all other models
  bfinteraction <- bayesfactor_inclusion(bfLC)
  
  print(bfLC)
  print(bfinteraction)
  
}

genR2PLComparisonsBayesfollowup <- function() {
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
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

revR2PLANOVA <- function() {
  
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_5' | LC_part2$set == 'S2_6'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_5','S2_6'))
  
  #ANOVA's
  # for ez, case ID should be a factor:
  
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=dv, within= c(set), type=3, return_aov = TRUE) #df is k-1 or 3 levels minus 1; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Path Length during first block in session 2 and third block in session 2:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#follow up on significant interaction
revR2PLComparisonMeans <- function(){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_5' | LC_part2$set == 'S2_6'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_5','S2_6'))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
  
  cellmeans <- emmeans(secondAOV,specs=c('set'))
  print(cellmeans)
  
}

revR2PLComparisons <- function(method='bonferroni'){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
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
revR2PLComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- revR2PLComparisons(method=method)
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
revR2PLBayesANOVA <- function() {
  
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_5' | LC_part2$set == 'S2_6'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_5','S2_6'))
  
  cat('Path Length during first block in session 2 and third block in session 2:\n')
  bfLC<- anovaBF(dv ~ set + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  #bfinteraction <- bfLC[4]/bfLC[3]
  
  #bfinclude to compare model with interactions against all other models
  bfinteraction <- bayesfactor_inclusion(bfLC)
  
  print(bfLC)
  print(bfinteraction)
  
}

revR2PLComparisonsBayesfollowup <- function() {
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
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

trainR2PLANOVA <- function() {
  
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_7' | LC_part2$set == 'S2_8'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_7','S2_8'))
  
  #ANOVA's
  # for ez, case ID should be a factor:
  
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=dv, within= c(set), type=3, return_aov = TRUE) #df is k-1 or 3 levels minus 1; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Path Length during first block in session 2 and last block in session 2:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#follow up
trainR2PLComparisonMeans <- function(){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_7' | LC_part2$set == 'S2_8'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_7','S2_8'))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
  
  cellmeans <- emmeans(secondAOV,specs=c('set'))
  print(cellmeans)
  
}

trainR2PLComparisons <- function(method='bonferroni'){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs)
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
trainR2PLComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- trainR2PLComparisons(method=method)
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
trainR2PLTtest <- function(){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
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
trainR2PLBayesANOVA <- function() {
  
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_7' | LC_part2$set == 'S2_8'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_7','S2_8'))
  
  cat('Path Length during first block in session 2 and last block in session 2:\n')
  bfLC<- anovaBF(dv ~ set + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  #bfinteraction <- bfLC[4]/bfLC[3]
  
  #bfinclude to compare model with interactions against all other models
  bfinteraction <- bayesfactor_inclusion(bfLC)
  
  print(bfLC)
  print(bfinteraction)
  
}

trainR2PLComparisonsBayesfollowup <- function() {
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
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

# Statistics: PL IN (Preprocess data) ----

getR2BlockedPLIN<- function(session, blockdefs) {
  
  dat <- read.csv(file=sprintf('data/Run2_PathLength_in_AllTrack_S%03d.csv', session))
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

#Statistics: PL IN (Retention; Frequentist) ----

retentionR2PLINANOVA <- function() {
  
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getR2BlockedPLIN(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs) 
  LC_part2 <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2'),]
  
  #but we only want to analyze participants with data in both
  LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_1','S2_2'))
  
  #ANOVA's
  # for ez, case ID should be a factor:
  
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=dv, within= c(set), type=3, return_aov = TRUE) #df is k-1 or 3 levels minus 1; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Path Length in during first and last set in session 1 and first and second set in session 2:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#follow up unnecessary
#means below
retentionR2PLINComparisonMeans <- function(){
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getR2BlockedPLIN(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs) 
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

# retentionR2PLINComparisons <- function(method='bonferroni'){
#   #session1
#   blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
#   LC_part1 <- getR2BlockedPLIN(session = 1, blockdefs=blockdefs)
#   #session2
#   blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
#   LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs)
#   LC_part2 <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2'),]
# 
#   #but we only want to analyze participants with data in both
#   LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
#   LC4aov <- rbind(LC_part1, LC_part2)
#   LC4aov$participant <- as.factor(LC4aov$participant)
#   LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_1','S2_2'))
# 
#   secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
# 
#   #specify contrasts
#   S1_firstvsS1_last <- c(-1,1,0,0)
#   S1_firstvsS2_1 <- c(-1,0,1,0)
#   S1_lastvsS2_1 <- c(0,-1,1,0)
#   S1_lastvsS2_2 <- c(0,-1,0,1)
# 
#   contrastList <- list('Session 1 Set 1 vs Session 1 Set Last' = S1_firstvsS1_last,
#                        'Session 1 Set 1 vs Session 2 Set 1' = S1_firstvsS2_1,
#                        'Session 1 Set Last vs Session 2 Set 1' = S1_lastvsS2_1,
#                        'Session 1 Set Last vs Session 2 Set 2' = S1_lastvsS2_2)
# 
#   comparisons<- contrast(emmeans(secondAOV,specs=c('set')), contrastList, adjust=method)
# 
#   print(comparisons)
# 
# }
# 
# #effect size
# retentionR2PLINComparisonsEffSize <- function(method = 'bonferroni'){
#   comparisons <- retentionR2PLINComparisons(method=method)
#   #we can use eta-squared as effect size
#   #% of variance in DV(percentcomp) accounted for
#   #by the difference between target1 and target2
#   comparisonsdf <- as.data.frame(comparisons)
#   etasq <- ((comparisonsdf$t.ratio)^2)/(((comparisonsdf$t.ratio)^2)+(comparisonsdf$df))
#   comparisons1 <- cbind(comparisonsdf,etasq)
# 
#   effectsize <- data.frame(comparisons1$contrast, comparisons1$etasq)
#   colnames(effectsize) <- c('contrast', 'etasquared')
#   #print(comparisons)
#   print(effectsize)
# }

#Statistics: PL IN (Retention; Bayesian) ----
retentionR2PLINBayesANOVA <- function() {
  
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getR2BlockedPLIN(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs) 
  LC_part2 <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2'),]
  
  #but we only want to analyze participants with data in both
  LC_part1 <- LC_part1[which(LC_part1$participant %in% LC_part2$participant),]
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last','S2_1','S2_2'))
  
  cat('Path Length in during first and last set in session 1 and first and second set in session 2:\n')
  bfLC<- anovaBF(dv ~ set + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  #bfinteraction <- bfLC[4]/bfLC[3]
  
  #bfinclude to compare model with interactions against all other models
  bfinteraction <- bayesfactor_inclusion(bfLC)
  
  print(bfLC)
  print(bfinteraction)
  
}

retentionR2PLINComparisonsBayesfollowup <- function() {
  #session1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getR2BlockedPLIN(session = 1, blockdefs=blockdefs) 
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs) 
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

#Statistics: PL IN (Generalization; Frequentist) ----

genR2PLINANOVA <- function() {
  
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_3' | LC_part2$set == 'S2_4'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_3','S2_4'))
  
  #ANOVA's
  # for ez, case ID should be a factor:
  
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=dv, within= c(set), type=3, return_aov = TRUE) #df is k-1 or 3 levels minus 1; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Path Length in during first block in session 2 and second block in session 2:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#follow up unnecessary
#means below
genR2PLINComparisonMeans <- function(){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_3' | LC_part2$set == 'S2_4'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_3','S2_4'))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
  
  cellmeans <- emmeans(secondAOV,specs=c('set'))
  print(cellmeans)
  
}

# genR2PLINComparisons <- function(method='bonferroni'){
#   #session2
#   blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
#   LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs)
#   LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_3' | LC_part2$set == 'S2_4'),]
#   
#   LC4aov$participant <- as.factor(LC4aov$participant)
#   LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_3','S2_4'))
#   
#   secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
#   
#   #specify contrasts
#   S2_1vsS2_3 <- c(-1,0,1,0)
#   S2_2vsS2_3 <- c(0,-1,1,0)
#   S2_2vsS2_4 <- c(0,-1,0,1)
#   
#   contrastList <- list('Session 2 Set 1 vs Session 2 Set 3' = S2_1vsS2_3,
#                        'Session 2 Set 2 vs Session 2 Set 3' = S2_2vsS2_3,
#                        'Session 2 Set 2 vs Session 2 Set 4' = S2_2vsS2_4)
#   
#   comparisons<- contrast(emmeans(secondAOV,specs=c('set')), contrastList, adjust=method)
#   
#   print(comparisons)
#   
# }
# 
# #effect size
# genR2PLINComparisonsEffSize <- function(method = 'bonferroni'){
#   comparisons <- genR2PLINComparisons(method=method)
#   #we can use eta-squared as effect size
#   #% of variance in DV(percentcomp) accounted for
#   #by the difference between target1 and target2
#   comparisonsdf <- as.data.frame(comparisons)
#   etasq <- ((comparisonsdf$t.ratio)^2)/(((comparisonsdf$t.ratio)^2)+(comparisonsdf$df))
#   comparisons1 <- cbind(comparisonsdf,etasq)
#   
#   effectsize <- data.frame(comparisons1$contrast, comparisons1$etasq)
#   colnames(effectsize) <- c('contrast', 'etasquared')
#   #print(comparisons)
#   print(effectsize)
# }

#Statistics: PL IN (Generalization; Bayesian) ----
genR2PLINBayesANOVA <- function() {
  
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_3' | LC_part2$set == 'S2_4'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_3','S2_4'))
  
  cat('Path Length in during first block in session 2 and second block in session 2:\n')
  bfLC<- anovaBF(dv ~ set + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  #bfinteraction <- bfLC[4]/bfLC[3]
  
  #bfinclude to compare model with interactions against all other models
  bfinteraction <- bayesfactor_inclusion(bfLC)
  
  print(bfLC)
  print(bfinteraction)
  
}

genR2PLINComparisonsBayesfollowup <- function() {
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs) 
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

#Statistics: PL IN (Reverse direction; Frequentist) ----

revR2PLINANOVA <- function() {
  
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_5' | LC_part2$set == 'S2_6'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_5','S2_6'))
  
  #ANOVA's
  # for ez, case ID should be a factor:
  
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=dv, within= c(set), type=3, return_aov = TRUE) #df is k-1 or 3 levels minus 1; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Path Length in during first block in session 2 and third block in session 2:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#follow up on significant interaction
revR2PLINComparisonMeans <- function(){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_5' | LC_part2$set == 'S2_6'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_5','S2_6'))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
  
  cellmeans <- emmeans(secondAOV,specs=c('set'))
  print(cellmeans)
  
}

revR2PLINComparisons <- function(method='bonferroni'){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs) 
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
revR2PLINComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- revR2PLINComparisons(method=method)
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
revR2PLINBayesANOVA <- function() {
  
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_5' | LC_part2$set == 'S2_6'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_5','S2_6'))
  
  cat('Path Length in during first block in session 2 and third block in session 2:\n')
  bfLC<- anovaBF(dv ~ set + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  #bfinteraction <- bfLC[4]/bfLC[3]
  
  #bfinclude to compare model with interactions against all other models
  bfinteraction <- bayesfactor_inclusion(bfLC)
  
  print(bfLC)
  print(bfinteraction)
  
}

revR2PLINComparisonsBayesfollowup <- function() {
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs) 
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

#Statistics: PL IN (Trained direction; Frequentist) ----

trainR2PLINANOVA <- function() {
  
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_7' | LC_part2$set == 'S2_8'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_7','S2_8'))
  
  #ANOVA's
  # for ez, case ID should be a factor:
  
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=dv, within= c(set), type=3, return_aov = TRUE) #df is k-1 or 3 levels minus 1; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Path Length in during first block in session 2 and last block in session 2:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#follow up not necessary
#means below
trainR2PLINComparisonMeans <- function(){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_7' | LC_part2$set == 'S2_8'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_7','S2_8'))
  
  secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
  
  cellmeans <- emmeans(secondAOV,specs=c('set'))
  print(cellmeans)
  
}

# trainR2PLINComparisons <- function(method='bonferroni'){
#   #session2
#   blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
#   LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs)
#   LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_7' | LC_part2$set == 'S2_8'),]
# 
#   LC4aov$participant <- as.factor(LC4aov$participant)
#   LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_7','S2_8'))
# 
#   secondAOV <- aov_ez("participant","dv",LC4aov,within=c("set"))
# 
#   #specify contrasts
#   S2_1vsS2_7 <- c(-1,0,1,0)
#   S2_2vsS2_7 <- c(0,-1,1,0)
#   S2_2vsS2_8 <- c(0,-1,0,1)
# 
#   contrastList <- list('Session 2 Set 1 vs Session 2 Set 7' = S2_1vsS2_7,
#                        'Session 2 Set 2 vs Session 2 Set 7' = S2_2vsS2_7,
#                        'Session 2 Set 2 vs Session 2 Set 8' = S2_2vsS2_8)
# 
#   comparisons<- contrast(emmeans(secondAOV,specs=c('set')), contrastList, adjust=method)
# 
#   print(comparisons)
# 
# }
# 
# #effect size
# trainR2PLINComparisonsEffSize <- function(method = 'bonferroni'){
#   comparisons <- trainR2PLINComparisons(method=method)
#   #we can use eta-squared as effect size
#   #% of variance in DV(percentcomp) accounted for
#   #by the difference between target1 and target2
#   comparisonsdf <- as.data.frame(comparisons)
#   etasq <- ((comparisonsdf$t.ratio)^2)/(((comparisonsdf$t.ratio)^2)+(comparisonsdf$df))
#   comparisons1 <- cbind(comparisonsdf,etasq)
# 
#   effectsize <- data.frame(comparisons1$contrast, comparisons1$etasq)
#   colnames(effectsize) <- c('contrast', 'etasquared')
#   #print(comparisons)
#   print(effectsize)
# }

# T-tests to compare last set in block 1 and first set in block 4 (both trained track in Session 2)
trainR2PLINTtest <- function(){
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs) 
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

#Statistics: PL IN (Trained direction; Bayesian) ----
trainR2PLINBayesANOVA <- function() {
  
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs) 
  LC4aov <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2' | LC_part2$set == 'S2_7' | LC_part2$set == 'S2_8'),]
  
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S2_1','S2_2','S2_7','S2_8'))
  
  cat('Path Length in during first block in session 2 and last block in session 2:\n')
  bfLC<- anovaBF(dv ~ set + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  #bfinteraction <- bfLC[4]/bfLC[3]
  
  #bfinclude to compare model with interactions against all other models
  bfinteraction <- bayesfactor_inclusion(bfLC)
  
  print(bfLC)
  print(bfinteraction)
  
}

trainR2PLINComparisonsBayesfollowup <- function() {
  #session2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs) 
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

compareStudiesPLANOVA <- function() {
  
  #study1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedPL(session = 1, blockdefs=blockdefs)
  LC_part1$study <- rep(1, nrow(LC_part1))
  #study2
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part2 <- getR2BlockedPL(session = 1, blockdefs=blockdefs)
  LC_part2$study <- rep(2, nrow(LC_part2))
  
  #between subjects comparison
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last'))
  LC4aov$study <- factor(LC4aov$study, levels = c(1,2))
  
  #ANOVA's
  # for ez, case ID should be a factor:
  
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=dv, within= c(set), between = c(study), type=3, return_aov = TRUE) #df is k-1 or 3 levels minus 1; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Path Length during first and last set in session 1 between studies:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#main effect of block is expected given learning across blocks
#we can test for differences in main effect of study
studiesPLComparisonMeans <- function(){
  #study1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedPL(session = 1, blockdefs=blockdefs)
  LC_part1$study <- rep(1, nrow(LC_part1))
  #study2
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part2 <- getR2BlockedPL(session = 1, blockdefs=blockdefs)
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

studiesPLComparisons <- function(method='bonferroni'){
  #study1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedPL(session = 1, blockdefs=blockdefs)
  LC_part1$study <- rep(1, nrow(LC_part1))
  #study2
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part2 <- getR2BlockedPL(session = 1, blockdefs=blockdefs)
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
studiesPLComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- studiesPLComparisons(method=method)
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
studiesPLBayesANOVA <- function() {
  
  #study1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedPL(session = 1, blockdefs=blockdefs)
  LC_part1$study <- rep(1, nrow(LC_part1))
  #study2
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part2 <- getR2BlockedPL(session = 1, blockdefs=blockdefs)
  LC_part2$study <- rep(2, nrow(LC_part2))
  
  #between subjects comparison
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last'))
  LC4aov$study <- factor(LC4aov$study, levels = c(1,2))
  
  cat('Path Length during first and last set in session 1 between studies:\n')
  bfLC<- anovaBF(dv ~ set*study + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  #bfinteraction <- bfLC[4]/bfLC[3]
  
  #bfinclude to compare model with interactions against all other models
  bfinteraction <- bayesfactor_inclusion(bfLC)
  
  print(bfLC)
  print(bfinteraction)
  
}

studiesPLComparisonsBayesfollowup <- function() {
  #study1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedPL(session = 1, blockdefs=blockdefs)
  LC_part1$study <- rep(1, nrow(LC_part1))
  #study2
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part2 <- getR2BlockedPL(session = 1, blockdefs=blockdefs)
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

compareStudiesS2PLANOVA <- function() {
  
  #study1
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part1 <- getBlockedPL(session = 2, blockdefs=blockdefs) 
  #LC_part1 <- LC_part1[which(LC_part1$set == 'S2_1' | LC_part1$set == 'S2_2'),]
  LC_part1$study <- rep(1, nrow(LC_part1))
  
  #study2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
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
  cat('Path Length in session 2 between studies:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#we can test for differences in main effect of study
studiesS2PLComparisonMeans <- function(){
  #study1
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part1 <- getBlockedPL(session = 2, blockdefs=blockdefs) 
  #LC_part1 <- LC_part1[which(LC_part1$set == 'S2_1' | LC_part1$set == 'S2_2'),]
  LC_part1$study <- rep(1, nrow(LC_part1))
  
  #study2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
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

studiesS2PLComparisons <- function(method='bonferroni'){
  #study1
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part1 <- getBlockedPL(session = 2, blockdefs=blockdefs) 
  #LC_part1 <- LC_part1[which(LC_part1$set == 'S2_1' | LC_part1$set == 'S2_2'),]
  LC_part1$study <- rep(1, nrow(LC_part1))
  
  #study2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
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
studiesS2PLComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- studiesS2PLComparisons(method=method)
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
studiesS2PLBayesANOVA <- function() {
  
  #study1
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part1 <- getBlockedPL(session = 2, blockdefs=blockdefs) 
  #LC_part1 <- LC_part1[which(LC_part1$set == 'S2_1' | LC_part1$set == 'S2_2'),]
  LC_part1$study <- rep(1, nrow(LC_part1))
  
  #study2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
  #LC_part2 <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2'),]
  LC_part2$study <- rep(2, nrow(LC_part2))
  
  #between subjects comparison
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- as.factor(LC4aov$set)
  LC4aov$study <- factor(LC4aov$study, levels = c(1,2))
  
  cat('Path Length in session 2 between studies:\n')
  bfLC<- anovaBF(dv ~ set*study + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  #bfinteraction <- bfLC[4]/bfLC[3]
  
  #bfinclude to compare model with interactions against all other models
  bfinteraction <- bayesfactor_inclusion(bfLC)
  
  print(bfLC)
  print(bfinteraction)
  
}

studiesS2PLComparisonsBayesfollowup <- function() {
  #study1
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part1 <- getBlockedPL(session = 2, blockdefs=blockdefs) 
  #LC_part1 <- LC_part1[which(LC_part1$set == 'S2_1' | LC_part1$set == 'S2_2'),]
  LC_part1$study <- rep(1, nrow(LC_part1))
  
  #study2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPL(session = 2, blockdefs=blockdefs) 
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

#Statistics (IN Compare studies: Session 1; Frequentist) ----

compareStudiesPLINANOVA <- function() {
  
  #study1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedPLIN(session = 1, blockdefs=blockdefs)
  LC_part1$study <- rep(1, nrow(LC_part1))
  #study2
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part2 <- getR2BlockedPLIN(session = 1, blockdefs=blockdefs)
  LC_part2$study <- rep(2, nrow(LC_part2))
  
  #between subjects comparison
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last'))
  LC4aov$study <- factor(LC4aov$study, levels = c(1,2))
  
  #ANOVA's
  # for ez, case ID should be a factor:
  
  firstAOV <- ezANOVA(data=LC4aov, wid=participant, dv=dv, within= c(set), between = c(study), type=3, return_aov = TRUE) #df is k-1 or 3 levels minus 1; N-1*k-1 for denom, total will be (N-1)(k1 -1)(k2 - 1)
  cat('Path Length inside during first and last set in session 1 between studies:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#we can test for differences in main effect of study
studiesPLINComparisonMeans <- function(){
  #study1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedPLIN(session = 1, blockdefs=blockdefs)
  LC_part1$study <- rep(1, nrow(LC_part1))
  #study2
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part2 <- getR2BlockedPLIN(session = 1, blockdefs=blockdefs)
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

studiesPLINComparisons <- function(method='bonferroni'){
  #study1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedPLIN(session = 1, blockdefs=blockdefs)
  LC_part1$study <- rep(1, nrow(LC_part1))
  #study2
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part2 <- getR2BlockedPLIN(session = 1, blockdefs=blockdefs)
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
studiesPLINComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- studiesPLINComparisons(method=method)
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

#Statistics (IN Compare studies: Session 1; Bayesian) ----
studiesPLINBayesANOVA <- function() {
  
  #study1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedPLIN(session = 1, blockdefs=blockdefs)
  LC_part1$study <- rep(1, nrow(LC_part1))
  #study2
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part2 <- getR2BlockedPLIN(session = 1, blockdefs=blockdefs)
  LC_part2$study <- rep(2, nrow(LC_part2))
  
  #between subjects comparison
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- factor(LC4aov$set, levels = c('S1_first','S1_last'))
  LC4aov$study <- factor(LC4aov$study, levels = c(1,2))
  
  cat('Path Length inside during first and last set in session 1 between studies:\n')
  bfLC<- anovaBF(dv ~ set*study + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  #bfinteraction <- bfLC[4]/bfLC[3]
  
  #bfinclude to compare model with interactions against all other models
  bfinteraction <- bayesfactor_inclusion(bfLC)
  
  print(bfLC)
  print(bfinteraction)
  
}

studiesPLINComparisonsBayesfollowup <- function() {
  #study1
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedPLIN(session = 1, blockdefs=blockdefs)
  LC_part1$study <- rep(1, nrow(LC_part1))
  #study2
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part2 <- getR2BlockedPLIN(session = 1, blockdefs=blockdefs)
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

#Statistics (IN Compare studies: Session 2; Frequentist) ----

compareStudiesS2PLINANOVA <- function() {
  
  #study1
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part1 <- getBlockedPLIN(session = 2, blockdefs=blockdefs) 
  #LC_part1 <- LC_part1[which(LC_part1$set == 'S2_1' | LC_part1$set == 'S2_2'),]
  LC_part1$study <- rep(1, nrow(LC_part1))
  
  #study2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs) 
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
  cat('Path Length inside for session 2 between studies:\n')
  print(firstAOV[1:3]) #so that it doesn't print the aov object as well
  
}

#we can test for differences in main effect of study
studiesS2PLINComparisonMeans <- function(){
  #study1
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part1 <- getBlockedPLIN(session = 2, blockdefs=blockdefs) 
  #LC_part1 <- LC_part1[which(LC_part1$set == 'S2_1' | LC_part1$set == 'S2_2'),]
  LC_part1$study <- rep(1, nrow(LC_part1))
  
  #study2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs) 
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

studiesS2PLINComparisons <- function(method='bonferroni'){
  #study1
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part1 <- getBlockedPLIN(session = 2, blockdefs=blockdefs) 
  #LC_part1 <- LC_part1[which(LC_part1$set == 'S2_1' | LC_part1$set == 'S2_2'),]
  LC_part1$study <- rep(1, nrow(LC_part1))
  
  #study2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs) 
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
studiesS2PLINComparisonsEffSize <- function(method = 'bonferroni'){
  comparisons <- studiesS2PLINComparisons(method=method)
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

#Statistics (IN Compare studies: Session 2, Block 1; Bayesian) ----
studiesS2PLINBayesANOVA <- function() {
  
  #study1
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part1 <- getBlockedPLIN(session = 2, blockdefs=blockdefs) 
  #LC_part1 <- LC_part1[which(LC_part1$set == 'S2_1' | LC_part1$set == 'S2_2'),]
  LC_part1$study <- rep(1, nrow(LC_part1))
  
  #study2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs) 
  #LC_part2 <- LC_part2[which(LC_part2$set == 'S2_1' | LC_part2$set == 'S2_2'),]
  LC_part2$study <- rep(2, nrow(LC_part2))
  
  #between subjects comparison
  LC4aov <- rbind(LC_part1, LC_part2)
  LC4aov$participant <- as.factor(LC4aov$participant)
  LC4aov$set <- as.factor(LC4aov$set)
  LC4aov$study <- factor(LC4aov$study, levels = c(1,2))
  
  cat('Path Length inside for session 2 between studies:\n')
  bfLC<- anovaBF(dv ~ set*study + participant, data = LC4aov, whichRandom = 'participant') #include data from participants, but note that this is a random factor
  #compare interaction contribution, over the contribution of both main effects
  #bfinteraction <- bfLC[4]/bfLC[3]
  
  #bfinclude to compare model with interactions against all other models
  bfinteraction <- bayesfactor_inclusion(bfLC)
  
  print(bfLC)
  print(bfinteraction)
  
}

studiesS2PLINComparisonsBayesfollowup <- function() {
  #study1
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part1 <- getBlockedPLIN(session = 2, blockdefs=blockdefs) 
  #LC_part1 <- LC_part1[which(LC_part1$set == 'S2_1' | LC_part1$set == 'S2_2'),]
  LC_part1$study <- rep(1, nrow(LC_part1))
  
  #study2
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getR2BlockedPLIN(session = 2, blockdefs=blockdefs) 
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


#CHECK: Path Length of track limits----
getR2ParticipantTrackLimitsPL <- function(group, id, session) {
  
  filepath <- sprintf('data/data_run2/%s/%s/S%03d/trial_results.csv', group, id, session)
  df <- read.csv(filepath, stringsAsFactors = F)
  
  #setup relevant vectors
  trialno <- c()
  participant <- c()
  session <- c()
  group <- c()
  track_orientation <- c()
  path_length_in <- c()
  path_length_out <- c()
  
  for (trial in c(1:dim(df)[1])) {
    #print(trial)
    trial_num <- df$trial_num[trial]
    pp <- df$ppid[trial]
    session_num <- df$session_num[trial]
    group_cond <- df$experiment[trial]
    orientation <- df$Track_orientation[trial]
    
    #unpack data into its own cell
    x_inner <- convertCellToNumVector(df$inner.track.point_x[trial])
    z_inner <- convertCellToNumVector(df$inner.track.point_z[trial])
    x_centre <- convertCellToNumVector(df$centre.track.point_x[trial])
    z_centre <- convertCellToNumVector(df$centre.track.point_z[trial])
    x_outer <- convertCellToNumVector(df$outer.track.point_x[trial])
    z_outer <- convertCellToNumVector(df$outer.track.point_z[trial])
    
    x_m <- convertCellToNumVector(df$cursor_path_x[trial])
    z_m <- convertCellToNumVector(df$cursor_path_z[trial])
    
    #convert meters to screen cm for x and z positions
    #multiply by -1 for track mirroring in run2
    x_in <- ((x_inner * 62)/ 36)*-1
    z_in <- ((z_inner * 62)/ 36)*-1
    x_cen <- ((x_centre * 62)/ 36)*-1
    z_cen <- ((z_centre * 62)/ 36)*-1
    x_out <- ((x_outer * 62)/ 36)*-1
    z_out <- ((z_outer * 62)/ 36)*-1
    
    x <- (x_m * 62)/ 36
    z <- (z_m * 62)/ 36
    
    plot(NA, NA, xlim = c(-11,11), ylim = c(-11, 11), 
         xlab = "", ylab = "", frame.plot = FALSE, #frame.plot takes away borders
         main = sprintf("Trial %s trajectory", trial), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
    #abline(v = c(30, 60, 90, 120, 150, 180, 210, 240, 270), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
    axis(1, at = c(-10, -5, 0, 5, 10)) #tick marks for x axis
    axis(2, at = c(-10, -5, 0, 5, 10), las=2) #tick marks for y axis
    
    lines(x_in, z_in, col='red')
    lines(x_cen, z_cen, col='green')
    lines(x_out, z_out, col='red')
    points(x,z, col='blue')
    
    # 0 degree track orientation - remove pit stop samples
    points(x_cen[c(1:141, 183:344)],z_cen[c(1:141, 183:344)], col='orange')
    points(x_in[c(1:141, 183:344)],z_in[c(1:141, 183:344)], col='orange')
    points(x_out[c(1:141, 183:344)],z_out[c(1:141, 183:344)], col='orange')
    
    x_cen_1 <- x_cen[c(1:141)]
    z_cen_1 <- z_cen[c(1:141)]
    x_in_1 <- x_in[c(1:141)]
    z_in_1 <- z_in[c(1:141)]
    x_out_1 <- x_out[c(1:141)]
    z_out_1 <- z_out[c(1:141)]
    
    x_cen_2 <- x_cen[c(183:344)]
    z_cen_2 <- z_cen[c(183:344)]
    x_in_2 <- x_in[c(183:344)]
    z_in_2 <- z_in[c(183:344)]
    x_out_2 <- x_out[c(183:344)]
    z_out_2 <- z_out[c(183:344)]
    
    #Centre
    ndat_1 <- data.frame()
    for (samp in c(2:length(x_cen_1))){ #start with second sample, since first will calculate a difference from origin
      sampx <- x_cen_1[samp] - x_cen_1[samp-1]
      sampz <- z_cen_1[samp] - z_cen_1[samp-1]
      absvec <- sqrt(((sampx)^2)+((sampz)^2))
      
      if (prod(dim(ndat_1)) == 0){
        ndat_1 <- absvec
      } else {
        ndat_1 <- rbind(ndat_1, absvec)
      }
      
    }
    PL_1<- sum(ndat_1[1:length(ndat_1)])
    
    ndat_2 <- data.frame()
    for (samp in c(2:length(x_cen_2))){ #start with second sample, since first will calculate a difference from origin
      sampx <- x_cen_2[samp] - x_cen_2[samp-1]
      sampz <- z_cen_2[samp] - z_cen_2[samp-1]
      absvec <- sqrt(((sampx)^2)+((sampz)^2))
      
      if (prod(dim(ndat_2)) == 0){
        ndat_2 <- absvec
      } else {
        ndat_2 <- rbind(ndat_2, absvec)
      }
      
    }
    PL_2<- sum(ndat_2[1:length(ndat_2)])
    PL_centre <- PL_1 + PL_2
    
    #Inner
    ndat_1 <- data.frame()
    for (samp in c(2:length(x_in_1))){ #start with second sample, since first will calculate a difference from origin
      sampx <- x_in_1[samp] - x_in_1[samp-1]
      sampz <- z_in_1[samp] - z_in_1[samp-1]
      absvec <- sqrt(((sampx)^2)+((sampz)^2))
      
      if (prod(dim(ndat_1)) == 0){
        ndat_1 <- absvec
      } else {
        ndat_1 <- rbind(ndat_1, absvec)
      }
      
    }
    PL_1<- sum(ndat_1[1:length(ndat_1)])
    
    ndat_2 <- data.frame()
    for (samp in c(2:length(x_in_2))){ #start with second sample, since first will calculate a difference from origin
      sampx <- x_in_2[samp] - x_in_2[samp-1]
      sampz <- z_in_2[samp] - z_in_2[samp-1]
      absvec <- sqrt(((sampx)^2)+((sampz)^2))
      
      if (prod(dim(ndat_2)) == 0){
        ndat_2 <- absvec
      } else {
        ndat_2 <- rbind(ndat_2, absvec)
      }
      
    }
    PL_2<- sum(ndat_2[1:length(ndat_2)])
    PL_inner <- PL_1 + PL_2
    
    #Outer
    ndat_1 <- data.frame()
    for (samp in c(2:length(x_out_1))){ #start with second sample, since first will calculate a difference from origin
      sampx <- x_out_1[samp] - x_out_1[samp-1]
      sampz <- z_out_1[samp] - z_out_1[samp-1]
      absvec <- sqrt(((sampx)^2)+((sampz)^2))
      
      if (prod(dim(ndat_1)) == 0){
        ndat_1 <- absvec
      } else {
        ndat_1 <- rbind(ndat_1, absvec)
      }
      
    }
    PL_1<- sum(ndat_1[1:length(ndat_1)])
    
    ndat_2 <- data.frame()
    for (samp in c(2:length(x_out_2))){ #start with second sample, since first will calculate a difference from origin
      sampx <- x_out_2[samp] - x_out_2[samp-1]
      sampz <- z_out_2[samp] - z_out_2[samp-1]
      absvec <- sqrt(((sampx)^2)+((sampz)^2))
      
      if (prod(dim(ndat_2)) == 0){
        ndat_2 <- absvec
      } else {
        ndat_2 <- rbind(ndat_2, absvec)
      }
      
    }
    PL_2<- sum(ndat_2[1:length(ndat_2)])
    PL_outer <- PL_1 + PL_2
    
    cat('Path length centre:\n')
    print(PL_centre)
    cat('Path length inner:\n')
    print(PL_inner)
    cat('Path length outer:\n')
    print(PL_outer)
    
  }
}

# PL centre = 49.68201
# PL inner = 42.06827
# PL outer = 57.29763
