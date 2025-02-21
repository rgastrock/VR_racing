source('ana/shared.R')

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

# Unlist samples in array----

convertCellToNumVector <- function(v) {
  
  # # remove opening bracket:
  # v <- gsub('"', replacement='', x=v)
  # # remove closing bracket:
  # v <- gsub('"', replacement='', x=v)
  
  # split by underscores:
  v <- strsplit(v, '_')
  # convert to numeric:
  v <- lapply(v, FUN=as.numeric)
  # make vector:
  v <- as.vector(unlist(v))
  
  return(v)
  
}

# Path Length----
getParticipantTrajectory <- function(group, id, session) {
  
  filepath <- sprintf('data/%s/%s/S%03d/trial_results.csv', group, id, session)
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

#Session 1----
getGroupPL <- function(group, session){
  
  # exlude participants due to experiment problems
  pp_exclude <- c('01', '02', '03', '04', '05', '20', '22', '25', '44', '46')
  pp_group <- unique(list.files(sprintf('data/%s', group)))
  pp_group <- pp_group[which(!pp_group %in% pp_exclude)]
  
  dataoutput <- data.frame()
  for(pp in pp_group){
    ppdat <- getParticipantTrajectory(group = group, id = pp, session = session)
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

getGroupPLCI <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 1, type = 'b'){
  
  for(group in groups){
    data <- getGroupPL(group = group, session = session)
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
    write.csv(confidence, file=sprintf('data/PathLengthCI_%s_S%03d.csv', group, session), row.names = F) 
  }
  
}

plotPathLength <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 1, target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig3_PathLength.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
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
    groupconfidence <- read.csv(file=sprintf('data/PathLengthCI_%s_S%03d.csv', group, session))
    
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

#Session 2----

getS2GroupPL <- function(group, session = 2){
  
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
    ppdat <- getParticipantTrajectory(group = group, id = pp, session = session)
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

getS2GroupPLCI <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 2, type = 'b'){
  
  for(group in groups){
    data <- getS2GroupPL(group = group, session = session)
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
    write.csv(confidence, file=sprintf('data/PathLengthCI_%s_S%03d.csv', group, session), row.names = F) 
  }
  
}

plotS2PathLength <- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 2, target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig3A_PathLength_Session2.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
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
    groupconfidence <- read.csv(file=sprintf('data/PathLengthCI_%s_S%03d.csv', group, session))
    
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

# CHECK: plot individual data----
plotIndividualS2PL <- function(group, session = 2){
  
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
    ppdat <- getParticipantTrajectory(group = group, id = pp, session = session)
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

plotParticipantTrajectory <- function(group, id, session=2) {
  
  filepath <- sprintf('data/%s/%s/S%03d/trial_results.csv', group, id, session)
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

plotAcrossSessionPathLength <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig3B_PathLength_AllSessions.svg', width=16, height=6, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(3,3), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotPathLength()
  mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotS2PathLength()
  mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}



