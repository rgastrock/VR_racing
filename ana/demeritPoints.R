source('ana/shared.R')

#Demerit Points per pp----
getParticipantPts <- function(group, id, session) {
  
  filepath <- sprintf('data/%s/%s/S%03d/trial_results.csv', group, id, session)
  df <- read.csv(filepath, stringsAsFactors = F)
  
  #setup relevant vectors
  trialno <- c()
  participant <- c()
  session <- c()
  group <- c()
  track_orientation <- c()
  dempts <- c()
  
  for (trial in c(1:dim(df)[1])) {
    trial_num <- df$trial_num[trial]
    pp <- df$ppid[trial]
    session_num <- df$session_num[trial]
    group_cond <- df$experiment[trial]
    orientation <- df$Track_orientation[trial]
    pts <- df$demerit_points[trial]
    
    trialno <- c(trialno, trial_num)
    participant <- c(participant, pp)
    session <- c(session, session_num)
    group <- c(group, group_cond)
    track_orientation <- c(track_orientation, orientation)
    dempts <- c(dempts, pts)    
    
  }
  
  ppdata <- data.frame(trialno, participant, session, group, track_orientation, dempts)
  return(ppdata)
  
}

getGroupPts <- function(group, session){
  
  # exlude participants due to experiment problems
  pp_exclude <- c('01', '02', '03', '04', '05', '20', '22', '25', '44', '46')
  pp_group <- unique(list.files(sprintf('data/%s', group)))
  pp_group <- pp_group[which(!pp_group %in% pp_exclude)]
  
  dataoutput <- data.frame()
  for(pp in pp_group){
    ppdat <- getParticipantPts(group = group, id = pp, session = session)
    trial <- ppdat$trialno
    dempts <- ppdat$dempts
    ppdat <- data.frame(trial, dempts)
    names(ppdat)[names(ppdat) == 'dempts'] <- pp
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, dempts)
      names(dataoutput)[names(dataoutput) == 'dempts'] <- pp
    }
    
  }
  return(dataoutput)
}

getS2GroupPts <- function(group, session = 2){
  
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
    ppdat <- getParticipantPts(group = group, id = pp, session = session)
    trial <- ppdat$trialno
    dempts <- ppdat$dempts
    ppdat <- data.frame(trial, dempts)
    names(ppdat)[names(ppdat) == 'dempts'] <- pp
    
    if (prod(dim(dataoutput)) == 0){
      dataoutput <- ppdat
    } else {
      dataoutput <- cbind(dataoutput, dempts)
      names(dataoutput)[names(dataoutput) == 'dempts'] <- pp
    }
    
  }
  return(dataoutput)
}

# Session 1: Combine track orientations----

getAllTrackGroupPts<- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 1, target = 0){
  
  alldata <- data.frame()
  
  for(group in groups){
    data <- getGroupPts(group = group, session = session)
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
    write.csv(ndat, file=sprintf('data/DemeritPoints_AllTrack_S%03d.csv', session), row.names = F) 
  } else {
    return(ndat)
  }
}

getAllTrackGroupPtsCI <- function(session = 1, type = 'b'){
  
  data <- getAllTrackGroupPts(session = session)
  trialno <- data$trial
  data1 <- as.matrix(data[,2:(dim(data)[2])])
  
  confidence <- data.frame()
  
  
  for (t in trialno){
    dempts <- data1[t, ]
    
    if (type == "t"){
      dempts <- dempts[!is.na(dempts)]
      citrial <- getConfidenceInterval(data = dempts, variance = var(dempts), method = type)
    } else if(type == "b"){
      citrial <- getConfidenceInterval(data = dempts, variance = var(dempts), method = type)
    }
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
  }
  write.csv(confidence, file=sprintf('data/DemeritPointsCI_AllTrack_S%03d.csv', session), row.names = F) 
  
}

plotAllTrackPts <- function(session = 1, target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig17_DemeritPoints_AllTrack.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  plot(NA, NA, xlim = c(0,301), ylim = c(0, 5), 
       xlab = "Trial", ylab = "Demerit Points", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Mean demerit points across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  #abline(v = c(30, 60, 90, 120, 150, 180, 210, 240, 270), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300)) #tick marks for x axis
  axis(2, at = c(0, 1, 2, 3, 4, 5), las=2) #tick marks for y axis
  
  
  #read in files created by CI function
  groupconfidence <- read.csv(file=sprintf('data/DemeritPointsCI_AllTrack_S%03d.csv', session))
  
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

# Session 2: Combine track orientations----

getS2AllTrackGroupPts<- function(groups = c('T-RACING_0', 'T-RACING_180', 'T-RACING_90', 'T-RACING_270'), session = 2, target = 0){
  
  alldata <- data.frame()
  
  for(group in groups){
    data <- getS2GroupPts(group = group, session = session)
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
    write.csv(ndat, file=sprintf('data/DemeritPoints_AllTrack_S%03d.csv', session), row.names = F) 
  } else {
    return(ndat)
  }
}

getS2AllTrackGroupPtsCI <- function(session = 2, type = 'b'){
  
  data <- getS2AllTrackGroupPts(session = session)
  trialno <- data$trial
  data1 <- as.matrix(data[,2:(dim(data)[2])])
  
  confidence <- data.frame()
  
  
  for (t in trialno){
    dempts <- data1[t, ]
    
    if (type == "t"){
      dempts <- dempts[!is.na(dempts)]
      citrial <- getConfidenceInterval(data = dempts, variance = var(dempts), method = type)
    } else if(type == "b"){
      citrial <- getConfidenceInterval(data = dempts, variance = var(dempts), method = type)
    }
    
    if (prod(dim(confidence)) == 0){
      confidence <- citrial
    } else {
      confidence <- rbind(confidence, citrial)
    }
  }
  write.csv(confidence, file=sprintf('data/DemeritPointsCI_AllTrack_S%03d.csv', session), row.names = F) 
  
}

plotS2AllTrackPts<- function(session = 2, blocks = c(1,2,3,4), target='inline') {
  
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/fig/Fig17A_DemeritPoints_AllTrack_Session2.svg', width=12, height=7, pointsize=14, system_fonts=list(sans="Arial"))
  }
  
  # create plot
  #NA to create empty plot
  plot(NA, NA, xlim = c(0,121), ylim = c(0, 5), 
       xlab = "Trial", ylab = "Demerit points", frame.plot = FALSE, #frame.plot takes away borders
       main = sprintf("Mean demerit points across trials: Session %s", session), xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  abline(v = c(30, 60, 90), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 30, 60, 90, 120)) #tick marks for x axis
  axis(2, at = c(0, 1, 2, 3, 4, 5), las=2) #tick marks for y axis
  
  
  #read in files created by CI function
  groupconfidence <- read.csv(file=sprintf('data/DemeritPointsCI_AllTrack_S%03d.csv', session))
  
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



# Statistics (Preprocess data) ----

getBlockedDemPts <- function(session, blockdefs) {
  
  dat <- read.csv(file=sprintf('data/DemeritPoints_AllTrack_S%03d.csv', session))
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

#Session 1: compare demerit points within session----
getDemPtsSession1 <- function(){
  blockdefs <- list('S1_first'=c(2,5), 'S1_last'=c(295,6))
  LC_part1 <- getBlockedDemPts(session = 1, blockdefs=blockdefs) 

  LC_part1$participant <- as.factor(LC_part1$participant)
  LC_part1$set <- factor(LC_part1$set, levels = c('S1_first','S1_last'))
  
  subdat1 <- LC_part1[which(LC_part1$set == 'S1_first'),]
  subdat2 <- LC_part1[which(LC_part1$set == 'S1_last'),]
  
  # cat('Session 1 Set 1 vs. Session 1 Set 50:\n')
  # print(t.test(subdat1$dv, subdat2$dv, paired = T))
  # cat('Effect Size - Cohen d:\n')
  # print(cohensD(subdat1$dv, subdat2$dv))
  cat('Mean for trial set 1:\n')
  print(mean(subdat1$dv))
  cat('SD for trial set 1:\n')
  print(sd(subdat1$dv))
  cat('Mean for trial set 50:\n')
  print(mean(subdat2$dv))
  cat('SD for trial set 50:\n')
  print(sd(subdat2$dv))
  
}

#Session 2: compare demerit points within session----
getDemPtsSession2 <- function(){
  blockdefs <- list('S2_1'=c(2,5), 'S2_2'=c(25,6), 'S2_3'=c(32,5), 'S2_4'=c(55,6), 'S2_5'=c(62,5), 'S2_6'=c(85,6), 'S2_7'=c(92,5), 'S2_8'=c(115,6))
  LC_part2 <- getBlockedDemPts(session = 2, blockdefs=blockdefs) 
  
  LC_part2$participant <- as.factor(LC_part2$participant)
  LC_part2$set <- factor(LC_part2$set, levels = c('S2_1','S2_2','S2_3','S2_4','S2_5','S2_6','S2_7','S2_8'))
  
  subdat1 <- LC_part2[which(LC_part2$set == 'S2_1'),]
  subdat2 <- LC_part2[which(LC_part2$set == 'S2_2'),]
  subdat3 <- LC_part2[which(LC_part2$set == 'S2_3'),]
  subdat4 <- LC_part2[which(LC_part2$set == 'S2_4'),]
  subdat5 <- LC_part2[which(LC_part2$set == 'S2_5'),]
  subdat6 <- LC_part2[which(LC_part2$set == 'S2_6'),]
  subdat7 <- LC_part2[which(LC_part2$set == 'S2_7'),]
  subdat8 <- LC_part2[which(LC_part2$set == 'S2_8'),]
  
  # cat('Session 1 Set 1 vs. Session 1 Set 50:\n')
  # print(t.test(subdat1$dv, subdat2$dv, paired = T))
  # cat('Effect Size - Cohen d:\n')
  # print(cohensD(subdat1$dv, subdat2$dv))
  cat('Mean for trial set 1:\n')
  print(mean(subdat1$dv))
  cat('SD for trial set 1:\n')
  print(sd(subdat1$dv))
  cat('Mean for trial set 2:\n')
  print(mean(subdat2$dv))
  cat('SD for trial set 2:\n')
  print(sd(subdat2$dv))
  cat('Mean for trial set 3:\n')
  print(mean(subdat3$dv))
  cat('SD for trial set 3:\n')
  print(sd(subdat3$dv))
  cat('Mean for trial set 4:\n')
  print(mean(subdat4$dv))
  cat('SD for trial set 4:\n')
  print(sd(subdat4$dv))
  cat('Mean for trial set 5:\n')
  print(mean(subdat5$dv))
  cat('SD for trial set 5:\n')
  print(sd(subdat5$dv))
  cat('Mean for trial set 6:\n')
  print(mean(subdat6$dv))
  cat('SD for trial set 6:\n')
  print(sd(subdat6$dv))
  cat('Mean for trial set 7:\n')
  print(mean(subdat7$dv))
  cat('SD for trial set 7:\n')
  print(sd(subdat7$dv))
  cat('Mean for trial set 8:\n')
  print(mean(subdat8$dv))
  cat('SD for trial set 8:\n')
  print(sd(subdat8$dv))
  
}