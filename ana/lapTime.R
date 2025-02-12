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
       main = "Lap time across trials", xaxt = 'n', yaxt = 'n') #xaxt and yaxt to allow to specify tick marks
  #abline(v = c(30, 60, 90, 120, 150, 180, 210, 240, 270), col = 8, lty = 2) #creates horizontal dashed lines through y =  0 and 30
  axis(1, at = c(1, 25, 50, 75, 100, 150, 200, 250, 300)) #tick marks for x axis
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





