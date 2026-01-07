source('ana/shared.R')
source('ana/lapTime.R')
source('ana/Run2_lapTime.R')
source('ana/percentOnTrack.R')
source('ana/Run2_percentOnTrack.R')
source('ana/pathLength.R')
source('ana/Run2_pathLength.R')
source('ana/SpeedAccuracy.R')
source('ana/Run2_SpeedAccuracy.R')


plotSessionsLapTimes <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/manuscript/Fig1_LapTimes.svg', width=18, height=5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2,3,4), 1, 4, byrow = TRUE), widths=c(2,2,2,2), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotS1FirstLastLapTime()
  # mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotS2FirstLastLapTime()
  # mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel C: Run 2 - Session 1
  plotR2S1FirstLastLapTime()
  # mtext('c', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel D: Run 2 - Session 2
  plotR2S2FirstLastLapTime()
  # mtext('d', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIndSessionsLapTimes <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/manuscript/Fig1B_IndividualLapTimes.svg', width=18, height=5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2,3,4), 1, 4, byrow = TRUE), widths=c(2,2,2,2), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotS1IndFirstLastLapTime()
  # mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotS2IndFirstLastLapTime()
  # mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel C: Run 2 - Session 1
  plotR2S1IndFirstLastLapTime()
  # mtext('c', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel D: Run 2 - Session 2
  plotR2S2IndFirstLastLapTime()
  # mtext('d', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotSessionsAccuracy <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/manuscript/Fig2_Accuracy.svg', width=18, height=5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2,3,4), 1, 4, byrow = TRUE), widths=c(2,2,2,2), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotS1FirstLastAccuracy()
  # mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotS2FirstLastAccuracy()
  # mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel C: Run 2 - Session 1
  plotR2S1FirstLastAccuracy()
  # mtext('c', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel D: Run 2 - Session 2
  plotR2S2FirstLastAccuracy()
  # mtext('d', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIndSessionsAccuracy <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/manuscript/Fig2B_IndividualAccuracy.svg', width=18, height=5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2,3,4), 1, 4, byrow = TRUE), widths=c(2,2,2,2), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotS1IndFirstLastAccuracy()
  # mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotS2IndFirstLastAccuracy()
  # mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel C: Run 2 - Session 1
  plotR2S1IndFirstLastAccuracy()
  # mtext('c', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel D: Run 2 - Session 2
  plotR2S2IndFirstLastAccuracy()
  # mtext('d', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotSessionsPL <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/manuscript/Fig3_PathLength.svg', width=18, height=5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2,3,4), 1, 4, byrow = TRUE), widths=c(2,2,2,2), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotS1FirstLastPL()
  # mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotS2FirstLastPL()
  # mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel C: Run 2 - Session 1
  plotR2S1FirstLastPL()
  # mtext('c', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel D: Run 2 - Session 2
  plotR2S2FirstLastPL()
  # mtext('d', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIndSessionsPL <- function(target='inline'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/manuscript/Fig3B_IndividualPathLength.svg', width=18, height=5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2,3,4), 1, 4, byrow = TRUE), widths=c(2,2,2,2), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotS1IndFirstLastPL()
  # mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotS2IndFirstLastPL()
  # mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel C: Run 2 - Session 1
  plotR2S1IndFirstLastPL()
  # mtext('c', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel D: Run 2 - Session 2
  plotR2S2IndFirstLastPL()
  # mtext('d', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotSessionsPLInside <- function(target='inline', trackloc = 'in'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/manuscript/Fig4_PathLength_InsideTrack.svg', width=18, height=5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2,3,4), 1, 4, byrow = TRUE), widths=c(2,2,2,2), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotS1InOutFirstLastPL(trackloc = trackloc)
  # mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotS2InOutFirstLastPL(trackloc = trackloc)
  # mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel C: Run 2 - Session 1
  plotR2S1InOutFirstLastPL(trackloc = trackloc)
  # mtext('c', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel D: Run 2 - Session 2
  plotR2S2InOutFirstLastPL(trackloc = trackloc)
  # mtext('d', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotIndSessionsPLInside <- function(target='inline', trackloc = 'in'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/manuscript/Fig4B_IndividualPathLength_InsideTrack.svg', width=18, height=5, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2,3,4), 1, 4, byrow = TRUE), widths=c(2,2,2,2), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Session 1
  plotS1IndInOutFirstLastPL(trackloc = trackloc)
  # mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Session 2
  plotS2IndInOutFirstLastPL(trackloc = trackloc)
  # mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel C: Run 2 - Session 1
  plotR2S1IndInOutFirstLastPL(trackloc = trackloc)
  # mtext('c', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel D: Run 2 - Session 2
  plotR2S2IndInOutFirstLastPL(trackloc = trackloc)
  # mtext('d', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  
  
  
  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}

plotSessionsSAF <- function(target='inline', trackloc = 'in'){
  
  #but we can save plot as svg file
  if (target=='svg') {
    svglite(file='doc/manuscript/Fig5_SpeedAccFuncs.svg', width=12, height=6, pointsize=16, system_fonts=list(sans="Arial"))
  }
  
  #par(mfrow=c(1,2), mar=c(4,4,2,0.1))
  par(mar=c(4,4,2,0.1))
  
  #layout(matrix(c(1,2,3), nrow=1, ncol=3, byrow = TRUE), widths=c(2,2,2), heights=c(1,1))
  layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(2,2), heights=c(1))
  
  # # # # # # # # # #
  # panel A: Study 1
  plotSAF()
  # mtext('a', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  
  # # # # # # # # # #
  # panel B: Study 2
  plotR2SAF()
  # mtext('b', side=3, outer=FALSE, line=-1, adj=0, padj=1, font=2)
  

  #close everything if you saved plot as svg
  if (target=='svg') {
    dev.off()
  }
  
}


