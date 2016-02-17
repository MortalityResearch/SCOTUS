##########################################################
## Program: SCOTUS_SMR_plot.R                           ##
## Created by: Robert J. Reynolds, MPH PhD              ##
## Description: This program reads in a set of SMRs and ##
##   converts them into a plot                          ##
## Last modified: 15 February 2016                      ##
##########################################################

## Read in the SMRs
  smr.data.all <- read.table("smr.txt",header=TRUE,sep=" ")
  summary(smr.data.all)
smr.data <- smr.data.all[-23,]
summary(smr.data)
## Set graphical parameters (margins)
  par(mar = c(6, 4, 1, 1))

## Create the SMR plot backbone
  plot(seq(1,22,1),smr.data$smr,pch=22,col="black",bg="black",xaxt="n",yaxt="n",
       ylim=c(0,4),xlab="",ylab="",cex=1.5)

## Add axes and tick-mark labels
  axis(1,at=1:22,labels=FALSE)
  axis(2,at=seq(0,4,0.5), labels=FALSE)
  text(1:22,par("usr")[3]-0.25,srt=45,adj=1,labels=smr.data$period,xpd=TRUE,cex=0.9)
  axis(2,las=1,at=c(0,0.5,1,1.5,2,2.5,3,3.5,4),labels=c("0.0","0.5","1.0","1.5","2.0","2.5","3.0","3.5","4.0"))

## Add Axis labels
  mtext(1,text="Period",line = 5,cex=1.25)
  mtext(2,text="Standardized Mortality Ratio (SMR)",line=3,cex=1.25)

## Add the NULL line
  abline(h=1,lty="longdash")

## Put in all the confidence bands 
for (i in 1:22) {
  segments(i,smr.data$lci[i],i,smr.data$uci[i],col="black")
}

######################################################################################
## This section is for the extended study which includes the death of Justice Scalia
## Adds a 23rd point and confidence band in red
######################################################################################

## Create the SMR plot backbone
  plot(seq(1,23,1),smr.data.all$smr,pch=22,col="black",bg="black",xaxt="n",yaxt="n",
       ylim=c(0,4),xlab="",ylab="",cex=1.5)
  points(23,smr.data.all$smr[23],pch=22,col="red",bg="red",cex=1.5)

## Add axes and tick-mark labels
  axis(1,at=1:23,labels=FALSE)
  axis(2,at=seq(0,4,0.5), labels=FALSE)
  text(1:22,par("usr")[3]-0.25,srt=45,adj=1,labels=smr.data.all$period[1:22],xpd=TRUE,cex=0.9)
  text(23,par("usr")[3]-0.25,srt=45,adj=1,labels=smr.data.all$period[23],xpd=TRUE,cex=0.9,col="red")
  axis(2,las=1,at=c(0,0.5,1,1.5,2,2.5,3,3.5,4),labels=c("0.0","0.5","1.0","1.5","2.0","2.5","3.0","3.5","4.0"))

## Add Axis labels
  mtext(1,text="Period",line = 5,cex=1.25)
  mtext(2,text="Standardized Mortality Ratio (SMR)",line=3,cex=1.25)

## Add the NULL line
  abline(h=1,lty="longdash")

## Put in all the confidence bands
  for (i in 1:22) {
    segments(i,smr.data.all$lci[i],i,smr.data.all$uci[i],col="black")
  }
  segments(23,smr.data.all$lci[23],23,smr.data.all$uci[23],col="red") 
  
  
  
  
  