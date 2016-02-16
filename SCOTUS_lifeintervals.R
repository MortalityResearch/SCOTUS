##########################################################
## Program: SCOTUS_lifeintervals.R                      ##
## Created by: Robert J. Reynolds, MPH PhD              ##
## Description: This program converts cohort data to    ##
##   person-time life intervals                         ##
## Date created: 15 February 2016                       ##
##########################################################

## Set up a matrix with the right number of rows
  rows <- as.numeric(sum(scotus$deathyear-scotus$debutyear+1)*3+3*nrow(scotus))
  scotus2 <- matrix(0,rows,5)
  colnames(scotus2) <- c('appt','month','day','year','first')
  rownum <- 0 

## Create rows for appointment to the Court for each Justice
  for (i in 1:nrow(scotus)) { 
    rownum <- rownum+1
    scotus2[rownum,1] <- scotus$appt[i]
    scotus2[rownum,2] <- scotus$debutmonth[i]
    scotus2[rownum,3] <- scotus$debutday[i]
    scotus2[rownum,4] <- scotus$debutyear[i]
    scotus2[rownum,5] <- 1

## Create rows for retirement events
    if ((scotus$retired[i]==1)*(scotus$retiredate[i]!=scotus$debutdate[i])==1) {
      rownum <- rownum+1
      scotus2[rownum,1] <- scotus$appt[i]
      scotus2[rownum,2] <- scotus$retiremonth[i]
      scotus2[rownum,3] <- scotus$retireday[i]
      scotus2[rownum,4] <- scotus$retireyear[i]
    }
 
    if (scotus$debutdate[i]!=scotus$deathdate[i]) {
      rownum <- rownum+1
      scotus2[rownum,1] <- scotus$appt[i]
      scotus2[rownum,2] <- scotus$deathmonth[i]
      scotus2[rownum,3] <- scotus$deathday[i]
      scotus2[rownum,4] <- scotus$deathyear[i]
    }

## Create all the rows between appointment and an ending event
    for (j in scotus$debutyear[i]:scotus$deathyear[i]) { # loop over each year
      rownum <- rownum+1
      scotus2[rownum,1] <- scotus$appt[i]
      scotus2[rownum,2] <- 1
      scotus2[rownum,3] <- 1
      scotus2[rownum,4] <- j

      rownum <- rownum+1
      scotus2[rownum,1] <- scotus$appt[i]
      scotus2[rownum,2] <- scotus$birthmonth[i]
      scotus2[rownum,3] <- scotus$birthday[i]
      scotus2[rownum,4] <- j
    }
  }  

## Transform matrix into dataframe
  scotus2 <- as.data.frame(scotus2)
  scotus2 <- scotus2[scotus2$appt!=0,]


## Turn the separate m/d/y fields into a real date
  scotus2$start <- ISOdatetime(scotus2$year,scotus2$month,scotus2$day,0,0,0,tz="UTC")

## Sort the data and keep only the appt, start date, "first" indicator
  scotus2 <- scotus2[with(scotus2,order(appt,start)),c(1,5,6)]

## Define the stop date as the next row's start date
  scotus2$stop  <- ISOdatetime(0,0,0,0,0,0,tz="UTC")
  scotus2$stop[1:I(nrow(scotus2)-1)] <- scotus2$start[2:nrow(scotus2)]

## Compute st on each interval
  scotus2$st <- as.numeric(difftime(scotus2$stop,scotus2$start,tz="UTC",units="days"))/365

## Eliminate the last record for each appt
  scotus2$keep <- 1
  scotus2$keep[scotus2[1:I(nrow(scotus2)-1),1]!=scotus2[2:nrow(scotus2),1]] <- 0
  scotus2$keep[nrow(scotus2)] <- 0
  scotus2 <- scotus2[scotus2$keep==1,]

## Create file with fixed variables
  static <- scotus[,c(1,4,10,23:29,9)]

## Merge intervals and fixed variables
  scotus3 <- merge(scotus2,static,by="appt")

## Compute the age on each interval
  scotus3$age <- floor(as.numeric(difftime(scotus3$start,scotus3$birthdate,tz="UTC"))/365.25)
  scotus3$age5 <- floor((scotus3$age/5))*5

## Time variables
  scotus3$year   <- year(scotus3$start)
  scotus3$decade <- (floor((year(scotus3$start)-1700)/10)*10)+1700

## Mark intervals as active or retired
  scotus3$active <- 0
  scotus3$active[scotus3$start<scotus3$retiredate] <- 1

## Compute the cumulative career length on each interval
  scotus3$careerlength <- (as.numeric(difftime(scotus3$start,scotus3$debutdate,tz="UTC",units="days"))/365.25)*scotus3$active
  scotus3$careerlength <- scotus3$careerlength+(scotus3$tenure*(1-scotus3$active))

## Get rid of out-of-range records
  scotus3 <- scotus3[scotus3$st>0 & 
             scotus3$start>=scotus3$debutdate & 
             scotus3$stop<=scotus3$deathdate,]

## Mark the last records and assign the deaths
  scotus3$last  <- 0
  scotus3$last[scotus3[1:I(nrow(scotus3)-1),1]!=scotus3[2:nrow(scotus3),1]] <- 1
  scotus3$last[nrow(scotus3)] <- 1
  scotus3$delta <- scotus3$last*scotus3$died

## Check to be sure the total person-years and deaths matches the original cohort dataset
length(unique(scotus3$appt))
sum(scotus3$st)fx
