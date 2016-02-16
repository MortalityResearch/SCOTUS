##########################################################
## Program: SCOTUS_lifeintervals.R                      ##
## Created by: Robert J. Reynolds, MPH PhD              ##
## Description: This program converts cohort data to    ##
##   person-time life intervals                         ##
## Date created: 15 February 2016                       ##
##########################################################

require(lubridate)

## Set the end of the study
endday   <-   31
endmonth <-   12
endyear  <- 2013

## Note: change to 13/2/2016 to include death of Justice Scalia

## Read in the raw data and compute needed variables
  scotus <- read.csv("SCOTUS.csv",header=T)
    scotus$DOB    <- as.character(scotus$DOB)
    scotus$DOD    <- as.character(scotus$DOD)
    scotus$debut  <- as.character(scotus$debut)
    scotus$retire <- as.character(scotus$retire)

  #Extract birth date components to numeric vectors
    scotus$birthmonth <- as.numeric(substr(scotus$DOB,1,2))
    scotus$birthday   <- as.numeric(substr(scotus$DOB,4,5))
    scotus$birthyear  <- as.numeric(substr(scotus$DOB,7,10))

  #Transform birth date components based on missing values
    scotus$birthday[scotus$birthmonth==99]   <- 1
    scotus$birthmonth[scotus$birthmonth==99] <- 7
    scotus$birthday[scotus$birthday==99]     <- 15
    scotus$birthday[scotus$birthmonth==2 & scotus$birthday==29] <- 28

  #Extract appointment date components to numeric vectors
    scotus$debutmonth <- as.numeric(substr(scotus$debut,1,2))
    scotus$debutday   <- as.numeric(substr(scotus$debut,4,5))
    scotus$debutyear  <- as.numeric(substr(scotus$debut,7,10))

  #Transform birth date components based on missing values
    scotus <- scotus[scotus$debutyear!=9999,]
    scotus$debutday[scotus$debutmonth==99]   <- 15
    scotus$debutmonth[scotus$debutmonth==99] <- 12
    scotus$debutday[scotus$debutday==99]     <- 15
    scotus$debutday[scotus$debutmonth==2 & scotus$debutday==29] <- 28

  #Extract death date components to numeric vectors
    scotus$deathmonth <- as.numeric(substr(scotus$DOD,1,2))
    scotus$deathday   <- as.numeric(substr(scotus$DOD,4,5))
    scotus$deathyear  <- as.numeric(substr(scotus$DOD,7,10))

  #Transform death date components based on missing values
    scotus <- scotus[scotus$deathyear!=9999 | scotus$died==0,]
    scotus$deathmonth[scotus$deathyear==9999] <- endmonth
    scotus$deathday[scotus$deathyear==9999]   <- endday
    scotus$deathyear[scotus$deathyear==9999]  <- endyear
    scotus$deathday[scotus$deathmonth==99]    <- 1
    scotus$deathmonth[scotus$deathmonth==99]  <- 7
    scotus$deathday[scotus$deathday==99]      <- 15


  #Extract retirement date components to numeric vectors
    scotus$retiremonth <- as.numeric(substr(scotus$retire,1,2))
    scotus$retireday   <- as.numeric(substr(scotus$retire,4,5))
    scotus$retireyear  <- as.numeric(substr(scotus$retire,7,10))
    scotus$retiremonth[scotus$retireyear==9999] <- endmonth
    scotus$retireday[scotus$retireyear==9999]   <- endday
    scotus$retireyear[scotus$retireyear==9999]  <- endyear
    scotus$retireday[scotus$retiremonth==99]    <- 1
    scotus$retiremonth[scotus$retiremonth==99]  <- 7
    scotus$retireday[scotus$retireday==99]      <- 15

  #Make real dates out of the numeric vector components
    scotus$birthdate  <- ISOdatetime(scotus$birthyear,scotus$birthmonth,scotus$birthday,0,0,0,tz="UTC")
    scotus$debutdate  <- ISOdatetime(scotus$debutyear,scotus$debutmonth,scotus$debutday,0,0,0,tz="UTC")
    scotus$retiredate <- ISOdatetime(scotus$retireyear,scotus$retiremonth,scotus$retireday,0,0,0,tz="UTC")
    scotus$deathdate  <- ISOdatetime(scotus$deathyear,scotus$deathmonth,scotus$deathday,0,0,0,tz="UTC")

  #Compute retirement status relative to end of study period
    scotus$retired <- 1
    scotus$retired[scotus$retiredate>ISOdatetime(endyear-2,endmonth,endday,0,0,0,tz="UTC")] <- 0
    scotus$retiredate[scotus$retiredate>ISOdatetime(endyear,endmonth,endday,0,0,0,tz="UTC")] <- ISOdatetime(endyear,endmonth,endday,0,0,0)

  #Compute vital status relative to end of study period
    scotus$died[scotus$deathdate>ISOdatetime(endyear,endmonth,endday,0,0,0,tz="UTC")] <- 0
    scotus$deathdate[scotus$deathyear==9999] <- ISOdatetime(endyear,endmonth,endday,0,0,0,tz="UTC")

  #Compute interval lengths
    scotus$fut       <- round(as.numeric(difftime(scotus$deathdate, scotus$debutdate,tz="UTC",units="days"))/365,4)
    scotus$tenure    <- round(as.numeric(difftime(scotus$retiredate,scotus$debutdate,tz="UTC",units="days"))/365,4)

  #Compute ages at various events
    scotus$deathage  <- round(as.numeric(difftime(scotus$deathdate, scotus$birthdate,tz="UTC",units="days"))/365,4)
    scotus$debutage  <- round(as.numeric(difftime(scotus$debutdate, scotus$birthdate,tz="UTC",units="days"))/365,4)
    scotus$retireage <- round(as.numeric(difftime(scotus$retiredate, scotus$birthdate,tz="UTC",units="days"))/365,4)

## Finalize dataset to have only records with known years for major events and 
## Justices who are appointed before the end of the study period
  scotus <- scotus[scotus$birthyear!=9999 & 
            scotus$deathyear!=9999 & 
            scotus$debutyear!=9999 &
            scotus$debutdate<=ISOdatetime(endyear,endmonth,endday,0,0,0,tz="UTC"),]

## Check the data
  nrow(scotus)
  sum(scotus$fut)

  mean(scotus$debutage)
  sd(scotus$debutage)

  mean(scotus$retireage)
  sd(scotus$retireage)

  mean(scotus$deathage[scotus$died==0])
  sd(scotus$deathage[scotus$died==0])
