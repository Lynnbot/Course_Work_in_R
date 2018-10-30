#getwd()
setwd("~/Documents/2018FALL_Class/R/Applied Epid/E3")
library(tidyverse)



#1. Read in data. 
#Simplify and format the data into what we need.
rawdat <- read.csv("frmgham2.csv")


fram.age.interested <- filter(rawdat, PERIOD == 1 & PREVSTRK == 0)
dim(fram.age.interested) #4402 observations included

#simplify the data to what we need
fram.age <- select(fram.age.interested, SEX, AGE, PREVSTRK,
                   STROKE, TIMESTRK, TIMEDTH)
rm(fram.age.interested)



#2.
#compute the age of each observations at the end of their follow-up
AGE.STRK <- fram.age$AGE + fram.age$TIMESTRK/365.25
fram.age <- mutate(fram.age, AGE.STRK)
str(fram.age)



#3. Compute age group specific stroke incidence rates by sex

#Subset by sex
fram.age.mal <- filter(fram.age, SEX == 1)
fram.age.fem <- filter(fram.age, SEX == 2)


############
# Create an Incidence Rate computing function for male
IR.compute <- function(start_age, end_age, sex){
  fram.age.sex <- fram.age[fram.age$SEX == sex,]
  #Compute the numerator
  ir.nume <- 0
  for(i in 1:nrow(fram.age.sex)){  # "for" loop! In R, index starts in 1.
    if(fram.age.sex$AGE.STRK[i] < end_age + 1 & fram.age.sex$AGE.STRK[i] >= start_age ){
      ir.nume <- ir.nume + as.integer(fram.age.sex$STROKE[i])
    }
  }
  
  #Compute the denominator
  per.yr <- vector(length = nrow(fram.age.sex))
  
  for(i in 1:nrow(fram.age.sex)){
    per.yr[i] <- min(fram.age.sex$AGE.STRK[i], end_age + 1) - max(fram.age.sex$AGE[i], start_age)
    if(per.yr[i] < 0)  per.yr[i] <- 0
  }
  subj.contrb <- nrow(fram.age.sex) - length(per.yr[which (per.yr == 0)])
  ir.denom <- sum(per.yr)
  IR <- ir.nume/ir.denom
  #print(paste("The incidence rate is", round(IR*1000,2), "per 1000 person years."))
  print(paste(start_age, end_age, ir.nume, ir.denom, subj.contrb, IR*1000))
}
############


#Return the results of the variables needed in the table
start_age <- c(35,45,55,65,75,85)
end_age <- c(44,54,64,74,84,94)
for (i in 1:6){
  IR.compute(start_age[i], end_age[i], 1)
  IR.compute(start_age[i], end_age[i], 2)
}
