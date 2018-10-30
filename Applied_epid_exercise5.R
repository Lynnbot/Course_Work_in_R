#Part 1
toy1<-data.frame(letters1=c(rep("A", 4), rep("B", 4), rep("C", 4)), numbers1=c(1:6, 1:6))
aggregate(toy1$numbers1, by=list(toy1$letters1), sum)
aggregate(cbind(numbers1.group=toy1$numbers1),
          by=list(letters1=toy1$letters1), sum)

toy2<-data.frame(letters1=c(rep("A", 4), rep("B", 4), rep("C", 4)), letters2=rep(c("X", "X", "Y", "Y"), 3), numbers1=c(1:6, 1:6), numbers2=1:12)
aggregate(cbind(number1.group=toy2$numbers1, number2.group=toy2$numbers2),
          by = list(letters1=toy2$letters1, letters2=toy2$letters2), sum)

toy3<-data.frame(letters1=c("A", "B", "C", "D"),
                 numbers3=c(1000, 2000, 3000, 4000))
 merge(toy1, toy3)
 
 
#################
#Part 2
#Set up working directory and read in data
setwd("~/Documents/2018FALL_Class/R/Applied Epid/E5/data")
library(tidyverse)
pennlc.raw <- read.csv(file = "pennlc.csv", header = T)
str(pennlc.raw)

#8.
group.by.age.sex <- aggregate(cbind(population.sum = pennlc.raw$population, case.sum = pennlc.raw$cases),
          by = list(sex = pennlc.raw$sex, age = pennlc.raw$age), sum)
group.by.age.sex <- mutate(group.by.age.sex, cIR = (case.sum/population.sum))



#9.
pennlc <- pennlc.raw
pennlc <- pennlc.raw %>% 
  merge(group.by.age.sex, by = c("sex","age")) %>% 
  mutate("cIR/10000" = cIR * 1e5, expected.cases = population * cIR) %>% 
  select(county, sex, age, population, cases, expected.cases, cIR, "cIR/10000")

exp.total.cases <- aggregate(cbind(expected.cases = pennlc$expected.cases,
                                   observed.cases = pennlc$cases, 
                                   population = pennlc$population),
                             by = list(county = pennlc$county), sum)
exp.total.cases <- exp.total.cases %>%
  mutate("smr_percent" = observed.cases * 100/expected.cases,
         cIR = observed.cases/population, "cIR/100000" = cIR * 1e5)

write.csv(exp.total.cases, file = "pennlc_smr.csv")


###################
# Part III
#13.
#install.packages("maptools")
#install.packages("spdep")
#install.packages("rgdal")
library(maptools);
library(spdep);
#library(rgdal)

rss <- readShapeSpatial("PA_Counties_clip.shp")
#myShapeInr <- readOGR(".", "PA_Counties_clip")
LCinPennlc <- merge(rss, exp.total.cases, by.x = "NAME", by.y = "county")
# Plot the map
spplot(LCinPennlc, "smr_percent")
spplot(LCinPennlc, "cIR/100000")
