#Learn more about ploting survival curve with ggsurvplot()
#http://www.sthda.com/english/rpkgs/survminer/
setwd("~/Documents/2018FALL_Class/R/Applied Epid/E4")

raw.dat <- read.csv(file = "dig.csv", header = T)
str(raw.dat)

#install the "survival" packages
#install.packages("survival")
#install.packages("ggfortify")
install.packages("survminer")
library(survival)
library(ggfortify)

#3. Outside of the treatment in question
# Possible factors: age, sex, race, BMI, heart condition at baseline (sings/symptoms of CHF) etc.
library(tidyverse)
plb.subjects <- filter(raw.dat, TRTMT == 0)
dig.subjects <- filter(raw.dat, TRTMT == 1)
summarise(plb.subjects, AGE_avg = mean(AGE), AGE_sd = sd(AGE))
summarise(dig.subjects, AGE_avg = mean(AGE), AGE_sd = sd(AGE))

#I. Worsening Heart Failure
#I.4. Counting the exact number and percnetage of the hospitalization due to worsening heart failure
HrtFl.plb <- count(plb.subjects, WHF == 1)
HrtFl.plb #1180/3403
HrtFl.per.plb <- 1180/3403
HrtFl.dig <- count(dig.subjects, WHF == 1)
HrtFl.dig #910/3397
HrtFl.per.dig <- 910/3397

#I.5. create a new column using Surv()
dat <- raw.dat
dat$SURV <- Surv(dat$WHFDAYS, dat$WHF)
# To understand the meaning of "SURV", for instance,
# 123 means days from recruiting to death,
# while 123+ means days from recruting to lost to follow-up

#I.6. Creating data frame with the data of survival
survl.WHF <- data.frame(SURV = dat$SURV, WHF = dat$WHF, WHFDAYS = dat$WHFDAYS)
str(survl.WHF)

#I.7. survfit()
KaplanM_WHF <- survfit(dat$SURV ~ dat$TRTMT)

#I.8. summarise the output of survfit()
summary(KaplanM_WHF)
str(KaplanM_WHF)

#I.9. From the result of Problem I.8. 
#In the digonxin group, 3397 participants are at risk at baseline (time = 1),
#8 events occurred at time = 1. At time = 2, 3386 participants are at risk.
#Those who were not at risk at time = 2 but did not have an event at time = 1
#were censored. In other word, they left the study or passed away due to reasons
#other than WHF.

#I.10. Plot Kaplan-Meier curves.
library(survminer)
par(mar = c(3,3,5,1))
ggsurvplot(KaplanM_WHF, data = dat, censor = F, conf.int = T, pval = T, legend.labs = c("Placebo", "Digoxin"),
           xlab = "Time in Months", axes.offset = F, break.x.by = 365.25, xscale = "d_m", surv.scale = "percent",
           ylab = "Proportion not hospitalized due to WHF",
           title = "Time-to-event Curves of Worsening Heart Failure(WHF)\nin Placebo and Digonxin Treatment Group",
           font.main = c(14, "bold"))


# II. Reapeated Step 4-10 for Deaths due to worsening heart failure
# II.4 Counting the exact number and percnetage of the death due to worsening heart failure
Death_HF.plb <- count(plb.subjects, REASON == 1)
Death_HF.plb #449/(449+745)
Death_HF.per.plb <- 449/(449+745)
Death_HF.dig <- count(dig.subjects, REASON == 1)
Death_HF.dig #910/3397
Death_HF.per.dig <- 394/(394+787)


#II.5. create a new column using Surv()
dat$SURV_Death <- Surv(dat$DEATHDAY, dat$DEATH)

#I.6. Creating data frame with the data of survival
survl.DEA <- data.frame(SURV = dat$SURV_Death, DEATH = dat$DEATH, REASON = dat$REASON, DEATHDAYS = dat$DEATHDAY)
str(survl.DEA)

#II.7. survfit()
KaplanM_DEA <- survfit(dat$SURV_Death ~ dat$TRTMT)

#II.8. summarise the output of survfit()
summary(KaplanM_DEA)
str(KaplanM_DEA)

#II.10. Plot Kaplan-Meier curves.
ggsurvplot(KaplanM_DEA, data = dat, censor = F, conf.int = F, pval = T, legend.labs = c("Placebo", "Digoxin"),
           xlab = "Time in Months", axes.offset = F, break.x.by = 365.25, xscale = "d_m", surv.scale = "percent",
           title = "Kaplan-Meier Curve of Death from Worsening Heart Failure\nin Placebo and Digonxin Treatment Group",
           font.main = c(11, "bold"))


