#setwd("~/Documents/2018FALL_Class/R/Applied Epid/E2/")
rawdat <- read.csv(file = "frmgham2.csv")
#install.packages("dplyr", "tidyr")
library("dplyr", "tidyr")
head(rawdat)

#3. Turn categorical data into "factor".
for (i in c(2, 7, 10, 11, 14, 15:19, 21, 24:31)){
 rawdat[,i] <- as.factor(rawdat[,i])
}
str(rawdat); rm(i)

#4. Reality check. Confirm that two results are the same.
reality_check_1 <- filter(rawdat, TIME == 0)
nrow(reality_check_1)
reality_check_2 <- reality_check_1[which (reality_check_1$PERIOD == 1), ]
nrow(reality_check_2)


#5. mean and range of # days since baseline for each period
# a, b and c repectively represent PERIOD 1, 2 and 3
a <- filter(rawdat, PERIOD == 1)
b <- filter(rawdat, PERIOD == 2)
c <- filter(rawdat, PERIOD == 3)

mean(a$TIME); range(a$TIME)
mean(b$TIME); range(b$TIME)
mean(c$TIME); range(c$TIME)

#7. Compute the prevalence
# The diabetes prevalence from PERIOD 1 to 3 is significantly increasing.
Pa <- as.numeric(count(a[which(a$DIABETES == 1),]) / count(a))
Pb <- as.numeric(count(b[which(b$DIABETES == 1),]) / count(b))
Pc <- as.numeric(count(c[which(c$DIABETES == 1),]) / count(c))
print(c(Pa, Pb, Pc))

#8. Female has a slightly higher prevalance of diabetes at baseline.
Pa_male <- as.numeric(count(a[which(a$DIABETES == 1 & a$SEX == 1),]) / count(a))
Pa_fema <- as.numeric(count(a[which(a$DIABETES == 1 & a$SEX == 2),]) / count(a))
print(c(Pa_male, Pa_fema))

#9. The high prevalence in female might be the result of relatively high average age in the female subgroup.
avg.age_a_male <- summarise(filter(a, SEX == 1), avg = mean(AGE))
#avg.age_a_male_d <- summarise(filter(a, DI  ABETES == 1 & SEX == 1), avg = mean(AGE))
avg.age_a_fema <- summarise(filter(a, SEX == 2), avg = mean(AGE))
#avg.age_a_fema_d <- summarise(filter(a, DIABETES == 1 & SEX == 2), avg = mean(AGE))
print(c(avg.age_a_male,  avg.age_a_fema))

#10. Build a table
# Learning resources:
#https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html
#https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html
colnames(a)  # a is the baseline data subseteed from the original raw data
variab <- c("AGE", "SYSBP", "DIABP", "BPMEDS", "CURSMOKE",
            "BMI", "DIABETES", "educ")

library(table1)
a$SEX <- factor(a$SEX, levels = c(1,2), labels = c("Men", "Women"))
a$BPMEDS <- factor(a$BPMEDS, levels = c(0,1), 
                   labels = c("Not currently used", "Current use"))
a$CURSMOKE <- factor(a$CURSMOKE, levels = c(0,1), 
                     labels = c("Not current smoker", "Current Use"))
a$DIABETES <- factor(a$DIABETES, levels = c(0,1), 
                     labels = c("Noa a diabetic", "Diabetic"))
a$educ <- factor(a$educ, levels = c(1,2,3,4),
                 labels = c("0-11 years", "High School Diploma, GED",
                            "Some College, Vocational School", "College (BS, BA) degree or more"))
str(a)

label(a$AGE) <- "Age"
label(a$SYSBP) <- "Systolic Blood Pressure"
label(a$DIABP) <- "Diastolic Blood Pressure"
label(a$BPMEDS) <- "Use of Anti-hypertensive medication at exam"
label(a$CURSMOKE) <- "Curren cigarette smoking at exam"
label(a$educ) <- "Attained Education"
label(a$BMI) <- "Body Mass Index*"
label(a$DIABETES) <- "Diabetic"

units(a$AGE) <- "years"
units(a$DIABP) <- "mmHg"
units(a$SYSBP) <- "mmHg"

table1(~ AGE + SYSBP+ DIABP+ BPMEDS+ CURSMOKE+BMI+ DIABETES+ educ
       | SEX, data = a, render.continuous = c(.="Mean (SD)"))

#11.
par(mar = c(4,4,3,2), mgp = c(2.5,1,0), cex = 0.75)
hist(a$SYSBP,
     main = "Distribution of Systolic Blood Pressure at Baseline\nwith untransformed data (n=4434)",
     xlab = "Systolic Blood Pressure (mmHg)",
     xlim = c(80, 300), ylim = c(0, 1800), labels = T, col = "bisque3")

#12. Distribution transformation by logarithm.
# 10 outliers were detected ased on the previous graph and delected.
par(mar = c(5,4,3,2), mgp = c(2.5,1,0), cex = 0.7)
sysbp12 <- sort(a$SYSBP, decreasing = T)
head(sysbp12)
hist(log(sysbp12[11:4424]),
     main = "Distribution of Systolic Blood Pressure at Baseline \n with logarithmic transformed data  (n=4424)",
     xlab = "Distribution of Systolic Blood Pressure",
     sub = "Noted: 10 outliers were removed for better fitting.",
     ylim = c(0, 630), labels = T, col = "azure3")


#13. The incidence rate of myocardial infarction is 4.4 cases per 1,000 person-years.
mi_cases <- filter(a, PREVMI == 0 & HOSPMI == 1)
numerator <- count(mi_cases)
deno <- filter(a, PREVMI == 0)
denominator <- sum(deno$TIMEMI)
ir <- numerator/(denominator / 365)
print(ir)



######### just some testing code ########
test.m <- matrix(rexp(60), nrow = 20, ncol = 3)
m.sum <- rowsum(test.m)
