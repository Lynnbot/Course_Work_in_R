setwd("~/Documents/2018FALL_Class/R/Applied Epid/E6")
library(tidyverse)

# Read in Framingham Data
rawdat <- read.csv(file = "frmgham2.csv")
basedat <- filter(rawdat, TIME == 0)
str(basedat)

#2.
smoke_m <- count(basedat[basedat$SEX == 1,], CURSMOKE)
smoke_f <- count(basedat[basedat$SEX == 2,], CURSMOKE)
smoke_f; smoke_m

#3.
#In the final result, more than half observations smoke in male 
#while less than half in female.
smoke.by.sex <- data.frame(smoke_f, smoke_m)
smoke.by.sex <- smoke.by.sex %>%
  select(2, 4) %>%
  t() 
colnames(smoke.by.sex) <- c("Non-smoker", "Smoker")
rownames(smoke.by.sex) <- c("Female", "Male")
smoke.by.sex <- smoke.by.sex[,c(2,1)]
smoke.by.sex

write.csv(smoke.by.sex, file = "smoke_by_sex.csv")

#4.
# Two usages of chisq.test() yeild the same result.
chisq.test(basedat$SEX, basedat$CURSMOKE)
chisq.test(basedat$CURSMOKE, basedat$SEX)

#5. prop.test()
# prop.test() can test the Null hypothesis that the proportions in several groups 
# are the equivalent or equal to a given value.
smoke.f.m <- smoke.by.sex[,1]
f.m.total <- rowSums(smoke.by.sex)
prop.test(smoke.f.m, f.m.total)

# Association btw age group and smoking
#6.
colnames(basedat)
basedat <- mutate(basedat, AGEGROUP = as.factor(floor(basedat$AGE/10)))
basedat$AGEGROUP[basedat$AGEGROUP == 7] <- 6
summary(basedat$AGEGROUP)

#7.
smoke.by.agegrp <- basedat %>%
    group_by(AGEGROUP, CURSMOKE) %>%
    count()

smoke.by.agegrp.ctg <- smoke.by.agegrp %>%
    spread(., CURSMOKE, n) %>%
    setNames(c("AGEGROUP", "NonSmoker", "Smoker")) %>%
    data.frame()

smoke.by.agegrp.ctg[,1] <- c("30-39","40-49", "50-59", "60+")
write.csv(smoke.by.agegrp.ctg, file = "smoke_by_agegroup.csv")

#8. perform a chi-squared test for trend in proportions of smoking with incresing age.
# Proportions of smoking decrease with increasing age.
# This result is statistically significant with the p-value far less than 0.05.
prop.trend.test(smoke.by.agegrp.ctg[,3], rowSums(smoke.by.agegrp.ctg[,2:3]))


#9. plot the trend in the  proportion of baseline current smokers with age
agegrp.sum <- rowSums(smoke.by.agegrp.ctg[,2:3])
agegrp.sum2 <- data.frame(no = rep(1:4,2), sum = rep(0,8)) %>%
  mutate(sum = rep(agegrp.sum,2)) %>%
  arrange(no)
smoke.by.agegrp.prop <- cbind(data.frame(smoke.by.agegrp), sum = agegrp.sum2[,2]) %>%
  mutate(prop = n/sum)
smoke.by.agegrp.prop[,1] <- c(rep("30-39",2), rep("40-49",2), 
                              rep("50-59",2), rep("60+",2))

ggplot(data = smoke.by.agegrp.prop, aes(x = AGEGROUP,
        y = prop, fill = as.factor(CURSMOKE))) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_brewer(palette="Greens") + 
    theme_minimal() +
    theme(plot.title = element_text(size = 11), legend.title = element_text(size = 8)) +
    ggtitle("Smoking proportion decline with increasing age (n=4434) (p<0.05)") +
    xlab("Age Group") + ylab("Proportion") +
    labs(fill = "Smoking Status\n 0 as non-smokers\n 1 as smokers")


#10.  
summary(basedat$TOTCHOL[basedat$SEX == 1])
mean(basedat$TOTCHOL[basedat$SEX == 1], na.rm = T)
mean(basedat$TOTCHOL[basedat$SEX == 2], na.rm = T)

#11. t test
# The average of cholesterol at baseline in man is lower than that in women.
# This result is statistically significant. (P < 0.05)
t.test(basedat$TOTCHOL[basedat$SEX == 1], basedat$TOTCHOL[basedat$SEX == 2])
t.test(basedat$TOTCHOL ~ as.factor(basedat$SEX))

#12. plot the result of t test
par(mar= c(4,5,4,1), cex = 0.8)
plot(log(basedat$TOTCHOL) ~ as.factor(basedat$SEX),
     main = "Serum total cholesterol in women is higher than men\n(n=4434) (p<0.05)",
     ylab = "Logarithm of Serum Total Cholesterol (mg/dL)", xlab = "Sex", xaxt = "n")
axis(1, at=c(1:2), labels = c("Male", "Female"))



