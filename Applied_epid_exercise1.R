#3. Create a vector of forearm lengths of the class
fl <- c(18.0, 27.5, 26.0, 24.5, 28.0, 26.0, 26.1, 28.0, 27.5, 29.0, 26.5, 26.0, 25.8, 28.5)
#4. Create a vector of hand lengths of the class
hl <- c(17.5, 18.5, 19.5, 14.5, 21.5, 19.7, 18.6, 20.0, 20.0 ,22.0, 17.0, 18.5, 18.4, 20.5)

#5. Combine  fl and hl into a data frame and display its structure
dat <- data.frame(cbind(fl, hl))
str(dat)

#6.
names(dat) #Return: "fl" "hl"
names(dat) <- c("Forearm_Lengths", "Hand_Lengths")

#7. Add data
dat <- rbind(dat, c(25.5, 20.5), c(25.5, 20), 
              c(25, 18.5), c(18, 12), c(11.5, 9))

#8. Differentiate adults and children by adding variables. 1 as adult, 2 as children.
age <- c(rep(1, 17), 2, 2)
dat <- cbind(dat, age)

#9. Numeric/Integer best represents the adult/children variable.
str(dat)
class(dat[,3]) #Return: "numeric"
summary(age)

age.fac <- as.factor(dat[,3])
summary(age.fac)

age.int <- as.integer(dat[,3])
summary(age.int)

age.cha <- as.character(dat[,3])
summary(age.cha)

rm(age.cha, age.fac, age.int)

#10. Create a variable that indicates the forearm/hand length ratio.
fh_ratio <- dat[,1]/dat[,2]
dat <- cbind(dat, fh_ratio)
str(dat) #Return

#11. 19 observations with 4 variables
dim(dat)
nrow(dat)
ncol(dat)
str(dat)

#12. Minimum, maximum, standard deviation, median and mean.
stati.mat <- matrix(nrow = 4, ncol = 5)
stati <- as.data.frame(stati.mat)
rm(stati.mat)

for (i in 1:4){
  stati[i,] <- c(min(dat[,i]), max(dat[,i]), sd(dat[,i]),
                 median(dat[,i]), mean(dat[,i]))
}
stati <- stati[-3,]
names(stati) <- c("min", "max", "sd", "median", "mean")
rownames(stati) <- c("fl", "hl", "fh_r")

#13. Coefficient of variation(CV). The forearm/hand length ratio is predicted to have the smallest CV.

#14. Creating my own function
CV <- function(x){
  sd(x)/mean(x)
}

cv.fl <- CV(dat[,1])
cv.hl <- CV(dat[,2])
cv.fh_r <- CV(dat[,4])
stati[,6] <- c(cv.fl, cv.hl, cv.fh_r)
names(stati)[6] <- "cv"
str(stati)

#15. CSV
write.csv(stati, file = "~/Desktop/Statistics.csv")
write.csv(dat, file = "~/Desktop/Dataset_hw1_AppEpid.csv")

Stati_new <- read.csv(file = "~/Desktop/Statistics.csv")

#16. Scatterplot  
#17. Correlation
col.list = c("hotpink", "purple")
palette(col.list)
plot(dat[,1], dat[,2], 
     xlab = "Forearm Lengths (cm)", ylab = "Hand Lengths (cm)",
     main = "Relationship of Forearm Lengths-Hand Lengths (n=19)", 
     pch = 18, col = age
     )
legend("bottomright", c("Adults", "Children"), 
       col = col.list, pch = 18)

corl = cor(dat[,1], dat[,2])
legend("topleft", paste("cor =", round(corl,3)))

#18. Scatterplot of adults only  
#19. Correlation of adults only
plot(dat[age == 1, 1], dat[age == 1, 2], 
     xlab = "Forearm Lengths (cm)", ylab = "Hand Lengths (cm)",
     main = "Relationship of Adults Forearm Lengths-Hand Lengths (n=17)", 
     pch = 18, col = "blue"
)
legend("bottomright", c("Adults"), 
       col = "blue", pch = 18)

corl_ad = cor(dat[ age == 1, 1], dat[age ==1, 2])
legend("topleft", paste("cor =", round(corl_ad, 3)))
