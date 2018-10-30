setwd("~/Documents/2018FALL_Class/R/Math")

#1. read in data
webhits <- scan("comwebhits.dat")
webhits <- webhits[order(webhits)]
#compute the cdf & pdf
n <- length(webhits)
Fx <- (1:n)/n
fx <- density(webhits)

#1.a
ave <- mean(webhits)
medi <- webhits[n/2]
### my compution of the mode
summary(as.factor(round(webhits, 0)))
#From the call, we can see that the mode of webhits is at 10am with 14 hits.
mod <- 10
### Prof. Demidenko's method to compute the mode
fx$x[fx$y == max(fx$y)]



par(mar=c(4,3.5,5,1), mgp = c(2.5,1,0), cex = .7)
plot(density(webhits), lwd = 2,
     main = "Density Function of Webhits in a Day", 
     xlab = "Time (hour)", ylab = "Probability")
rug(webhits)
lines(c(ave,ave),c(0,.14), col = "tomato2")
lines(c(mod, mod),c(0,.14), col= "slateblue")
lines(c(medi,medi),c(0,.14), col= "slategray4")
text(22, .06, "The mode is estimated to be at 10am.\n
     Mean is at 11:48am and median at 11:12am.", cex = .8)

legend("topright",c("Mode","Median","Mean"), bty = "n",
       lty=1,col=c("slateblue", "slategray4", "tomato2"))


#1.b
#empirically
after7 <- webhits[which(webhits > 19)]
p1 <- length(after7)/length(webhits)

#display
par(mar=c(5,4,3,1), mgp=c(2.5,1,0), cex=.8)
plot(webhits, Fx, type="l",
     xlab = "Time (hour)", ylab = "Cumulative probability",
     main = "Web Hits occurred after 7PM",
     sub ="#web hits after 7PM= 7, #total web hits = 100")
segments(webhits[webhits>19], rep(0,7), webhits[webhits>19], Fx[94:100],
         col = "coral3")


#based on the pdf
kd <- fx$x[fx$x>19]
p2 <- sum(fx$x[fx$x>19]*fx$y[fx$x>19])/ sum(fx$x * fx$y)
### CORRECTED
p2.corrected <- sum(fx$y[fx$x>19])


print(paste("The proportion of visitors who come to the website after 7PM is ", 
            p1*100, "% and ", round(p2,2)*100,
            "% computed seperately by empirical data and the kernel density function.",
            sep = ""))
#display
par(mar=c(5,4,3,1), mgp=c(2.5,1,0), cex=.8)
m <- fx$x[fx$x>19]
u <- fx$y[fx$x>19]
plot(m,u,type="h", xlim=c(1.569,26.642),ylim=c(0,.12), col="coral3",
     bty = "n", xaxt = "n", yaxt = "n", xlab ="", ylab="")
par(new=T)
plot(density(webhits), lwd = 2,
     main = "Density Function of Webhits in a Day", 
     xlab = "Time (hour)", ylab = "Probability")

#polygon(x = c(fx$x[fx$x>19][1],fx$x[fx$x>19][1],max(fx$x)),
        y = c(0,fx$y[fx$x>19][1],fx$y[max(fx$x)]), col="coral3")

#p2
dat <- read.csv("DeathYears.csv", header = F)
dat <- dat[,1]
dat <- dat[order(dat)]
Fx <- (1:length(dat))/length(dat)
Sx <- 1-Fx

#p2.a
par(mar = c(4,4,5,1), cex = .8, mgp = c(2.2,1,0))
plot(dat, 1-Fx, type = "s", lwd = 2,
     main = "Survival Curve on Blood Cancer Patients after Chemotherapy\n(n=138)",
     xlab = "Years", ylab = "Proportion of the Survivals", col = "cornflowerblue")

#p2.b
E <- mean(dat)
lambda <- 1/E
a <- seq(from=0, to=20, by=.01)

par(new = T)
plot(a, exp(-lambda*a), type = "l", col = "coral3", lwd = 1.5,
     bty = "n", xaxt = "n", yaxt = "n", xlab ="", ylab="")
legend("topright", lty = 1, pch = 17,
       legend = c("Empirical median", "Theoretical median"),
       col = c("cornflowerblue", "coral3"))
points(dat[length(dat)/2],.5, pch = 17, col = "cornflowerblue")
points(log(.5)/(-lambda),.5, pch = 17, col = "coral3")
text(dat[length(dat)/2]+.2,.5, "x = 1.1", col = "cornflowerblue", pos = 2, cex = .8)
text(log(.5)/(-lambda),.5, "x = 1.4", col = "coral3", pos = 4, cex = .8)

#p2.c
lambda.new <- -log(.5)/dat[length(dat)/2]

par(new = T)
plot(a, exp(-lambda.new*a), type = "l", 
     col = "tan1", lwd = 1.5,
     bty = "n", xaxt = "n", yaxt = "n", xlab ="", ylab="")
legend("topright", lty = 1, lwd = 2,
       legend = c("Empirical", "Theoretical", "Adjusted by equating\nempirical & theoretical median"),
       col = c("cornflowerblue", "coral3", "tan1"))
points(dat[length(dat)/2],.5, pch = 17)
text(dat[length(dat)/2]+.2,.5, "Median at x=1.1", pos = 4)



#3
1-2*pnorm(-3)

