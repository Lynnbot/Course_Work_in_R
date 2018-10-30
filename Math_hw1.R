t = seq(from = 0, to = 25, by = .1)
e = exp(1)
y = 2*e^(0.04*t)
plot(t, y, type = "l", col = "blue")
points(log(2.5)/0.04, 5, col = "red")

t = seq(from = 0, to = 25, by = .1)
e = exp(1)
y = 2*e^(0.04*t)
t1 = log(2.5)/0.04
plot(t, y, main = "Development of A Breast Cancer Case", 
     xlab = "Time (month)", ylab = "Volume (cm^3)",
     type = "l", col = "blue")
points(t1, 5, col = "red")
abline(h = 5, col = "red")
text(t1, 5, "V(t) = 2e^(0.04t)", pos = 2, cex = 1)
legend("left", title = ("The tumor will reach 5cm^3 in"), paste( round(t1, 1) , "months or",
       round(t1/12, 1), "years."), adj = 0.12)


#Function of the development of tumor grouwth, where v0 is initial volume, 
#and b the growth rate (per volumn per month).
f <- function(x, v0, b){
  e = exp(1)
  y = v0 * e^(b*x)
  return(y)
}


v0 = 2; b = 0.04; Expected_Volume = 5

#N is the maximum times of iteration.
#delta is the change of x.
N <- 50
delta <- 0.05
x0 <- 20; x1 <- x0
xk <- numeric(N)
for (i in c(1:N)){
  df.dx <- (f(x0 + delta, v0, b) - f(x0, v0, b)) / delta
  x1 <- x0 - (f(x0, v0, b) - Expected_Volume)/ df.dx
  xk[i] <- x1
  if(f(x1, v0, b)-5 < 1E-12) {
    break
  }
  x0 <- x1
}

Expected_Time = xk[i]
print(Expected_Time)

#########################
#REFERENCE: http://www.theresearchkitchen.com/archives/642
