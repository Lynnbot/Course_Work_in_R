##### Problem 1 #####
# a
f <- function(x)  (0.8*x^1.5)/(1+ 0.8*x^1.5)
# Compute First-order derivative
D(expression((0.8*x^1.5)/(1+ 0.8*x^1.5)), 'x')
# 0.8 * (1.5 * x^0.5)/(1 + 0.8 * x^1.5) - 
# (0.8 * x^1.5) * (0.8 * (1.5 * x^0.5))/(1 + 0.8 * x^1.5)^2
df.dx <- function(x) 0.8 * (1.5 * x^0.5)/(1 + 0.8 * x^1.5) - (0.8 * x^1.5) * (0.8 * (1.5 * x^0.5))/(1 + 0.8 * x^1.5)^2
df.dx(3)

# Compute Second Derivative
D(expression(0.8 * (1.5 * x^0.5)/(1 + 0.8 * x^1.5) - 
               (0.8 * x^1.5) * (0.8 * (1.5 * x^0.5))/(1 + 0.8 * x^1.5)^2), 'x')
d2f.dx2 <- function(x) {0.8 * (1.5 * (0.5 * x^-0.5))/(1 + 0.8 * x^1.5) -
    0.8 * (1.5 * x^0.5) * (0.8 * (1.5 * x^0.5))/(1 + 0.8 * x^1.5)^2 -
    ((0.8 *  (1.5 * x^0.5) * (0.8 * (1.5 * x^0.5)) + (0.8 * x^1.5) * (0.8 *(1.5 * (0.5 * x^-0.5))))/(1 + 0.8 * x^1.5)^2 -
       (0.8 * x^1.5) * (0.8 * (1.5 * x^0.5)) * (2 * (0.8 * (1.5 * x^0.5) * (1 +0.8 * x^1.5)))/((1 + 0.8 * x^1.5)^2)^2)}
install.packages("pracma")
library(pracma)
xmax <- uniroot(d2f.dx2, c(0.01,10))$root

# Compute Third order derivative
D(expression(0.8 * (1.5 * (0.5 * x^-0.5))/(1 + 0.8 * x^1.5) -
               0.8 * (1.5 * x^0.5) * (0.8 * (1.5 * x^0.5))/(1 + 0.8 * x^1.5)^2 -
               ((0.8 *  (1.5 * x^0.5) * (0.8 * (1.5 * x^0.5)) + (0.8 * x^1.5) * (0.8 *(1.5 * (0.5 * x^-0.5))))/(1 + 0.8 * x^1.5)^2 -
                  (0.8 * x^1.5) * (0.8 * (1.5 * x^0.5)) * (2 * (0.8 * (1.5 * x^0.5) * (1 +0.8 * x^1.5)))/((1 + 0.8 * x^1.5)^2)^2)), 'x')
d3f.dx3 <- function(x) {-(0.8 * (1.5 * (0.5 * (x^-(0.5 + 1) * 0.5)))/(1 + 0.8 * x^1.5) + 
                            0.8 * (1.5 * (0.5 * x^-0.5)) * (0.8 * (1.5 * x^0.5))/(1 + 0.8 * x^1.5)^2 +
                            ((0.8 * (1.5 * (0.5 * x^-0.5)) * (0.8 *(1.5 * x^0.5)) +
                                0.8 * (1.5 * x^0.5) * (0.8 * (1.5 * (0.5 * x^-0.5))))/(1 + 0.8 * x^1.5)^2 - 
                               0.8 * (1.5 * x^0.5) * (0.8 * (1.5 * x^0.5)) * (2 * (0.8 * (1.5 * x^0.5) * (1 + 0.8 * x^1.5)))/((1 + 0.8 * x^1.5)^2)^2) +
                            ((0.8 * (1.5 * (0.5 * x^-0.5)) * (0.8 *(1.5 * x^0.5)) + 
                                0.8 * (1.5 * x^0.5) * (0.8 * (1.5 * (0.5 * x^-0.5))) + (0.8 * (1.5 * x^0.5) * (0.8 * (1.5 * (0.5 * x^-0.5))) -
                              (0.8 * x^1.5) * (0.8 * (1.5 * (0.5 * (x^-(0.5 + 1) * 0.5))))))/(1 +0.8 * x^1.5)^2 - (0.8 * (1.5 * x^0.5) * (0.8 * (1.5 * x^0.5)) +
                              (0.8 * x^1.5) * (0.8 * (1.5 * (0.5 * x^-0.5)))) * (2 * (0.8 *  (1.5 * x^0.5) * (1 + 0.8 * x^1.5)))/
                               ((1 + 0.8 * x^1.5)^2)^2 - (((0.8 * (1.5 * x^0.5) * (0.8 * (1.5 * x^0.5)) + (0.8 * x^1.5) *
                               (0.8 * (1.5 * (0.5 * x^-0.5)))) * (2 * (0.8 * (1.5 * x^0.5) * (1 + 0.8 * x^1.5))) + (0.8 * x^1.5) * 
                                 (0.8 *(1.5 * x^0.5)) * (2 * (0.8 * (1.5 * (0.5 * x^-0.5)) *  (1 + 0.8 * x^1.5) +
                              0.8 * (1.5 * x^0.5) * (0.8 * (1.5 *x^0.5)))))/((1 + 0.8 * x^1.5)^2)^2 - (0.8 * x^1.5) *
                                (0.8 * (1.5 * x^0.5)) * (2 * (0.8 * (1.5 * x^0.5) * (1 +0.8 * x^1.5))) *
                                (2 * (2 * (0.8 * (1.5 * x^0.5) * (1 +0.8 * x^1.5)) * 
                                        ((1 + 0.8 * x^1.5)^2)))/(((1 + 0.8 * x^1.5)^2)^2)^2))) }

d3f.dx3 <- function(x) {
  D(expression(0.8 * (1.5 * (0.5 * x^-0.5))/(1 + 0.8 * x^1.5) -
                 0.8 * (1.5 * x^0.5) * (0.8 * (1.5 * x^0.5))/(1 + 0.8 * x^1.5)^2 -
                 ((0.8 *  (1.5 * x^0.5) * (0.8 * (1.5 * x^0.5)) + (0.8 * x^1.5) * (0.8 *(1.5 * (0.5 * x^-0.5))))/(1 + 0.8 * x^1.5)^2 -
                    (0.8 * x^1.5) * (0.8 * (1.5 * x^0.5)) * (2 * (0.8 * (1.5 * x^0.5) * (1 +0.8 * x^1.5)))/((1 + 0.8 * x^1.5)^2)^2)), 'x')
  
}
d3f.dx3(xmax)

par(mar=c(3,3,4,1), cex = 0.7)
plot(df.dx, xlim = c(0, 10),
     main = "Illustration of the Function of\nthe First Derivative of M(x)")
points(xmax, df.dx(xmax), col = 2, pch = 18)
text(0.287, 0.525, "(0.397, 0.525)", pos = 4)


#c
relative.rate <- df.dx(3)/f(3)

#d
elasticity <- (df.dx(3)*3)/f(3)

#e
x <- seq(from=0, to=10, length=100)
par(mar = c(5,5,4,1), cex = 0.8, family = "serif")
plot(x, f(x), type = "l", main = "Illustration of the Effect of a Chemotherapy Drug on Killing Cancer Cells",
     xlab = "Concentration of a Chemotherapy Drug (mg/L)",
     ylab = "Proportion of Killed Cancer Cells (%)", lwd = 2, col = "darkgrey",
     bty = "n", xaxt = "n", yaxt = "n")
axis(1, at = c(0:10), labels = c(0:10))
axis(1, at = xmax, tck = f(xmax)+0.02, labels = round(xmax, 3),
     las = 1, col = "red", col.ticks = "red", lty = 5)
axis(2, at =  seq(from=0, to=1, length=6), labels = seq(from=0, to=100, length=6), las=2)
tan.line <- (f(xmax)/xmax) * (x - xmax) + f(xmax)
lines(x, tan.line, col = "blue", lwd = 1.3, lty = 1)
points(xmax, f(xmax), pch = 18, col = "red")
legend(5, 0.5, "The mortality rate reaches maximum\nwhen the concentration of drug is 0.397 mg/L.
       \nSlope of the tangent line = 0.078",
       cex = 0.9, box.lty = 0)


## 3
par(family = "serif")
plot.new()
axis(1, at=seq(from = 0, to=1, length=11), labels = seq(from = 0, to=1, length=11))
arrows(1,0, 0,0, length = 0.2, code = 1, lwd = 2)
arrows(0.5,sqrt(0.75), 0,0, length = 0.2 , code = 1, lwd = 2)
lines()
th = seq(from = 0, to = pi/3, length = 60)
lines(0.1*cos(th), 0.1*sin(th))
text(0.13, 0.09, "θ")





