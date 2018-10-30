
# Problem 1
par(mfrow = c(1,1), mar = c(5,5,4,3), mgp = c(3,1,0))
#par(nfrow = c(1,1), cex.lab = 1.5, cex.main = 2)
# global setting
t <- seq(from = 0, to = 8, by = .1)
y <- 5.358*10^(-6) * t^6 * (8-t)^3
plot(t, y, type = "l", lwd = 2,
     main = "Quality sleep of a person",
     sub = paste("Max quality sleep occurs after",tm,"hours from falling asleep"), cex = 1.2,
     xlab = "Hours after falling asleep", ylab = "Score of quality sleep",
     axes = F)
ym <- max(y); tm <- t[y == ym]

#segments(0, ym, tm, ym, col = "red", lwd = 1, lty = 2)
segments(tm, 0, tm, ym, col = "red", lwd = 1, lty = 2)
points(tm, ym, pch = 4, col = "red")
text(tm, ym, "Best quality sleep",
     pos = 4, cex = 0.8)
#axis(1, at = c(0:4,6:8))
axis(side = 1, at = 0:8, c("11pm", "Midnight", "1am", "2am","3am","4","5","6","7"))
axis(side )
axis(1, at = tm, labels = round(tm,2), col.axis="red", 
     las = 0, tck = 0.05, col.ticks = "red", lwd = 2)
axis(2, at = ym, labels = round(ym,2), col.axis="red", 
     las = 2, tck = 0.05, col.ticks = "red", lwd = 2)

mtext(side = 1, 0:8, cex = 1.5, lines(5))

#Problem 2

Ngrid = 100
K <- seq(from = 0, to = 1.5, length=Ngrid)
L <- seq(from = 0, to = 1.5, length=Ngrid)
PF <- matrix(nrow=Ngrid, ncol=Ngrid)

for(i in 1:Ngrid)
    for(j in 1:Ngrid)
  {
    PF[i,j] = 2.5 * K[i]^0.6 * L[j]^0.4 + (1.5 - K[i] - L[j])
}


#a
par(mfrow = c(1,1), mar = c(4,4,6,1))
contour(K,L,PF, lwd = 2,
        levels = c(1.5+.233, 1.5+.41),
        main = "The annual revenue of a hospital modeled\nby the Cobb-Douglas PF (billion dollar)",
        xlab = "Capital cost (billion dollar)", ylab = "Labor cost (billion dollar)")

## add the cost line K+L=1.5
x <- seq(from = 0, to = 1.5, by = .5)
y <- seq(from = 1.5, to = 0, by = -.5)
lines(x, y, col = "royalblue3", lwd = 2)
#text(0.2, 1.13, "Cost line\nas in K+L=1.5", cex = 0.8, col = "royalblue3")
## display the current values and the optimal one
points(1.2, 0.3, col = "purple", pch = 17)
text(1.2, 0.3, "current\n(0.9,0.6)", pos = 1, cex = 0.8)

points(0.9, 0.6, col = "red3", pch = 17)
text(0.9, 0.6, "optimal (1.2,0.3)", pos = 4, cex = .8)

legend("topright",pch = 17, col = c("red","purple"),
       c("Optimal (K,L) values","current"), cex = 0.8)
legend("left", "Total cost is constrained = 1.5 billion dollars", cex = 0.5)

#b
par(mar = c(3,2,4.5,1))
persp(K, L, PF, theta=286, phi=50, r=100, ticktype="detailed",shade=0.03,
      main = "Annual revenue of a hospital\nwith relation to cost (billion dollar)",
      xlab = "\ncapital cost", ylab = "\nlabor cost", zlab = "\nrevenue")

#c
par(mar = c(4,4,3,1), mgp = c(2.5,1,0))
c <- seq(from = 0, to = 3, by = .01)
p = 2.5 * (0.6*c)^0.6 * (0.4*c)^0.4 - c
plot(c, p, type = "l", lwd = 2,
     main = "Optimal profit-total cost (billion dollar)",
     xlab = "Total cost (K+L)", ylab = "Optimal profit")
c1 <- 1.5
p1 <- p[c == 1.5]
points(c1, p1, pch = 17, col = 2)
text(c1, p1, "(1.5, 0.41)",pos = 4, col = "red")
segments(0, p1, c1, p1, col = "red", lty = 2)
segments(c1, 0, c1, p1, col = "red", lty = 2)
text(2, 0.7, "op = 0.275*c")

#################
#Testing

x <- seq(from = 1, to =10, by=1)
y <- seq(from = 1, to = .1, by = -.1)
tsting <- matrix(nrow = 10, ncol = 10)

for(i in 1:10)
  for(j in 1:10){
    tsting[i,j] <- x[i]+2*y[j]
  }
tsting
tsting.l <- tsting
tsting.l[upper.tri(tsting)] <- NA
tsting.l

tsting.u <- tsting
tsting.u[lower.tri(tsting)] <- NA
tsting.u

#############
C.d <- D(expression(e^(-0.03*t) * (1-e^(-0.01*t))), "t")
C.d
C.d.d <- D(expression(e^(-0.03 * t) * (e^(-0.01 * t) * (log(e) * 0.01)) 
                      - e^(-0.03 * t) * (log(e) * 0.03) * (1 - e^(-0.01 * t))), "t")
C.d.d

D(C.d, C.d.d)


t = c(0:8)
mm = t^6 * (8-t)^3
mm.d <- D(expression(t^6 * (8-t)^3), "t")
mm.d.d <- D(expression(6 * t^5 * (8 - t)^3 - t^6 * (3 * (8 - t)^2)), "t")
mm.d.d


