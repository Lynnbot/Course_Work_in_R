#1.a
same.bdays <- function(Nexp = 5000){
  numerator <- 0
  for(iexp in 1:Nexp)
  {
    bdays <- sample(x = 1:365, size = 40, replace = T, prob = rep(1/365, 365))
    if (length(unique(bdays)) == 38 & length(unique(bdays[which(duplicated(bdays))])) == 1) numerator <- numerator + 1
  }
  prob <- numerator/Nexp
  return(prob)
}
same.bdays(10000)
same.bdays(20000)
print(paste("Probability that exactly 3 persons in the room with 40 people have the same birthday is approximately", same.bdays(50000)))

#an alternative way for conditioning
table(bdays)

####
tst <- c(1,1,2,2,3,4,5,6)
tst2 <- c(1,1,1,3,4,6,5,9)
which(unique(tst))
if (length(tst[which(duplicated(tst))]) ==  1 & length(unique(tst)) == 6) print("tst")
if (unique(tst2[which(duplicated(tst2))]) == 1 & length(unique(tst2)) == 6) print("tst2")
####

#1.b
same.book <- function(Nexp = 5000){
  numerator <- 0
  for(iexp in 1:Nexp)
  {
    book <- sample(x = 1:5e4, size = 100, replace = T, prob = rep(1/5e4, 5e4))
    if (length(unique(book)) < 100) numerator <- numerator + 1
  }
  prob <- numerator/Nexp
  return(prob)
}
print(paste("Probability that at least two students want the same book on that day is approximatelt", same.book(10000)))


#3.
p <- 1e-3
individual.prob <- 1 - (1-p)^100 - 100*(1-p)^99*p
cancers <- individual.prob * 2e6
print(paste("The estimate number of cancers in the country with 200m people is", round(cancers)))
