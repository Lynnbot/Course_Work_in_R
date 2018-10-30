#setwd("~/Documents/2018FALL_Class/R/Math")

#A function to compare whether two matrices in r are identical.
compare_two_matrices <- function(x, y)
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)

# Solution to problem 3.
# To test whether the inverse traspositional matrix is identical
# to the transpositional inverse matrix.
w4_chen <- function(n){
  A <- matrix(data = runif(n^2, min = 0, max = 1), nrow = n)
  left = diag(n)/ t(A)
  right = t(diag(n)/A)
  ifelse(compare_two_matrices(left, right), print("The equation is valid."),
         print("The equation is invalid."))
}



