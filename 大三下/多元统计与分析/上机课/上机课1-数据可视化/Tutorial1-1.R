#######
# Chap 1

####################
# Exam 1
######################
A <- matrix(c(1,3,5,7),2,2)
A

B <- matrix(c(2,4,6,8),2,2)
B

A+B

cbind(A,B)

rbind(A,B)

as.vector(A)

det(A)

rowMeans(A)

rowSums(A)

colMeans(A)

colSums(A)


A*B

A%*%B

A%o%B   #外积

crossprod(A,B)

crossprod(A)

t(A)

kronecker(A,B)

solve(A)


####################
# Exam 2
######################
diag(c(1,3,5))

diag(4)

####################
# Exam 3
######################
b <- c(3,2)
solve(A,b)
solve(A)

library(MASS)
ginv(A)


####################
# Exam 4
######################

A <- matrix(c(5,3,7,10),2,2)
z<-qr(A)
qr.Q(z)
qr.R(z)

z<-chol(A)
z

z<-eigen(A)
z

z<-svd(A)
z


####################
# Exam 5
######################

library("Matrix")
set.seed(1)
m <- matrix(0, 10, 10)
m[sample(length(m),30)] <- sample(9,30,replace = T)
mm <- as(m, "CsparseMatrix")
mm

image(mm)

tril(mm)

tril(mm,-1)

tril(mm,1)

band(mm, -1, 2)

data(KNex)
names(KNex)

mtm <- with(KNex, crossprod(mm))
dq <- function(ch) paste('"',ch,'"', sep="") ## dQuote(<UTF-8>) gives bad plots
image(mtm, main=paste("crossprod(mm): Sparse", dq(class(mtm))))


C1 <- Cholesky(mtm) # uses show(<MatrixFactorization>)
cm1 <- as(C1, "sparseMatrix")
image(cm1, main= paste("as(Cholesky(crossprod(mm)),\"sparseMatrix\"):",
                       dq(class(cm1))))
