library(MASS)
library(Matrix)
#2.1
A1 = matrix(c(1,0,0,1), nr=2)
ginv(A1)
lambda1 = eigen(A)

#2.2
A = matrix(c(1,-1,2,-1,3,3,2,3,4),nr=3)
B = matrix(c(3,-2,4,-2,1,0,4,0,2),nr=3)
x = c(1,2,3) #행벡터가 아닌 열벡터로 선언됨.
y = c(3,5,4)

A+B
t(A)
t(x)%*%A%*%y
t(x)%*%x
t(x)%*%A%*%x
t(x)%*%y
t(A)%*%A
A%*%B
t(y)%*%B
x%*%t(x)
x+y
x-y
t(x-y)
x%*%t(y)
A-B
t(A)+t(B)
t(A+B)
3*x
(t(x)%*%y)%*%(t(x)%*%y)
B%*%A
rankMatrix(A)
ginv(A)
sum(diag(A))+sum(diag(B))
sum(diag(A%*%B)) == sum(diag(B%*%A))
t(A%*%B) == t(B)%*%t(A)


#2.3
A = matrix(c(2,1,1,4),nr = 2)
row_vector1 <- A[1, ]
row_vector2 <- A[2, ]
column_vector1 <- A[, 1]
column_vector2 <- A[,2]
plot(row_vector1, row_vector2)
plot(column_vector1,column_vector2)

rankMatrix(A)
eigen(A)
t(A)
lambda = matrix(c(eigen(A)$values[1],0,0,eigen(A)$values[2]),nrow=2)
eigen(A)$vectors%*%lambda%*%t(eigen(A)$vectors)
eigen(ginv(A))
t(A)%*%A
eigen(t(A)%*%A)
ginv(t(A)%*%A)
A2 <- (t(A)%*%A)%*%(t(A)%*%A)
A2
eigen(A2)
lambda = matrix(c(eigen(A2)$values[1],0,0,eigen(A2)$values[2]),nrow=2)
eigen(A2)$vectors%*%lambda%*%t(eigen(A2)$vectors)


#2.4
D = matrix(c(1,0,0,0,9,0,0,0,16),nr=3)
rankMatrix(D)
eigen(D)
ginv(D)
lambda =  matrix(c(eigen(D)$values[1],0,0,0,eigen(D)$values[2],0,0,0,eigen(D)$values[3]),nrow=3)
eigen(D)$vectors%*%lambda%*%t(eigen(D)$vectors)
eigen(rankMatrix(D))
cov2cor(D)
det(D) == prod(eigen(D)$values)
sum(diag(D)) == sum(eigen(D)$values)

#2.5
A = matrix(c(5,-4,3,-4,8,6,3,6,9),nr=3)
eigen(A)
sum(diag(A)) == sum(eigen(A)$values)
det(A) == prod(eigen(A)$values)
ginv(A)
