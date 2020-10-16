#Exercise 3 (09-function)

#1.1
f1<- function(x){
  hasil <- (x^3+x^2-6)
  print(hasil)
}
#1.2
f2<- function(a,b){
  hasil <- (a*b*(b-a))
  print(hasil)
}

#1.3
f3<- function(m,n){
  hasil <-sqrt(m/n)+m-2*n
  print(hasil)
}

#2.1
a <- matrix(c(1:10),2,2, TRUE)
b <- matrix(c(11:20),2,2, TRUE)

f <- function(a,b){
  result <- (a+b)%*%a%*%b
  return(result)
}
f(a,b)

#2.2
m <- matrix(c(5:10), 2,2, TRUE)
n <- matrix(c(15:20), 2,2, TRUE)
h <- function(m,n){
  result <- det(m)*n - m%*%n
  return(result)
}
h(m,n)

#2.3
x <- matrix(c(3,2,5,3),2,2, TRUE)
g <- function(x){
  result <- solve(x)%*%x-2*x
  return(result)
}
g(x)


#exercise 4
#1 
#f(x)=sin(x)
f <- function(x){
  hasil <- sin(x)
  return(hasil)
}
input <- seq(1, 10, 0.1)
plot(input,
     sapply(input, f), type = "l", xlab = "x", ylab = "f(x)")


#2
#f(x)=log(x)
f <- function(x){
  hasil <- log(x)
  return(hasil)
}
input <- 0:100
plot(input,
     sapply(input, f), type = "l", xlab = "x", ylab = "f(x)")


#3
#sqrt(x)-2
f <- function(x){
  hasil <- sqrt(x)-2
  return(hasil)
}
input <- 1:2
plot(input,
     sapply(input, f), type = "l", xlab = "x", ylab = "f(x)")


#4
#sqrt(x-2)
f <- function(x){
  hasil <- sqrt(x-2)
  return(hasil)
}
input <- 5:15
plot(input,
     sapply(input, f), type = "l", xlab = "x", ylab = "f(x)")