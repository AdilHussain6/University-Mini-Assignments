#R code for Task 1
set.seed(0)

N=1000
alpha=2
Beta=1
#Generate 1000 samples from the prior distribution
prior <- rgamma(N,alpha,rate=Beta)
#Plot a histogram of these samples
hist(prior,breaks=30,xlab="Radius of the circle",
     main="Histogram of the Prior distribution")


#Envelope Rejection Sampling Method Code
N <- 2000
count <- 0
res <- NULL # empty vector
n <- 10
k <- 3
c <- ((k^k) *(n-k)^(n-k))/(n^n)
while (count < N) {
  R <- rgamma(1,alpha)
  U <- runif(1)
  if (c* (R*exp(-R) * U <= (1-exp((-R^2)/2))^k * exp(-(n-k)*(R^2)/2)*R*exp(-R)) ) {
    count <- count+1
    #Accept R value and add to res vector
    res <- c(res, R)
  } else { #Reject R value
  }
}
#Plot the histogram
hist(res,breaks=30,col="grey",probability=T,xlab="Radius of the circle",
     main="Histogram of the Posterior Distribution",ylim=c(0,2),xlim=c(0,2))

#Plotting the dual histogram
hist(res,breaks=30,col="grey",probability=T,xlab="Radius of the circle",
     ylim=c(0,2),xlim=c(0,6),main="")
hist(prior,breaks=70,xlab="Radius of the circle",probability=T,xlim=c(0,6),add=T)


#Task 4

#Posterior expectation E(R|K=3)
Exp = mean(res)

#Posterior prob P(R<1/2|K=3)
Prob <- mean(res<0.5)

#How do i work out the error? The actual results
MSE = var(res)/N
MSE
