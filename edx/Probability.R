library(gtools)
library(tidyverse)

# Continuouos probability
set.seed(16, sample.kind = "Rounding")

act_scores<-rnorm(10000, 20.9, 5.7)
mean<-mean(act_scores)
sd<-sd(act_scores)

sum(ifelse(act_scores>=36,1,0))
dnorm(30, 20.9, 5.7)
mean(act_scores > 30)
mean(act_scores<=10)
x<-seq(1,36)
f_x<-dnorm(x,20.9,5.7)
plot(x, f_x)

z_score<-(act_scores-mean)/sd
mean(z_score>2)
qnorm(0.975, 20.84, 5.67)

four<-function(x){
  pnorm(x,20.84, 5.67)
}
numb<-seq(1,36)
sapply(numb, four)
qnorm(0.95, 20.9, 5.7)

p<-seq(0.01, 0.99, 0.01)
sample_quantiles<-qnorm(p, 20.84, 5.67)
theoretical_quantiles<-qnorm(p,20.9,5.7)
plot(theoretical_quantiles, sample_quantiles)

#  Random variables, sampling models and the central limit theorem

beads<- rep(c("red","blue"), times=c(2,3))
X<-ifelse(sample(beads, 1) == "blue", 1, 0)

# Casino example
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) #Build the roulette
n<-1000
X<-sample(ifelse(color=="Red", -1, 1), n, replace=TRUE) #If the ball lands in red, the casino loses 1$. Otherwise, the casino wins 1$. 
X[1:10]
#Because we know the proportions of red and black, we can write it like this:
X<-sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))
#This is a sampling model, we are modelling the random behaviour of a roulette.
S<-sum(X)     # Total winnings is the sum of 1,000 independent draws
S
# Probability disribution:
n<-1000 #1000 people playing the game
B<-10000 #Repeat the experiment 10,000 times
S<-replicate(B, {
  X<-sample(c(-1,1),n,replace = TRUE,prob = c(9/19, 10/19))
  sum(X)
})
mean(S<0) #How many times did we get sums smaller or equal to a

# Exercises datacamp 1
# The variables `green`, `black`, and `red` contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green<-green/(green+black+red)

# Print the variable `p_green` to the console
p_green
# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green<-1-p_green

# Create a model to predict the random variable `X`, your winnings from betting on green. Sample one time.
X<-sample(c(-1,17),1,prob=c(p_not_green, p_green))

# Print the value of `X` to the console
X
# Calculate the expected outcome if you win $17 if the ball lands on green and you lose $1 if the ball doesn't land on green
17*p_green+((-1)*p_not_green)
# Compute the standard error of the random variable
abs(-1-17)*sqrt(p_green*p_not_green)
samples
X<-sample(c(17,-1),n,prob=c(p_green, p_not_green), replace=TRUE)

# Assign the sum of all 1000 outcomes to the variable 'S'
S<-sum(X)

# Print the value of 'S' to the console
S
# Calculate the expected outcome of 1,000 spins if you win $17 when the ball lands on green and you lose $1 when the ball doesn't land on green
1000*(17*p_green+((-1)*p_not_green))
# Compute the standard error of the sum of 1,000 outcomes
sqrt(n)*(abs(-1-17)*sqrt(p_green*p_not_green))
# Calculate 'avg', the expected outcome of 100 spins if you win $17 when the ball lands on green and you lose $1 when the ball doesn't land on green
avg <- n * (17*p_green + -1*p_not_green)
# Create an object called `S` that replicates the sample code for `B` iterations and sums the outcomes.
S<-replicate(B, {
  X<-sample(c(17,-1),n,replace=TRUE,prob=c(p_green,p_not_green))
  sum(X)
})
# Compute the average value for 'S'
mean(S)
# Calculate the standard deviation of 'S'
sd(S)


# Compute 'se', the standard error of the sum of 100 outcomes
se <- sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)
# Calculate the proportion of outcomes in the vector `S` that exceed $0
mean(S>0)

# Using the expected value 'avg' and standard error 'se', compute the probability that you win money betting on green 100 times.
1-pnorm(0, avg, se)

n<-1000
loss_per_foreclosure<- -200000
p<-0.2
defaults<-sample(c(0,1), n, prob=c(1-p, p), replace=TRUE)
sum(defaults*loss_per_foreclosure)  #Default

B<-10000
losses<-replicate(B, {
  defaults<-sample(c(0,1), n, prob=c(1-p, p), replace=TRUE)
  sum(defaults*loss_per_foreclosure)
})
data.frame(losses_in_millions=losses/10^6)%>%
  ggplot(aes(losses_in_millions))+
  geom_histogram(binwidth=0.6, col="black")
n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error

l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans

B<- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money

# Assign the number of loans to the variable `n`
n <- 10000
# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000
# Assign the probability of default to the variable `p_default`
p_default <- 0.03
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)
# Generate a vector called `defaults` that contains the default outcomes of `n` loans
defaults<-sample(c(0,1), n, prob(1-p_default, p_default))
# Generate `S`, the total amount of money lost across all foreclosures. Print the value to the console.
S<-sum(defaults*loss_per_foreclosure)
S

data("death_prob")
head(death_prob)

death_prob%>%
  filter(age==50&sex=="Female")
p<-0.015
a<- -150000
b<-1150
E<-(a*p)+(b*(1-p))
SE<-abs(b-a)*sqrt(p*(1-p))
pnorm(-10^6, 1000*E, sqrt(1000)*SE)

death_prob%>%
  filter(age==50&sex=="Male")
p_m<-0.005013

quest<-function(x){
  avg<-1000*(a*x+(b*(1-x)))
  se<-sqrt(1000)*abs(b-a)*sqrt(x*(1-x))
  pnorm(-10^6,avg,se)
}
p <- seq(.01, .03, .0025)
p
round(sapply(p, quest), digits = 3)

set.seed(27, sample.kind = "Rounding")
n<-1000
p_loss<-0.015

p <- .015
loss <- -150000
profit <- 1150
n <- 1000
X<-replicate(10000, {
  outcomes <- sample(c(loss, profit), n, prob = c(p, 1-p), replace = TRUE)
  sum(outcomes)
})
mean(X<=-10^6)



outcomes <- sample(c(loss, profit), n, prob = c(p, 1-p), replace = TRUE)
sum(outcomes)/10^6
z<-qnorm(0.05)
z
l<--150000
n=1000
p=0.015
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x
n*(l*p+x*(1-p))

B<-10000
set.seed(28, sample.kind = "Rounding")
profit<-replicate(B, {
  draws<-sample(c(x,l), n, prob=c(1-p, p), replace = TRUE)
  sum(draws)
})
mean(profit<0)

set.seed(29, sample.kind = "Rounding")
B<-10000
p<-0.015+sample(seq(-0.01, 0.01, length = 100), 1)
profit<-replicate(B, {
  draws<-sample(c(x,l), n, prob=c(1-p, p), replace = TRUE)
  sum(draws)
})
mean(profit)
mean(profit<=-10^6)

profit <- replicate(B, {
  new_p <- p + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample(c(x, l), n, 
                  prob=c(1-new_p, new_p), replace = TRUE) 
  sum(draws)
})
mean(profit<=-10^6)

