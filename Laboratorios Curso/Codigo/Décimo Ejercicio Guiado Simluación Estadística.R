# https://towardsdatascience.com/statistical-simulation-in-r-part-1-d9cb4dc393c9

library(dplyr)
set.seed(2)
runif(25,min=0,max=10)

runif(25,min=0,max=10) %>% 
  round(.,digits = 0)

set.seed(24)
sample(seq(1,10),8)

x = 1:10000
sample(x,replace=TRUE)
t <-sample(x,replace=TRUE)
plot(t)
summary(t)
boxplot(t)


set.seed(1)
sample(letters,20)

#Scenario 1 Equal Probability
equal_prob_dist = sample(5,10000,prob=rep(0.1,5),replace=T)
hist(equal_prob_dist)

#Scenario 2 Unequal Probability

unequal_prob_dist = sample(5,10000,prob =c(0.1,0.25,0.4,0.25,0.1), replace=T)
hist(unequal_prob_dist)

#There are two 6-sided dices. If you roll them together, what is the probability of rolling a 7?

set.seed(1)
die = 1:6
# sample 10000 times with replacements for dice 1
die1 = sample(die,10000,replace = TRUE,prob=NULL)
# sample 10000 times with replacements for dice 2
die2= sample(die,10000,replace=TRUE,prob = NULL)
# the combined value of die_1 and die_2 
outcomes = die1+die2
# the probability of getting a 7
mean(outcomes == 7)

set.seed(1)
for (i in 10000){
  die_1 = sample(die,prob=NULL,replace=TRUE)
  die_2 = sample(die,prob=NULL,replace=TRUE)
  die_sum = die_1+die_2
  print(mean(die_sum==7))
}

#You have two dice. What is the probability of rolling a 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, and 13?

sapply(2:13,function(x) mean(outcomes==x))

#Generate a random sample of 10000 values, and how many unique observations are included? 
#How many are not included?

set.seed(1)
n=10000
included_obs = length(unique(sample(1:n, replace = TRUE)))
included_obs
missing_obs = n-included_obs
missing_obs


# Question 4: Generate an m*n matrix with randomly assigned 0 and 1.

m <- 10
n <- 10
# create an empty matrix
m00 <- matrix(0,m,n)
for (i in 1:m) {
  for (j in 1:n) {
    m00[i,j] <- sample(c(0,1),1)
  }
}
m00

m <-10
n<-10
m0 <- matrix(0,m,n)
apply(m0,c(1,2),function(x) sample(c(0,1),1))


#Flip a coin 10 times and simulate the process for 10,000 times. 
#Show the distribution of the number of heads shown up.

# create an empty list 
total_heads = c()
# use a for loop to simulate coin-flipping 10 times 
# repeat it for 10,000 times
for (i in 1:10000){
  sum_heads = sum(round(runif(10,0,1)))
  total_heads = c(total_heads, sum_heads)
}
hist(total_heads)

