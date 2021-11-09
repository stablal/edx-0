# Section 1: Parameters and Estimates
N<-25
p<-seq(0,1,length.out = 100)
se<-sqrt((p*(1-p))/N)
plot(N*p, se)

sample_sizes<-c(25,100,1000)
for (N in sample_sizes){
  se<-sqrt(p*(1-p)/N)
  plot(p, se, ylim = c(0,0.1))
}

p<-0.45
N<-1000
X<-sample(c(0,1), size=N, replace = TRUE, prob = c(1-p, p))
X_hat<-mean(X)
B<-10000
X_hat<-replicate(B, {
  X<-sample(c(0,1), size=N, replace = TRUE, prob=c(1-p,p))
  mean(X)
})
mean(X_hat)
sd(X_hat)

# Section 3: Confidence Intervals and p-Values
library(tidyverse)
library(dslabs)
data("nhtemp")
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Temperatures in New Haven")

p<-0.45
N<-1000
X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))    # generate N observations
X_hat <- mean(X)    # calculate X_hat
SE_hat <- sqrt(X_hat*(1-X_hat)/N)    # calculate SE_hat, SE of the mean of N observations
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # build interval of 2*SE above and below mean
# Monte Carlo simulation to confirm that the CI contains p 95% of the times
B <- 10000
inside <- replicate(B, {
  X <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)    # TRUE if p in confidence interval
})
mean(inside)

data("polls_us_election_2016")
polls<-polls_us_election_2016%>%
  filter(state=="U.S."&enddate>="2016-10-31")
nrow(polls)
N<-polls[1,6]
# For the first poll in `polls`, assign the estimated percentage of Clinton voters to a variable called `X_hat`. Print this value to the console.
X_hat<-polls[1,8]/100
X_hat
# Calculate the standard error of `X_hat` and save it to a variable called `se_hat`. Print this value to the console.
se_hat<-sqrt(X_hat*(1-X_hat)/N)
se_hat
# Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci<-c(X_hat-qnorm(0.975)*se_hat, X_hat+qnorm(0.975)*se_hat)
ci

# Create a new object called `pollster_results` that contains columns for pollster name, end date, X_hat, se_hat, lower confidence interval, and upper confidence interval for each poll.
pollster_results<-polls%>%
  mutate(X_hat = rawpoll_clinton/100)%>%
  mutate(se_hat = sqrt(X_hat*(1-X_hat)/samplesize))%>%
  mutate(lower = X_hat-qnorm(0.975)*se_hat)%>%
  mutate(upper = X_hat+qnorm(0.975)*se_hat)%>%
  select(pollster, enddate, X_hat, se_hat, lower, upper)

avg_hit <- pollster_results%>%
  mutate(hit = ifelse(lower<=0.482&upper>=0.482, 1, 0))%>%
  summarise(mean=mean(hit==1))

# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.")%>%
  mutate (d_hat = rawpoll_clinton/100-rawpoll_trump/100)

head(polls)
polls%>%
  mutate(X_hat = (d_hat+1)/2)%>%
  mutate(se_hat = 2*sqrt(X_hat*(1-X_hat)/N))%>%
  mutate(lower = d_hat - qnorm(0.975)*se_hat)%>%
  mutate(upper = d_hat + qnorm(0.975)*se_hat)%>%
  select(pollster, enddate, d_hat, lower, upper)

polls%>%
  mutate(error = 0.021-d_hat)%>%
  group_by(pollster)%>%
  filter(n()>=5)%>%
  ggplot(aes(x=error, y=pollster))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Section 4: Statistical models

d<-0.039  #Actual difference between Obama and Romney
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)  # Sample sizes
p <- (d+1)/2  #Proportion of democrats
# calculate confidence intervals of the spread
confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1), size=N, replace=TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) - 1
})
# generate a data frame storing results
polls <- data.frame(poll = 1:ncol(confidence_intervals),
                    t(confidence_intervals), sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls
# Reconstruct data
d_hat <- polls %>%
  summarize(avg = sum(estimate*sample_size) / sum(sample_size)) %>%
  .$avg
p_hat<-(1+d_hat)/2      # Estimate of people voting for Obama
moe<-2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))    # Standard error of the aggregated pool
moe
round(d_hat*100, 1)  # The spread will be
round(moe*100, 1)   # plus or minus

data("polls_us_election_2016")
names(polls_us_election_2016)

polls <- polls_us_election_2016 %>%  # Choose nation-wide polls, performed in the week before the elections. Filter out non-reliable pools
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) # Keep the polls that have not been graded
# add spread estimate
polls <- polls %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
# compute estimated spread for combined polls
d_hat <- polls %>%
  summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>%
  .$d_hat
# compute margin of error
p_hat <- (d_hat+1)/2
moe <- 1.96 * 2 * sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))
# histogram of the spread
polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01)
# Data is not normally distributed
# number of polls per pollster in week before election
polls %>% group_by(pollster) %>% summarize(n())
# plot results by pollsters with at least 6 polls
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  ggplot(aes(pollster, spread)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# standard errors within each pollster
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarize(se = 2 * sqrt(p_hat * (1-p_hat) / median(samplesize)))

# collect last result before the election for each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%      # keep latest poll
  ungroup()
# histogram of spread estimates
one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)
# construct 95% confidence interval
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
round(results*100, 1)

res<-polls%>%group_by(pollster)%>%
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)%>%
  summarize(mean=mean(spread), sd=sd(spread)/sqrt(samplesize), numb=n())
res<-res[c(1,6),]
res

# Bayesian statistics
prev<-0.00025
N<-100000
outcome<-sample(c("Disease", "Healthy"), N, replace=TRUE, prob=c(prev, 1-prev))
outcome
N_D<-sum(outcome=="Disease")
N_H<-sum(outcome=="Healthy")
accuracy<-0.99
test<-vector("character", N)
test[outcome=="Disease"]<-sample(c("+", "-"), N_D, replace=TRUE, prob=c(accuracy, 1-accuracy))
test[outcome=="Healthy"]<-sample(c("-","+"), N_H, replace=TRUE, prob = c(accuracy, 1-accuracy))
table(outcome, test)

# Load the libraries and poll data
library(dplyr)
library(dslabs)
data(polls_us_election_2016)
# Create an object `polls` that contains the spread of predictions for each candidate in Florida during the last polling days
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
head(polls)
results<-polls%>%
  summarize(avg = mean(spread), se=sd(spread)/sqrt(length(spread)))%>%
  mutate(start = avg-1.96*se, end=avg+1.96*se)
results

# Election forecasting
library(tidyverse)
library(dslabs)
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)
mu<-0
tau<-0.035
sigma<-results$se
Y<-results$avg
B<-sigma^2/(sigma^2+tau^2)

posterior_mean<-B*mu+(1-B)*Y
posterior_se<-sqrt(1/(1/sigma^2+1/tau^2))

posterior_mean + c(-1.96, 1.96)*posterior_se      # Credible interval
1-pnorm(0, posterior_mean, posterior_se)          # Probability that d is bigger than 0

# Simulated data with Xj=d+ej
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
X
#Simulated data with Xij
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})
X

#Simulated data with +hi
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
h <- rnorm(I, 0, 0.025)    # assume standard error of pollster-to-pollster variability is 0.025
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})
X

# Electoral college
results <- polls_us_election_2016 %>%
  filter(state != "U.S." &
           !grepl("CD", state) &
           enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))
results%>%arrange(abs(avg))
joining electoral college votes and results
results <- left_join(results, results_us_election_2016, by="state")
results
results_us_election_2016 %>% filter(!state %in% results$state)
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))
mu <- 0
tau <- 0.02
results %>% mutate(sigma = sd/sqrt(n),
                   B = sigma^2/ (sigma^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV > 269)  
# Accounting for general bias
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),    # added bias_sd term
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV_2 > 269) 

# Forecasting
# select all national polls by one pollster
one_pollster <- polls_us_election_2016 %>%
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# the observed standard error is higher than theory predicts
se <- one_pollster %>%
  summarize(empirical = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se

# the distribution of the data is not normal
one_pollster %>% ggplot(aes(spread)) +
  geom_histogram(binwidth = 0.01, color = "black")
#Trend across time for several pollsters
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)

# For the candidates
polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30, 50))


# Exercise
polls<-polls_us_election_2016%>%
  filter(state != "U.S."&enddate>="2016-10-31")%>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
cis<-polls%>%
  mutate(X_hat =(spread+1)/2) %>%
  mutate(se = 2*(sqrt(X_hat*(1-X_hat)/samplesize)))%>%
  mutate(lower = spread - qnorm(0.975)*se) %>%
  mutate(upper = spread + qnorm(0.975)*se) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)
# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

p_hits<-ci_data%>%
  mutate(hit = ifelse(actual_spread>=lower&actual_spread<=upper, 1, 0))%>%
  group_by(state)%>%
  filter(n()>=5)%>%
  summarize(proportion_hits = mean(hit==1), n=n())%>%
  arrange(-proportion_hits)
p_hits%>%
  ggplot(aes(state, proportion_hits))+
  geom_bar(stat="identity")+
  coord_flip()
cis%>%
  mutate(error = spread - actual_spread)%>%
  mutate(hit = sign(spread) == sign(actual_spread))


# Load the neccessary libraries and data
library(dslabs)
library(dplyr)
data(heights)

# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a logical vector 'res' that contains the results of the simulations
res<-replicate(B{
  sample(x, N, replace=TRUE)
})

data("research_funding_rates")
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum))%>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)
totals<-totals%>%summarize(percent_men = yes_men/(yes_men+no_men),
                           percent_women = yes_women/(yes_women+no_women))
tab <- matrix(c(3,1,1,3), 2, 2)
rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab

# p-value calculation with Fisher's Exact Test
fisher.test(tab, alternative = "greater")

funding_rate <- totals%>%
  summarize(percent_total = (yes_men+yes_women)/(yes_men+no_men+yes_women+no_women)) %>%
  .$percent_total
funding_rate

two_by_two<-tibble(awarded = c("no", "yes"),
                   men = c(totals$no_men, totals$yes_men),
                   women = c(totals$no_women, totals$yes_women))
two_by_two
tibble(awarded = c("no", "yes"),
       men = (totals$no_men + totals$yes_men) * c(1-funding_rate, funding_rate),
       women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate))

chisq_test <- two_by_two %>%
  select(-awarded) %>%
  chisq.test()

# Assessment

library(tidyverse)
options(digits = 3)
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread
N<-1500

head(brexit_polls)
# Estimate x_hat considering that d=2x-1
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)
head(brexit_polls)

brexit_polls[1,]
brexit_polls$x_hat[1]-qnorm(0.975)*sqrt(0.52*(1-0.62)/4772)

brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

june_polls <- brexit_polls %>%
  filter(enddate>="2016-06-01") %>%
  mutate (se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize)) %>%
  mutate (se_spread = 2*sqrt(x_hat*(1-x_hat)/samplesize)) %>%
  mutate (lower = spread - qnorm(0.975)*se_spread) %>%
  mutate (upper = spread + qnorm(0.975)*se_spread) %>%
  mutate (hit = ifelse(-0.038>=lower&-0.038<=upper, 1, 0))

june_polls%>%ggplot(aes(poll_type, spread))+
  geom_boxplot()

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)
combined_by_type%>%
  mutate(se_hat = sqrt(p_hat*(1-p_hat)/N))%>%
  mutate(se_spread = 2*se_hat) %>%
  mutate (lower = spread - qnorm(0.975)*se_spread) %>%
  mutate (upper = spread + qnorm(0.975)*se_spread)

brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

tab<-matrix(2,2,2)
rownames(tab)<-c("Telephone", "Online")
colnames(tab)<-c("True", "False")
tab
tab<-table(brexit_hit)
chisq.test(tab)

odds_online<-(tab[1,2]/sum(tab[1,]))/(tab[1,1]/sum(tab[1,]))
odds_telephone<-(tab[2,2]/sum(tab[2,]))/(tab[2,1]/sum(tab[2,]))

odds_online/odds_telephone

brexit_polls%>%ggplot(aes(enddate, spread, color=poll_type))+
  geom_smooth(method = "loess", span=0.4)+
  geom_point()

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

brexit_long%>%ggplot(aes(enddate, proportion, color=vote))+
  geom_smooth(method = "loess", span=0.3)

