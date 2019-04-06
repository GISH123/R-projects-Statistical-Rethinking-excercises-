library(rethinking)
library(tidybayes)
library(dplyr)

data(Howell1)

d = Howell1

model <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- alpha + beta*a,
    alpha ~ dnorm(178,20),
    beta ~ dlnorm(0,1),
    sigma ~ dunif(0,50)
  ),
  data <- d
)


some_weights <- c(46.95, 43.72, 64.78, 32.59, 54.63)
some_weights_standard <- (some_weights - mean(d$weight)) / sd(d$weight)
simulated_heights <- sim(model, data = list(a = some_weights_standard))

simulated_heights_mean <- apply(X = simulated_heights, MARGIN = 2, FUN = mean)

#dSimulated <- mutate(d, Expected_height = simulated_heights_mean)

# EightyNine_interval <- mode_hdi(dSimulated$Expected_height, .width = 0.89)
EightyNine_interval <- apply(X = simulated_heights, MARGIN = 2, FUN = mode_hdi, .width = 0.89)

boundmin = list()
boundmax = list()
for(i in 1:length(EightyNine_interval)){
  boundmin[i]<-EightyNine_interval[[i]]$ymin
  boundmax[i]<-EightyNine_interval[[i]]$ymax
}
#boundmin and boundmax are the 89% hpdi bound of simulated heights

samples <- extract.samples(model)


individualone <- rnorm(n = trials, mean = samples$alpha + samples$beta*some_weights_standard[1], sd = samples$sigma)
individualone_mean <- mean(individualone)
individualone_hdi <- mode_hdi(individualone, .width=0.89)

individualtwo <- rnorm(n = trials, mean = samples$alpha + samples$beta*some_weights_standard[2], sd = samples$sigma)
individualtwo_mean <- mean(individualtwo)
individualtwo_hdi <- mode_hdi(individualtwo, .width=0.89)

individualthree <- rnorm(n = trials, mean = samples$alpha + samples$beta*some_weights_standard[3], sd = samples$sigma)
individualthree_mean <- mean(individualthree)
individualthree_hdi <- mode_hdi(individualthree, .width=0.89)

individualfour <- rnorm(n = trials, mean = samples$alpha + samples$beta*some_weights_standard[4], sd = samples$sigma)
individualfour_mean <- mean(individualfour)
individualfour_hdi <- mode_hdi(individualfour, .width=0.89)

individualfive <- rnorm(n = trials, mean = samples$alpha + samples$beta*some_weights_standard[5], sd = samples$sigma)
individualfive_mean <- mean(individualfive)
individualfive_hdi <- mode_hdi(individualfive, .width=0.89)

listA = c(individualone_mean,individualtwo_mean ,individualthree_mean ,individualfour_mean ,individualfive_mean)
listB = c(individualone_hdi$ymin,individualtwo_hdi$ymin,individualthree_hdi$ymin,individualfour_hdi$ymin,individualfive_hdi$ymin)
listC = c(individualone_hdi$ymax,individualtwo_hdi$ymax,individualthree_hdi$ymax,individualfour_hdi$ymax,individualfive_hdi$ymax)

answer.sheet = tibble(Individual = 1:5, Weight = some_weights, Expected_height = listA, lowerbound89hdpi = listB, upperbound89hdpi = listC)
#question 2.a
d2 = d %>% filter(age < 18)

modelB <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- alpha + beta*d2$weight,
    alpha ~ dnorm(100,50),
    beta ~ dlnorm(0,1),
    sigma ~ dunif(0,50)
  ),
  data <- d2
)
#For every 10 units of increase in weight, how much taller does the model predict a child gets?
precis(modelB)
#We can see that the mean of beta is 2.72,that means on average, if there's 10 units of increase in weight
#a child gets 2.72* ln(10) ~= 6.263 taller

#question 2.b

library(MASS)

weight_seq <- seq(from = 0 , to = 100, by = 0.5)

#makes mean
twoBsamples <- data.frame( mvrnorm(n = 10000, coef(modelB), vcov(modelB)) )
simMu_func <- function(w){
  twoBsamples$alpha + twoBsamples$beta * w
}
simMu <- sapply(X = weight_seq, FUN = simMu_func)
simMu_mean <- apply(X = simMu, MARGIN = 2, FUN = mean)
simMu_hpdi <- apply(X = simMu, MARGIN = 2, FUN = HPDI, prob = 0.89)

#makes heights
height_func <- function(w) rnorm(n = nrow(twoBsamples), mean = simMu_func(w), sd = twoBsamples$sigma)
height_samples <- sapply(X = weight_seq, FUN = height_func)
height_hpdi <- apply(X = height_samples, MARGIN = 2, FUN = HPDI, prob = .89)

#plot
plot(height ~ weight, data = d2, col = col.alpha(rangi2, .5))
lines(x = weight_seq, y = simMu_mean)
shade(object = simMu_hpdi, lim = weight_seq)
shade(object = height_hpdi, lim = weight_seq)

#question 3.a

d3 <- Howell1
modelC <- map(
  alist(
    height ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta*log(weight),
    alpha ~ dnorm(mean = 178, sd = 100),
    beta ~ dnorm(mean = 0, sd = 100),
    sigma ~ dunif(min = 0, max = 50)
  ),
  data = d3
)


weight_seq <- seq(from = 0 , to = 100, by = 0.5)

# makes mean
twoCsamples <- data.frame( mvrnorm(n = 10000, coef(modelC), vcov(modelC)) )
simMu_func <- function(w){
  twoCsamples$alpha + twoCsamples$beta *  log(w)
}
simMu <- sapply(X = weight_seq, FUN = simMu_func)
simMu_mean <- apply(X = simMu, MARGIN = 2, FUN = mean)
simMu_hpdi <- apply(X = simMu, MARGIN = 2, FUN = HPDI, prob = 0.97)

# makes heights
height_func <- function(w) rnorm(n = nrow(twoCsamples), mean = simMu_func(w), sd = twoCsamples$sigma)
height_samples <- sapply(X = weight_seq, FUN = height_func)
height_hpdi <- apply(X = height_samples, MARGIN = 2, FUN = HPDI, prob = 0.97)

#plot
plot(height ~ weight, data = d3, col = col.alpha(rangi2, 0.4))
lines(x = weight_seq, y = simMu_mean)
shade(object = simMu_hpdi, lim = weight_seq)
shade(object = height_hpdi, lim = weight_seq)