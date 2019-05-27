library(rethinking)
library(MASS)
data(eagles)
d<-eagles

d$pd = ifelse(d$P == "L",1,0) #pirate body dummy
d$vd = ifelse(d$V == "L",1,0) # victim body dummy
d$ad = ifelse(d$A == "A",1,0) #pirate adult dummy


m <- map(
  alist(
    y ~ dbinom(n,p),
    logit(p) <- a + bpd*pd + bvd*vd + bad*ad,
    a ~ dnorm(0,10),
    c(bpd,bvd,bad) ~ dnorm( 0 , 5)
  ), data=d )


dstan <- d

mstan <- map2stan( m , data=dstan , iter=1e4 , warmup=1000 )

precis(m)
precis(mstan)

compare(mstan,m)

pairs(m)
pairs(mstan)


p <- link(mstan)
y <- sim(mstan)

p.mean <- apply(X = p, MARGIN = 2, FUN = mean)
p.PI <- apply(X = p, MARGIN = 2, FUN = PI)
y.mean <- apply(X = y, MARGIN = 2, FUN = mean)
y.PI <- apply(X = y, MARGIN = 2, FUN = PI)

d$proportion <- d$y / d$n
plot(d$proportion, col='red', ylab="predicted probability of success") #紅為模擬出來的比例
axis(1, at=1:8, labels=c( "111","110","101","100","011","010","001","000" ))
points( 1:8 , p.mean ) #黑為預測出來的比例
for ( i in 1:8 ) lines( c(i, i), p.PI[,i] )

plot(d$y, col='red', ylab="number of successes") #紅為模擬出來的比例
axis(1, at=1:8, labels=c( "111","110","101","100","011","010","001","000" ))
points( 1:8 , y.mean ) #黑為預測出來的比例
for ( i in 1:8 ) lines( c(i, i), y.PI[,i] )



mstan.i <- map2stan(
  alist(
    y ~ dbinom(n, p),
    logit(p) <-  a + bpd*pd + bvd*vd + bad*ad + bpa*pd*ad,
    a ~ dnorm(0,10),
    c(bpd,bvd,bad,bpa) ~ dnorm( 0 , 5)
  ),
  data = dstan, warmup = 1000, iter = 1e4
)

compare(mstan, mstan.i)
precis(mstan.i)
precis(mstan)
