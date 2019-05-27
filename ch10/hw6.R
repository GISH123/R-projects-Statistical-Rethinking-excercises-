library(rethinking)
data(salamanders)
d<- salamanders

#center the pctcover
d$PCTCOVER_c <- d$PCTCOVER - mean(d$PCTCOVER)


model <- map( 
               alist(
                 SALAMAN ~ dpois( lambda ),
                 log(lambda) <- a + bpc*PCTCOVER_c,
                 a ~ dnorm(0,30),
                 bpc ~ dnorm(0,1)
               ),
               data=d)

precis(model)

model.stan <- map2stan(model,chains=2)


coeftab( model, model.stan ) 
plot( coeftab( model, model.stan))

compare(model,model.stan)



pairs(model)
pairs(model.stan)
#probably stan is better


# sequence of percent ground cover
cover.seq <- seq(from=0, to=100, length.out=100)

d.pred <- data.frame(
  PCTCOVER = cover.seq,
  PCTCOVER_c = cover.seq - mean(d$PCTCOVER)
)

lambda.pred <- link(model.stan, data=d.pred)
lambda.med <- apply(lambda.pred, 2, median)
lambda.PI <- apply(lambda.pred, 2, PI, prob=0.89)


# plot data and plot prediction with lines
plot( d$PCTCOVER, d$SALAMAN)

lines( cover.seq, lambda.med)
shade( lambda.PI, cover.seq)

#當percentage of cover低時，可以發現幾乎沒什麼salamanders，代表percentage of cover高，是salamanders生存的很大關鍵之一

#在percentage of cover 高的時候 ，可以看到salamanders的數量變動很大
#=>可能有其他重要的factor在影響




#觀察forestage，發現多數不到100
hist(d$FORESTAGE)



d$logFORESTAGE <- log(d$FORESTAGE + 1)

#center forestage
d$logFORESTAGE_c <- d$logFORESTAGE - mean(d$logFORESTAGE)

modelB <- map2stan( 
  alist(
    SALAMAN ~ dpois( lambda ),
    log(lambda) <- a + bpc*PCTCOVER_c + bfa*logFORESTAGE_c,
    a ~ dnorm(0,30),
    bpc ~ dnorm(0,1),
    bfa ~ dnorm(0,1)
  ),
  data=d, chains = 2)



precis(modelB)
plot(precis(modelB))

#bfa標準差很大，區間-0.21~0.11 包含了0
#看起來應該是對模型解釋沒有什麼幫助



plot(PCTCOVER ~ FORESTAGE, data=d)
#可以看到其實forestage的高低就蠻影響percentage of cover,與log forestage有高度正相關

compare(model.stan,modelB)
compare(model.stan,modelB,func=LOO)
#直接使用percentage of cover模型即可)