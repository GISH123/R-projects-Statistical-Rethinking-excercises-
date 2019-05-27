data(rugged)

#Q1
#(a)
d <- rugged
dnoS <- rugged[!(d$country=="Seychelles"),]   #remove Seychelles
dnoS$loggdp <- log(dnoS$rgdppc_2000)

dF <- dnoS[complete.cases(dnoS$loggdp),]

modelAIS <- map(
  alist(
    loggdp ~ dnorm (mu,sigma),
    mu <- a + bA * cont_africa +bR * rugged +bAR * cont_africa * rugged,
    a ~ dnorm (0,10),
    bA ~ dnorm(0,10),
    bR ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dunif(0,30)
    
  ),
  data = dF
)

print(modelAIS)

d$loggdp <- log(d$rgdppc_2000)
d <- d[complete.cases(d$loggdp),]

modelAI <- map(
  alist(
    loggdp ~ dnorm (mu,sigma),
    mu <- a + bA * cont_africa +bR * rugged +bAR * cont_africa * rugged,
    a ~ dnorm (0,10),
    bA ~ dnorm(0,10),
    bR ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dunif(0,30)
    
  ),
  data = d
)

print(modelAI)

# 有Seychelles的資料比沒Seychelles的資料多了約0.1的bAR(即交互作用的係數)，可見納入Seychelles，使continents影響(ruggedness對於loggdp的效果)增加了0.1。

#(b)

ruggedSeq = seq(from=0, to=7,length.out=100)
mu.NotAfrica <- link(modelAI,data = data.frame(rugged = ruggedSeq,cont_africa = 0))
mu.Africa <- link(modelAI,data = data.frame(rugged = ruggedSeq,cont_africa = 1))
mu.NotAfrica.mean <- apply(X = mu.NotAfrica, MARGIN = 2, FUN = mean)
mu.NotAfrica.PI <- apply(X = mu.NotAfrica, MARGIN = 2, FUN = PI, prob = 0.97)
mu.Africa.mean <- apply(X = mu.Africa, MARGIN = 2, FUN = mean)
mu.Africa.PI <- apply(X = mu.Africa, MARGIN = 2, FUN = PI, prob = 0.97)


#with Seychelles,plot Africa regression
d.A1 <- d[ d$cont_africa==1 , ] 
d.A0 <- d[ d$cont_africa==0 , ]

plot(loggdp ~ rugged, data=d.A1)
lines( ruggedSeq , mu.Africa.mean )
shade(object = mu.Africa.PI,lim = ruggedSeq)
mtext( "African nations with S" , 3 )

plot(loggdp ~ rugged, data=d.A0)
lines( ruggedSeq , mu.NotAfrica.mean )
shade(object = mu.NotAfrica.PI,lim = ruggedSeq)
mtext( "Non-African nations with S" , 3 )

#without Seychelles,plot africa regression
dF.A1 <- dF[ dF$cont_africa==1 , ] 
dF.A0 <- dF[ dF$cont_africa==0 , ]

plot(loggdp ~ rugged, data=dF.A1)
lines( ruggedSeq , mu.Africa.mean )
shade(object = mu.Africa.PI,lim = ruggedSeq)
mtext( "African nations without S" , 3 )

plot(loggdp ~ rugged, data=dF.A0)
lines( ruggedSeq , mu.NotAfrica.mean )
shade(object = mu.NotAfrica.PI,lim = ruggedSeq)
mtext( "Non-African nations without S" , 3)

#africa的rugged斜率為正，表rugged越大gdp越大。non-africa的rugged斜率為負，表rugged越大gdp越小。
#而且其實Non-African nations with S = Non african nations without S 因為non african nation本來就沒有Seychelles國家
#由Africa nations with S 與without S看的出來其實變化很小，斜率只變大一點點，因為Seychelles只佔1/233筆資料


#(c)

modelc1 <- map(
  alist(
    loggdp ~ dnorm (mu,sigma),
    mu <- a + bR * rugged ,
    a ~ dnorm (0,10),
    bR ~ dnorm(0,10),
    sigma ~ dunif(0,30)
    
  ),
  data = dF
)

modelc2 <- map(
  alist(
    loggdp ~ dnorm (mu,sigma),
    mu <- a + bA * cont_africa +bR * rugged ,
    a ~ dnorm (0,10),
    bA ~ dnorm(0,10),
    bR ~ dnorm(0,10),
    sigma ~ dunif(0,30)
    
  ),
  data = dF
)

modelc3 <- map(
  alist(
    loggdp ~ dnorm (mu,sigma),
    mu <- a + bA * cont_africa +bR * rugged +bAR * cont_africa * rugged,
    a ~ dnorm (0,10),
    bA ~ dnorm(0,10),
    bR ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dunif(0,30)
    
  ),
  data = dF
)

compare(modelc1,modelc2,modelc3)

pdata <- data.frame(rugged = ruggedSeq, cont_africa = 1)
mu.ensemble <- ensemble(modelc1,modelc2,modelc3,data = pdata)
mu.mean <- apply(X = mu.ensemble$link, MARGIN = 2, FUN = mean)
mu.PI <- apply(X = mu.ensemble$link, MARGIN = 2, FUN = PI)
plot(loggdp ~ rugged, data=dF.A1)
lines( ruggedSeq, mu.mean )
#lines( ruggedSeq, mu.PI[1,], lty=2 )
#lines( ruggedSeq, mu.PI[2,], lty=2 )
shade(object = mu.PI,lim = ruggedSeq)

#可發現斜率幾乎等於零，rugged與loggdp變成幾乎沒關係
#(b)小題使用的是完整modelc3模型，也就是納入交互作用模型。但此c小題只使用80%的交互作用模型，20%沒有交互作用
#因此讓交互作用比較沒效果，由a小題可以知道africa其實會讓rugged對gdp有正向影響，但是如果為non-africa則讓rugged對gdp有負向影響
#c小題這邊把continent africa固定為1，即只看africa，然後又消除一些交互作用，此交互作用(只要是africa，就會讓rugged對gdp有正向影響) 的效果被消除了一些，因此斜率變小。

#Q2
data(nettle)
d <- nettle
d$lang.per.cap <- (d$num.lang / d$k.pop)
d$loglpc <- log(d$lang.per.cap)

#(a)

d$logarea <- log(d$area)

model2A <- map(
  alist(
    loglpc ~ dnorm(mu,sigma),
    mu <- a + bA * logarea + bMGS * mean.growing.season,
    a ~ dnorm(0,10),
    bA ~ dnorm(0,5),
    bMGS ~ dnorm(5,5), # because growing season與lang per cap為正向關係(依題意)
    sigma ~ dunif(0,2)
    
  ),data = d
               )

precis(model2A)
#看的出來mean growing season之係數(bMGS)為正，且95%信賴區間落在0.05~0.23均大於零，有很明顯的正向關係

#(b)
model2B <- map(
  alist(
    loglpc ~ dnorm(mu,sigma),
    mu <- a + bA * logarea + bSGS * sd.growing.season,
    a ~ dnorm(0,10),
    bA ~ dnorm(0,5),
    bSGS ~ dnorm(-5,5), # because standard deviation of growing season與lang per cap為負向關係(依題意)
    sigma ~ dunif(0,2)
    
  ),data = d
)
precis(model2B)
#看的出來sd growing season之係數(bSGS)為負，且95%信賴區間落在0.08 ~ -0.51，有還算明顯的負向關係

#(c)
model2C <- map(
  alist(
    loglpc ~ dnorm(mu,sigma),
    mu <- a + bA * logarea + bMGS * mean.growing.season + bSGS * sd.growing.season +bI *mean.growing.season*sd.growing.season,
    a ~ dnorm(0,10),
    bA ~ dnorm(0,5),
    bMGS ~ dnorm(5,5),
    bSGS ~ dnorm(-5,5), # because standard deviation of growing season與lang per cap為負向關係(依題意)
    bI ~ dnorm(-2,2), #交互作用為負相關(依題意)，但我認為影響沒有mgs sgs大
    sigma ~ dunif(0,2)
    
  ),data = d
)
precis(model2C)
