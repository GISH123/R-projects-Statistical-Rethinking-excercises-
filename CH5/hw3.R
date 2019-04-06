library(rethinking)
data("foxes")
foxData <- foxes

#Q1

modelA <- map(
  alist(
    weight ~ dnorm(mu,sigma),
    mu <- alpha + bArea * area,
    alpha ~ dnorm(0,5),
    bArea ~ dnorm(0,2),
    sigma ~ dunif(0,4)
  ),
  data = foxData
)
plot(weight ~ area, data = foxData, col=rangi2)

areaSeq <- seq(from = 0, to = 6, length.out = 50)
mu <- link(modelA, data = data.frame(area = areaSeq))
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI, prob = 0.95)

abline(modelA)
shade(object = mu.PI, lim = areaSeq)

modelB  <- map(
  alist(
    weight ~ dnorm(mu,sigma),
    mu <- alpha + bgroupsize * groupsize,
    alpha ~ dnorm(0,5),
    bgroupsize ~ dnorm(0,5),
    sigma ~ dunif(0,4)
  ),
  data = foxData
)
plot(weight ~ groupsize, data = foxData, col=rangi2)

groupsizeSeq <- seq(from = 0, to = 10, length.out = 100)
mu <- link(modelB, data = data.frame(groupsize = groupsizeSeq))
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI, prob = 0.95)

abline(modelB)
shade(object = mu.PI, lim = seq)

# 從兩張圖來看，資料都非常散佈，所畫出的回歸與區間只包含到一點點的資料
# 以area來說，資料幾乎看不出什麼關係，所求的回歸線斜率(bArea幾乎為0，得出area與weight從此張圖來看沒有顯著線性關係
# 以groupsize來說，groupsize為discrete資料，且看起來呈現散狀，看起來也與weight沒什麼關係，所求回歸線斜率(bgroupsize)也接近0，得出groupsize與weight從此張圖來看沒有顯著線性關係

#Q2

model2A <- map(
  alist(
    weight ~ dnorm(mu,sigma),
    mu <- alpha + bArea * area + bgroupsize * groupsize,
    alpha ~ dnorm(0,5),
    bArea ~ dnorm(0,2),
    bgroupsize ~ dnorm(0,5),
    sigma ~ dunif(0,4)
  ),
  data = foxData
)

#predict from area while holding groupsize constant
predictFromArea.data <- data.frame(
  area = areaSeq,
  groupsize = mean(foxData$groupsize)
)

mu <- link(model2A, data = predictFromArea.data)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI, prob = 0.95)


plot(weight ~ area, data = foxData)
lines(x = areaSeq, y = mu.mean)
shade(object = mu.PI, lim = areaSeq)

#predict from grouspzie while holding area constant
predictFromGroupsize.data <- data.frame(
  area = mean(foxData$area),
  groupsize = groupsizeSeq
)

mu <- link(model2A, data = predictFromGroupsize.data)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI, prob = 0.95)


plot(weight ~ groupsize, data = foxData)
lines(x = groupsizeSeq, y = mu.mean)
shade(object = mu.PI, lim = groupsizeSeq)

#從結果可以發現兩者回歸斜率的絕對值變大了，把另一個預測變數設為不變的情況下，該變數對weight的影響會比較顯著
#可以猜測兩者(area與groupsize)之間可能有很大的相關(correlation)，兩者變數對weight的影響卻為一正一負，剛好抵銷單一變數對weight的影響，使得模型只放單一變數的時候，斜率會非常小

#Q3

#body weight as an additive function of avgfood and groupsize
model3A <- map(
  alist(
    weight ~ dnorm(mu,sigma),
    mu <- alpha + bAvgfood * avgfood + bgroupsize * groupsize,
    alpha ~ dnorm(0,5),
    bAvgfood ~ dnorm(0,1),
    bgroupsize ~ dnorm(0,5),
    sigma ~ dunif(0,4)
  ),
  data = foxData
)

avgfoodSeq <- seq(from = 0, to = 2, length.out = 100)

predictFromFoodAndSize.data <- data.frame(
  avgfood = avgfoodSeq,
  groupsize = groupsizeSeq
)

mu <- link(model3A, data = predictFromFoodAndSize.data)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI, prob = 0.95)


plot(weight ~ avgfood, data = foxData)
lines(x = avgfoodSeq, y = mu.mean)
shade(object = mu.PI, lim = avgfoodSeq)

#body weight as an additive function of all three variables, avgfood and groupsize and area
model3B <- map(
  alist(
    weight ~ dnorm(mu,sigma),
    mu <- alpha + bArea * area + bAvgfood * avgfood + bgroupsize * groupsize,
    alpha ~ dnorm(0,5),
    bArea ~ dnorm(0,2),
    bAvgfood ~ dnorm(0,1),
    bgroupsize ~ dnorm(0,5),
    sigma ~ dunif(0,4)
  ),
  data = foxData
)

predictFromThree.data <- data.frame(
  area = areaSeq,
  avgfood = avgfoodSeq,
  groupsize = groupsizeSeq
)

mu <- link(model3B, data = predictFromThree.data)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI, prob = 0.95)

plot(weight ~ avgfood, data = foxData)
lines(x = avgfoodSeq, y = mu.mean)
shade(object = mu.PI, lim = avgfoodSeq)


# plot weight from average food, holding group size and area at mean constantly
predictFromFoodTwo.data <- data.frame(
  area = mean(d$area),
  groupsize = mean(d$groupsize),
  avgfood = avgfoodSeq
)

mu <- link(model3B, data = predictFromFoodTwo.data)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI, prob = 0.95)

plot(weight ~ avgfood, data = foxData)
lines(x = avgfoodSeq, y = mu.mean)
shade(object = mu.PI, lim = avgfoodSeq)
title('predict from avgfood while\n holding others constant')

#  plot weight from area, holding group size and avgfood at mean constantly
predictFromAreaTwo.data <- data.frame(
  area = areaSeq,
  groupsize = mean(d$groupsize),
  avgfood = mean(d$avgfood)
)

mu <- link(model3B, data = predictFromAreaTwo.data)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI, prob = 0.95)

plot(weight ~ area, data = foxData)
lines(x = areaSeq, y = mu.mean)
shade(object = mu.PI, lim = areaSeq)
title('predict from area while\n holding others constant')


plot(precis(model3A))
plot(precis(model3B))