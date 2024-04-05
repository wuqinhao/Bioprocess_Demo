library(tidyverse)

#1,303	9,747	12,274	9,660	27,480	4,560	7,116	12,203	9,747	27,480

#mode: batch - batch
x <- c(0, 7, 8)
y <- c(0, 650, 1303)
x1 <- c(2000, 2000, 2000)
x2 <- c(1,1,1)
df <- data.frame(day = x, mAb = y, bioVol = x1, batchNo = x2)

model <- glm( (mAb/(mAb+0.1)) ~day, data = df, family = binomial)

days <- seq(1, 10, 0.01)
newdata = data.frame(day=days)
mAb <- predict(model, newdata, type="response")*1303

plot(days, mAb, type = "l")

#mode: fed - batch
x <- c(0, 7 ,14)
y <- c(0, 6498, 9747)
x1 <- c(2000, 2000, 2000)
x2 <- c(5,5, 5)
df <- data.frame(day = x, mAb = y, bioVol = x1, batchNo = x2)

model <- glm( (mAb/(bioVol*batchNo)) ~ day, data = df, family = binomial)
biomass <- glm( (abs(mAb-(bioVol*batchNo))/(bioVol*batchNo)) ~ day, data = df, family = binomial)

days <- seq(1, 20, 0.01)
bioVol <- rep(2000, length(days))
batchNo <- rep(6, length(days))
newdata = data.frame(day=days, bioVol = bioVol, batchNo = batchNo)
mAb <- predict(model, newdata, type="response")*9747
biomass <- predict(biomass, newdata, type="response")*9747

plot(days, biomass, type = "l")

#w/N-1-perfusion
x <- c(0, 7 ,14)
y <- c(0, 8182, 12274)
x1 <- c(2000, 2000, 2000)
x2 <- c(7,7, 7)
df <- data.frame(day = x, mAb = y, bioVol = x1, batchNo = x2)

model <- glm( (mAb/(bioVol*batchNo)) ~ day, data = df, family = binomial)

days <- seq(1, 20, 0.01)
bioVol <- rep(2000, length(days))
batchNo <- rep(7, length(days))
newdata = data.frame(day=days, bioVol = bioVol, batchNo = batchNo)
mAb <- predict(model, newdata, type="response")*12274

plot(days, mAb, type = "l")

#perfusion: basal
x <- c(0, 18 ,35)
y <- c(0, 4830, 9660)
x1 <- c(500, 500, 500)
x2 <- c(20,20, 20)
df <- data.frame(day = x, mAb = y, bioVol = x1, batchNo = x2)

model <- glm( (mAb/(bioVol*batchNo)) ~ day, data = df, family = binomial)

days <- seq(1, 20, 0.01)
bioVol <- rep(500, length(days))
batchNo <- rep(20, length(days))
newdata = data.frame(day=days, bioVol = bioVol, batchNo = batchNo)
mAb <- predict(model, newdata, type="response")*9660

plot(days, mAb, type = "l")

#perfusion: basal+feed
x <- c(0, 18 ,35)
y <- c(0, 13740, 27480)
x1 <- c(500, 500, 500)
x2 <- c(55,55, 55)
df <- data.frame(day = x, mAb = y, bioVol = x1, batchNo = x2)

model <- glm( (mAb/(bioVol*batchNo)) ~ day, data = df, family = binomial)

days <- seq(1, 20, 0.01)
bioVol <- rep(500, length(days))
batchNo <- rep(55, length(days))
newdata = data.frame(day=days, bioVol = bioVol, batchNo = batchNo)
mAb <- predict(model, newdata, type="response")*27480

plot(days, mAb, type = "l")

#concentrated fed-batch: basal+feed
x <- c(0, 9 ,18)
y <- c(0, 3040, 4560)
x1 <- c(500, 500, 500)
x2 <- c(10,10, 10)
df <- data.frame(day = x, mAb = y, bioVol = x1, batchNo = x2)

model <- glm( (mAb/(bioVol*batchNo)) ~ day, data = df, family = binomial)

days <- seq(1, 20, 0.01)
bioVol <- rep(500, length(days))
batchNo <- rep(10, length(days))
newdata = data.frame(day=days, bioVol = bioVol, batchNo = batchNo)
mAb <- predict(model, newdata, type="response")*4560

plot(days, mAb, type = "l")

#concentrated fed-batch: basal+feed+cond1
x <- c(0, 9 ,18)
y <- c(0, 4744, 7116)
x1 <- c(500, 500, 500)
x2 <- c(15,15, 15)
df <- data.frame(day = x, mAb = y, bioVol = x1, batchNo = x2)

model <- glm( (mAb/(bioVol*batchNo)) ~ day, data = df, family = binomial)

days <- seq(1, 20, 0.01)
bioVol <- rep(500, length(days))
batchNo <- rep(15, length(days))
newdata = data.frame(day=days, bioVol = bioVol, batchNo = batchNo)
mAb <- predict(model, newdata, type="response")*7116

plot(days, mAb, type = "l")

#concentrated fed-batch: basal+feed+cond2
x <- c(0, 9 ,18)
y <- c(0, 8135, 12203)
x1 <- c(500, 500, 500)
x2 <- c(25,25, 25)
df <- data.frame(day = x, mAb = y, bioVol = x1, batchNo = x2)

model <- glm( (mAb/(bioVol*batchNo)) ~ day, data = df, family = binomial)

days <- seq(1, 20, 0.01)
bioVol <- rep(500, length(days))
batchNo <- rep(25, length(days))
newdata = data.frame(day=days, bioVol = bioVol, batchNo = batchNo)
mAb <- predict(model, newdata, type="response")*12203

plot(days, mAb, type = "l")

#batch-continuous
x <- c(0, 9 ,14)
y <- c(0, 6498, 9747)
x1 <- c(2000, 2000, 2000)
x2 <- c(5,5, 5)
df <- data.frame(day = x, mAb = y, bioVol = x1, batchNo = x2)

model <- glm( (mAb/(bioVol*batchNo)) ~ day, data = df, family = binomial)

days <- seq(1, 20, 0.01)
bioVol <- rep(2000, length(days))
batchNo <- rep(5, length(days))
newdata = data.frame(day=days, bioVol = bioVol, batchNo = batchNo)
mAb <- predict(model, newdata, type="response")*9747

plot(days, mAb, type = "l")

#continuous-continuous
x <- c(0, 18 ,35)
y <- c(0, 13740, 27480)
x1 <- c(500, 500, 500)
x2 <- c(55,55, 55)
df <- data.frame(day = x, mAb = y, bioVol = x1, batchNo = x2)

model <- glm( (mAb/(bioVol*batchNo)) ~ day, data = df, family = binomial)

days <- seq(1, 20, 0.01)
bioVol <- rep(500, length(days))
batchNo <- rep(55, length(days))
newdata = data.frame(day=days, bioVol = bioVol, batchNo = batchNo)
mAb <- predict(model, newdata, type="response")*27480

plot(days, mAb, type = "l")

#cost fit - or could do gam fit later
bioVol <- c(2000,	2000,	2000,	500,	500,	500,	500,	500,	2000,	500)
day <- c(8,	14,	14,	35,	35,	18,	18,	18,	14,	35)
cost <- c(20400,	111081,	118816,	164700,	277700,	79700,	139500,	207300,	207300,	277700)
df <- data.frame(day = day, bioVol = bioVol, cost = cost)
model <- glm(cost~day+bioVol, data = df, family = gaussian)

bioVol <- seq(500, 2000, 100)
days <- seq(1, 16, 1)
days <- rep(8, length(bioVol))

newdata = data.frame(day=days, bioVol = bioVol)
costPred <- predict(model, newdata, type="response")
plot(bioVol, costPred, type = "l")

#throughput fit - or could do gam fit later
bioVol <- c(2000,	2000,	2000,	500,	500,	500,	500,	500,	2000,	500)
mAb <- c(1303,	9747,	12274,	9660,	27480,	4560,	7116,	12203,	9747, 27480)
ff <- c(5000,	5000,	5000,	5000,	5000,	5000,	5000,	5000,	5000,	5000)
df <- data.frame(ff = ff, bioVol = bioVol, mAb = mAb)
model <- glm(ff~mAb/bioVol, data = df, family = gaussian)

bioVol <- seq(500, 2000, 100)
mAb <- seq(5000, 20000, 1000)

newdata = data.frame(mAb = mAb, bioVol = bioVol)
costPred <- predict(model, newdata, type="response")
plot(bioVol, costPred, type = "l")



generateCF <- function(mode = 4, predMAB = 2000, bioVol = 2000, basal){
  outDF <- c()
  if(!(mode %in% c(1:10))){
    e <- "the mode selection is not batch nor continuous."
    base::stop(e)
  }
  bioVolRef <- c(2000,	2000,	2000,	500,	500,	500,	500,	500,	2000,	500)
  costRef <- c(261840,	261840,	261840,	242595,	242595,	261840,	261840,	261840,	261840,	242595)
  mAbTarget <- c(1303,	9747,	12274,	9660,	27480,	4560,	7116,	12203,	9747,	27480)
  endTime <- c(8,	14,	14,	28,	24,	18,	18,	18,	14,	24)
  basalRef <- c(2040,	1378,	1618,	16250,	16250,	7970,	7450,	6930,	1378,	16250)
  carbon <- c(550.1710968,	600.1710968,	650.1710968,	117.5427742,	137.5427742,	127.5427742,	147.5427742,	157.5427742,	560.1710968,	167.5427742)
  mAbCarbon <- 14.15
  
  TimeTotal <- 42
  labour <- c(12,	12,	12,	6,	6,	12,	12,	12,	12,	6)
  superLabour <- floor(labour*0.2)
  wage <- 1000
  n=50
  
  if(bioVol != bioVolRef[mode]){
    mAbTarget[mode] <- mAbTarget[mode]/bioVolRef[mode]*bioVol
  }
  if(basal > basalRef[mode]){
    basalRef[mode] <- basalRef[mode]/bioVolRef[mode]*bioVol
  }else{
    basalRef[mode] <- basal
  }
  carbonTarget <- carbon + mAbTarget*mAbCarbon/1000
  
  if(mode < 4 || mode > 5){
    x <- c(0.0001, ceiling(endTime[mode]/3*2), endTime[mode])
    y <- c(carbon[mode]/endTime[mode], carbonTarget[mode]/3*2, carbonTarget[mode])
    x1 <- rep(bioVol, n=3)
    x2 <- rep(ceiling(carbonTarget[mode]/bioVol), n=3)
    basalCol <- rep(basalRef[mode], 3)
    labourNo <- rep((labour[mode]+superLabour[mode]), n=3)
    outDF <- data.frame(day = x, carbon = y, basal = basalCol)
  }
  
  if(mode == 4 || mode == 5){
    x <- c(0.0001, ceiling(endTime[mode]/2), endTime[mode])
    y <- c(carbon[mode]/endTime[mode], carbonTarget[mode]/2, carbonTarget[mode])
    x1 <- rep(bioVol, n=3)
    x2 <- rep(ceiling(carbonTarget[mode]/bioVol), n=3)
    basalCol <- c(basal/3, basal/3*2, basal)
    labourNo <- rep((labour[mode]+superLabour[mode]), n=3)
    outDF <- data.frame(day = x, carbon = y, basal = basalCol)
  }
  
  return(outDF)
}

df <- generateCF(4, 2000, 2000, 2000)

# preliminaryResult$carbonData <- df
# modeNo <- modeNumber(input = input)
# if(modeNo==1){
model <- glm( (carbon/(carbon+0.1)) ~ day+basal, data = df, family = binomial)
# }else{
#   model <- glm( (mAb/(bioVol*batchNo)) ~ day+basal, data = carbonDF(), family = binomial)
# }
days <- seq(1, input$dayInput, 0.01)
basal <- rep(input$basalInput, length(days))

newdata = data.frame(day=days, basal = basal)
preliminaryResult$propPred <- predict(model, newdata, type="response")
carbon <- predict(model, newdata, type="response")*carbonDF()$carbon[3]
data.frame(days = days, carbon = carbon)