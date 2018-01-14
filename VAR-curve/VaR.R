# Set the working directory
setwd("~/")

data <- cbind(1:200, read.csv('~/returns.csv'))
r1 <- data['r1']
T_r1 <- length(r1)-1
print(r1)
library(fBasics)
plot(data$`1:200`,data$r1) # график
basicStats(r1) # статистики
histPlot(timeSeries(r1))# гистограмма
acf(r1)

qqnormPlot(r1) # график квантиль-квантиль
jarqueberaTest(r1)

library(ghyp)
fit.ghypuv(r1,symmetric=FALSE,silent=TRUE)

r1.ghyp <- fit.ghypuv(r1,symmetric=FALSE,silent=TRUE)
hist(r1.ghyp) # гистограмма
qqghyp(r1.ghyp) # график квантиль-квантиль

aic.uv <-stepAIC.ghyp(r1,dist=c("gauss","t","ghyp"),symmetric=NULL,silent=TRUE)#критерий Акаике
summary(aic.uv$best.model)# статистики по модели

r1.t <- fit.tuv(r1,symmetric=TRUE,silent=TRUE)
hist(r1.t) # гистограмма
qqghyp(r1.t) # график квантиль-квантиль
?coredata
alpha <- 0.0
VaR_1 <-numeric()
TrainSize <- 100
TotalSize <- length(r1$r1)
for (i in (TrainSize):(TotalSize-1)) {
  d <- r1$r1[(i-TrainSize+1):i]
  Model <- fit.tuv(d, symmetric=TRUE, silent=TRUE)
  VaR_1[i-TrainSize+1] <- qghyp(alpha, object=Model)
}

plot(r1$r1[(TrainSize+1):(TotalSize)], type='l')
lines(VaR_1,col='red')

#r2
VaR_2 = numeric()
r2 <- data['r2']
for (i in (TrainSize):(TotalSize-1)) {
  d <- r2$r2[(i-TrainSize+1):i]
  Model <- fit.tuv(d, symmetric=TRUE, silent=TRUE)
  VaR_2[i-TrainSize+1] <- qghyp(alpha, object=Model)
}

plot(r2$r2[(TrainSize+1):(TotalSize)], type='l')
lines(VaR_2,col='red')

#r3
VaR_3 = numeric()
r3 <- data['r3']
for (i in (TrainSize):(TotalSize-1)) {
  d <- r3$r3[(i-TrainSize+1):i]
  Model <- fit.gaussuv(d)
  VaR_3[i-TrainSize+1] <- qghyp(alpha, object=Model)
}

plot(r3$r3[(TrainSize+1):(TotalSize)], type='l')
lines(VaR_3,col='red')

VaR_Total <- c(VaR_1,  VaR_2,  VaR_3)
print(VaR_Total)

VaR_Total <- cbind(0:299, VaR_Total)
VaR_Total
write.csv(VaR_Total, file = "VaR.csv", row.names=FALSE)

aic.uv <-stepAIC.ghyp(r2,dist=c("gauss","t","ghyp"),symmetric=NULL,silent=TRUE)#критерий Акаике
summary(aic.uv$best.model)# статистики по модели
aic.uv <-stepAIC.ghyp(r3,dist=c("gauss","t","ghyp"),symmetric=NULL,silent=TRUE)#критерий Акаике
summary(aic.uv$best.model)# статистики по модели
