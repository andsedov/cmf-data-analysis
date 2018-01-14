train <- read.csv('C:\\Users\\asedov001\\Documents\\CMF\\Anomaly Detection\\AD_comp_train.csv')
X <- train[c('X', 'X.1')]
y <- train['y']
plot (train$X, train$X.1, col=ifelse(train$y<1, 'black', 'red'))

m <- nrow(X)
# íîìåğà «àíîìàëüíûõ» íàáëşäåíèé
anom.obs<-(1:m)[y==1] 
la <- length(anom.obs)
m.cv <- round(0.2*(m-la)) + la 
m.train<- m-m.cv
# íîìåğà ıêçàìåíóşùåé è îáó÷àşùåé âûáîğîê
cv.obs <- c(sample((1:m)[-anom.obs], size=m.cv-la, replace=FALSE), anom.obs)

train.obs<-(1:m)[-cv.obs]
# ğàçäåëåíèå âûáîğêè
X.train <- X[train.obs,] 
X.cv <- X[cv.obs,]
y.train<-y[train.obs,]
y.cv <-y[cv.obs,]

# îöåíêè ïàğàìåòğîâ
mu <-apply(X.train, 2, mean)
sigma <-apply(X.train, 2, sd)
# ôóíêöèÿ «âåğîÿòíîñòè»
p <-function(X,mu,sigma) {
  m <-nrow(X); n <-ncol(X)
  prob<-matrix(nrow=m,ncol=n)
  for (j in 1:n) prob[,j] <-dnorm(X[,j],mu[j],sigma[j])
  apply(prob, 1, prod)
}
# îïğåäåëåíèå «âåğîÿòíîñòåé»
prob.train<-p(X.train,mu,sigma)
prob.cv <-p(X.cv,mu,sigma)


pr<-range(prob.cv) # ãğàíèöû âîçìîæíûõ çíà÷åíèé ????
res <-NULL# â íå¸ áóäóò ñîõğàíÿòüñÿ ğåçóëüòàòû ìîäåëèğîâàíèÿ
# äëÿ êàæäîãî íàáëşäåíèÿ ıêçàìåíóşùåé âûáîğêè ğàññ÷èòûâàåì
# ïğîãíîç ïğè îïğåäåë¸ííîì çíà÷åíèè è ñğàâíèâàåì åãî ñ ôàêòîì

#df <- data.frame(act=y.cv,prd=y.pred)
#precision <- nrow(df[df$prd==0 & df$act==0,])/(nrow(df[df$prd==0 & df$act==0,])+nrow(df[df$prd==0 & df$act==1,]))
#recall <- nrow(df[df$prd==0 & df$act==0,])/(nrow(df[df$prd==0 & df$act==0,])+nrow(df[df$prd==1 & df$act==0,]))

#library(caret)
fitStats<-function(y,y.pred) {
  
  df <- data.frame(act=y,prd=y.pred)
  precision <- nrow(df[df$prd==0 & df$act==0,])/(nrow(df[df$prd==0 & df$act==0,])+nrow(df[df$prd==0 & df$act==1,]))
  recall <- nrow(df[df$prd==0 & df$act==0,])/(nrow(df[df$prd==0 & df$act==0,])+nrow(df[df$prd==1 & df$act==0,]))
  f1.score <- (2 * precision * recall) / (precision + recall)
  if (precision + recall == 0) f1.score <-0
  stat <-c(precision,recall,f1.score)
  names(stat) <-c("precision","recall","f1.score")
  stat
}
for (eps in seq(pr[1], pr[2], length = 1000)) {
  y.pred<-1*(prob.cv < eps)
  res <-rbind(res, c(eps, fitStats(y.cv, y.pred)))
}

dimnames(res)[[2]][1] <- "epsilon" # çàãîëîâêè
# âûáîğ íàèáîëåå ïîäõîäÿùåãî ????
j <- which.max(res[,"f1.score"])
eps <- res[j,"epsilon"]
# îêîí÷àòåëüíûé ïğîãíîç
y.pred <-1 *(prob.cv < eps)
y.pred





test <- read.csv('C:\\Users\\asedov001\\Documents\\CMF\\Anomaly Detection\\AD_comp_test.csv')
test
#X.test <- test[c('X', 'X.1')]
#y <- test['y']
plot (test$V1, test$V2)

mu <-apply(test, 2, mean)
sigma <-apply(test, 2, sd)
prob.test<-p(test,mu,sigma)
pr<-range(prob.test) # ãğàíèöû âîçìîæíûõ çíà÷åíèé ????
res <-NULL

for (eps in seq(pr[1], pr[2], length = 1000)) {
  y.pred<-1*(prob.cv < eps)
  res <-rbind(res, c(eps, fitStats(y.cv, y.pred)))
}

dimnames(res)[[2]][1] <- "epsilon" # çàãîëîâêè
# âûáîğ íàèáîëåå ïîäõîäÿùåãî ????
j <- which.max(res[,"f1.score"])
eps <- res[j,"epsilon"]
# îêîí÷àòåëüíûé ïğîãíîç
y.pred <-1 *(prob.test < eps)
y.pred
result <- cbind(0:499, y.pred)

# Set the working directory
setwd("C:\\Users\\asedov001\\Documents\\CMF\\Anomaly Detection\\")
write.csv(result, file = "Anomaly.csv", row.names=FALSE)
