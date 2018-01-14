library(plm)

data(EmplUK)
head(EmplUK)

#Статические модели

model.formula<-log(emp) ~ log(wage) + log(capital)

##Объединенная регрессия
empl.ls<-plm(model.formula,data=EmplUK, index=c("firm","year"), model="pooling", effect="individual")
summary(empl.ls)

##Фиксированные эффекты
empl.ls<-plm(model.formula,data=EmplUK, index=c("firm","year"), model="within", effect="individual")
summary(empl.ls)

##Случайные эффекты
empl.ls<-plm(model.formula,data=EmplUK, index=c("firm","year"), model="random", effect="individual")
summary(empl.ls)

##Объединенная регрессия дает лучший результат r-squared и rss

pooltest(model.formula,data=EmplUK, index=c("firm","year"), model="within", effect="individual")

plmtest(model.formula,data=EmplUK, index=c("firm","year"), effect="individual", type="bp")

phtest(model.formula,data=EmplUK, index=c("firm","year"), model=c("within","random"), effect="individual")

#Динамическая модель
ar.model<-dynformula(log(emp) ~ log(wage) + log(capital),lag.form=list(1,0,0))
empl.argmm<-pgmm(ar.model,data=EmplUK,index=c("firm","year"),gmm.inst=~log(emp) ~ log(wage) + log(capital))
summary(empl.argmm)


