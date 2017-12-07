# Let's start from the loading of necessary packages
library(plm)
library(panelAR)
library(dplyr)
library(stats)
library(lmtest)
library(sjPlot)
library(sjmisc)
library(devtools)
ss1<- read.table(file = "ss1.txt", header = TRUE)
attach(ss1)
# Здесь надо добавить лагированные переменные
ss1_bad <- mutate(ss4, laglagged = dplyr::lag(lagged_good2), lagelec = dplyr::lag(elecpartyw), lagright = dplyr::lag(strength_of_right_party), lagfor = dplyr::lag(foreigners), lagpart_on_the_right = dplyr::lag(part_on_right), lagparty_elect = dplyr::lag(party_elect), laggedmil = dplyr::lag(meyermiller))
as.integer(Data, rdata)
Y <- cbind(importance_of_issue)
X <- cbind(laglagged, lagged_good2, lag_bis_nis, bi_nich_one, lagelec, elecpartyw, lagright, strength_of_right_party, lagfor, foreigners, party_ordanizme)

rdata$date<- as.integer(date, rdata)
rdata$date <- make.unique(date)
duplicated(rdata)
rdata[duplicated(rdata), ]
rdata[!duplicated(rdata), ]
rdata[!duplicated(rdata[,1:2]),]
rdata %>% distinct(party_code, Data)
f <-rdata %>% distinct(party_code, Data, .keep_all = TRUE)





#Сначала ordinary least squares
ols<-lm(Y ~ X + party_name - 1, data=rdata)
summary(ols)
#тут есть график, 
yhat <- ols$fitted
plot(rdata$meyermiller, rdata$good_or_bad_attitude, pch=19, xlab="нишевость", ylab="позиция партии")
abline(lm(rdata$meyermiller~rdata$good_or_bad_attitude),lwd=3, col="red")



#Далее изучим метод наименьших квадратов с дамми переменными
fixed.dum <-lm(Y ~ X + niche + country - 1, data=rdata)
summary(fixed.dum)
yhat <- fixed.dum$fitted
library(car)
scatterplot(yhat~rdata$importance_of_issue|rdata$country, boxplots=FALSE, xlab="важность миграционного измерения", ylab="yhat",smooth=FALSE)
abline(lm(rdata$importance_of_issue~rdata$strength_of_right_party),lwd=3, col="red")
scatterplot(Y~Data|country, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=rdata)
coplot(Y ~ Data|country, type="l", data=rdata)
coplot(Y ~ Data|country, type="b", data=rdata)

library(gplots)
plotmeans(Y ~ country, main="Heterogeineity across countries", data=rdata)
plotmeans(Y ~ Data, main="Heterogeineity across years", data=rdata)



library(apsrtable)
apsrtable(ols,fixed.dum, model.names = c("OLS", "OLS_DUM")) # Displays a table in Latex 



fixed <- plm(Y ~ X + niche + country - 1, data=rdata, index=c("party_code", "Data"), model="within")
summary(fixed)


#Далее идет тест на то, какая именно модель лучше: с фиксированными эффектами или  ОЛС.
#Если р-значение меньше 0.05, модель с фиксированными эффектами лучше
pFtest(fixed, ols) # Testing for fixed effects, null: OLS better than fixed


#Теперь модель со случайными эффектами
random <- plm(Y ~ X + niche + country - 1, data=rdata, index=c("party_code", "Data"), model="random")
summary(random)

#Проверяем, какая модель лучше: фикси или рэндом. Если р-значение меньше 0.05, ты выбираем модель с фиксированными эффектами
phtest(fixed, random)



pool <- plm(Y ~ X + niche + country - 1, data=rdata, index=c("party_code", "Data"), model="pooling")
summary(pool)


#Если р-значение меньше 0.05, то делаем выбор в пользу random, nor OLS
plmtest(pool, type=c("bp"))
pbgtest(fixed)




#Далее тест Бройша-Пагана. Если р-значение меньше 0.05, то есть гетероскедастичность
library(lmtest)
bptest(Y ~ X + niche + country - 1, data = rdata, studentize=F)
pcdtest(fixed, test = c("cd"))


#Если снова р-значение меньше 0.05, то есть корреляция
pbgtest(fixed)


#Мы выбираем модель с фиксированными эффектами, потому что того требуют результаты теста
fixed <- plm(Y ~ X + niche + country - 1, data=rdata, index=c("party_code", "Data"), model="within")
coeftest(fixed) # Original coefficients
coeftest(fixed, vcovHC) # Heteroskedasticity consistent coefficients
coeftest(fixed, vcovHC(fixed, method = "arellano")) # Heteroskedasticity consistent coefficients (Arellano)
coeftest(fixed, vcovHC(fixed, type = "HC3")) # Heteroskedasticity consistent coefficients, type 3

#Функция vcovHAC снова не работает в панельной регрессии
vcovHAC(fixed)  
coeftest(model, vcov. = vcovHAC(fixed))





random <-plm(Y ~ X + niche + country - 1, data=rdata, index=c("party_code", "Data"), model="random")
coeftest(random) # Original coefficients
coeftest(random, vcovHC) # Heteroskedasticityconsistent coefficients
coeftest(random, vcovHC(random, type = "HC3")) # Heteroskedasticityconsistent coefficients


#теперь проверим, как ведет себя модель с фиксированными эффектами, которая учитывает наличие дамми-переменных
fixed.dum <-lm(Y ~ X + niche + country - 1, data=rdata)
coeftest(fixed.dum) # Original coefficients
coeftest(fixed.dum, vcovHC) # Heteroskedasticity consistent coefficients
coeftest(fixed.dum, vcovHC(fixed, method = "arellano")) # Heteroskedasticity consistent coefficients (Arellano)
coeftest(fixed.dum, vcovHC(fixed, type = "HC3")) # Heteroskedasticity consistent coefficients, type 3







library(plm)
WorkingVersionLefts<- read.table(file = "WorkingVersionLefts.txt", header = TRUE)
attach(WorkingVersionLefts)
Y <- cbind(good_or_bad_attitude)
X <- cbind(foreigners, strength_of_right_party, meyermiller, elecpartyw, lagged_good2, party_ordanizme, party_elect, part_on_right)
rdata<- plm.data(ss1, index=c("party_code","Data"))
ols <- lm(Y ~ X, data=rdata)
summary(ols)
library(pcse)
ols <- lm(Y ~ X + as.factor(Data), data=rdata)
summary(ols)
rdata.pcse <- pcse(ols, groupN = rdata$party_code, groupT = rdata$Data, pairwise = TRUE)
summary(rdata.pcse)

fixed.dum <-lm(Y ~ X + party_name - 1, data=rdata)
summary(fixed.dum)
library(apsrtable)
apsrtable(ols,fixed.dum, model.names = c("OLS", "OLS_DUM")) # Displays a table in Latex 
fixed <- plm(Y ~ X  + party_name - 1, data=rdata, index=c("party_code", "Data"), model = "within")
summary(fixed)
fixed.time <- plm(Y ~ X  + as.factor(Data), data=rdata, index=c("party_code", "Data"))
summary(fixed)
pFtest(fixed.time, fixed)
pFtest(fixed, ols) # Testing for fixed effects, null: OLS better than fixed
random <- plm(Y ~ X + party_name - 1, data=rdata, index=c("party_code", "Data"), model="random")
summary(random)
phtest(fixed, random)
pool <- plm(Y ~ X + party_name - 1, data=rdata, index=c("party_code", "Data"), model="pooling")
summary(pool)
plmtest(pool, type=c("bp"))
phtest(fixed, random)
#Тут будет Прайс-Уинстон
panelAR(pool, rdata, panelVar = "party_code", timeVar = "Data", autoCorr = c("ar1"), panelCorrMethod = c("pwls"), rhotype ="breg", bound.rho = FALSE, rho.na.rm = FALSE, panel.weight = c("t-1", "t"), dof.correction = TRUE, complete.case = FALSE, seq.times = FALSE, singular.ok=TRUE)
prais.winsten(Y ~ X + country - 1,data=rdata)
library(prais)


pbgtest(fixed)
library(lmtest)
bptest(Y ~ X + party_name - 1, data = rdata, studentize=F)
pcdtest(pool, test = c("cd"))
pbgtest(pool)
pbgtest(fixed)
random <-plm(Y ~ X + niche + country - 1, data=rdata, index=c("party_code", "Data"), model="random")
coeftest(random) # Original coefficients
coeftest(random, vcovHC) # Heteroskedasticityconsistent coefficients
coeftest(random, vcovHC(random, type = "HC3")) # Heteroskedasticityconsistent coefficients
pool_country <- plm(Y ~ X + country - 1, data=rdata, index=c("party_code", "Data"), model="pooling")
summary(pool_country)
coeftest(pool_country)
coeftest(pool_country,  vcovBK(pool_country))
coeftest(pool, vcov=vcovBK)
coeftest(pool_country, vcov=function(x) vcovBK(x, type="HC1"))
## idem, cluster by time period
## (robust vs. cross-sectional correlation)
coeftest(pool_country, vcov=function(x) vcovBK(x,
                                     type="HC1", cluster="time"))
## idem with parameters, pass vcov as a matrix argument
coeftest(pool, vcov=vcovBK(pool, type="HC1"))


coeftest(pool_country, vcov = vcovBK(pool_country, type = "HC3"))
pwtest(Y ~X + niche + country -1, data=rdata)
pbltest(Y ~X + country -1, data=rdata, alternative="onesided")
random <-plm(Y ~ X + niche + country - 1, data=rdata, index=c("party_code", "Data"), model="random")
resettest(random)
fixed <- plm(Y ~ X  + country - 1, data=rdata, index=c("party_code", "Data"), model = "within")
coeftest(fixed)
coeftest(fixed, vcovHC(fixed, method = "arellano"))
fixed.time <- plm(Y ~ X + country - 1 + as.factor(Data), data=rdata, index=c("party_code","Data"), model="within")
summary(fixed.time)
# I use party ad dummy variable
pooled_party <- plm(Y ~ X + party_name - 1, data=rdata, index=c("party_code", "Data"), model="pooling")
summary(pooled_party)
coeftest(pooled_party, vcov=vcovBK(pooled_party, type="HC1"))


coeftest(pooled_party, vcov = vcovBK(pooled_party, cluster="time",type = "HC3"))
plm1 <- coeftest(pooled_party, vcov = vcovBK(pooled_party, cluster="time",type = "HC3"))
rdata$good_or_bad_attitude <- plm:::predict.plm(pooled_party, rdata)

ggplot(rdata, aes(x=meyermiller, y=good_or_bad_attitude)) +
  geom_point() +
  facet_wrap(rural ~ married) 
plot(marginal_effects(pooled_party), ask = FALSE)
