rm(list=ls())
hs=read.csv("hous.csv")
dim(hs)
str(hs)
summary(hs[,-c(1,2)])

library(png)
library(ggplot2)
library(grid)
library(dplyr)
cal=png::readPNG("map.png")
hs%>%ggplot(aes(longitude,latitude,col=median_house_value))+
  annotation_custom(rasterGrob(cal,width = unit(1,"npc"),height = unit(1,"npc")),-Inf, Inf, -Inf, Inf) +
  geom_point() + scale_color_gradientn(colours = c("blue","green","red"))

hs%>%ggplot(aes(median_house_value))+geom_histogram(bins=20,col="black",fill="blue")

hs$value=ifelse(hs$median_house_value<=100000,"below 100k",ifelse(hs$median_house_value >100000 & hs$median_house_value <=250000,"btw 100k and 250k","above 250k"))
hs$value=as.factor(hs$value)

hs$ocean_proximity[hs$ocean_proximity=="ISLAND"]="NEAR BAY"
summary(hs)


out=boxplot(hs$total_rooms)$out
hs[hs$total_rooms %in% out,"total_rooms"] = NA
summary(hs)

out=boxplot(hs$total_bedrooms)$out
hs[hs$total_bedrooms %in% out,"total_bedrooms"] = NA
summary(hs)

out=boxplot(hs$population)$out
hs[hs$population %in% out,"population"] = NA
summary(hs)

out=boxplot(hs$households)$out
hs[hs$households %in% out,"households"] = NA
summary(hs)

out=boxplot(hs$median_income)$out
hs[hs$median_income %in% out,"median_income"] = NA
summary(hs)

hs=hs[,-c(1,2)]
summary(hs)

library(mice)
md.pattern(hs)
mymice=mice(hs,m=5,method="pmm")
mymiceComplete=complete(mymice,2)
summary(mymiceComplete)
hs=mymiceComplete
summary(hs)

hs1=hs[,-c(8,9)]

#histogram
par(mfrow=c(2,4))
for (i in names(hs1)){
  hist(hs1[[i]],col=c("blue"),main = names(hs1[i]))
}
#boxplots
for (i in names(hs1)){
  boxplot(hs1[[i]],col=c("blue"),main = names(hs1[i]))
}
par(mfrow=c(1,1))















hs$ratio_of_bedrooms=hs$total_bedrooms/hs$total_rooms
hs$household_per_population=hs$households/hs$population
hs$rooms_per_household=hs$total_rooms/hs$households

str(hs)

library(corrplot)
corrplot(cor(hs[,-c(8,9)]),type="upper",method="number")

library(DataExplorer)
plot_density(hs)


hs=hs[,-c(3:5)]


#creating dummy variable for location and area type
library(caret)
dd=dummyVars(" ~ ocean_proximity", data = hs,fullRank = F)
d1=data.frame(predict(dd, newdata = hs))
hs=cbind(hs,d1)

dd=dummyVars(" ~ value", data = hs,fullRank = F)
d1=data.frame(predict(dd, newdata = hs))
hs=cbind(hs,d1)

hs=hs[,-c(5,6,12)]

str(hs)




library(caTools)
set.seed(500)
spl=sample.split(hs,SplitRatio = 0.7)
tr=subset(hs,spl==T)
ts=subset(hs,spl==F)

set.seed(152)
lm=lm(median_house_value~housing_median_age + total_rooms + 
        median_income + ratio_of_bedrooms + household_per_population + 
        rooms_per_household + ocean_proximity.INLAND + ocean_proximity.NEAR.BAY + 
        value.above.250k + value.below.100k,data=tr)
summary(lm)

step(lm,direction = "both")

par(mfrow=c(2,2))
plot(lm)

cd=cooks.distance(lm)
i=which(cd>4*mean(cd))
length(i)
tr1=tr[-i,]

set.seed(58)
lm=lm(median_house_value~housing_median_age + total_rooms + 
        median_income + ratio_of_bedrooms + household_per_population + 
        rooms_per_household + ocean_proximity.INLAND + ocean_proximity.NEAR.BAY + 
        value.above.250k + value.below.100k,data=tr1)
summary(lm)
plot(lm)

library(lmtest)
bptest(lm)

bc=MASS::boxcox(lm)
best_lam=bc$x[which(bc$y==max(bc$y))]
best_lam

set.seed(211)
lm=lm((median_house_value)^0.141~housing_median_age + total_rooms + 
        median_income + ratio_of_bedrooms + household_per_population + 
        rooms_per_household + ocean_proximity.INLAND + ocean_proximity.NEAR.BAY + 
        value.above.250k + value.below.100k,data=tr1)
summary(lm)
par(mfrow=c(1,1))

ts$pred=predict(lm,newdata=ts)
library(MLmetrics)
RMSE(ts$median_house_value,ts$pred)
MAPE(ts$median_house_value,ts$pred)

#Compute R^2 from true and predicted values
rsq <- function (x, y) cor(x, y) ^ 2
rsquare=rsq(ts$median_house_value,ts$pred)
rsquare

### Ridge
set.seed(200)
r1=train(median_house_value~housing_median_age + total_rooms + 
           median_income + ratio_of_bedrooms + household_per_population + 
           rooms_per_household + ocean_proximity.INLAND + ocean_proximity.NEAR.BAY + 
           value.above.250k + value.below.100k,method="glmnet",data=tr,trControl=trainControl(method="repeatedcv",number=10,repeats=5,verboseIter = T),tuneGrid=expand.grid(alpha=0,lambda=seq(-10,10,length=10)))
summary(r1)
plot(r1)
ts$pred=predict(r1,ts,type="raw")
RMSE(ts$pred,ts$median_house_value)
#Compute R^2 from true and predicted values
rsq <- function (x, y) cor(x, y) ^ 2
rsquare=rsq(ts$median_house_value,ts$pred)
rsquare

set.seed(258)
la=train(median_house_value~housing_median_age + total_rooms + 
           median_income + ratio_of_bedrooms + household_per_population + 
           rooms_per_household + ocean_proximity.INLAND + ocean_proximity.NEAR.BAY + 
           value.above.250k + value.below.100k,method="glmnet",data=tr,trControl=trainControl(method="repeatedcv",number=10,repeats=5,verboseIter = T),tuneGrid=expand.grid(alpha=1,lambda=seq(-10,250,length=10)))
plot(la)
ts$pred=predict(la,ts,type="raw")
RMSE(ts$pred,ts$median_house_value)
#Compute R^2 from true and predicted values
rsq <- function (x, y) cor(x, y) ^ 2
rsquare=rsq(ts$median_house_value,ts$pred)
rsquare



library(rpart)
library(rpart.plot)
library(rattle)
set.seed(897)
r.ctrl=rpart.control(minisplit=10,minbucket=5,cp=0,xval=10)
dt=rpart(formula=tr$median_house_value~housing_median_age + total_rooms + 
           median_income + ratio_of_bedrooms + household_per_population + 
           rooms_per_household + ocean_proximity.INLAND + ocean_proximity.NEAR.BAY + 
           value.above.250k + value.below.100k,data=tr,control = r.ctrl)
plotcp(dt)
dt$cptable
r.ctrl=rpart.control(minisplit=30,minbucket=5,cp=0.00049,xval=10)
dt1=rpart(formula=tr$median_house_value~.,data=tr,control = r.ctrl)
ts$pred=predict(dt1,ts,type="vector")
RMSE(ts$pred,ts$median_house_value)
#Compute R^2 from true and predicted values
rsq <- function (x, y) cor(x, y) ^ 2
rsquare=rsq(ts$median_house_value,ts$pred)
rsquare

library(randomForest)
#set seed again for randomness
set.seed(1000)
#build first RF model
rf=randomForest(median_house_value~.,data=tr,ntree=200,mtry=3,nodesize=30,importance=T)
print(rf)
plot(rf)
#tune rf to identify the best mtry
set.seed(1000)
trrf=tuneRF(tr[,-c(4)],y=tr$median_house_value,mtryStart = 2,stepFactor = 1.5,ntree=200,improve = 0.0001,nodesize=10,
            trace=T,plot=T,doBest = T,importance=T)
print(trrf)
plot(trrf)
rf1=randomForest(median_house_value~housing_median_age + total_rooms + 
                   median_income + ratio_of_bedrooms + household_per_population + 
                   rooms_per_household + ocean_proximity.INLAND + ocean_proximity.NEAR.BAY + 
                   value.above.250k + value.below.100k,data=tr,ntree=100,mtry=4,nodesize=10,importance=T)
plot(rf1)
print(rf1)

ts$pred=predict(rf1,ts,type="response")
RMSE(ts$pred,ts$median_house_value)
#Compute R^2 from true and predicted values
rsq <- function (x, y) cor(x, y) ^ 2
rsquare=rsq(ts$median_house_value,ts$pred)
rsquare


library(ipred)
library(rpart)
set.seed(128)
bg=bagging(median_house_value ~housing_median_age + total_rooms + 
             median_income + ratio_of_bedrooms + household_per_population + 
             rooms_per_household + ocean_proximity.INLAND + ocean_proximity.NEAR.BAY + 
             value.above.250k + value.below.100k,data=tr,control=rpart.control(minbucket=10,maxdepth=30))

ts$pred=predict(bg,ts,type="response")
RMSE(ts$pred,ts$median_house_value)
#Compute R^2 from true and predicted values
rsq <- function (x, y) cor(x, y) ^ 2
rsquare=rsq(ts$median_house_value,ts$pred)
rsquare

library(caTools)
set.seed(5001)
spl=sample.split(hs,SplitRatio = 0.7)
tr=subset(hs,spl==T)
ts=subset(hs,spl==F)
set.seed(1233)
library(xgboost)
gd_features_train<-as.matrix(tr[,-4])
gd_label_train<-as.matrix(tr[,4])
gd_features_test<-as.matrix(ts[,-4])
#checking for the best tuning parameters
tp_xgb<-vector()
lr=c(0.01,0.1,0.3,0.5,0.7,1)
md=c(1,3,5,9,7,15)
nr=c(50,100,500,1000)
mc=c(1,3,7,9,10,15)
gm=c(1,2,3,4,6,7,8,9,10)
for (i in gm){
  xgb.fit <- xgboost(
    data = gd_features_train,
    label = gd_label_train,
    eta = 0.1,
    max_depth =7,
    min_child_weight = 10,
    nrounds = 100,
    nfold = 10,
    objective = "reg:linear", 
    verbose = 0,               
    early_stopping_rounds = 10,
   
  )
  ts$pred=predict(xgb.fit, gd_features_test)
  tp_xgb=cbind(tp_xgb,(rsq(ts$median_house_value,ts$pred)))
}
tp_xgb



#Running xgboost with the best parameters
library(caTools)
set.seed(502)
spl=sample.split(hs,SplitRatio = 0.8)
tr=subset(hs,spl==T)
ts=subset(hs,spl==F)
set.seed(1243)
gd_features_train<-as.matrix(tr[,-4])
gd_label_train<-as.matrix(tr[,4])
gd_features_test<-as.matrix(ts[,-4])
xgb.fit <- xgboost(
  data = gd_features_train,
  label = gd_label_train,
  eta = 0.1,
  max_depth =7,
  min_child_weight = 10,
  nrounds = 100,
  nfold = 10,
  objective = "reg:linear", 
  verbose = 0,               
  early_stopping_rounds = 10,
  
)
ts$pred=predict(xgb.fit, gd_features_test)
RMSE(ts$pred,ts$median_house_value)
#Compute R^2 from true and predicted values
rsq <- function (x, y) cor(x, y) ^ 2
rsquare=rsq(ts$median_house_value,ts$pred)
rsquare

set.seed(1271)
?gbm
#gbm 
library(gbm)
set.seed(501)
gbb <- gbm(
  formula = median_house_value~.,
  distribution = "gaussian",
  data = tr,
  n.trees = 500,
  interaction.depth = 2,
  shrinkage = 0.03,
  cv.folds = 10,
)  
summary.gbm(gbb)
#fiiting on train
ts$pred=predict(gbb,ts,type="response")

RMSE(ts$pred,ts$median_house_value)
#Compute R^2 from true and predicted values
rsq <- function (x, y) cor(x, y) ^ 2
rsquare=rsq(ts$median_house_value,ts$pred)
rsquare





library(caTools)
sc=scale(hs[,c(1:3,5:7)])
head(sc)
sc=as.data.frame(sc)
sc=cbind(hs,sc)
sc=sc[,-c(1:3,5:7)]
set.seed(400)
spl=sample.split(sc,SplitRatio = 0.7)
tr=subset(sc,spl==T)
ts=subset(sc,spl==F)
set.seed(200)
ctrl=trainControl(method="cv",number=10)
knn=train(median_house_value ~., data = tr, method = "knn", trControl = ctrl,tuneGrid = expand.grid(k = c(3,5,7,9)))
knn
plot(knn)

ts$pred=predict(knn, ts,type="raw")
RMSE(ts$pred,ts$median_house_value)
#Compute R^2 from true and predicted values
rsq <- function (x, y) cor(x, y) ^ 2
rsquare=rsq(ts$median_house_value,ts$pred)
rsquare
q()

############################################################################

hs=read.csv("housing.csv")
hs=hs[,-c(1,2,12)]
summary(hs)
str(hs)
sapply(hs, function(x) sum(is.na(x)))
library(dplyr)
hs%>%group_by(CITY)%>%summarize(count=n())%>%arrange(desc(count))%>%top_n(14,count)

hs$value=ifelse(hs$median_house_value<=100000,"below 100k",ifelse(hs$median_house_value >100000 & hs$median_house_value <=250000,"btw 100k and 250k","above 250k"))
hs$value=as.factor(hs$value)

hs$ocean_proximity[hs$ocean_proximity=="ISLAND"]="NEAR BAY"
summary(hs)


out=boxplot(hs$total_rooms)$out
hs[hs$total_rooms %in% out,"total_rooms"] = NA
summary(hs)

out=boxplot(hs$total_bedrooms)$out
hs[hs$total_bedrooms %in% out,"total_bedrooms"] = NA
summary(hs)

out=boxplot(hs$population)$out
hs[hs$population %in% out,"population"] = NA
summary(hs)

out=boxplot(hs$households)$out
hs[hs$households %in% out,"households"] = NA
summary(hs)

out=boxplot(hs$median_income)$out
hs[hs$median_income %in% out,"median_income"] = NA
summary(hs)


hs1=hs[,-c(9)]
summary(hs1)

library(mice)
md.pattern(hs1)
mymice=mice(hs1,m=5,method="pmm")
mymiceComplete=complete(mymice,2)
summary(mymiceComplete)
hs1=mymiceComplete
summary(hs1)

hs1=cbind(hs1,hs$CITY)
colnames(hs1)[10]="City"

summary(hs1)


hs1$ratio_of_bedrooms=hs1$total_bedrooms/hs1$total_rooms
hs1$household_per_population=hs1$households/hs1$population
hs1$rooms_per_household=hs1$total_rooms/hs1$households


library(caret)
dd=dummyVars(" ~ ocean_proximity", data = hs1,fullRank = F)
d1=data.frame(predict(dd, newdata = hs1))
hs1=cbind(hs1,d1)

dd=dummyVars(" ~ value", data = hs1,fullRank = F)
d1=data.frame(predict(dd, newdata = hs1))
hs1=cbind(hs1,d1)

dd=dummyVars(" ~ City", data = hs1,fullRank = F)
d1=data.frame(predict(dd, newdata = hs1))
hs1=cbind(hs1,d1)

hs1=hs1[,-c(8,9,10)]

summary(hs1)


library(caTools)
set.seed(500)
spl=sample.split(hs1,SplitRatio = 0.7)
tr=subset(hs1,spl==T)
ts=subset(hs1,spl==F)

set.seed(152)
lm=lm(median_house_value~.,data=tr)
summary(lm)

step(lm,direction = "both")

par(mfrow=c(2,2))
plot(lm)

cd=cooks.distance(lm)
i=which(cd>4*mean(cd))
length(i)
tr1=tr[-i,]

set.seed(58)
lm=lm(median_house_value~housing_median_age + total_rooms + 
        total_bedrooms + population + median_income + ratio_of_bedrooms + 
        household_per_population + rooms_per_household + ocean_proximity..1H.OCEAN + 
        ocean_proximity.INLAND + ocean_proximity.NEAR.BAY + value.above.250k + 
        value.below.100k + City.Blythe + City.El.Monte + City.Fullerton + 
        City.Long.Beach + City.Los.Alamos + City.Madera + City.Moss.Landing + 
        City.Oceanside + City.Other + City.Palm.Springs + City.Pasadena + 
        City.Riverside,data=tr1)
summary(lm)
plot(lm)

library(lmtest)
bptest(lm)

bc=MASS::boxcox(lm)
best_lam=bc$x[which(bc$y==max(bc$y))]
best_lam

set.seed(211)
lm=lm((median_house_value)^0.141~housing_median_age + total_rooms + 
        total_bedrooms + population + median_income + ratio_of_bedrooms + 
        household_per_population + rooms_per_household + ocean_proximity..1H.OCEAN + 
        ocean_proximity.INLAND + ocean_proximity.NEAR.BAY + value.above.250k + 
        value.below.100k + City.Blythe + City.El.Monte + City.Fullerton + 
        City.Long.Beach + City.Los.Alamos + City.Madera + City.Moss.Landing + 
        City.Oceanside + City.Other + City.Palm.Springs + City.Pasadena + 
        City.Riverside,data=tr1)
summary(lm)
par(mfrow=c(1,1))

ts$pred=predict(lm,newdata=ts)
library(MLmetrics)
RMSE(ts$median_house_value,ts$pred)
MAPE(ts$median_house_value,ts$pred)

#Compute R^2 from true and predicted values
rsq <- function (x, y) cor(x, y) ^ 2
rsquare=rsq(ts$median_house_value,ts$pred)
rsquare

q()
