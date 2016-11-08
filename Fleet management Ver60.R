#Remove previous objects stored in R environment & Create working directory

rm(list=ls(all=TRUE))
setwd("C:/Users/Deepali Yelisetty/Desktop/Fleet Management")

library(lubridate)

fleetdata<-read.table("FleetManagement.csv", header = TRUE, sep = ",")

##Converting from 12HR AM:PM format to 24 HR HH:MM format

fleetdata$FLStartTime<-substr(strptime(fleetdata$FLStartTime, "%I:%M %p"),11,19)
fleetdata$CLReachTime<-substr(strptime(fleetdata$CLReachTime, "%I:%M %p"),11,19)
fleetdata$CustLeftTime<-substr(strptime(fleetdata$CustLeftTime, "%I:%M %p"),11,19)
fleetdata$FLReachTime<-substr(strptime(fleetdata$FLReachTime, "%I:%M %p"),11,19)

fleetdata$MM<-formatC(fleetdata$MM, width = 2) 
fleetdata$DD<-formatC(fleetdata$DD, width = 2)

fleetdata$Date <- as.character(paste(fleetdata$YY, fleetdata$MM,
                                     fleetdata$DD, sep = "-"))


fleetdata<-fleetdata[,-(2:4)]



fleetdata$Date.FLStartTime <- as.character(paste(fleetdata$Date,
                                                 fleetdata$FLStartTime
                                                 , sep = " "))


fleetdata$Date.FLReachTime <- as.character(paste(fleetdata$Date,
                                                 fleetdata$FLReachTime
                                                 , sep = " "))


fleetdata$Date.CustLeftTime <- as.character(paste(fleetdata$Date,
                                                  fleetdata$CustLeftTime
                                                  , sep = " "))


fleetdata$Date.CLReachTime<- as.character(paste(fleetdata$Date,
                                                fleetdata$CLReachTime
                                                , sep = " "))

head(fleetdata$Date.FLReachTime)

fleetdata<-fleetdata[,-(8:11)]
fleetdata$TimetoCust<-difftime(fleetdata$Date.CLReachTime,fleetdata$Date.FLStartTime,units="hours")
fleetdata$TimeatCust<-difftime(fleetdata$Date.CustLeftTime,fleetdata$Date.CLReachTime,units="hours")
fleetdata$TimetoFL<-difftime(fleetdata$Date.FLReachTime,fleetdata$Date.CustLeftTime,units="hours")


fleetdata$TimetoCust<-as.numeric(fleetdata$TimetoCust)
fleetdata$TimeatCust<-as.numeric(fleetdata$TimeatCust)
fleetdata$TimetoFL<-as.numeric(fleetdata$TimetoFL)

summary(fleetdata)

write.csv(fleetdata, "preprocessfleet.csv")

fleet<-fleetdata[,c(1:12,17:19)]
write.csv(fleet,"fleet.csv")
rm(fleetdata)

summary(fleet)

# Plotting histograms in r
hist(fleet$QtyTons)
hist(fleet$KM)
hist(fleet$TimetoCust)
hist(fleet$TimeatCust)
hist(fleet$TimetoFL)

# Plotting simple barchart in R

customer <- table(fleet$Customer.ID)
barplot(customer, main="count", 
        xlab="Customer$ID")

driver<- table(fleet$Driver.ID)
driver
barplot(driver, main="count",         
        xlab="Driver$ID")

# Boxplot of WasteType by QtyTons 
boxplot(QtyTons~WasteType,data=fleet, main="Waste type by Qty in Tons", 
        xlab="QtyTons", ylab="WasteType")

####Using table command to evaluate multiple variables
library(reshape2)
driver.waste <- data.frame(table(fleet$Driver.ID,fleet$WasteType))
driver.waste<- acast(driver.waste, Var1~Var2,mean)

barplot(driver.waste, main="count",         
        xlab="Driver counts by Waste Type")

customer.waste<-data.frame(table(fleet$Customer.ID,fleet$WasteType))
X<-ggplot(customer.waste, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(position = "stack", stat = "identity") 
X <- X + geom_text(aes(label = Freq>100))+geom_bar(colour = 'black')
  
customer.waste<-acast(customer.waste,Var1~Var2,mean)


barplot(customer.waste, main="count",         
        xlab="Customer counts by Waste Type", col=c("400"))



barplot(customer.waste, main="count",         
        xlab="Customer counts by Waste Type", col=c("400"))

summary(customer.waste)

driver.model <- data.frame(table(fleet$Driver.ID,fleet$Model))
driver.model<- acast(driver.model, Var1~Var2,mean)

barplot(driver.model, main="count",         
        xlab="Driver counts by Model")


driver.make <- data.frame(table(fleet$Driver.ID,fleet$Make))
driver.make<- acast(driver.make, Var1~Var2,mean)

barplot(driver.make, main="count",         
        xlab="Driver counts by make")

driver.vehicle<- data.frame(table(fleet$Driver.ID,fleet$VehicleID))
Y<-ggplot(driver.vehicle, aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(position = "stack", stat = "identity") 
Y <- Y + geom_text(aes(label = Freq(Freq>20))+geom_bar(colour = 'black')
print(Y)
                   
driver.vehicle<- acast(driver.vehicle, Var1~Var2,mean)

bp<-barplot(driver.vehicle, main="Driver Trip Count by Vehicle ID")


#####text(bp, row.names,cex=1,pos=3)

vehicle.make<- data.frame(table(fleet$VehicleID,fleet$Make))
vehicle.make<- acast(vehicle.make, Var1~Var2,mean)

###replace with Group bar plot

barplot(vehicle.make, main="count",         
        xlab="Vehicle counts by Make")





###Binning numeric variable in R ###
library(car)
###Only use this code for plotting

fleet$QtyTons1<-cut(fleet$QtyTons,breaks=10,labels=c(1,2,3,4,5,6,7,8,9,10))
fleet$QtyTons1<-as.numeric(fleet$QtyTons1)
hist(fleet$QtyTons1)

fleet$KM1<-cut(fleet$KM,breaks=11,labels=c(1,2,3,4,5,6,7,8,9,10,11))
fleet$KM1<-as.numeric(fleet$KM1)
hist(fleet$KM1)

###Binning variables using Recode in Deducer 

####fleet11[c("WasteType")] <- recode.variables(fleet11[c("WasteType")] ,
####                          "'W-TYP007' -> 'W-TYP007';
####                            'W-TYP008' -> 'W-TYP008';
####                            'W-TYP012' -> 'W-TYP012';
####                            'W-TYP018' -> 'W-TYP018';
####                            'W-TYP021' -> 'W-TYP021';
####                            else -> 'Others';")


#### fleet11[c("Customer.ID.Bin")] <- recode.variables(fleet11[c("Customer.ID")] ,
####"'C00ID106' -> 'C00ID106';
####'C00ID009' -> 'C00ID009';
####'C00ID039' -> 'C00ID039';
####'C00ID019' -> 'C00ID019';
####'C00ID047' -> 'C00ID047';
####'C00ID084' -> 'C00ID084';
####'C00ID058' -> 'C00ID058';
####'C00ID048' -> 'C00ID048';
####'C00ID053' -> 'C00ID053';
####'C00ID015' -> 'C00ID015';
####'C00ID066' -> 'C00ID066';
####'C00ID049' -> 'C00ID049';
####else -> 'Others';")


######---Plotting done in DEducer-----------###

########### Aggregate command ----------------------------
library(VIM)
library(stats)

rm(counts,customer,customer.waste,driver,driver.waste)

FLtoCLt<-fleet

###  Why does the covariance matrix work work in deducer but not in R

library(som)

cor(normalize(fleet[,c(10,11,13:15)],byrow=TRUE), use="complete.obs")

#####Sorting command#####

library(plyr)
FLtoCLt <- ddply(FLtoCLt, c("KM","WasteType","QtyTons"), summarise,
                 N    = length(TimetoCust),
                 TimetoCust = mean(TimetoCust),
                 TimeatCust = mean(TimeatCust),
                 TimetoFL = mean(TimetoFL))

####Aggregate command###

library(plyr)
FltoCLtCount<- ddply(FLtoCLt,.(FLtoCLt$KM, FLtoCLt$WasteType), .fun = NULL)


####################################################3
#we use set.seed to ensure that you get same results
#every time you run the experiment. Seperating the data into test and train data

rows<-seq(1:1127)
set.seed(7)
trainRows=sample(rows,1000)
testRows<-rows[-(trainRows)]


train = fleet[trainRows,] 
test=fleet[testRows,]


summary(train)
summary(test)


head(train)
summary(train)
attach(train)

#### Build a linear model using R, to predict times T1,T2 & T3
##### Predicting TimetoCustT1 using lm
mylmT1<-lm(TimetoCust ~ KM+WasteType+QtyTons+VehicleID+Driver.ID+Vehicle.Type+Make+Model,
           data = train)
summary(mylmT1)

plot(mylmT1)

testlm<-test[-c(67,76,127),]
predlmTimetoCust <- predict(mylmT1, 
                            newdata = testlm, 
                            type = ("response"))

test<-cbind(testlm,predlmTimetoCust)

### Plotted using excel plot(test$pred~test$x)
library(hydroGOF)
library(arules)
ErrorlmTimetoCust<-mse(sim=test$predlmTimetoCust, obs=test$TimetoCust)
ErrorlmTimetoCust

####Predicting Time@ CustomerT2

mylmT2<-lm(TimeatCust ~ Customer.ID+KM+WasteType+QtyTons+VehicleID, 
           data = train)
summary(mylmT2)

plot(mylmT2)

testlm<-test[-c(30,60,21,31,87,123),]
predlmTimeatCust <- predict(mylmT2, 
                            newdata = testlm, 
                            type = ("response"))

test<-cbind(testlm,predlmTimeatCust)

### Plotted using excel plot(test$pred~test$x)
ErrorlmTimeatCust<-mse(sim=test$predlmTimeatCust, obs=test$TimeatCust)
ErrorlmTimeatCust

## Predicting timeT3 TimetoFL
mylmT3<-lm(TimetoFL ~ KM+WasteType+QtyTons+VehicleID+Driver.ID+Vehicle.Type+Make+Model, 
           data = train)
summary(mylmT3)

plot(mylmT3)

predlmTimetoFL <- predict(mylmT3, 
                          newdata = test, 
                          type = ("response"))

test<-cbind(test,predlmTimetoFL)

### Plotted using excel plot(test$pred~test$x)
ErrorlmTimetoFL<-mse(sim=test$predlmTimetoFL, obs=test$TimetoFL)
ErrorlmTimetoFL

write.csv(test,"testlm.csv")


### Build model 2 with Naive Bayes for all 3 time types###

library(e1071)

trainnb<-train
trainnb$TimetoCust<-cut(trainnb$TimetoCust,breaks=c(0,0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0,2.25,2.5,2.75,3.0,3.25,3.5,3.75,4.0,4.25,4.5,4.75,5.0),
                        label=c(0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0,2.25,2.5,2.75,3.0,3.25,3.5,3.75,4.0,4.25,4.5,4.75,5.0))
trainnb$TimeatCust<-cut(trainnb$TimeatCust,breaks=c(0,0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0,2.25,2.5,2.75,3.0,3.25,3.5,3.75,4.0,4.25,4.5,4.75,5.0),
                        label=c(0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0,2.25,2.5,2.75,3.0,3.25,3.5,3.75,4.0,4.25,4.5,4.75,5.0))
trainnb$TimetoFL<-cut(trainnb$TimetoFL,breaks=c(0,0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0,2.25,2.5,2.75,3.0,3.25,3.5,3.75,4.0,4.25,4.5,4.75,5.0),
                      label=c(0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0,2.25,2.5,2.75,3.0,3.25,3.5,3.75,4.0,4.25,4.5,4.75,5.0))

modelTimetoCust <- naiveBayes(trainnb$TimetoCust ~ ., data = trainnb, laplace = 3)
modelTimetoCust$apriori
modelTimeatCust <- naiveBayes(trainnb$TimetoCust ~ ., data = trainnb, laplace = 3)
modelTimeatCust$apriori
modelTimetoFL <- naiveBayes(trainnb$TimetoCust ~ ., data = trainnb, laplace = 3)
modelTimetoFL$apriori

testnb<-test[,-(16:18)]

prednBTimetoCust <- predict(modelTimetoCust, testnb)
prednBTimeatCust <- predict(modelTimeatCust, testnb)
prednBTimetoFL <- predict(modelTimetoFL, testnb)

testnb<-cbind(testnb,prednBTimetoCust,prednBTimeatCust,prednBTimetoFL)

testnb$prednBTimetoCust<-as.numeric(as.character(testnb$prednBTimetoCust))
testnb$prednBTimeatCust<-as.numeric(as.character(testnb$prednBTimeatCust))
testnb$prednBTimetoFL<-as.numeric(as.character(testnb$prednBTimetoFL))

ErrornbTimetoCust<-mse(sim=testnb$prednBTimetoCust, obs=test$TimetoCust)
ErrornbTimetoCust

ErrornbTimeatCust<-mse(sim=testnb$prednBTimeatCust, obs=test$TimeatCust)
ErrornbTimeatCust

ErrornbTimetoFL<-mse(sim=testnb$prednBTimetoFL, obs=test$TimetoFL)
ErrornbTimetoFL

write.csv(testnb,"testnb.csv")
rm(trainnb,testnb)

### Build all three time with Random Forest

library(randomForest)

trainrf<-train[,c(2:6,8:11,13:15)]
trainrf$Customer.ID<-cut(trainrf$KM,breaks=11,labels=c(1,2,3,4,5,6,7,8,9,10,11))
ModelrfTimetoCust <- randomForest(TimetoCust ~ KM+WasteType+QtyTons+VehicleID+Driver.ID+Vehicle.Type+Make+Model,   data=trainrf)
print(ModelrfTimetoCust) # view results 
importance(ModelrfTimetoCust) # importance of each predictor

testrf<-test[,c(2:6,8:11,13:15)]
rfTimetoCust<-predict(ModelrfTimetoCust,newdata=testrf, 
                      type="class")

ModelrfTimeatCust <- randomForest(TimeatCust ~ KM+WasteType+QtyTons+VehicleID,   data=trainrf)
print(ModelrfTimeatCust) # view results 
importance(ModelrfTimeatCust) # importance of each predictor

rfTimeatCust<-predict(ModelrfTimeatCust,newdata=testrf, 
                      type="class")


ModelrfTimetoFL <- randomForest(TimetoFL ~ KM+WasteType+QtyTons+VehicleID+Driver.ID+Vehicle.Type+Make+Model,   data=trainrf)
print(ModelrfTimetoFL) # view results 
importance(ModelrfTimetoFL) # importance of each predictor


rfTimetoFL<-predict(ModelrfTimetoFL,newdata=testrf, 
                    type="class")

testrf<-cbind(testrf,rfTimetoCust,rfTimeatCust,rfTimetoFL)


ErrorrfTimetoCust<-mse(sim=testrf$rfTimetoCust, obs=testrf$TimetoCust)
ErrorrfTimetoCust

ErrorrfTimeatCust<-mse(sim=testrf$rfTimeatCust, obs=testrf$TimeatCust)
ErrorrfTimeatCust

ErrorrfTimetoFL<-mse(sim=testrf$rfTimetoFL, obs=testrf$TimetoFL)
ErrorrfTimetoFL

write.csv(testrf,"testrf.csv")

rm(trainrf)


### Build model 4 with Decision Trees

library(party)
library(C50)
trainc50<-train[,-c(1,2,7,12)]
trainc50$TimetoCust<-cut(trainc50$TimetoCust,breaks=c(0,0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0,2.25,2.5,2.75,3.0,3.25,3.5,3.75,4.0,4.25,4.5,4.75,5.0),
                         label=c(0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0,2.25,2.5,2.75,3.0,3.25,3.5,3.75,4.0,4.25,4.5,4.75,5.0))

trainc50$TimeatCust<-cut(trainc50$TimeatCust,breaks=c(0,0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0,2.25,2.5,2.75,3.0,3.25,3.5,3.75,4.0,4.25,4.5,4.75,5.0),
                         label=c(0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0,2.25,2.5,2.75,3.0,3.25,3.5,3.75,4.0,4.25,4.5,4.75,5.0))

trainc50$TimetoFL<-cut(trainc50$TimetoFL,breaks=c(0,0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0,2.25,2.5,2.75,3.0,3.25,3.5,3.75,4.0,4.25,4.5,4.75,5.0),
                       label=c(0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0,2.25,2.5,2.75,3.0,3.25,3.5,3.75,4.0,4.25,4.5,4.75,5.0))


dtC50TimetoCust= C5.0(trainc50$TimetoCust ~ ., 
                      data = trainc50[,-c(10:13)], 
                      rules=TRUE)
summary(dtC50TimetoCust)
C5imp(dtC50TimetoCust, pct=TRUE)


testc50<-test[,c(3:6,8:11,13:15)]

c50TimetoCust=predict(dtC50TimetoCust, 
                      newdata=testc50[,(1:8)], 
                      type="class")

dtC50TimeatCust= C5.0(trainc50$TimeatCust ~ ., 
                      data = trainc50[,-c(9,11,12,13)], 
                      rules=TRUE)

summary(dtC50TimeatCust)
C5imp(dtC50TimeatCust, pct=TRUE)


c50TimeatCust=predict(dtC50TimeatCust, 
                      newdata=testc50[,(1:8)], 
                      type="class")


dtC50TimetoFL= C5.0(trainc50$TimetoFL ~ ., 
                    data = trainc50[,-c(9:10,12,13)], 
                    rules=TRUE)
summary(dtC50TimetoFL)
C5imp(dtC50TimetoFL, pct=TRUE)


c50TimetoFL=predict(dtC50TimetoFL, 
                    newdata=testc50[,(1:8)], 
                    type="class")

testc50<-cbind(testc50,c50TimetoCust,c50TimeatCust,c50TimetoFL)

testc50$c50TimetoCust<-as.numeric(as.character(testc50$c50TimetoCust))
testc50$c50TimeatCust<-as.numeric(as.character(testc50$c50TimeatCust))
testc50$c50TimetoFL<-as.numeric(as.character(testc50$c50TimetoFL))

Errorc50TimetoCust<-mse(sim=testc50$c50TimetoCust, obs=testc50$TimetoCust)
Errorc50TimetoCust

Errorc50TimeatCust<-mse(sim=testc50$c50TimeatCust, obs=test$TimeatCust)
Errorc50TimeatCust

Errorc50TimetoFL<-mse(sim=testc50$c50TimetoFL, obs=test$TimetoFL)

Errorc50TimetoFL


write.csv(testc50,"testc50.csv")

rm(trainc50,testc50)


### Creating Error Matrix for all models

Error<-c(Errorc50TimetoCust,Errorc50TimeatCust,Errorc50TimetoFL,ErrorlmTimetoCust,ErrorlmTimeatCust,ErrorlmTimetoFL,
         ErrornbTimetoCust,ErrornbTimeatCust,ErrornbTimetoFL,
         ErrorrfTimetoCust,ErrorrfTimeatCust,ErrorrfTimetoFL)
Error<-matrix(unlist(Error),nrow=3,ncol=4,byrow=FALSE)
colnames(Error) <- c("C50", "lm", "nb","rf")
Time<-c("TimetoCust","TimeatCust","TimetoFL")

Error<-cbind(Time,Error)

write.csv(Error,"Error.csv")

###############Removing unused object
rm(Errorc50TimetoCust,Errorc50TimeatCust,Errorc50TimetoFL,ErrorlmTimetoCust,ErrorlmTimeatCust,ErrorlmTimetoFL,
   ErrornbTimetoCust,ErrornbTimeatCust,ErrornbTimetoFL,
   ErrorrfTimetoCust,ErrorrfTimeatCust,ErrorrfTimetoFL)
rm(Time)

rm(dtC50TimeatCust,dtC50TimetoCust,dtC50TimetoFL)

rm(modelTimetoCust,modelTimeatCust,modelTimetoFL)

###rm(mylmT1,mylmT2,mylmT3)

rm(rfTimetoCust,rfTimeatCust,rfTimetoFL)

rm(c50TimetoCust,c50TimeatCust,c50TimetoFL)

###rm(predlmTimetoCust,predlmTimeatCust,predlmTimetoFL)

rm(prednBTimetoCust,prednBTimeatCust,prednBTimetoFL)

rm(train,testlm,testrf)

#### Plotting T1+T2+T3### 

write.csv(test,"plotdata.csv")

plotdata<-read.table("plotdata.csv", header = TRUE, sep = ",")
plotdata<-plotdata[,-c(1,2,4:13,17:18)]
plotdata<-plotdata[,c(1,2,5,3,6,4,7)]
colnames(plotdata)<-c("Customer.ID","TtoCustAct","TtoCustPred","TatCustAct",
                      "TatCustPred","TtoFLAct","TtoFLPred")


plotdata<-aggregate(x=plotdata,list(plotdata$Customer.ID), mean)

plotdata<-plotdata[,-2]

write.csv(plotdata,"plotdatafinal.csv")
plot1<-plotdata[,1:3]
plot1<-reshape(plot1,idvar="Group.1",varying=(2:3),
               v.names = "TimetoCust",direction="long")
plot1$time<-as.factor(plot1$time)
library(ggplot2)


plot2<-plotdata[,c(1,4,5)]
plot2<-reshape(plot2,idvar="Group.1",varying=(2:3),
               v.names = "TimeatCust",direction="long")
plot2$time<-as.factor(plot2$time)

plot3<-plotdata[,c(1,6,7)]
plot3<-reshape(plot3,idvar="Group.1",varying=(2:3),
               v.names = "TimetoFL",direction="long")
plot3$time<-as.factor(plot3$time)


library(ggplot2)
qplot(factor(Group.1),data=plot1,geom="bar", fill=time , weight=TimetoCust,position="dodge",
      main = "Customer Prediction", xlab="TimetoCust",ylab="Time")

qplot(factor(Group.1),data=plot2,geom="bar", fill=time , weight=TimeatCust,position="dodge",
      main = "Customer Prediction", xlab="TimeatCust",ylab="Time")


qplot(factor(Group.1),data=plot3,geom="bar", fill=time , weight=TimetoFL,position="dodge",
      main = "Customer Prediction", xlab="TimetoFLCust",ylab="Time")


library(reshape2)
plotdata1tmp<-plotdata[,c(1,2,4,6)]
colnames(plotdata1tmp)<-paste(c("Customer","TtoCust","TatCust","TtoFL"))
plotdata1tmp <- melt(plotdata1tmp, id.var="Customer")
colnames(plotdata1tmp)<-paste(c("Customer","variable","Actual Time"))

plotdata2tmp<-plotdata[,c(1,3,5,7)]
colnames(plotdata2tmp)<-paste(c("Customer","TtoCust","TatCust","TtoFL"))
plotdata2tmp <- melt(plotdata2tmp, id.var="Customer")
colnames(plotdata2tmp)<-paste(c("Customer","variable","Predicted Time"))

plotdatafinal<-cbind(plotdata1tmp,plotdata2tmp)
plotdatafinal<-plotdatafinal[,c(1:3,6)]
plotdatafinal <- melt(plotdatafinal, id.var=c("Customer","variable"))
plotdatafinal<-plotdatafinal[,c(1,3,2,4)]
colnames(plotdatafinal)<-paste(c("Customer","Time","variable","value"))
write.csv(plotdatafinal,"plotdatafinalpres.csv")

ggplot(plotdatafinal, aes(x = Customer, y = value, fill = variable)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_wrap( ~ Time)


rm(plot1,plot2,plot3,plotdata,plotdata1tmp,plotdata2tmp,plotdatafinal)
rm(FLtoCLt,FltoCLtCount)

###Customer wise prediction plots and integrating with Shiny






#### Building Longitudinal Model for Predicting time #####

fleetdata<-read.table("preprocessfleet.csv", header = TRUE, sep = ",")
fleetdata<-fleetdata[,c(3:13,18:20)]

rfT1<-predict(ModelrfTimetoCust,newdata=fleetdata[,c(2:5,7:10,12:14)], 
              type="class")
rfT2<-predict(ModelrfTimeatCust,newdata=fleetdata[,c(2:5,7:10,12:14)],
              type="class")
rfT3<-predict(ModelrfTimetoFL,newdata=fleetdata[,c(2:5,7:10,12:14)], 
              type="class")


fleetdata<-cbind(fleetdata,rfT1,rfT2,rfT3)
fleetdata<-fleetdata[with(fleetdata, order(fleetdata$Customer.ID)), ]

fleetdata<-fleetdata[with(fleetdata,order(fleetdata$Date)),]

### Build LM starting at this point

### Predicting Time to Customer from RF

fleetdata$Avg_TimetoCust<-0
fleetdata$StdDev_TimetoCust<-0
fleetdata$Pred_Time<-0
fleetdata$Error<-0

### 1. First LM for time to Cust

head(fleetdata)
for(i in 1:nrow(fleetdata))
{
  if(i==1){
    fleetdata$Avg_TimetoCust[i]<-NA
    fleetdata$StdDev_TimetoCust[i]<-NA
    fleetdata$Pred_Time[i]<-fleetdata$rfT1[i]
    fleetdata$Error[i]<-abs(fleetdata$Pred_Time[i]-fleetdata$TimetoCust[i])
  }
  if(i==2)
  {
    fleetdata$Avg_TimetoCust[i]<-fleetdata$TimetoCust[i-1]
    fleetdata$StdDev_TimetoCust[i]<-NA
    fleetdata$Pred_Time[i]<-fleetdata$rfT1[i]*0.25+fleetdata$TimetoCust[i-1]*0.75
    fleetdata$Error[i]<-(abs(fleetdata$Pred_Time[i]-fleetdata$TimetoCust[i])*100/fleetdata$TimetoCust[i])
  }
  
  if(i==3)
  {
    fleetdata$Avg_TimetoCust[i]<-mean(fleetdata$TimetoCust[1:i-1])
    fleetdata$StdDev_TimetoCust[i]<-sd(fleetdata$TimetoCust[1:i-1])
    fleetdata$Pred_Time[i]<-fleetdata$rfT1[i]*0.25+(mean(fleetdata$TimetoCust[1:i-1])*0.75)
    fleetdata$Error[i]<-(abs(fleetdata$Pred_Time[i]-fleetdata$TimetoCust[i])*100/fleetdata$TimetoCust[i])
  }
  
  if(i>3)
  {
    fleetdata$Avg_TimetoCust[i]<-mean(fleetdata$TimetoCust[1:i-1])
    fleetdata$StdDev_TimetoCust[i]<-sd(fleetdata$TimetoCust[1:i-1])
    lm<-lm(TimetoCust~ rfT1 + Avg_TimetoCust + StdDev_TimetoCust ,data=fleetdata[1:i-1,])
    fleetdata$Pred_Time[i]<-predict(lm,fleetdata[i,c(15,18:19)])
    fleetdata$Error[i]<-(abs(fleetdata$Pred_Time[i]-fleetdata$TimetoCust[i])*100/fleetdata$TimetoCust[i])
  }
}


LMErrorT1<-mse(sim=fleetdata$Pred_Time, obs=fleetdata$TimetoCust)
LMErrorT1

fleetdata$Pred_TimetoCust<-fleetdata$Pred_Time

### Predicting time T2 TimeatCust

fleetdata$Avg_TimeatCust<-fleetdata$Avg_TimetoCust
fleetdata$StdDev_TimeatCust<-fleetdata$StdDev_TimetoCust
fleetdata$Pred_Time<-0
fleetdata$Error<-0

fleetdata<-fleetdata[,-c(18,19)]
fleetdata<-fleetdata[,c(1:17,20,21,22,18,19)]
head(fleetdata)
for(i in 1:nrow(fleetdata))
{
  if(i==1){
    fleetdata$Avg_TimeatCust[i]<-NA
    fleetdata$StdDev_TimeatCust[i]<-NA
    fleetdata$Pred_Time[i]<-fleetdata$rfT2[i]
    fleetdata$Error[i]<-abs(fleetdata$Pred_Time[i]-fleetdata$TimeatCust[i])
  }
  if(i==2)
  {
    fleetdata$Avg_TimeatCust[i]<-fleetdata$TimeatCust[i-1]
    fleetdata$StdDev_TimeatCust[i]<-NA
    fleetdata$Pred_Time[i]<-fleetdata$rfT2[i]*0.25+fleetdata$TimeatCust[i-1]*0.75
    fleetdata$Error[i]<-(abs(fleetdata$Pred_Time[i]-fleetdata$TimeatCust[i])*100/fleetdata$TimeatCust[i])
  }
  
  if(i==3)
  {
    fleetdata$Avg_TimeatCust[i]<-mean(fleetdata$TimeatCust[1:i-1])
    fleetdata$StdDev_TimeatCust[i]<-sd(fleetdata$TimeatCust[1:i-1])
    fleetdata$Pred_Time[i]<-fleetdata$rfT2[i]*0.25+(mean(fleetdata$TimeatCust[1:i-1])*0.75)
    fleetdata$Error[i]<-(abs(fleetdata$Pred_Time[i]-fleetdata$TimeatCust[i])*100/fleetdata$TimeatCust[i])
  }
  
  if(i>3)
  {
    
    fleetdata$Avg_TimeatCust[i]<-mean(fleetdata$TimeatCust[1:i-1])
    fleetdata$StdDev_TimeatCust[i]<-sd(fleetdata$TimeatCust[1:i-1])
    lm<-lm(TimeatCust~ rfT2 + Avg_TimeatCust + StdDev_TimeatCust ,data=fleetdata[1:i-1,])
    fleetdata$Pred_Time[i]<-predict(lm,fleetdata[i,c(16,19:20)])
    fleetdata$Error[i]<-(abs(fleetdata$Pred_Time[i]-fleetdata$TimeatCust[i])*100/fleetdata$TimeatCust[i])
  }
}


LMErrorT2<-mse(sim=fleetdata$Pred_Time, obs=fleetdata$TimeatCust)
LMErrorT2

fleetdata$Pred_TimeatCust<-fleetdata$Pred_Time
fleetdata<-fleetdata[,c(1:18,23,19:22)]


fleetdata$Avg_TimetoFL<-fleetdata$Avg_TimeatCust
fleetdata$StdDev_TimetoFL<-fleetdata$StdDev_TimeatCust
fleetdata$Pred_Time<-0
fleetdata$Error<-0

fleetdata<-fleetdata[,c(1:19,22:25)]
fleetdata<-fleetdata[,c(1:19,22,23,20,21)]
head(fleetdata)

for(i in 1:nrow(fleetdata))
{
  if(i==1){
    fleetdata$Avg_TimetoFL[i]<-NA
    fleetdata$StdDev_TimetoFL[i]<-NA
    fleetdata$Pred_Time[i]<-fleetdata$rfT3[i]
    fleetdata$Error[i]<-abs(fleetdata$Pred_Time[i]-fleetdata$TimetoFL[i])
  }
  if(i==2)
  {
    fleetdata$Avg_TimetoFL[i]<-fleetdata$TimetoFL[i-1]
    fleetdata$StdDev_TimetoFL[i]<-NA
    fleetdata$Pred_Time[i]<-fleetdata$rfT3[i]*0.25+fleetdata$TimetoFL[i-1]*0.75
    fleetdata$Error[i]<-(abs(fleetdata$Pred_Time[i]-fleetdata$TimetoFL[i])*100/fleetdata$TimetoFL[i])
  }
  
  if(i==3)
  {
    fleetdata$Avg_TimetoFL[i]<-mean(fleetdata$TimetoFL[1:i-1])
    fleetdata$StdDev_TimetoFL[i]<-sd(fleetdata$TimetoFL[1:i-1])
    fleetdata$Pred_Time[i]<-fleetdata$rfT3[i]*0.25+(mean(fleetdata$TimetoFL[1:i-1])*0.75)
    fleetdata$Error[i]<-(abs(fleetdata$Pred_Time[i]-fleetdata$TimetoFL[i])*100/fleetdata$TimetoFL[i])
  }
  
  if(i>3)
  {
    fleetdata$Avg_TimetoFL[i]<-mean(fleetdata$TimetoFL[1:i-1])
    fleetdata$StdDev_TimetoFL[i]<-sd(fleetdata$TimetoFL[1:i-1])
    lm<-lm(TimetoFL ~ rfT3 + Avg_TimetoFL + StdDev_TimetoFL ,data=fleetdata[1:i-1,])
    fleetdata$Pred_Time[i]<-predict(lm,fleetdata[i,c(17,20:21)])
    fleetdata$Error[i]<-(abs(fleetdata$Pred_Time[i]-fleetdata$TimetoFL[i])*100/fleetdata$TimetoFL[i])
  }
}


LMErrorT3<-mse(sim=fleetdata$Pred_Time, obs=fleetdata$TimetoFL)
LMErrorT3

fleetdata$Pred_TimetoFL<-fleetdata$Pred_Time

fleetdata<-fleetdata[,-c(20:23)]
fleetdata<-fleetdata[,-c(15:17)]

write.csv(fleetdata,"fleetdatalm.csv")
Longitudinal.Model<-c(LMErrorT1,LMErrorT2,LMErrorT3)
Error<-cbind(Error,Longitudinal.Model)
write.csv(Error,"Error.csv")
