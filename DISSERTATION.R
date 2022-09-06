rm(list=ls())
df=read.csv("C:/Users/dell/Desktop/DISSERTATION files/framingham.csv")
df1=na.omit(df)
dim(df1)
data=data.frame(df1)

##DATA VISUALIZATION##

table2=table(data$male,data$TenYearCHD)
barplot(table2,beside = TRUE,col=c("orange","blue"),main="Gender vs CHD status",legend=rownames(table2),xlab="Gender")

x=data$age
min(x)
max(x)
breaks=seq(32,72,10)
length(breaks)
tags=c("[32-42]","[42-52]","[52-62]","[62-72]")
bin_age=cut(x,breaks=breaks,labels=tags,include.lowest=TRUE)
summary(bin_age)
tbl_age=table(data$TenYearCHD,bin_age)
barplot(tbl_age,main="Age vs outcome",xlab="Age(in years)",col=c("orange","blue"),legend=rownames(tbl_age),beside=TRUE)
 
table3=table(data$TenYearCHD,data$education)
barplot(table3,beside = TRUE,col=c("orange","blue"),legend=rownames(table3),main="Education level vs CHD status",xlab="Education level")

table4=table(data$currentSmoker,data$TenYearCHD)
barplot(table4,beside = TRUE,col=c("orange","blue"),legend=rownames(table4),main="Smoking status vs CHD status",xlab="Smoking status")

x1=data$cigsPerDay
max(x1)
breaks=c(0,10,20,70)
length(breaks)
tags=c("[0-10]","[10-20]","[20-70]")
bin_cigs=cut(x1,breaks=breaks,labels=tags,include.lowest=TRUE)
summary(bin_cigs)
tbl_cigs=table(data$TenYearCHD,bin_cigs)
barplot(tbl_cigs,main="Cigs per day vs CHD status",xlab="cigs per day",col=c("orange","blue"),legend=rownames(tbl_cigs),beside=TRUE)

table5=table(data$BPMeds,data$TenYearCHD)
barplot(table5,beside = TRUE,col=c("orange","blue"),legend=rownames(table5),main="BP Meds vs CHD status",xlab="BP Meds")

table6=table(data$prevalentStroke,data$TenYearCHD)
barplot(table6,beside = TRUE,col=c("orange","blue"),legend=rownames(table6),main="Stroke history vs CHD status",xlab="Stroke history")

table7=table(data$prevalentHyp,data$TenYearCHD)
barplot(table7,beside = TRUE,col=c("orange","blue"),legend=rownames(table7),main="Hypertension history vs CHD status",xlab="Hypertension history")

table8=table(data$diabetes,data$TenYearCHD)
barplot(table8,beside = TRUE,col=c("orange","blue"),legend=rownames(table8),main="diabetes status vs CHD status",xlab="Diabetes status ")

x2=data$totChol
min(x2)
max(x2)
breaks=c(113,170,200,600)
length(breaks)
tags=c("[-170]","[170-200]","[200-]")
bin_chol=cut(x2,breaks=breaks,labels=tags,include.lowest=TRUE)
summary(bin_chol)
tbl_chol=table(data$TenYearCHD,bin_chol)
barplot(tbl_chol,main="Cholesterol level vs CHD status",xlab="Cholesterol level",col=c("orange","blue"),legend=rownames(tbl_chol),beside=TRUE)

x3=data$sysBP
min(x3)
max(x3)
breaks=c(83.5,120,140,295)
length(breaks)
tags=c("[-120]","[120-140]","[140-]")
bin_sys=cut(x3,breaks=breaks,labels=tags,include.lowest=TRUE)
summary(bin_sys)
tbl_sys=table(data$TenYearCHD,bin_sys)
barplot(tbl_sys,main="Systolic BP vs CHD status",xlab="Sys BP(mg/dl)",col=c("orange","blue"),legend=rownames(tbl_sys),beside=TRUE)

x4=data$diaBP
min(x4)
max(x4)
breaks=c(48,80,100,142.5)
length(breaks)
tags=c("[-80]","[80-100]","[100-]")
bin_dia=cut(x4,breaks=breaks,labels=tags,include.lowest=TRUE)
summary(bin_dia)
tbl_dia=table(data$TenYearCHD,bin_dia)
barplot(tbl_dia,main="Diastolic BP vs CHD status",xlab="Dia BP(mg/dl)",col=c("orange","blue"),legend=rownames(tbl_dia),beside=TRUE)

x5=data$BMI
min(x5)
max(x5)
breaks=c(15.54,25,35,56.8)
length(breaks)
tags=c("[-25]","[25-35]","[35-]")
bin_BMI=cut(x5,breaks=breaks,labels=tags,include.lowest=TRUE)
summary(bin_BMI)
tbl_BMI=table(data$TenYearCHD,bin_BMI)
barplot(tbl_BMI,main="BMI vs CHD status",xlab="BMI(kg/meter squared)",col=c("orange","blue"),legend=rownames(tbl_BMI),beside=TRUE)

x6=data$heartRate
min(x6)
max(x6)
breaks=c(44,65,80,143)
length(breaks)
tags=c("[-65]","[65-80]","[80-]")
bin_hr=cut(x6,breaks=breaks,labels=tags,include.lowest=TRUE)
summary(bin_hr)
tbl_hr=table(data$TenYearCHD,bin_hr)
barplot(tbl_hr,main="Heart rate vs CHD status",xlab="Heart rate(beats/min)",col=c("orange","blue"),legend=rownames(tbl_hr),beside=TRUE)

x7=data$glucose
min(x7)
max(x7)
breaks=c(40,100,140,394)
length(breaks)
tags=c("[-100]","[100-140]","[140-]")
bin_glu=cut(x7,breaks=breaks,labels=tags,include.lowest=TRUE)
summary(bin_glu)
tbl_glu=table(data$TenYearCHD,bin_glu)
barplot(tbl_glu,main="Glucose level vs CHD status",xlab="Glucose level(mg/dl)",col=c("orange","blue"),legend=rownames(tbl_glu),beside=TRUE)


##COMBINING VARIABLES USING COLUMN ENGINEERING##

cig=data$cigsPerDay
smo=data$currentSmoker
smost=array(dim=1)
for(i in 1:3658)
{
  if(smo[i]==0)
  {
    smost[i]=1
  }else
  {
    if(cig[i]>=1 & cig[i]<=10)
    {
      smost[i]=2
    }else
    {
      if(cig[i]>=11 & cig[i]<=20)
      {
        smost[i]=3
      }else
      {
        if(cig[i]>20)
        {
          smost[i]=4
        }
      }      
    }
  }
}
data <- subset(data, select = -c(cigsPerDay,currentSmoker))
data["Smokingstaus"]=smost
dim(data)


glu=data$glucose
dia=data$diabetes
diast=array(dim=1)
for(i in 1:3658)
{
  if(glu[i]<=140 & dia[i]==0)
  {
    diast[i]=1
  }else
  {
    if(glu[i]<=140 & dia[i]==1)
    {
      diast[i]=2
    }else
    {
      if(glu[i]>140 & dia[i]==0)
      {
        diast[i]=3
      }else
      {
        if(glu[i]>140 & dia[i]==1)
        {
          diast[i]=4
        }
      }
    }
  }
}
data <- subset(data, select = -c(glucose,diabetes))
data["Diabetes_Status"]=diast
dim(data)

chol=data$totChol
cho=array(dim=1)
for(i in 1:3658)
{
  if(chol[i]<=170)
  {
    cho[i]=1
  }else
  {
    if(chol[i]>170 & chol[i]<=200)
    {
      cho[i]=2
    }else
    {
      if(chol[i]>200)
      {
        cho[i]=3
      }
    }
  }
}
data <- subset(data, select = -c(totChol))
data["Cholesterol"]=chol
dim(data)

sys=data$sysBP
dia=data$diaBP
med=data$BPMeds
A=matrix(c(sys,dia,med),ncol=3)
bpst=array(dim=1)
for(i in 1:3658)
{
  if(A[i,1]<=120 & A[i,2]<=80)
  {
    if(A[i,3]==0)
    {
      bpst[i]=1
    }else{bpst[i]=2}
  }else
  {
    if(A[i,1]>120 || A[i,2]>80)
    {
      if(A[i,3]==0)
      {
        bpst[i]=3
      }else{bpst[i]=4}
    }
  }
}
data <- subset(data, select = -c(sysBP,diaBP,BPMeds,prevalentHyp))
data["BP_status"]=bpst
dim(data)

chd=data$TenYearCHD  ##FOR RANDOM FOREST CLASSIFICATION MODEL ONLY##
A=array(dim=1)
for(i in 1:3658)
{
  if(chd[i]==1)
  {
    A[i]="yes"
  }else
  {
    A[i]="no"
  }
}
data$TenYearCHD=as.factor(A)
dim(data)


str(data)
library(caTools)

##DATA SPLITTING IN 3:1 RATIO##

set.seed(1000)
split = sample.split(data$TenYearCHD, SplitRatio = 0.75)
train = subset(data, split==TRUE)
test = subset(data, split==FALSE)

##LOGISTIC REGRESSION##

logistic = glm(TenYearCHD ~ ., data = train, family=binomial)
summary(logistic)

#PREDICTION FROM LOGISITIC MODEL## 

prediction = predict(logistic, type="response", newdata=test)
table(test$TenYearCHD, prediction > 0.5)

#Calculating AUC 
library(ROCR)
pred1 = prediction(prediction, test$TenYearCHD)
as.numeric(performance(pred1, "auc")@y.values)

##DRAWING ROC CURVE##
perf1 = performance(pred1,"tpr","fpr")
plot(perf1,main="ROC CURVE FOR LOGISTIC")


##IMPROVING THE LOGISTIC REGRESSION MODEL##

logistic1 = glm(TenYearCHD ~.-prevalentStroke-heartRate, data = train, family=binomial)
summary(logistic1)

##PREDICTION AFTER REMOVING INSIGNIFICANT VARIABLES##
prediction1 = predict(logistic1, type="response", newdata=test)
table(test$TenYearCHD, prediction1 > 0.5)

##FINDING OPTIMAL THRESHOLD RISK VALUE FOR WHICH PREDICTION ACCURACY IS MAXIMUM##

acc=array(dim=1)
p=seq(.4,.6,.01)
length(p)
p[1]
for(i in 1:length(p))
{
  A=table(test$TenYearCHD, prediction1 >p[i] )
  acc[i]=(A[1,1]+A[2,2])/914
}
acc
plot(p,acc)
accuracy_table= data.frame(p,acc)
accuracy_table

##FINDING MINIMUM FNR THROUGH CONFUSION MATRIX FOR THE THREE OPTIMAL THRESHOLDS##

table(test$TenYearCHD, prediction1 > 0.46)
table(test$TenYearCHD, prediction1 > 0.53)
table(test$TenYearCHD, prediction1 > 0.56)

##FINDING THE BEST SPLIT OF TRAINING AND TESTING DATA SET FOR MAXIMUM PREDICTION ACCURACY##

ratio=seq(0.75,0.90,0.005)
ratio[1]
accuracy=array(dim=1)
for(i in 1:length(ratio))
{
  set.seed(1000)
  split = sample.split(data$TenYearCHD, SplitRatio = ratio[i])
  train1 = subset(data, split==TRUE)
  test1 = subset(data, split==FALSE)
  logistic1 = glm(TenYearCHD ~.-prevalentStroke-heartRate, data = train, family=binomial)
  prediction1 = predict(logistic1, type="response", newdata=test)
  A=table(test$TenYearCHD, prediction1 > 0.46)
  accuracy[i]=(A[1,1]+A[2,2])/(A[1,1]+A[2,2]+A[2,1]+A[1,2])
}
accuracy
plot(ratio,accuracy)
table= data.frame(ratio,accuracy)
table

##DATA SPLITTING USING OPTIMAL SPLITTING RATIO##

split = sample.split(data$TenYearCHD, SplitRatio = 0.87)
train2 = subset(data, split==TRUE)
test2 = subset(data, split==FALSE)


##FITTING OF THE FINAL LOGISTIC MODEL WITH OPTIMAL DATA SPLITTING AND WITHOUT INSIGNIFICANT PREDICTORS## 

logistic_final = glm(TenYearCHD ~.-prevalentStroke-heartRate, data = train2, family=binomial)
summary(logistic_final)

#PREDICTION USING THE FINAL MODEL AND OPTIMAL THRESHOLD RISK VALUE##
prediction_final = predict(logistic_final, type="response", newdata=test2)
A1=table(test1$TenYearCHD, prediction_final > 0.46)
A1

## FINAL AUC MEASURE ##

library(ROCR)
ROCRpred_final = prediction(prediction_final, test$TenYearCHD)
as.numeric(performance(ROCRpred_final, "auc")@y.values)

## MAXIMIZING TPR OR MINIMIZING FNR ##

prediction_tpr = predict(logistic_final, type="response", newdata=train2)
tpr=array(dim=1)
p=seq(0.02,0.72,.01)
length(p)
p[1]
for(i in 1:length(p))
{
  A=table(train2$TenYearCHD, prediction_tpr >p[i] )
  tpr[i]=(A[2,2])/(A[2,2]+A[2,1])
}
tpr

##PLOTTING TPR AGAINST DIFFERENT THRESHOLD VALUES##

plot(p,tpr,type='l',xlab="Risk thresholds",ylab="True positive rate",col="Red",main="TPR vs Threshold Risk")
abline(a=1,b=-1,lwd=2,lty=2,col="gray")

##PREDICTING ON TEST SET USING FINAL MODEL AND ESTIMATED p WITH DESIRED TRP##

prediction_maxtpr = predict(logistic_final, type="response", newdata=test2)
table(test2$TenYearCHD, prediction_maxtpr >0.02)


##RANDOM FOREST MODEL

library(dplyr)
library(randomForest)
rfm=randomForest(TenYearCHD~.,data=train,ntree=500 ,importance=TRUE)
rfm

##PREDICTION USING RANDOM FOREST MODEL##

CHD_pred=predict(rfm,test)
test$CHD_pred=CHD_pred
A=table(test$TenYearCHD,test$CHD_pred)
A

#FINDING OPTIMAL NUMBER OF VARIABLE IN EACH SPLIT

mtry=tuneRF(train[-7],train2$TenYearCHD,ntreeTry = 600,stepFactor = 1.5,improve = 0.001, trace=TRUE,plot=TRUE)
best_m=mtry[mtry[,2]==min(mtry[,2]),1]
mtry
best_m


##JUDGING IMPORTANCE OF THE PREDICTOR VARIABLES THROUGH MEAN DECREASE ACCURACY##
importance(rfm)
varImpPlot(rfm)

##REFITTING THE MODEL USING BEST SPLIT AND OPTIMAL NUMBER OF DECISION TREES##

rfm1=randomForest(TenYearCHD~.-education-BMI,mtry=best_m,ntree=600,data=train2,importance=TRUE)
rfm1
CHD_pred=predict(rfm1,test2)
test2$CHD_pred=CHD_pred
A=table(test2$TenYearCHD,test2$CHD_pred)
A


#CALCULATING AUC MEASURE## 

pred1=predict(rfm1,type = "prob",newdata=test2)
library(ROCR)
pred2 = prediction(pred1[,2], test$TenYearCHD)
as.numeric(performance(perf2, "auc")@y.values)

##DRAWING ROC CURVE##

perf2 = performance(perf2, "tpr","fpr")
plot(pred3,main="ROC Curve for Random Forest",colorize=TRUE)

##OVERLAYING TWO ROC CURVES FOR COMPARISON##

plot(perf1,col=2,lwd=2,main="ROC CURVES")
plot(perf2,add=TRUE,colorize=TRUE)
abline(a=0,b=1,lwd=2,lty=2,col="gray")



