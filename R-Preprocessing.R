length(unique(studentInfo$id_student))
str(studentInfo)
sapply(studentInfo,unique)
sum(studentInfo$num_of_prev_attempts==6)
hist(studentInfo$age_band)
library(magrittr)
library(dplyr)     
table(studentInfo$age_band)
table(studentInfo$region)
table(studentInfo$highest_education)
table(studentInfo$imd_band)
table(studentInfo$final_result,studentInfo$region)
sum(assessments$weight[assessments$code_module=="BBB"&assessments$code_presentation=="2013J"])
sinfo<-studentInfo[studentInfo$final_result=="Pass"|studentInfo$final_result=="Distinction"|studentInfo$final_result=="Fail",]
length(na.omit(studentRegistration$date_unregistration))
sinfo<-sinfo[order(sinfo$id_student),]
studentRegistration<-studentRegistration[order(studentRegistration$id_student),]
studentRegistration[1,]
i<-1
j<-1
sreg<-NULL
while (i<22438) {
  if (studentRegistration[j,3]==sinfo[i,3]){
    sreg<-rbind(sreg,data.frame(studentRegistration[j,]))
    i<-i+1
    j<-j+1
  } else {
    j<-j+1
  }
}
studentVle<-studentVle[order(studentVle$id_student),]
i<-1
j<-1
svle<-NULL
while (i<10655281&j<22438) {
  if (studentVle[i,1]==sinfo[j,1]&studentVle[i,2]==sinfo[j,2]&studentVle[i,3]==sinfo[j,3]){
    svle<-rbind(svle,data.frame(studentVle[i,]))
    i<-i+1
  } else if (studentVle[i,3]>sinfo[j,3]){
    j<-j+1
  } else {
    i<-i+1
  }
}
length(unique(studentVle$id_student))
head(studentVle,30)
svle1<- studentVle %>%
  group_by(id_student,code_module,code_presentation) %>%
  summarise(s=sum(sum_click))
length(unique(sinfo$id_student))
sinfo$vleSum<-1
i<-1
j<-1
while (j<29228){
  if (svle1[j,2]==sinfo[i,1]&&svle1[j,3]==sinfo[i,2]&&svle1[j,1]==sinfo[i,3]){
    sinfo$vleSum[i]<-svle1$s[j]
    i<-i+1
    j<-j+1
  }
  else if (svle1[j,1]>sinfo[i,3]) {
    i<-i+1
  }
  else {
    j<-j+1
  }
}
sum((sinfo$vleSum==1)==TRUE)
sum(is.na(sinfo$final_result))
table(sinfo$final_result)
boxplot(sinfo$final_result,sinfo$vleAv)
hist(sinfo$vleAv)
plot(sinfo$highest_education,sinfo$gender)
library(ggplot2)
plot(sinfo$final_result)
sinfo$code_module<-as.factor(sinfo$code_module)
sinfo$code_presentation<-as.factor(sinfo$code_presentation)
sinfo$gender<-as.factor(sinfo$gender)
sinfo$region<-as.factor(sinfo$region)
sinfo$imd_band<-as.factor(sinfo$imd_band)
sinfo$highest_education<-as.factor(sinfo$highest_education)
sinfo$disability<-as.factor(sinfo$disability)
sinfo$age_band<-as.factor(sinfo$age_band)

plot(sinfo$age_band,sinfo$vleAv)
sinfo%>%
  group_by(final_result) %>%
  summarise(avg=mean(vleAv))

sinfo%>%
  subset(final_result=="Distinction") %>%
  ggplot(aes(x=vleAv))

sum(studentAssessment$date_submitted>270)

studentAssessment$clength<-1
for (i in 1:173912){
  id<- studentAssessment$id_assessment[i]
  mod<-assessments$code_module[assessments$id_assessment==id]
  studentAssessment$clength[i]<-courses$module_presentation_length[courses$code_module==mod]
}

sass<- studentAssessment%>%
  subset(date_submitted<clength/2)

max(sass$date_submitted)
length(unique(sass$id_student))
avass<-sass%>%
  group_by(id_student)%>%
  summarise(av=mean(score))

summary(studentVle$date)
svleb<- studentVle%>%
  subset(date<134)

svle2<-svleb%>%
  group_by(id_student,code_module,code_presentation)%>%
  summarise(s=sum(sum_click))


sinfo$vleSum<-1
i<-1
j<-1
while (j<29207){
  if (svle2[j,2]==sinfo[i,1]&&svle2[j,3]==sinfo[i,2]&&svle2[j,1]==sinfo[i,3]){
    sinfo$vleSum[i]<-svle2$s[j]
    i<-i+1
    j<-j+1
  }
  else if (svle2[j,1]>sinfo[i,3]) {
    i<-i+1
  }
  else {
    j<-j+1
  }
}

sinfo$avAss<-1
i<-1
j<-1
while (j<23347){
  if (avass[j,1]==sinfo[i,3]){
    sinfo$avAss[i]<-avass$av[j]
    i<-i+1
    j<-j+1
  }
  else if (avass[j,1]>sinfo[i,3]) {
    i<-i+1
  }
  else {
    j<-j+1
  }
}

sinfo$avAss[sinfo$avAss==1]<-0
plot(sinfo$vleAvBef,sinfo$avAss)
byReg<-prop.table(table(sinfo$final_result,sinfo$region),2)
(byReg<-byReg[,order(byReg[2,],decreasing = TRUE)])

(byBand<-prop.table(table(sinfo$final_result,sinfo$imd_band),2))

sinfo$res<-"Fail"
sinfo$res[sinfo$final_result=="Distinction"|sinfo$final_result=="Pass"]<-"Pass"
sum(sinfo$res=="Pass")
sum(sinfo$final_result=="Fail")
sinfo$res<-as.factor(sinfo$res)

set.seed(3)
sample <- sample.split(sreg[,1],SplitRatio = 0.75)
train3 =subset(sinfo,sample ==TRUE) 
test3=subset(sinfo, sample==FALSE)


map2(nestedrf$model,nestedTest$data, rf_pred)
nestedPredsrf}

formula<- as.formula(final_result~ code_module+gender+region+highest_education+imd_band+age_band+num_of_prev_attempts+studied_credits+disability+vleAvBef+avAss)
formula2<- as.formula(res~ code_module+highest_education+num_of_prev_attempts+vleAvBef+avAss)
formula3<- as.formula(res~ code_module+gender+region+highest_education+imd_band+age_band+num_of_prev_attempts+studied_credits+disability)

getLibs<- function(){
  library(nnet)
  library(gbm)
  library(ipred)
  library(caret)
  library(randomForest)
  library(tidyr)
  library(purrr)
  library(dplyr)
  library(ROCR)
  library(mclust)
  library(C50)
  library(magrittr)
  library(rpart)
  library(e1071)
  library(naivebayes)
  library(ggplot2)
  library(Boruta)
  library(mice)
}
getLibs()

train1%>%
  group_by(code_presentation,final_result)%>%
  summarize(avA=mean(avAss),avV=mean(vleAvBef))

f<- ggplot(train1, aes(avAss,vleAv,color=train1$final_result))
f+geom_jitter()

min<-min(sinfo$vleSum)
max<-max(sinfo$vleSum)
sinfo$vleSumNorm<- (sinfo$vleSum-min)/(max-min)
min<-min(sinfo$avAss)
max<-max(sinfo$avAss)
sinfo$avAssNorm<- (sinfo$avAss-min)/(max-min)

train3<- sinfo%>%
  subset(code_presentation=="2013B"|code_presentation=="2014B"|code_presentation=="2013J")
test3<- sinfo%>%
  subset(code_presentation=="2014J")

formula2<- as.formula(res~ code_module+highest_education+num_of_prev_attempts+vleSumNorm+avAssNorm)
forest_mod <- randomForest(formula2, data = train3, ntree = 50,nodesize=30,mtry=2, importance=TRUE )
Predict_forest <- predict(forest_mod, newdata = test3, type = "class")
tb<-table(test3$res,Predict_forest)
confusionMatrix(tb)
confusionMatrix(table(train3$res,predict(forest_mod,newdata = train3,type="class")))

##Data imputation
tempData <- mice(test3,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)
completedData <- complete(tempData,1)
test3<-completedData

mod_glm_fit <- train(formula2,  data = train3, method = "glm", family = "binomial")
Predict_best_glm <- predict(mod_glm_fit, newdata = test3, type = "raw")
tb<-table(test3$res,Predict_best_glm)
confusionMatrix(tb)

#Linear Discriminant Analysis
library(devtools)
install_github("Displayr/flipMultivariates")

library(flipMultivariates)
lda <- lda(formula2, data = train3)
Predict_lda <- predict(lda, newdata = test3, type = "raw")
tb<-table(test3$res,Predict_lda$class)
confusionMatrix(tb)

plot(lda)

#PCA
pca <- prcomp(train3[,c(13,14,17:19)], center = TRUE,scale. = TRUE)
summary(pca)

svle2<- studentVle %>%
  group_by(id_student,code_module,code_presentation) %>%
  nest()
binning<- function(s){
  dat<-s[[1]]$date
  clk<-s[[1]]$sum_click
  ta<-integer(28)
  for (k in 1:length(dat)){
    if (dat[k]<0){ bin<-0
    }else {bin=dat[k]/10
    }
    bin=as.integer(bin)
    ta[bin]<-ta[bin]+clk[k]
  }
  return(sd(ta))
}
svle2['sd']<-0
k<-1

for (k in 1:29228){
  svle2$sd[[k]]<-binning(svle2$data[k])
}
ff<-svle2$data[3]
