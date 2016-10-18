a<-read.csv(file.choose(),check.names = FALSE,stringsAsFactors = FALSE)
data4<-read.csv(file.choose(),colClasses =a$x)
data4<-data.table(data4)

data9<-sapply(data4,as.numeric)
data9<-data.table(data9)
abc<-vif(y=data9$y,x=data9,trace=FALSE)
a<-abc$select
data5<-data4[,a,with=FALSE]
data5$state<-NULL
data5$time<-NULL
data5$Z_Total_time_chng<-as.integer(data5$Z_Total_time_chng)
intrain<-createDataPartition(y=data6$y,p=.7,list=FALSE)
training<-data6[intrain,]
testing<-data6[-intrain,]
fit2<-glm(y~.,data=training,family="binomial")



a<-c[which(is.na(c[5,]==TRUE))]
a<-a[which(a[4,]<=200)]
e<-colnames(a)
data4<-data4[,e:=NULL,with=FALSE]
data5<-data4
b <- sapply(data4, class)
write.csv(b,"class.csv",row.names = FALSE)

p<-predict(fit,newdata =testing,type="response")
pr<-prediction(p,testing$y)
auc<-performance(pr,measure = "auc")
auc@y.values

testing$pred <- predict(fit,newdata=testing,type='response')
testing$pred1 <-0
testing$pred1[testing$pred>.16733]=1
table(testing$pred1)
summary(fit)
a<-sapply(data5,class)
