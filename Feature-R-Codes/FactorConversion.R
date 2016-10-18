data5<-fread(file.choose())

data4$V1<-NULL
data5$V1<-NULL
rm(a,b,A,mp,xx,bi_variate,data.data1,data2,data3,data6,data7,C,going_down,going_up,i,var)
rm(c,d)
b<-colnames(data4)
data5<-data4[,grep('z',ignore.case = TRUE,b),with=FALSE]
c<-colnames(data7)
data7<-data5[,grep('[[:alpha:]]count[[:alpha:]]',ignore.case = TRUE,c):=NULL,with=FALSE]
c<-colnames(data7)
data5<-data7[,grep('[[:alpha:]]_Total_time_[[:alpha:]]',ignore.case = TRUE,c):=NULL,with=FALSE]
c<-colnames(data5)
data7<-data5[,grep('[[:alpha:]]_numberof_[[:alpha:]]',ignore.case = TRUE,c):=NULL,with=FALSE]
c<-colnames(data7)
data6<-data4[,grep('z',ignore.case = FALSE,b) := NULL,with=FALSE]
data7<-data7[,c(20,21):=NULL,with=FALSE]
data5<-data7
data5$age_oldest<-data4$age_oldest
data5$age_youngest<-data4$age_youngest
data5$car_age<-data4$car_age
data5$cost<-data4$cost
data5$customer_ID=data4$customer_ID
b<-colnames(data5)
data6<-data4[,b:=NULL,with=FALSE]
data8=data.frame(sapply(data6,as.factor))
data8=data.table(data8)
data6<-data8
data4<-data6[data5]
rm(data,data1,b,c,Bivariate,engthu,lengthu,Mode)
data4<-data4[,which(c[1,]>1),with=FALSE]
fit<-glm(y~.,data= data4[,c(2:252),with=FALSE],family=binomial)




b<-1:3

data[,get(b[1]):= sum*grep([[1|2|3]])]