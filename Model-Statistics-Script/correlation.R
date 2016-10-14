c<-colnames(data4)
data7<-data4[,grep('z_[[:alpha:]]',ignore.case = TRUE,c):=NULL,with=FALSE]
data7<-data4[,grep('ind_[[:alpha:]]',ignore.case = TRUE,c):=NULL,with=FALSE]
data7<-data4[,grep('diff_[[:alpha:]]',ignore.case = TRUE,c):=NULL,with=FALSE]
data7<-data7[,c(1:7):=NULL,with=FALSE]
data7<-data7[,c(10,18:23):=NULL,with=FALSE]
b<-colnames(data7)
data5<-data4
data8<-data4[,b:=NULL,with= FALSE]
data8$duration_previous<-as.numeric(data8$duration_previous)
a<-cor(data7)

