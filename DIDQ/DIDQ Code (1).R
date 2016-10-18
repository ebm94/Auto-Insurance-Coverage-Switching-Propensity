##DIDQ File Generation Code
data2<- read.csv(file.choose())
data2<- data.table(data2)
data2$X<-NULL
b<-colnames(data4)
c<-data.frame()
d<-data.frame()


for (i in 1: length(b))
{
  c[1,i]<-length(unique(data4[,get(b[i])]))
  c[2,i]<-as.numeric(data4[is.na(get(b[i])),.N])    
  colnames(c)[i]<- as.character(b[i])
  e<-data4[,.N,by=get(b[i])]
  c[3:(length(e$N)+2),i]<-e$N
  d[1:length(e$N),i]<-e$get
  colnames(d)[i]<- as.character(b[i])
}
# c[2,i]<-as.numeric(data[is.na(get(b[i])),.N])  
# e<-data[,.N,by=get(b[1])]
# c[3:(length(e$N)+2),1]<-e$N

