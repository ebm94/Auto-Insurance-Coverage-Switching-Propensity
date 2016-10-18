##DIDQ File Generation Code
data<- read.csv(file.choose())
data<- Claim_Data
data<- data.table(data)
b<-colnames(data)
c<-data.frame()
d<-data.frame()
for (i in 1: length(b))
{
  c[1,i]<-data[as.integer(unique(get(b[i]))),.N]
  c[2,i]<-as.numeric(data[is.na(get(b[i])),.N])    
  colnames(c)[i]<- as.character(b[i])
  e<-data[,.N,by=get(b[i])]
  c[3:(length(e$N)+2),i]<-e$N
  d[1:length(e$N),i]<-e$get
  colnames(d)[i]<- as.character(b[i])
}
c[2,i]<-as.numeric(data[is.na(get(b[i])),.N])  
?get
e<-data[,.N,by=get(b[1])]
c[3:(length(e$N)+2),1]<-e$N

