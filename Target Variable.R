data<-fread("train.csv")

#selecting rows where record_type ==1
data1<- data[record_type==1]
#Ranking the Row on customer_ID and descending shopping points
data2<-data[order(rank(customer_ID),-shopping_pt)]
setkey(data1,customer_ID)
#selection the first row for every remaining group by customer_ID
data2<-data2[!record_type==1,.SD[1],by=customer_ID]
setkey(data2,customer_ID)
#Merging the rows on customer_ID
data3<-data1[data2]

data3$switch<-1
data3[A==i.A & B==i.B&C==i.C&D==i.D&E==i.E & F==i.F,switch:=0]
