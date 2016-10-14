#cost variable for difference between  2 consecutive shopping points
#for gettting Class Variable 
 
data5<-data[!record_type==1]
#replacing column shopping_pt with letters
data5<-data5[,.(customer_ID,shopping_pt,cost)][,shopping_pt1:=LETTERS[shopping_pt]]
#splitting cost Data using shopping_pt
data5<-data5[,.(customer_ID,shopping_pt1,cost)]%>%spread(shopping_pt1,cost)
a<-colnames(data5)
a<-a[2:length(a)]
A<-letters[14:26]

for (i in seq_along(data5)) set(data5, i=which(is.na(data5[[i]])), j=i, value=0)
for(i in 1:(length(a)-1))
{
data5<-data5[,A[i]:=(get(a[i])-get(a[i+1]))]

}
for (i in 14:22) set(data5, i=which(data5[[i]]>250), j=i, value=0)
C<-paste("z","costdiff",1:11,2:12,sep = "_")
setnames(data5,c("customer_ID",a,C))
data5<-data5[,c("customer_ID",C),with=FALSE]

#for gettting Count Variable 
data4<-data5
a<-colnames(data5)
a<-a[2:length(a)]
A<-letters[14:26]
for (i in seq_along(data4)) set(data4, i=which(is.na(data4[[i]])), j=i, value=0)
for(i in 1:length(a))
{
  data4<-data4[get(a[i])>=0,A[i] := 0]
  data4<-data4[get(a[i])<0,A[i] := 1]
}
C<-paste("IND","costdiff",1:11,2:12,sep = "_")
setnames(data4,c("customer_ID",a,C))
rm(A,a,C,i)

#positive cost difference for Purchase and lst transaction
#selecting rows where there is last quote
data1<- data[!record_type==1,.SD[.N],by=customer_ID]
#Ranking the Row on customer_ID and descending shopping points
data2<-data[order(rank(customer_ID),-shopping_pt)]
setkey(data1,customer_ID)
#selection the first row for every remaining group by customer_ID
data2<-data2[!record_type==1,.SD[1],by=customer_ID]
setkey(data2,customer_ID)
#Merging the rows on customer_ID
data3<-data1[data2]
data5<-data3[cost>i.cost,IND_costdiff_purchasept_lstransaction :=1 ]
data5<-data3[cost<=i.cost,IND_costdiff_purchasept_lstransaction :=0 ]
data5<-data5[,.(customer_ID,IND_costdiff_purchasept_lstransaction)]
setkey(data5,customer_ID)
setkey(data4,customer_ID)
data4<-data4[data5]

#for gettting Class Variable  for difference between min and max cost
data5<-data[!record_type==1]
#replacing column shopping_pt with letters
data5<-data5[,.(customer_ID,shopping_pt,cost)][,shopping_pt1:=LETTERS[shopping_pt]]
#splitting cost Data using shopping_pt
data5<-data5[,.(customer_ID,shopping_pt1,cost)]%>%spread(shopping_pt1,cost)
#getting maximum and minimum value
data5<-data5[, ':=' (max =apply(data5[,2:14,with=FALSE] ,1, max,na.rm=TRUE), min = apply(data5[,2:14,with=FALSE] ,1, min,na.rm=TRUE))]
#creating variable
