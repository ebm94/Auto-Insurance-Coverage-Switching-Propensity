#for Variable A

data5<-data[!record_type==1]
#replacing column shopping_pt with letters
data5<-data5[,.(customer_ID,shopping_pt,A)][,shopping_pt1:=LETTERS[shopping_pt]]
#splitting cost Data using shopping_pt
data5<-data5[,.(customer_ID,shopping_pt1,A)]%>%spread(shopping_pt1,A)
a<-colnames(data5)
a<-a[2:length(a)]
A<-letters[14:26]

for (i in seq_along(data5)) set(data5, i=which(is.na(data5[[i]])), j=i, value=100)
for(i in 1:(length(a)-1))
{
  data5<-data5[,A[i]:=(get(a[i])-get(a[i+1]))]
  
}
C<-paste("z","A_optiondiff",1:11,2:12,sep = "_")
setnames(data5,c("customer_ID",a,C))
data5<-data5[,c("customer_ID",C),with=FALSE]


#for gettting Count Variable 
data6<-data5
a<-colnames(data5)
a<-a[2:length(a)]
A<-letters[14:26]

for (i in 2:12) set(data6, i=which(data6[[i]]>10), j=i, value=0)
for (i in 2:12) set(data6, i=which(data6[[i]]< -10), j=i, value=0)
for(i in 1:length(a))
{
  data6<-data6[get(a[i])==0,A[i]:= 0]
  data6<-data6[get(a[i])!=0,A[i] := 1]
}
C<-paste("IND","A_optiondiff",1:11,2:12,sep = "_")
setnames(data6,c("customer_ID",a,C))

setcolorder(data6,c(1,13:23,2:12))
data6<-data6[,Z_A_Total_time_chng := apply(data6[,2:12,with=FALSE],1,sum)]
setcolorder(data6,c(1:12,24,13:23))
data6<-data6[,1:13,with=FALSE]
setkey(data6,customer_ID)
data4<-data4[data6]


####################for B Variable B#########################


data5<-data[!record_type==1]
#replacing column shopping_pt with letters
data5<-data5[,.(customer_ID,shopping_pt,B)][,shopping_pt1:=LETTERS[shopping_pt]]
#splitting cost Data using shopping_pt
data5<-data5[,.(customer_ID,shopping_pt1,B)]%>%spread(shopping_pt1,B)
a<-colnames(data5)
a<-a[2:length(a)]
A<-letters[14:26]

for (i in seq_along(data5)) set(data5, i=which(is.na(data5[[i]])), j=i, value=100)
for(i in 1:(length(a)-1))
{
  data5<-data5[,A[i]:=(get(a[i])-get(a[i+1]))]
  
}
C<-paste("z","A_optiondiff",1:11,2:12,sep = "_")
setnames(data5,c("customer_ID",a,C))
data5<-data5[,c("customer_ID",C),with=FALSE]


#for gettting Count Variable 
data6<-data5
a<-colnames(data5)
a<-a[2:length(a)]
A<-letters[14:26]

for (i in 2:12) set(data6, i=which(data6[[i]]>10), j=i, value=0)
for (i in 2:12) set(data6, i=which(data6[[i]]< -10), j=i, value=0)
for(i in 1:length(a))
{
  data6<-data6[get(a[i])==0,A[i]:= 0]
  data6<-data6[get(a[i])!=0,A[i] := 1]
}
C<-paste("IND","B_optiondiff",1:11,2:12,sep = "_")
setnames(data6,c("customer_ID",a,C))
setcolorder(data6,c(1,13:23,2:12))
data6<-data6[,Z_B_Total_time_chng := apply(data6[,2:12,with=FALSE],1,sum)]
setcolorder(data6,c(1:12,24,13:23))
data6<-data6[,1:13,with=FALSE]
setkey(data6,customer_ID)
data4<-data4[data6]
####################for C Variable B#########################


data5<-data[!record_type==1]
#replacing column shopping_pt with letters
data5<-data5[,.(customer_ID,shopping_pt,C)][,shopping_pt1:=LETTERS[shopping_pt]]
#splitting cost Data using shopping_pt
data5<-data5[,.(customer_ID,shopping_pt1,C)]%>%spread(shopping_pt1,C)
a<-colnames(data5)
a<-a[2:length(a)]
A<-letters[14:26]

for (i in seq_along(data5)) set(data5, i=which(is.na(data5[[i]])), j=i, value=100)
for(i in 1:(length(a)-1))
{
  data5<-data5[,A[i]:=(get(a[i])-get(a[i+1]))]
  
}
C<-paste("z","A_optiondiff",1:11,2:12,sep = "_")
setnames(data5,c("customer_ID",a,C))
data5<-data5[,c("customer_ID",C),with=FALSE]


#for gettting Count Variable 
data6<-data5
a<-colnames(data5)
a<-a[2:length(a)]
A<-letters[14:26]

for (i in 2:12) set(data6, i=which(data6[[i]]>10), j=i, value=0)
for (i in 2:12) set(data6, i=which(data6[[i]]< -10), j=i, value=0)
for(i in 1:length(a))
{
  data6<-data6[get(a[i])==0,A[i]:= 0]
  data6<-data6[get(a[i])!=0,A[i] := 1]
}
C<-paste("IND","C_optiondiff",1:11,2:12,sep = "_")
setnames(data6,c("customer_ID",a,C))
setcolorder(data6,c(1,13:23,2:12))
data6<-data6[,Z_C_Total_time_chng := apply(data6[,2:12,with=FALSE],1,sum)]
setcolorder(data6,c(1:12,24,13:23))
data6<-data6[,1:13,with=FALSE]
setkey(data6,customer_ID)
data4<-data4[data6]
####################for D Variable B#########################


data5<-data[!record_type==1]
#replacing column shopping_pt with letters
data5<-data5[,.(customer_ID,shopping_pt,D)][,shopping_pt1:=LETTERS[shopping_pt]]
#splitting cost Data using shopping_pt
data5<-data5[,.(customer_ID,shopping_pt1,D)]%>%spread(shopping_pt1,D)
a<-colnames(data5)
a<-a[2:length(a)]
A<-letters[14:26]

for (i in seq_along(data5)) set(data5, i=which(is.na(data5[[i]])), j=i, value=100)
for(i in 1:(length(a)-1))
{
  data5<-data5[,A[i]:=(get(a[i])-get(a[i+1]))]
  
}
C<-paste("z","A_optiondiff",1:11,2:12,sep = "_")
setnames(data5,c("customer_ID",a,C))
data5<-data5[,c("customer_ID",C),with=FALSE]


#for gettting Count Variable 
data6<-data5
a<-colnames(data5)
a<-a[2:length(a)]
A<-letters[14:26]

for (i in 2:12) set(data6, i=which(data6[[i]]>10), j=i, value=0)
for (i in 2:12) set(data6, i=which(data6[[i]]< -10), j=i, value=0)
for(i in 1:length(a))
{
  data6<-data6[get(a[i])==0,A[i]:= 0]
  data6<-data6[get(a[i])!=0,A[i] := 1]
}
C<-paste("IND","D_optiondiff",1:11,2:12,sep = "_")
setnames(data6,c("customer_ID",a,C))
setcolorder(data6,c(1,13:23,2:12))
data6<-data6[,Z_D_Total_time_chng := apply(data6[,2:12,with=FALSE],1,sum)]
setcolorder(data6,c(1:12,24,13:23))
data6<-data6[,1:13,with=FALSE]
setkey(data6,customer_ID)
data4<-data4[data6]
####################for E Variable B#########################


data5<-data[!record_type==1]
#replacing column shopping_pt with letters
data5<-data5[,.(customer_ID,shopping_pt,E)][,shopping_pt1:=LETTERS[shopping_pt]]
#splitting cost Data using shopping_pt
data5<-data5[,.(customer_ID,shopping_pt1,E)]%>%spread(shopping_pt1,E)
a<-colnames(data5)
a<-a[2:length(a)]
A<-letters[14:26]

for (i in seq_along(data5)) set(data5, i=which(is.na(data5[[i]])), j=i, value=100)
for(i in 1:(length(a)-1))
{
  data5<-data5[,A[i]:=(get(a[i])-get(a[i+1]))]
  
}
C<-paste("z","A_optiondiff",1:11,2:12,sep = "_")
setnames(data5,c("customer_ID",a,C))
data5<-data5[,c("customer_ID",C),with=FALSE]


#for gettting Count Variable 
data6<-data5
a<-colnames(data5)
a<-a[2:length(a)]
A<-letters[14:26]

for (i in 2:12) set(data6, i=which(data6[[i]]>10), j=i, value=0)
for (i in 2:12) set(data6, i=which(data6[[i]]< -10), j=i, value=0)
for(i in 1:length(a))
{
  data6<-data6[get(a[i])==0,A[i]:= 0]
  data6<-data6[get(a[i])!=0,A[i] := 1]
}
C<-paste("IND","E_optiondiff",1:11,2:12,sep = "_")
setnames(data6,c("customer_ID",a,C))
setcolorder(data6,c(1,13:23,2:12))
data6<-data6[,Z_E_Total_time_chng := apply(data6[,2:12,with=FALSE],1,sum)]
setcolorder(data6,c(1:12,24,13:23))
data6<-data6[,1:13,with=FALSE]
setkey(data6,customer_ID)
data4<-data4[data6]
####################for F Variable B#########################


data5<-data[!record_type==1]
#replacing column shopping_pt with letters
data5<-data5[,.(customer_ID,shopping_pt,F)][,shopping_pt1:=LETTERS[shopping_pt]]
#splitting cost Data using shopping_pt
data5<-data5[,.(customer_ID,shopping_pt1,F)]%>%spread(shopping_pt1,F)
a<-colnames(data5)
a<-a[2:length(a)]
A<-letters[14:26]

for (i in seq_along(data5)) set(data5, i=which(is.na(data5[[i]])), j=i, value=100)
for(i in 1:(length(a)-1))
{
  data5<-data5[,A[i]:=(get(a[i])-get(a[i+1]))]
  
}
C<-paste("z","A_optiondiff",1:11,2:12,sep = "_")
setnames(data5,c("customer_ID",a,C))
data5<-data5[,c("customer_ID",C),with=FALSE]


#for gettting Count Variable 
data6<-data5
a<-colnames(data5)
a<-a[2:length(a)]
A<-letters[14:26]

for (i in 2:12) set(data6, i=which(data6[[i]]>10), j=i, value=0)
for (i in 2:12) set(data6, i=which(data6[[i]]< -10), j=i, value=0)
for(i in 1:length(a))
{
  data6<-data6[get(a[i])==0,A[i]:= 0]
  data6<-data6[get(a[i])!=0,A[i] := 1]
}
C<-paste("IND","F_optiondiff",1:11,2:12,sep = "_")
setnames(data6,c("customer_ID",a,C))
setcolorder(data6,c(1,13:23,2:12))
data6<-data6[,Z_F_Total_time_chng := apply(data6[,2:12,with=FALSE],1,sum)]
setcolorder(data6,c(1:12,24,13:23))
data6<-data6[,1:13,with=FALSE]
setkey(data6,customer_ID)
data4<-data4[data6]

####################for G Variable B#########################


data5<-data[!record_type==1]
#replacing column shopping_pt with letters
data5<-data5[,.(customer_ID,shopping_pt,F)][,shopping_pt1:=LETTERS[shopping_pt]]
#splitting cost Data using shopping_pt
data5<-data5[,.(customer_ID,shopping_pt1,F)]%>%spread(shopping_pt1,F)
a<-colnames(data5)
a<-a[2:length(a)]
A<-letters[14:26]

for (i in seq_along(data5)) set(data5, i=which(is.na(data5[[i]])), j=i, value=100)
for(i in 1:(length(a)-1))
{
  data5<-data5[,A[i]:=(get(a[i])-get(a[i+1]))]
  
}
C<-paste("z","A_optiondiff",1:11,2:12,sep = "_")
setnames(data5,c("customer_ID",a,C))
data5<-data5[,c("customer_ID",C),with=FALSE]


#for gettting Count Variable 
data6<-data5
a<-colnames(data5)
a<-a[2:length(a)]
A<-letters[14:26]

for (i in 2:12) set(data6, i=which(data6[[i]]>10), j=i, value=0)
for (i in 2:12) set(data6, i=which(data6[[i]]< -10), j=i, value=0)
for(i in 1:length(a))
{
  data6<-data6[get(a[i])==0,A[i]:= 0]
  data6<-data6[get(a[i])!=0,A[i] := 1]
}
C<-paste("IND","G_optiondiff",1:11,2:12,sep = "_")
setnames(data6,c("customer_ID",a,C))
setcolorder(data6,c(1,13:23,2:12))
data6<-data6[,Z_G_Total_time_chng := apply(data6[,2:12,with=FALSE],1,sum)]

setcolorder(data6,c(1:12,24,13:23))
data6<-data6[,1:13,with=FALSE]
setkey(data6,customer_ID)
data4<-data4[data6]
data4<-data4[,Z_Total_time_chng := apply(data4[,c(41,53,65,77,89,101,113),with=FALSE],1,sum)]
rm(A,a,C,i,ndx,data6)
