################Run following function
Mode <- function(x) {
  x<-x[!is.na(x)]
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
lengthu<-function(x){
  x<-x[!is.na(x)]
  ux<-length(unique(x))
}
################for A Variable A########################

data5<-data[!record_type==1]
#replacing column shopping_pt with letters
data5<-data5[,.(customer_ID,shopping_pt,A)][,shopping_pt1:=LETTERS[shopping_pt]]
#splitting cost Data using shopping_pt
data5<-data5[,.(customer_ID,shopping_pt1,A)]%>%spread(shopping_pt1,A)
data5<-data5[,Z_A_most_choosen_option := apply(data5[,2:13,with=FALSE],1,Mode)]
data5<-data5[,Z_A_numberof_choosen_option := apply(data5[,2:13,with=FALSE],1,lengthu)]
setkey(data5,customer_ID)
#getting the last option of column 
data2<-data[!record_type==1,.SD[2],by=customer_ID]
setkey(data2,customer_ID)
data5<-data5[data2]
data5<-data5[Z_A_most_choosen_option==i.A,IND_A_equal_most_occuring_value :=1]
data5<-data5[Z_A_most_choosen_option!=i.A,IND_A_equal_most_occuring_value :=0]
data5<-data5[,c(1,15,40),with=FALSE]
data4<-data4[data5]

################for B Variable A########################

data5<-data[!record_type==1]
#replacing column shopping_pt with letters
data5<-data5[,.(customer_ID,shopping_pt,B)][,shopping_pt1:=LETTERS[shopping_pt]]
#splitting cost Data using shopping_pt
data5<-data5[,.(customer_ID,shopping_pt1,B)]%>%spread(shopping_pt1,B)
data5<-data5[,Z_B_most_choosen_option := apply(data5[,2:13,with=FALSE],1,Mode)]
data5<-data5[,Z_B_numberof_choosen_option := apply(data5[,2:13,with=FALSE],1,lengthu)]
setkey(data5,customer_ID)
#getting the last option of column 
data2<-data[!record_type==1,.SD[2],by=customer_ID]
setkey(data2,customer_ID)
data5<-data5[data2]
data5<-data5[Z_B_most_choosen_option==i.B,IND_B_equal_most_occuring_value :=1]
data5<-data5[Z_B_most_choosen_option!=i.B,IND_B_equal_most_occuring_value :=0]
data5<-data5[,c(1,15,40),with=FALSE]
data4<-data4[data5]

################for C Variable A########################

data5<-data[!record_type==1]
#replacing column shopping_pt with letters
data5<-data5[,.(customer_ID,shopping_pt,C)][,shopping_pt1:=LETTERS[shopping_pt]]
#splitting cost Data using shopping_pt
data5<-data5[,.(customer_ID,shopping_pt1,C)]%>%spread(shopping_pt1,C)
data5<-data5[,Z_C_most_choosen_option := apply(data5[,2:13,with=FALSE],1,Mode)]
data5<-data5[,Z_C_numberof_choosen_option := apply(data5[,2:13,with=FALSE],1,lengthu)]
setkey(data5,customer_ID)
#getting the last option of column 
data2<-data[!record_type==1,.SD[2],by=customer_ID]
setkey(data2,customer_ID)
data5<-data5[data2]
data5<-data5[Z_C_most_choosen_option==i.C,IND_C_equal_most_occuring_value :=1]
data5<-data5[Z_C_most_choosen_option!=i.C,IND_C_equal_most_occuring_value :=0]
data5<-data5[,c(1,15,40),with=FALSE]
data4<-data4[data5]

################for D Variable A########################

data5<-data[!record_type==1]
#replacing column shopping_pt with letters
data5<-data5[,.(customer_ID,shopping_pt,D)][,shopping_pt1:=LETTERS[shopping_pt]]
#splitting cost Data using shopping_pt
data5<-data5[,.(customer_ID,shopping_pt1,D)]%>%spread(shopping_pt1,D)
data5<-data5[,Z_D_most_choosen_option := apply(data5[,2:13,with=FALSE],1,Mode)]
data5<-data5[,Z_D_numberof_choosen_option := apply(data5[,2:13,with=FALSE],1,lengthu)]
setkey(data5,customer_ID)
#getting the last option of column 
data2<-data[!record_type==1,.SD[2],by=customer_ID]
setkey(data2,customer_ID)
data5<-data5[data2]
data5<-data5[Z_D_most_choosen_option==i.D,IND_D_equal_most_occuring_value :=1]
data5<-data5[Z_D_most_choosen_option!=i.D,IND_D_equal_most_occuring_value :=0]
data5<-data5[,c(1,15,40),with=FALSE]
data4<-data4[data5]
################for E Variable A########################

data5<-data[!record_type==1]
#replacing column shopping_pt with letters
data5<-data5[,.(customer_ID,shopping_pt,E)][,shopping_pt1:=LETTERS[shopping_pt]]
#splitting cost Data using shopping_pt
data5<-data5[,.(customer_ID,shopping_pt1,E)]%>%spread(shopping_pt1,E)
data5<-data5[,Z_E_most_choosen_option := apply(data5[,2:13,with=FALSE],1,Mode)]
data5<-data5[,Z_E_numberof_choosen_option := apply(data5[,2:13,with=FALSE],1,lengthu)]
setkey(data5,customer_ID)
#getting the last option of column 
data2<-data[!record_type==1,.SD[2],by=customer_ID]
setkey(data2,customer_ID)
data5<-data5[data2]
data5<-data5[Z_E_most_choosen_option==i.E,IND_E_equal_most_occuring_value :=1]
data5<-data5[Z_E_most_choosen_option!=i.E,IND_E_equal_most_occuring_value :=0]
data5<-data5[,c(1,15,40),with=FALSE]
data4<-data4[data5]
################for F Variable A########################

data5<-data[!record_type==1]
#replacing column shopping_pt with letters
data5<-data5[,.(customer_ID,shopping_pt,F)][,shopping_pt1:=LETTERS[shopping_pt]]
#splitting cost Data using shopping_pt
data5<-data5[,.(customer_ID,shopping_pt1,F)]%>%spread(shopping_pt1,F)
data5<-data5[,Z_F_most_choosen_option := apply(data5[,2:13,with=FALSE],1,Mode)]
data5<-data5[,Z_F_numberof_choosen_option := apply(data5[,2:13,with=FALSE],1,lengthu)]
setkey(data5,customer_ID)
#getting the last option of column 
data2<-data[!record_type==1,.SD[2],by=customer_ID]
setkey(data2,customer_ID)
data5<-data5[data2]
data5<-data5[Z_F_most_choosen_option==i.F,IND_F_equal_most_occuring_value :=1]
data5<-data5[Z_F_most_choosen_option!=i.F,IND_F_equal_most_occuring_value :=0]
data5<-data5[,c(1,15,40),with=FALSE]
data4<-data4[data5]
################for G Variable A########################

data5<-data[!record_type==1]
#replacing column shopping_pt with letters
data5<-data5[,.(customer_ID,shopping_pt,G)][,shopping_pt1:=LETTERS[shopping_pt]]
#splitting cost Data using shopping_pt
data5<-data5[,.(customer_ID,shopping_pt1,G)]%>%spread(shopping_pt1,G)
data5<-data5[,Z_G_most_choosen_option := apply(data5[,2:13,with=FALSE],1,Mode)]
data5<-data5[,Z_G_numberof_choosen_option := apply(data5[,2:13,with=FALSE],1,lengthu)]
setkey(data5,customer_ID)
#getting the last option of column 
data2<-data[!record_type==1,.SD[2],by=customer_ID]
setkey(data2,customer_ID)
data5<-data5[data2]
data5<-data5[Z_G_most_choosen_option==i.G,IND_G_equal_most_occuring_value :=1]
data5<-data5[Z_G_most_choosen_option!=i.G,IND_G_equal_most_occuring_value :=0]
data5<-data5[,c(1,15,40),with=FALSE]
data4<-data4[data5]

