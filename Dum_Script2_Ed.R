getwd()
setwd("D:/Other Training Program/CLA Induction")
DT<- fread("train.csv")
View(DT)
library(data.table)
library(dplyr)
library(tidyr)
library(magrittr)
table(DT$location) %>% View()
nrow(DT)
prop.table(table(DT$state))
summary(DT)
which(is.na(DT$C_previous))
tables()
###tbl<-tbl_df(file)
#DT[customer_ID<10000050, tail(.SD, 2), by=customer_ID] # Very slow execn??????

######16 Aug


# 274-280
#Value of A from first quote to purchase point

# Option A
a<-DT[, .N, by=.(customer_ID, A)]
setkey(a,customer_ID)  # Set primary key for join a and DT2
key(a)


DT2<-DT[record_type==1,.(customer_ID, shopping_pt)]
setkey(DT2,customer_ID)   # Set primary key
A1<-a[DT2]  # Join operation
A1<-A1[N==shopping_pt, Switch_A:=0]
A1<-A1[, .SD[1], by=customer_ID]
A1<-A1[is.na(Switch_A), Switch_A:=1][,.(customer_ID, Switch_A)]
setnames(A1, "Switch_A", "Ind_A_same_first_to_purchase_switch_likelihood")

# Option B
a<-DT[, .N, by=.(customer_ID, B)]
setkey(a,customer_ID)  # Set primary key for join a and DT2
key(a)


DT2<-DT[record_type==1,.(customer_ID, shopping_pt)]
setkey(DT2,customer_ID)   # Set primary key
A<-a[DT2]  # Join operation
A<-A[N==shopping_pt, Switch_B:=0]
A<-A[, .SD[1], by=customer_ID]
A2<-A[is.na(Switch_B), Switch_B:=1][,.(customer_ID, Switch_B)]
setnames(A2, "Switch_B", "Ind_B_same_first_to_purchase_switch_likelihood")

# Option C
a<-DT[, .N, by=.(customer_ID, C)]
setkey(a,customer_ID)  # Set primary key for join a and DT2
key(a)


DT2<-DT[record_type==1,.(customer_ID, shopping_pt)]
setkey(DT2,customer_ID)   # Set primary key
A<-a[DT2]  # Join operation
A<-A[N==shopping_pt, Switch_C:=0]
A<-A[, .SD[1], by=customer_ID]
A3<-A[is.na(Switch_C), Switch_C:=1][,.(customer_ID, Switch_C)]
setnames(A3, "Switch_C", "Ind_C_same_first_to_purchase_switch_likelihood")

# Option D
a<-DT[, .N, by=.(customer_ID, D)]
setkey(a,customer_ID)  # Set primary key for join a and DT2
key(a)


DT2<-DT[record_type==1,.(customer_ID, shopping_pt)]
setkey(DT2,customer_ID)   # Set primary key
A<-a[DT2]  # Join operation
A<-A[N==shopping_pt, Switch_D:=0]
A<-A[, .SD[1], by=customer_ID]
A4<-A[is.na(Switch_D), Switch_D:=1][,.(customer_ID, Switch_D)]
setnames(A4, "Switch_D", "Ind_D_same_first_to_purchase_switch_likelihood")

# Option E
a<-DT[, .N, by=.(customer_ID, E)]
setkey(a,customer_ID)  # Set primary key for join a and DT2
key(a)


DT2<-DT[record_type==1,.(customer_ID, shopping_pt)]
setkey(DT2,customer_ID)   # Set primary key
A<-a[DT2]  # Join operation
A<-A[N==shopping_pt, Switch_E:=0]
A<-A[, .SD[1], by=customer_ID]
A5<-A[is.na(Switch_E), Switch_E:=1][,.(customer_ID, Switch_E)]
setnames(A5, "Switch_E", "Ind_E_same_first_to_purchase_switch_likelihood")

# Option F
a<-DT[, .N, by=.(customer_ID, F)]
setkey(a,customer_ID)  # Set primary key for join a and DT2
key(a)


DT2<-DT[record_type==1,.(customer_ID, shopping_pt)]
setkey(DT2,customer_ID)   # Set primary key
A<-a[DT2]  # Join operation
A<-A[N==shopping_pt, Switch_F:=0]
A<-A[, .SD[1], by=customer_ID]
A6<-A[is.na(Switch_F), Switch_F:=1][,.(customer_ID, Switch_F)]
setnames(A6, "Switch_F", "Ind_F_same_first_to_purchase_switch_likelihood")

# Option G
a<-DT[, .N, by=.(customer_ID, G)]
setkey(a,customer_ID)  # Set primary key for join a and DT2
key(a)


DT2<-DT[record_type==1,.(customer_ID, shopping_pt)]
setkey(DT2,customer_ID)   # Set primary key
A<-a[DT2]  # Join operation
A<-A[N==shopping_pt, Switch_G:=0]
A<-A[, .SD[1], by=customer_ID]
A7<-A[is.na(Switch_G), Switch_G:=1][,.(customer_ID, Switch_G)]
setnames(A7, "Switch_G", "Ind_G_same_first_to_purchase_switch_likelihood")



#267-273
#Value of A from first quote to last quote

#Option A
a<-DT[record_type==0, .N, by=.(customer_ID, A)] # Find no. of times various options are selected in a coverage till last quote point
setkey(a,customer_ID)  # Set primary key for a


DT2<-DT[,last_quote_pt:=shopping_pt-1][record_type==1,.(customer_ID, last_quote_pt)]
setkey(DT2,customer_ID)   # Set primary key for DT2
A<-a[DT2]  # Join operation of A and DT2
A<-A[,Switch_A:=1]
A<-A[N==last_quote_pt, Switch_A:=0]
A<-A[, .SD[1], by=customer_ID]
A8<-A[,.(customer_ID, Switch_A)]
setnames(A8, "Switch_A", "Ind_A_same_first_to_last_switch_likelihood")

#Option B
a<-DT[record_type==0, .N, by=.(customer_ID, B)] # Find no. of times various options are selected in a coverage till last quote point
setkey(a,customer_ID)  # Set primary key for a


DT2<-DT[,last_quote_pt:=shopping_pt-1][record_type==1,.(customer_ID, last_quote_pt)]
setkey(DT2,customer_ID)   # Set primary key for DT2
A<-a[DT2]  # Join operation of A and DT2
A<-A[,Switch_B:=1]
A<-A[N==last_quote_pt, Switch_B:=0]
A<-A[, .SD[1], by=customer_ID]
A9<-A[,.(customer_ID, Switch_B)]
setnames(A9, "Switch_B", "Ind_B_same_first_to_last_switch_likelihood")

#Option C
a<-DT[record_type==0, .N, by=.(customer_ID, C)] # Find no. of times various options are selected in a coverage till last quote point
setkey(a,customer_ID)  # Set primary key for a


DT2<-DT[,last_quote_pt:=shopping_pt-1][record_type==1,.(customer_ID, last_quote_pt)]
setkey(DT2,customer_ID)   # Set primary key for DT2
A<-a[DT2]  # Join operation of A and DT2
A<-A[,Switch_C:=1]
A<-A[N==last_quote_pt, Switch_C:=0]
A<-A[, .SD[1], by=customer_ID]
A10<-A[,.(customer_ID, Switch_C)]
setnames(A10, "Switch_C", "Ind_C_same_first_to_last_switch_likelihood")

#Option D
a<-DT[record_type==0, .N, by=.(customer_ID, D)] # Find no. of times various options are selected in a coverage till last quote point
setkey(a,customer_ID)  # Set primary key for a


DT2<-DT[,last_quote_pt:=shopping_pt-1][record_type==1,.(customer_ID, last_quote_pt)]
setkey(DT2,customer_ID)   # Set primary key for DT2
A<-a[DT2]  # Join operation of A and DT2
A<-A[,Switch_D:=1]
A<-A[N==last_quote_pt, Switch_D:=0]
A<-A[, .SD[1], by=customer_ID]
A11<-A[,.(customer_ID, Switch_D)]
setnames(A11, "Switch_D", "Ind_D_same_first_to_last_switch_likelihood")

#Option E
a<-DT[record_type==0, .N, by=.(customer_ID, E)] # Find no. of times various options are selected in a coverage till last quote point
setkey(a,customer_ID)  # Set primary key for a


DT2<-DT[,last_quote_pt:=shopping_pt-1][record_type==1,.(customer_ID, last_quote_pt)]
setkey(DT2,customer_ID)   # Set primary key for DT2
A<-a[DT2]  # Join operation of A and DT2
A<-A[,Switch_E:=1]
A<-A[N==last_quote_pt, Switch_E:=0]
A<-A[, .SD[1], by=customer_ID]
A12<-A[,.(customer_ID, Switch_E)]
setnames(A12, "Switch_E", "Ind_E_same_first_to_last_switch_likelihood")

#Option F
a<-DT[record_type==0, .N, by=.(customer_ID, F)] # Find no. of times various options are selected in a coverage till last quote point
setkey(a,customer_ID)  # Set primary key for a


DT2<-DT[,last_quote_pt:=shopping_pt-1][record_type==1,.(customer_ID, last_quote_pt)]
setkey(DT2,customer_ID)   # Set primary key for DT2
A<-a[DT2]  # Join operation of A and DT2
A<-A[,Switch_F:=1]
A<-A[N==last_quote_pt, Switch_F:=0]
A<-A[, .SD[1], by=customer_ID]
A13<-A[,.(customer_ID, Switch_F)]
setnames(A13, "Switch_F", "Ind_F_same_first_to_last_switch_likelihood")

#Option G
a<-DT[record_type==0, .N, by=.(customer_ID, G)] # Find no. of times various options are selected in a coverage till last quote point
setkey(a,customer_ID)  # Set primary key for a


DT2<-DT[,last_quote_pt:=shopping_pt-1][record_type==1,.(customer_ID, last_quote_pt)]
setkey(DT2,customer_ID)   # Set primary key for DT2
A<-a[DT2]  # Join operation of A and DT2
A<-A[,Switch_G:=1]
A<-A[N==last_quote_pt, Switch_G:=0]
A<-A[, .SD[1], by=customer_ID]
A14<-A[,.(customer_ID, Switch_G)]
setnames(A14, "Switch_G", "Ind_G_same_first_to_last_switch_likelihood")





# 168-174
# most popular coverage option
getmode<-function(v){
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
data<-DT[, most_pop_A:=getmode(A), by=state]
data1<-data[, Change:=ifelse(most_pop_A==A, 0, 1),][record_type==1, .(customer_ID, Change)]
setnames(data1, "Change", "Ind_A_equalto_most_popular_A_in_state_switch_likelihood")
 
data<-DT[, most_pop_B:=getmode(B), by=state]
data2<-data[, Change:=ifelse(most_pop_B==B, 0, 1),][record_type==1, .(customer_ID, Change)]
setnames(data2, "Change", "Ind_B_equalto_most_popular_B_in_state_switch_likelihood")

data<-DT[, most_pop_C:=getmode(C), by=state]
data3<-data[, Change:=ifelse(most_pop_C==C, 0, 1),][record_type==1, .(customer_ID, Change)]
setnames(data3, "Change", "Ind_C_equalto_most_popular_C_in_state_switch_likelihood")

data<-DT[, most_pop_D:=getmode(D), by=state]
data4<-data[, Change:=ifelse(most_pop_D==D, 0, 1),][record_type==1, .(customer_ID, Change)]
setnames(data4, "Change", "Ind_D_equalto_most_popular_D_in_state_switch_likelihood")

data<-DT[, most_pop_E:=getmode(E), by=state]
data5<-data[, Change:=ifelse(most_pop_E==E, 0, 1),][record_type==1, .(customer_ID, Change)]
setnames(data5, "Change", "Ind_E_equalto_most_popular_E_in_state_switch_likelihood")

data<-DT[, most_pop_F:=getmode(F), by=state]
data6<-data[, Change:=ifelse(most_pop_F==F, 0, 1),][record_type==1, .(customer_ID, Change)]
setnames(data6, "Change", "Ind_F_equalto_most_popular_F_in_state_switch_likelihood")

data<-DT[, most_pop_G:=getmode(G), by=state]
data7<-data[, Change:=ifelse(most_pop_G==G, 0, 1),][record_type==1, .(customer_ID, Change)]
setnames(data7, "Change", "Ind_G_equalto_most_popular_G_in_state_switch_likelihood")

# 203
#Customers for whom value of duration_previous at last quote and  value of duration_previous at second last quote is differnet are more likely to switch
data<-DT[record_type==0, tail(duration_previous, 2), by=customer_ID]
library(sqldf)
x<-sqldf('select count(distinct(V1)) as ndistinct, customer_ID  from data group by customer_ID')
x<-as.data.table(x)
y<-x[,Different:=ifelse(ndistinct==1, 0, 1),]
y<-y[,.(customer_ID, Different)]
setnames(y, "Different", "Ind_duration_previous_last_secondlast_different")


# 205
# Higher the number of times a customer increases his  duration_previous , he is more likely to switch
data<-DT[record_type==0]
data<-data[,.(customer_ID,shopping_pt,duration_previous)][,shopping_pt1:=LETTERS[shopping_pt]]
data<-data[,.(customer_ID,shopping_pt1,duration_previous)]%>%spread(shopping_pt1,duration_previous)
data<-data[, Count:=0,][B-A>0, Count:=Count+1][C-B>0, Count:=Count+1][D-C>0, Count:=Count+1][E-D>0, Count:=Count+1][F-E>0, Count:=Count+1][G-F>0, Count:=Count+1][H-G>0, Count:=Count+1][I-H>0, Count:=Count+1][J-I>0, Count:=Count+1][K-J>0, Count:=Count+1][L-K>0, Count:=Count+1]
datax<-data[,.(customer_ID, Count)]
setnames(datax, "Count", "z_no_of_times_duration_previous_increases")

# 209
# Customers for whom cost at third  last quote is equal to cost at second last quote, and both are less than cost at last quote point, are less likely to switch
data5<-DT[!record_type==1][,tail(cost,3), by=customer_ID][,New:=rep(c('A', 'B', 'C'), times=95153)]
x<-spread(data5, New, V1)
x<-x[,Check:=0][(A==B)&(B<C), Check:=1]
xx<-x[,.(customer_ID, Check)]
setnames(xx, "Check", "Ind_cost_third_last_equals_cost_second_last_lesss_than_cost_last")

#260
# Customers who keep increasing their duration_previous are more likely to switch
data<-DT[record_type==0]
data<-data[,.(customer_ID,shopping_pt,duration_previous)][,shopping_pt1:=LETTERS[shopping_pt]]
data<-data[,.(customer_ID,shopping_pt1,duration_previous)]%>%spread(shopping_pt1,duration_previous)
data<-data[, Count:=0,][B-A<0, Count:=Count+1][C-B<0, Count:=Count+1][D-C<0, Count:=Count+1][E-D<0, Count:=Count+1][F-E<0, Count:=Count+1][G-F<0, Count:=Count+1][H-G<0, Count:=Count+1][I-H<0, Count:=Count+1][J-I<0, Count:=Count+1][K-J<0, Count:=Count+1][L-K<0, Count:=Count+1][Count!=0, Count:=1]
datay<-data[,.(customer_ID, Count)]
setnames(datay, "Count", "Ind_keeps_increasing_duration_previous")

#266
# Customers for whom the cost of combination one at third last quote is less than cost of combination two at last quote which is less than cost of combination one at second last quote, are more likely to switch
data5<-DT[!record_type==1][, Combo:=paste(A,B,C,D,E,F,G, sep="")][,.(customer_ID,cost,Combo),]
data5<-data5[,tail(.SD,3), by=customer_ID][,New:=rep(c('A', 'B', 'C'), times=95153)]
x<-data5 %>% select(customer_ID, New, cost) %>% spread(New, cost) 
y<-data5 %>% select(customer_ID, New, Combo) %>% spread(New, Combo)
x<-x[,Check:=0][(A==B)&(B<C), Check:=1]
setkey(x, customer_ID)
setkey(y, customer_ID)
z<-x[y]
z[, Switch:=0][(A<C)&(C<B)&(i.A==i.B)&(i.B!=i.C),Switch:=1]
zz<-z[,.(customer_ID, Switch)]
setnames(zz, "Switch", "Ind_cost_combo1_third_last_lessthan_cost_combo2_last_lessthan_cost_combo1_second_last")


em_var<-Reduce(merge, list(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, data1, data2, data3, data4, data5, data6, data7, y, datax, xx, datay, zz))

write.csv(em_var, "em_var.csv", row.names=FALSE, col.names=TRUE)
