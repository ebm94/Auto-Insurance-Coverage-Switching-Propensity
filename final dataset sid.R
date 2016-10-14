#coverage combination frequency variable
data=fread("train.csv")
#data of purchase points
data1=data[record_type==1]
#all data except purchase points
data2=data[record_type==0]
#concatenating the combination of coverages as to form a code so it is easy to compare later on with remaining combinations
data$combination=paste(data$A,data$B,data$C,data$D,data$E,data$F,data$G,sep = "")
#seggregating data of customer ID,shopping pts,record type and combination as to compare from later on
data4=data%>%select(customer_ID,shopping_pt,record_type,combination)%>%filter(record_type==0)%>%arrange(customer_ID,desc(shopping_pt))
#converting data from to data table 
data4a=as.data.table(data4)
#setting customer ID as primary key
setkey(data4a,customer_ID)
#checking if key has been assgined to data table
haskey(data4a)
#subseting last quotes combination  
data5=data4a[J(unique(customer_ID)),mult='first']
data5=as.data.table(data5)
#rename column name of combination so it is easier to compare at a later stage
setnames(data5,"combination","combination1")
setnames(data5,"customer_ID","customer_ID1")
setnames(data5,"shopping_pt","shopping_pt1")
setnames(data5,"record_type","record_type1")
#removing last quote for every customer so to subset data to compare to last quote combination
data6=sqldf("select * from data4a except select * from data5")
#joining two tables so as to compare the last quote combination with all other quote combinations
data7=sqldf("select * from data6 a left join data5 b on (a.customer_ID=b.customer_ID1) ")
data7 <- as.data.table(data7)
#creating match column with values 0 for no match and 1 for match
data7[,match:= ifelse(combination==combination1,1,0) ]
#dropping same columns that occur twice after joining
data7[,customer_ID1:=NULL]
data7[,record_type1:=NULL]
data7[,shopping_pt1:=NULL]
#counting number of matches at customer level
data8=sqldf("select customer_ID,sum(match) from data7 group by customer_ID")
setnames(data8,"sum(match)","z_frequency_last_quote")




#car age variables
#reading input file
car=fread("train.csv")
#subsetting required data:-customer id,shopping point,record type,car age
car1=car%>%select(customer_ID,shopping_pt,record_type,car_age)%>%filter(record_type==0)%>%arrange(customer_ID,desc(shopping_pt))
car1=as.data.table(car1)
#assigning 1 in a new column with car age less than 5
car1[,ind_new_car:= ifelse(car_age<5,1,0)]
#assigning 0 in a new column with car age greater than 30
car1[,ind_old_car:= ifelse(car_age>30,1,0)]
car1=sqldf("select * from car1 group by customer_ID")
car1=as.data.table(car1)
car1[,record_type:=NULL]
car1[,shopping_pt:=NULL]
car1[,car_age:=NULL]



#age_oldest & age_youngest variables
#difference between highest and younest age at customer level
#reading the file
age=fread("train.csv")
#subsetting customer id,record type,shopping pt and oldest and youngest age
age1=age%>%select(customer_ID,shopping_pt,record_type,age_oldest,age_youngest)%>%filter(record_type==0)%>%arrange(customer_ID,desc(shopping_pt))
age1=as.data.table(age1)
#create column with age difference in it
age1[,z_diff_age:=age_oldest-age_youngest]
#grouping by customer id
age2=sqldf("select * from age1 group by customer_ID")
age2=as.data.table(age2)
age2[,record_type:=NULL]
age2[,shopping_pt:=NULL]
age2[,age_oldest:=NULL]
age2[,age_youngest:=NULL]



#changes in age oldest or youngest at customer level
age3=sqldf("select customer_ID,count(distinct(age_oldest)) from age1 group by customer_ID")
age3=as.data.table(age3)
setnames(age3,"count(distinct(age_oldest))","value_distinct")
#change in oldest age is  one minus the distinct values of oldest age
age3[,z_change_ageoldest:= value_distinct-1]
#dropping unnecessary column
age3[,value_distinct:=NULL]
age3[,z_distinct_ageoldest:=NULL]

age4=sqldf("select customer_ID,count(distinct(age_youngest)) from age1 group by customer_ID")
age4=as.data.table(age4)
setnames(age4,"count(distinct(age_youngest))","value_distinct")
#change in youngest age is  one minus the distinct values of youngest age
age4[,z_change_ageyoungest:= value_distinct-1]
#dropping unnecessary column
age4[,value_distinct:=NULL]
#senior citizen variable
age5=sqldf("select customer_ID,age_oldest from age1 group by customer_ID")
age5=as.data.table(age5)
age5[,ind_senior_or_not:=ifelse(age_oldest>60,1,0)]
age5[,age_oldest:=NULL]

# change in c and c_previous value variable
data=fread("train.csv")
#seggregating data containing c and c previous
data9=data%>%select(customer_ID,shopping_pt,record_type,C,C_previous)%>%filter(record_type==0)%>%arrange(customer_ID,desc(shopping_pt))
data9=as.data.table(data9)
#matching values of c and c previous and for match put 0 and if not then 1
data9[,match:= ifelse(C==C_previous,0,1)]
#dropping unnecessary columns
data9[,C:=NULL]
data9[,C_previous:=NULL]
data9[,shopping_pt:=NULL]
data9[,record_type:=NULL]
#counting number of no matches 
data10=sqldf("select customer_ID,sum(match) from data9 group by customer_ID")
data10=as.data.table(data10)
setnames(data10,"sum(match)","z_frequency_nomatch_C&c_prev")
#indicator variable for matching c & c_previous at shopping_pt =1
data11=data%>%select(customer_ID,shopping_pt,record_type,C,C_previous)%>%filter(record_type==0& shopping_pt==1)%>%arrange(customer_ID,desc(shopping_pt))
data11=as.data.table(data11)
#matching values of c and c previous and for match put 0 and if not then 1
data11[,match:= ifelse(C==C_previous,0,1)]
#renaming column
setnames(data11,"match","ind_nomatch_c&c_prev")
setnames(data11,"ind_nomatch_c&c_prev","ind_nomatch_c&c_prev_shoppt1")
#dropping unnecessary column
data11[,C:=NULL]
data11[,C_previous:=NULL]
data11[,shopping_pt:=NULL]
data11[,record_type:=NULL]
#indicator variable for matching c & c_previous at shopping_pt =2
data12=data%>%select(customer_ID,shopping_pt,record_type,C,C_previous)%>%filter(record_type==0& shopping_pt==2)%>%arrange(customer_ID,desc(shopping_pt))
data12=as.data.table(data12)
#matching values of c and c previous and for match put 0 and if not then 1
data12[,match:= ifelse(C==C_previous,0,1)]
#renaming column
setnames(data12,"match","ind_nomatch_c&c_prev_shoppt2")
#dropping unnecessary columns
data12[,C:=NULL]
data12[,C_previous:=NULL]
data12[,shopping_pt:=NULL]
data12[,record_type:=NULL]

#indicator variable for matching c & c_previous at shopping_pt =3
data13=data%>%select(customer_ID,shopping_pt,record_type,C,C_previous)%>%filter(record_type==0& shopping_pt==3)%>%arrange(customer_ID,desc(shopping_pt))
data13=as.data.table(data13)
#matching values of c and c previous and for match put 0 and if not then 1
data13[,match:= ifelse(C==C_previous,0,1)]
#renaming column
setnames(data13,"match","ind_nomatch_c&c_prev_shoppt3")
#dropping unnecessary columns
data13[,C:=NULL]
data13[,C_previous:=NULL]
data13[,shopping_pt:=NULL]
data13[,record_type:=NULL]

#indicator variable for matching c & c_previous at shopping_pt =4
data14=data%>%select(customer_ID,shopping_pt,record_type,C,C_previous)%>%filter(record_type==0& shopping_pt==4)%>%arrange(customer_ID,desc(shopping_pt))
data14=as.data.table(data14)
#matching values of c and c previous and for match put 0 and if not then 1
data14[,match:= ifelse(C==C_previous,0,1)]
#renaming column
setnames(data14,"match","ind_nomatch_c&c_prev_shoppt4")
#dropping unnecessary columns
data14[,C:=NULL]
data14[,C_previous:=NULL]
data14[,shopping_pt:=NULL]
data14[,record_type:=NULL]

#indicator variable for matching c & c_previous at shopping_pt =5
data15=data%>%select(customer_ID,shopping_pt,record_type,C,C_previous)%>%filter(record_type==0& shopping_pt==5)%>%arrange(customer_ID,desc(shopping_pt))
data15=as.data.table(data15)
#matching values of c and c previous and for match put 0 and if not then 1
data15[,match:= ifelse(C==C_previous,0,1)]
#renaming column
setnames(data15,"match","ind_nomatch_c&c_prev_shoppt5")
#dropping unnecessary columns
data15[,C:=NULL]
data15[,C_previous:=NULL]
data15[,shopping_pt:=NULL]
data15[,record_type:=NULL]

#indicator variable for matching c & c_previous at shopping_pt =6
data16=data%>%select(customer_ID,shopping_pt,record_type,C,C_previous)%>%filter(record_type==0& shopping_pt==6)%>%arrange(customer_ID,desc(shopping_pt))
data16=as.data.table(data16)
#matching values of c and c previous and for match put 0 and if not then 1
data16[,match:= ifelse(C==C_previous,0,1)]
#renaming column
setnames(data16,"match","ind_nomatch_c&c_prev_shoppt6")
#dropping unnecessary columns
data16[,C:=NULL]
data16[,C_previous:=NULL]
data16[,shopping_pt:=NULL]
data16[,record_type:=NULL]

#indicator variable for matching c & c_previous at shopping_pt =7
data17=data%>%select(customer_ID,shopping_pt,record_type,C,C_previous)%>%filter(record_type==0& shopping_pt==7)%>%arrange(customer_ID,desc(shopping_pt))
data17=as.data.table(data17)
#matching values of c and c previous and for match put 0 and if not then 1
data17[,match:= ifelse(C==C_previous,0,1)]
#renaming column
setnames(data17,"match","ind_nomatch_c&c_prev_shoppt7")
#dropping unnecessary columns
data17[,C:=NULL]
data17[,C_previous:=NULL]
data17[,shopping_pt:=NULL]
data17[,record_type:=NULL]

#indicator variable for matching c & c_previous at shopping_pt =8
data18=data%>%select(customer_ID,shopping_pt,record_type,C,C_previous)%>%filter(record_type==0& shopping_pt==8)%>%arrange(customer_ID,desc(shopping_pt))
data18=as.data.table(data18)
#matching values of c and c previous and for match put 0 and if not then 1
data18[,match:= ifelse(C==C_previous,0,1)]
#renaming column
setnames(data18,"match","ind_nomatch_c&c_prev_shoppt8")
#dropping unnecessary columns
data18[,C:=NULL]
data18[,C_previous:=NULL]
data18[,shopping_pt:=NULL]
data18[,record_type:=NULL]

#indicator variable for matching c & c_previous at shopping_pt =9
data19=data%>%select(customer_ID,shopping_pt,record_type,C,C_previous)%>%filter(record_type==0& shopping_pt==9)%>%arrange(customer_ID,desc(shopping_pt))
data19=as.data.table(data19)
#matching values of c and c previous and for match put 0 and if not then 1
data19[,match:= ifelse(C==C_previous,0,1)]
#renaming column
setnames(data19,"match","ind_nomatch_c&c_prev_shoppt9")
#dropping unnecessary columns
data19[,C:=NULL]
data19[,C_previous:=NULL]
data19[,shopping_pt:=NULL]
data19[,record_type:=NULL]

#indicator variable for matching c & c_previous at shopping_pt =10
data20=data%>%select(customer_ID,shopping_pt,record_type,C,C_previous)%>%filter(record_type==0& shopping_pt==10)%>%arrange(customer_ID,desc(shopping_pt))
data20=as.data.table(data20)
#matching values of c and c previous and for match put 0 and if not then 1
data20[,match:= ifelse(C==C_previous,0,1)]
#renaming column
setnames(data20,"match","ind_nomatch_c&c_prev_shoppt10")
#dropping unnecessary columns
data20[,C:=NULL]
data20[,C_previous:=NULL]
data20[,shopping_pt:=NULL]
data20[,record_type:=NULL]

#indicator variable for matching c & c_previous at shopping_pt =11
data21=data%>%select(customer_ID,shopping_pt,record_type,C,C_previous)%>%filter(record_type==0& shopping_pt==11)%>%arrange(customer_ID,desc(shopping_pt))
data21=as.data.table(data21)
#matching values of c and c previous and for match put 0 and if not then 1
data21[,match:= ifelse(C==C_previous,0,1)]
#renaming column
setnames(data21,"match","ind_nomatch_c&c_prev_shoppt11")
#dropping unnecessary columns
data21[,C:=NULL]
data21[,C_previous:=NULL]
data21[,shopping_pt:=NULL]
data21[,record_type:=NULL]

#indicator variable for matching c & c_previous at shopping_pt =12
data22=data%>%select(customer_ID,shopping_pt,record_type,C,C_previous)%>%filter(record_type==0& shopping_pt==12)%>%arrange(customer_ID,desc(shopping_pt))
data22=as.data.table(data22)
#matching values of c and c previous and for match put 0 and if not then 1
data22[,match:= ifelse(C==C_previous,0,1)]
#renaming column
setnames(data22,"match","ind_nomatch_c&c_prev_shoppt12")
#dropping unnecessary columns
data22[,C:=NULL]
data22[,C_previous:=NULL]
data22[,shopping_pt:=NULL]
data22[,record_type:=NULL]


setkey(data11,customer_ID)
setkey(data12,customer_ID)
setkey(data13,customer_ID)
setkey(data14,customer_ID)
setkey(data15,customer_ID)
setkey(data16,customer_ID)
setkey(data17,customer_ID)
setkey(data18,customer_ID)
setkey(data19,customer_ID)
setkey(data20,customer_ID)
setkey(data21,customer_ID)
setkey(data22,customer_ID)


a=merge(data11,data12,all.x = TRUE)
a=merge(a,data13,all.x=TRUE)
a=merge(a,data14,all.x=TRUE)
a=merge(a,data15,all.x=TRUE)
a=merge(a,data16,all.x=TRUE)
a=merge(a,data17,all.x=TRUE)
a=merge(a,data18,all.x=TRUE)
a=merge(a,data19,all.x=TRUE)
a=merge(a,data20,all.x=TRUE)
a=merge(a,data21,all.x=TRUE)
a=merge(a,data22,all.x=TRUE)
data23=a



#car value change variables
getwd()
carvalue=fread("train.csv")
#subestting required data:-customer id,shoppng pt,record type,car value
carvalue1=carvalue%>%select(customer_ID,shopping_pt,record_type,car_value)%>%filter(record_type==0)%>%arrange(customer_ID,desc(shopping_pt))
carvalue1=as.data.table(carvalue1)
#making auxillary column which will be headers of column containing car value after spreading
carvalue1=carvalue1[,.(customer_ID,shopping_pt,car_value)][,shopping_pt1:=LETTERS[shopping_pt]]
#spreading data
carvalue2=carvalue1[,.(customer_ID,shopping_pt1,car_value)] %>%spread(shopping_pt1,car_value)                                                    
#carvalue2[,ind_change_a_to_b:= ifelse((((A=="a")|(B=="a")|(C=="a")|(D=="a")|(E=="a")|(F=="a")|(G=="a"))&((A=="b")|(B=="b")|(C=="b")|(D=="b")|(E=="b")|(F=="b")|(G=="b"))),1,0)]
#imputing NA's with z
carvalue3=carvalue2[,lapply(.SD, function(x){ifelse(is.na(x),"z",x)})]
for(i in 1:7){
  for(j in (i+1):7){
    
    carvalue3[,dummy:= ifelse((((A==letters[i])|(B==letters[i])|(C==letters[i])|(D==letters[i])|(E==letters[i])|(F==letters[i])|(G==letters[i])|(H==letters[i])|(I==letters[i])|(J==letters[i])|(K==letters[i])|(L==letters[i]))&((A==letters[j])|(B==letters[j])|(C==letters[j])|(D==letters[j])|(E==letters[j])|(F==letters[j])|(G==letters[j])|(H==letters[j])|(I==letters[j])|(J==letters[j])|(K==letters[j])|(L==letters[j]))),1,0)]
    setnames(carvalue3,"dummy",paste("ind_change_",letters[i],"_to_",letters[j],sep=""))
  }
  
}
carvalue3=as.data.table(carvalue3)
carvalue3[,ind_change_g_to_g:=NULL]


carvalue3=as.data.table(carvalue3)
carvalue3[,A:=NULL]
carvalue3[,B:=NULL]
carvalue3[,C:=NULL]
carvalue3[,D:=NULL]
carvalue3[,E:=NULL]
carvalue3[,F:=NULL]
carvalue3[,G:=NULL]
carvalue3[,H:=NULL]
carvalue3[,I:=NULL]
carvalue3[,J:=NULL]
carvalue3[,K:=NULL]
carvalue3[,L:=NULL]

#group size variables
groupsize=fread("train.csv")
#subsetting customer id,shoppng pt,record type,group size
groupsize1=groupsize%>%select(customer_ID,shopping_pt,record_type,group_size)%>%filter(record_type==0)%>%arrange(customer_ID,desc(shopping_pt))
#counting distinct group sizes at customer level
groupsize2=sqldf("select  customer_ID,group_Size,shopping_pt  from groupsize1 group by customer_ID,group_size ")
groupsize2=as.data.table(groupsize2)
groupsize2[,shopping_pt2:=rank(shopping_pt,ties.method = 'first'),by = customer_ID]
groupsize2[,shopping_pt3:=LETTERS[shopping_pt2]]
groupsize2[,shopping_pt:=NULL]
groupsize2[,shopping_pt2:=NULL]
groupsize2=as.data.table(groupsize2)
groupsize3=groupsize2%>%spread(shopping_pt3,group_size)
groupsize3[,diff_first:=abs(A-B)]
groupsize3[,v_diff_first:=(A-B)]
groupsize3[,diff_second:=abs(A-C)]
groupsize3[,v_diff_second:=(A-C)]
groupsize3[,diff_third:=abs(A-D)]
groupsize3[,v_diff_third:=(A-D)]
groupsize3[,diff_fourth:=abs(B-C)]
groupsize3[,v_diff_fourth:=(B-C)]
groupsize3[,diff_fifth:=abs(B-D)]
groupsize3[,v_diff_fifth:=(B-D)]
groupsize3[,diff_sixth:=abs(C-D)]
groupsize3[,v_diff_sixth:=(C-D)]
groupsize3[,ind_change_one:=ifelse(((diff_first==1)|(diff_second==1)|(diff_third==1)|(diff_fourth==1)|(diff_fifth==1)|(diff_sixth==1)),1,0)]
groupsize3[,ind_change_two:=ifelse(((diff_first==2)|(diff_second==2)|(diff_third==2)|(diff_fourth==2)|(diff_fifth==2)|(diff_sixth==2)),1,0)]
groupsize3[,ind_change_three:=ifelse(((diff_first==3)|(diff_second==3)|(diff_third==3)|(diff_fourth==3)|(diff_fifth==3)|(diff_sixth==3)),1,0)]
groupsize3[,ind_decrease:=ifelse(((v_diff_first<0)|(v_diff_second<0)|(v_diff_third<0)|(v_diff_fourth<0)|(v_diff_fifth<0)|(v_diff_sixth<0)),1,0)]
groupsize3[,ind_increase:=ifelse(((v_diff_first>0)|(v_diff_second>0)|(v_diff_third>0)|(v_diff_fourth>0)|(v_diff_fifth>0)|(v_diff_sixth>0)),1,0)]

groupsize3[, z_grpsize_negatives := ifelse(is.na(v_diff_first),0,ifelse(v_diff_first < 0, 1, 0))
           + ifelse(is.na(v_diff_second),0,ifelse(v_diff_second < 0, 1, 0))
           + ifelse(is.na(v_diff_third),0,ifelse(v_diff_third < 0, 1, 0))
           + ifelse(is.na(v_diff_fourth),0,ifelse(v_diff_fourth < 0, 1, 0))
           + ifelse(is.na(v_diff_fifth),0,ifelse(v_diff_fifth < 0, 1, 0))
           + ifelse(is.na(v_diff_sixth),0,ifelse(v_diff_sixth < 0, 1, 0))]
#unique(groupsize3$negatives)
groupsize3[, z_grpsize_positives := ifelse(is.na(v_diff_first),0,ifelse(v_diff_first > 0, 1, 0))
           + ifelse(is.na(v_diff_second),0,ifelse(v_diff_second > 0, 1, 0))
           + ifelse(is.na(v_diff_third),0,ifelse(v_diff_third > 0, 1, 0))
           + ifelse(is.na(v_diff_fourth),0,ifelse(v_diff_fourth > 0, 1, 0))
           + ifelse(is.na(v_diff_fifth),0,ifelse(v_diff_fifth > 0, 1, 0))
           + ifelse(is.na(v_diff_sixth),0,ifelse(v_diff_sixth > 0, 1, 0))]
#unique(groupsize3$positives)
groupsize3[,z_switches_grpsize:=z_grpsize_positives+z_grpsize_negatives]
groupsize3[,A:=NULL]
groupsize3[,B:=NULL]
groupsize3[,C:=NULL]
groupsize3[,D:=NULL]
groupsize3[is.na(groupsize3)]<-0



dataset=Reduce(merge,list(data8,car1,age2,age3,age4,groupsize3,data23,carvalue3))
dataset[is.na(dataset)]<-0
dataset=as.data.frame(dataset)
getwd()
write.csv2(dataset,file = "D:/Other Training Program/CLA Induction/Purchase Prediction/CSV file/dataset.csv")
