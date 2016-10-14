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
