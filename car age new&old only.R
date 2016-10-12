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
