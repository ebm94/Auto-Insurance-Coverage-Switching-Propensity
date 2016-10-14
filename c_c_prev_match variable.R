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
setnames(data10,"z_frequency_nomatch_C_c_prev","z_frequency_nomatch_C&c_prev")
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
