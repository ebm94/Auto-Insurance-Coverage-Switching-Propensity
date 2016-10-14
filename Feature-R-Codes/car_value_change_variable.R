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
