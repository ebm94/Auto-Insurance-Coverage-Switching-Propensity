########################shopping_pt_state#########################
data5<-data[!record_type==1,.SD[.N],customer_ID]
data6<-data5[,c('percentile25','percentile50','percentile75','percentile100') :=.( quantile(shopping_pt,.25), 
              quantile(shopping_pt,.50),quantile(shopping_pt,.75), quantile(shopping_pt,.999)),by=state]
data6$IND_0_25_percentile_shopping_pt_state<-0
data6$IND_25_50_percentile_shopping_pt_state<-0
data6$IND_50_75_percentile_shopping_pt_state<-0
data6$IND_75_100_percentile_shopping_pt_state<-0

data6<-data6[shopping_pt<=percentile25,IND_0_25_percentile_shopping_pt_state:=1]
data6<-data6[shopping_pt>percentile25 & shopping_pt<=percentile50,IND_25_50_percentile_shopping_pt_state:=1]
data6<-data6[shopping_pt>percentile50 & shopping_pt<=percentile75,IND_50_75_percentile_shopping_pt_state:=1]
data6<-data6[shopping_pt>percentile75,IND_75_100_percentile_shopping_pt_state:=1]
data6<-data6[,c(1,30:33),with=FALSE]

data4<-data4[data6]
