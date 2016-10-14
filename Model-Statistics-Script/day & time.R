######################## Day Variables ###############################
data5<-data[!record_type==1,.SD[.N],by=customer_ID]
data5<-data5[day>=5,IND_day_lastquote_weekend := 1]
data5<-data5[day< 5,IND_day_lastquote_weekend := 0]

######################## Time Variables ###############################
data5<-data[!record_type==1]
data5$time<-as.POSIXct(data5$time,format="%H:%M")
data5$IND_time_09_06 <-0
data5<-data5[time>='2016-08-26 09:00:00'&time<='2016-08-26 18:00:00',IND_time_09_06:=1]
data5<-data5[!record_type==1,.SD[.N],by=customer_ID]
data5$IND_time_00_06 <-0
data5$IND_time_06_12 <-0
data5$IND_time_12_18 <-0
data5$IND_time_18_24 <-0
data5<-data5[time>='2016-08-26 00:00:00'&time<'2016-08-26 06:00:00',IND_time_00_06:=1]
data5<-data5[time>='2016-08-26 06:00:00'&time<'2016-08-26 12:00:00',IND_time_06_12:=1]
data5<-data5[time>='2016-08-26 12:00:00'&time<'2016-08-26 18:00:00',IND_time_12_18:=1]
data5<-data5[time>='2016-08-26 18:00:00'&time<'2016-08-26 24:00:00',IND_time_18_24:=1]
data5<-data5[,c(1,26:30),with=FALSE]
data4<-data4[data5]




