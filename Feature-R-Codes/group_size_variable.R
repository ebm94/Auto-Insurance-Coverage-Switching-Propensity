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

groupsize3[, negatives := ifelse(is.na(v_diff_first),0,ifelse(v_diff_first < 0, 1, 0))
                          + ifelse(is.na(v_diff_second),0,ifelse(v_diff_second < 0, 1, 0))
                          + ifelse(is.na(v_diff_third),0,ifelse(v_diff_third < 0, 1, 0))
                          + ifelse(is.na(v_diff_fourth),0,ifelse(v_diff_fourth < 0, 1, 0))
                          + ifelse(is.na(v_diff_fifth),0,ifelse(v_diff_fifth < 0, 1, 0))
                          + ifelse(is.na(v_diff_sixth),0,ifelse(v_diff_sixth < 0, 1, 0))]
unique(groupsize3$negatives)
groupsize3[, positives := ifelse(is.na(v_diff_first),0,ifelse(v_diff_first > 0, 1, 0))
           + ifelse(is.na(v_diff_second),0,ifelse(v_diff_second > 0, 1, 0))
           + ifelse(is.na(v_diff_third),0,ifelse(v_diff_third > 0, 1, 0))
           + ifelse(is.na(v_diff_fourth),0,ifelse(v_diff_fourth > 0, 1, 0))
           + ifelse(is.na(v_diff_fifth),0,ifelse(v_diff_fifth > 0, 1, 0))
           + ifelse(is.na(v_diff_sixth),0,ifelse(v_diff_sixth > 0, 1, 0))]
unique(groupsize3$positives)
groupsize3[,z_switches_grpsize:=positives+negatives]
