#----------------------------------------------------------------------------------  
#----------------------------------------------------------------------------------  
# Bi-variate Code

library(grid)
library(gridExtra)

#Step-1      
Bivariate<-function(variable)
var<-"IND_costdiff_2_3"
a= data4[,.(Cat_count=.N) ,by=get(var)]
b=a[,Cat_per_cent:=Cat_count/sum(Cat_count) * 100]


#Step-2
c=data4[,.(y_count=.N) ,by= .(get(var),y)]
d=c[,y_per_cent:=y_count/sum(y_count) * 100,by=get]





#Step-3
setkey(b,get)
setkey(d,get)
bi_variate=b[d]

#Step-4
bi_variate=bi_variate[(bi_variate$y %in% c(1)), ]


data6=data4[,(data4 %in% c("z","Z")) ]



bi_variate

#Step-5
x <- barplot(bi_variate$Cat_count, 
             
             col = "blue", 
             xlab = "",
             ylab = var,
             ylim = c(0, 100000) )[, 1]
axis(1, at = x, labels = bi_variate$get)
text(x = x, y = bi_variate$Cat_count, label = bi_variate$Cat_count, pos = 3, col = "blue",cex = 1)
par(new=TRUE)
plot(x, bi_variate$y_per_cent,type="b",col="red",xaxt="n",yaxt="n",ylab="",xlab="",ylim = c(5,35),xlim = c(0,2.5))
axis(4)
text(x=x ,y = bi_variate$y_per_cent, label = round(bi_variate$y_per_cent,2),pos =3,cex =.8, col = "red")


# plot(bi_variate$get,xlim = unique(bi_variate$get),bi_variate$Cat_count,xaxt='n',col="blue",type="h",lwd=30,ylim = c(0,120000),ylab =var)
# text(x = bi_variate$get, y = bi_variate$Cat_count, label = bi_variate$Cat_count, pos = 3, col = "blue",cex = .9)
# axis(1, at=bi_variate$get, labels=bi_variate$get)
# par(new=TRUE)
# plot(bi_variate$get, bi_variate$y_per_cent,type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="",ylim = c(5,35))
# axis(4)
# text(x=bi_variate$get ,y = bi_variate$y_per_cent, label = round(bi_variate$y_per_cent,2), pos = 3,cex =.8, col = "red")
# legend(2,col=c("red","blue"),lty=1,legend=c("Response","Frequency"))


############## for Continuous Variables #############################

var<-"z_costdiff_6_7"

a<- data4[,':='(quant1=quantile(data4[,get(var)],.20),quant2=quantile(data4[,get(var)],.40)
                ,quant3=quantile(data4[,get(var)],.60),quant4=quantile(data4[,get(var)],.80))]

a<-a[,.(customer_ID,y,quant1,quant2,quant3,quant4,get(var))]
a<-a[V7<=quant1,variable:=1]
a<-a[V7>quant1&V7<=quant2,variable:=2]
a<-a[V7>quant2&V7<=quant3,variable:=3]
a<-a[V7>quant3&V7<=quant4,variable:=4]
a<-a[V7>quant4,variable:=5]
a= a[,.(Cat_count=.N) ,by=variable]
b=a[,Cat_per_cent:=Cat_count/sum(Cat_count) * 100]


#step2
c<- data4[,':='(quant1=quantile(data4[,get(var)],.20),quant2=quantile(data4[,get(var)],.40)
                ,quant3=quantile(data4[,get(var)],.60),quant4=quantile(data4[,get(var)],.80))]

c<-c[,.(customer_ID,y,quant1,quant2,quant3,quant4,get(var))]
c<-c[V7<=quant1,variable:=1]
c<-c[V7>quant1&V7<=quant2,variable:=2]
c<-c[V7>quant2&V7<=quant3,variable:=3]
c<-c[V7>quant3&V7<=quant4,variable:=4]
c<-c[V7>quant4,variable:=5]
c= c[,.(y_count=.N) ,by=.(variable,y)]
d=c[,y_per_cent:=y_count/sum(y_count) * 100,by=variable]

#step3
setkey(b,variable)
setkey(d,variable)
bi_variate=b[d]

#Step-4
bi_variate=bi_variate[(bi_variate$y %in% c(1)), ]
bi_variate
#Step-5
plot(bi_variate$variable,xlim =range(bi_variate$variable),xaxt='n',bi_variate$Cat_count,col="blue",type="h",lwd=25,ylim = c(0,120000),ylab =var)
text(x = bi_variate$variable, y = bi_variate$Cat_count, label = bi_variate$Cat_count, pos = 3, col = "blue",cex = 1)
axis(1, at=bi_variate$variable, labels=bi_variate$variable)
par(new=TRUE)
plot(bi_variate$variable, bi_variate$y_per_cent,type="l",col="red",xaxt="n",yaxt="n",xlab="",ylab="",ylim = c(5,35))
axis(4)
text(x=bi_variate$variable ,y = bi_variate$y_per_cent, label = round(bi_variate$y_per_cent,2), pos = 3,cex =.8, col = "red")



ar(mar = rep(4, 4))
x <- barplot(bi_variate$Cat_count, 
            
             col = "blue", 
             xlab = "",
             ylab = "",
             ylim = c(0, 120000) )[, 1]
axis(1, at = x, labels = bi_variate$variable)
text(x = x, y = bi_variate$Cat_count, label = bi_variate$Cat_count, pos = 3, col = "blue",cex = 1)
par(new=TRUE)
plot(bi_variate$variable, bi_variate$y_per_cent,type="b",col="red",yaxt="n",xlab="",ylab="",ylim = c(5,35),xlim = c(.5,.5+max(bi_variate$variable)))
axis(4)
text(x=bi_variate$variable,y = bi_variate$y_per_cent, label = round(bi_variate$y_per_cent,2), pos = 3,cex =.8, col = "red")
