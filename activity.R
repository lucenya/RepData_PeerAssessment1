data<-read.csv("./RepData_PeerAssessment1/activity.csv")

sumStep<-aggregate(data$steps,by=list(data$date),sum,na.rm=TRUE)

hist(sumStep$x,xlab="the total number of steps taken each day",
     main="a histogram of the total number of steps taken each day")
mean(sumStep$x)
median(sumStep$x)

meanDaily<-aggregate(data$steps,by=list(data$interval),mean,na.rm=TRUE)
plot(meanDaily$Group.1,meanDaily$x,type="l",xlab="interval",ylab="steps")
meanDaily$Group.1[meanDaily$x==max(meanDaily$x)]

sum(is.na(data$steps))

dataCopy<-data
for(i in 1:length(data$steps)){
        if(is.na(data$steps[i])){
                dataCopy$steps[i]<-meanDaily$x[meanDaily$Group.1==data$interval[i]]
        }
}

sumStep1<-aggregate(dataCopy$steps,by=list(dataCopy$date),sum,na.rm=TRUE)
hist(sumStep1$x,xlab="the total number of steps taken each day",
     main="a histogram of steps number after imputing missing data")
mean(sumStep1$x)
median(sumStep1$x)

day<-weekdays(as.Date(dataCopy$date))
weekday<-day
for(i in 1:length(day)){
        if(day[i]=="ĞÇÆÚÁù"||day[i]=="ĞÇÆÚÈÕ"){
                weekday[i]<-"weekend" 
        }
        else{
                weekday[i]<-"weekday" 
        }
}
dataCopy<-cbind(dataCopy,weekday=as.factor(weekday))

library(ggplot2)
meanDaily1<-aggregate(dataCopy$steps,
                      by=list(interval=dataCopy$interval,weekday=dataCopy$weekday),
                      mean,na.rm=TRUE)
qplot(x=interval,y=x,data=meanDaily1,geom="path",facets=weekday~.,
      xlab="Interval",ylab="Number of steps")

