#This code is intending to review the bikeshare data and answer 3 main questions

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

library(ggplot2)
library(plyr)

# we would like to review the 3 states combined
New_NY<-cbind(ny,replicate(dim(ny)[1],"NY"))
colnames(New_NY)[10]<-"City"
New_Wash<-cbind(ny,replicate(dim(ny)[1],"Washington"))
colnames(New_Wash)[10]<-"City"
New_Chi<-cbind(ny,replicate(dim(ny)[1],"Chicago"))
colnames(New_Chi)[10]<-"City"
Data<-rbind(New_NY,New_Wash,New_Chi)
names(Data)

# --Question 1 in the 3 states combined which birth date have more travels
# first we plot the number of birth dates by travles
qplot(x=Birth.Year, data=Data, color = I('black'),binwidth = 0.5,main = "Histogram of counts of travels y birth date", 
      xlab = "Birth Dates",ylab = "Count of Travels")+scale_x_continuous(breaks=seq(1920, 2001, by=5), lim=c(1920,2001))

#we count the number of travels by birth date
Birth_Max<-na.omit(count(Data$Birth.Year))

#lookup by the max count of travles
subset(Birth_Max,freq==max(Birth_Max))

# --Question 2 in the 3 states combined, which month have more travels.
qplot(x=format(as.Date(Data$Start.Time),'%B'),main = "Histogram of counts of travels by month", 
      xlab = "Month of travel",ylab = "Count of Travels")

#we create the table o travel counts
count(format(as.Date(Data$Start.Time),'%B'))


# --Question 3 What's the meadian of time travel in the 3 cities combined.
ggplot(Data,aes(x=Data$Trip.Duration)) +geom_histogram(aes(y=..density..),  
                                                       colour="black", fill="white")+ geom_density(colour="red")  +scale_x_continuous(limits = c(0, 3000)) 
+ labs(x = "Time of travel") + labs(y = "count of travels")+labs(title = "Count of travels by time")	  

summary(Data$Trip.Duration)