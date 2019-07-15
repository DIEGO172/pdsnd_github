
library(ggplot2)
library(plyr)

ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

# we would like to review the 3 states combined
New_NY<-cbind(ny,replicate(dim(ny)[1],"NY"))
colnames(New_NY)[10]<-"City"
New_Wash<-cbind(ny,replicate(dim(ny)[1],"Washington"))
colnames(New_Wash)[10]<-"City"
New_Chi<-cbind(ny,replicate(dim(ny)[1],"Chicago"))
colnames(New_Chi)[10]<-"City"
Data<-rbind(New_NY,New_Wash,New_Chi)

# first we plot the number of birth dates by travles
qplot(x=Birth.Year, data=Data, color = I('black'),binwidth = 0.5,main = "Histogram of Counts of Travels and Birth Date", 
      xlab = "Birth Dates",ylab = "Count of Travels")+scale_x_continuous(breaks=seq(1920, 2001, by=5), lim=c(1920,2001))

# we count the number of travels by birth date
Birth_Max<-na.omit(count(Data$Birth.Year))

# lookup by the max count of travles
subset(Birth_Max,freq==max(Birth_Max))

qplot(x=format(as.Date(Data$Start.Time),'%B'),main = "Histogram of Counts of Travels by Month", 
      xlab = "Month of Travel",ylab = "Count of Travels")

# we create the table o travel counts
count(format(as.Date(Data$Start.Time),'%B'))

ggplot(Data,aes(x=Data$Trip.Duration)) +geom_histogram(aes(y=..density..),  
       colour="black", fill="white")+ geom_density(colour="red")  +scale_x_continuous(limits = c(0, 3000)) 
       + labs(x = "Time of Travel") + labs(y = "Count of Travels")+labs(title = "Count of Travels by Time")	  

summary(Data$Trip.Duration)