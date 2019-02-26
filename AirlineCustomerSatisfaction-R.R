################################################################

#USA Flights Satisfaction Survey

###############################################################
#The analysis below is done for the all the attributes in the dataset and 
#considering all the airline companies present in the dataset.

getwd()
setwd("E:/Syracuse University/Academics/Sem 1/IST 687/Projects/Final project")
project= read.csv("E:/Syracuse University/Academics/Sem 1/IST 687/Projects/Final project/dataset/satisfactionSurvey.csv")
View(project)
str(project)
summary(project)
dim(project)

#DATA CLEANING
library(data.table)
setnames(project, old=c('X..of.Flight.with.other.Airlines'), new = c('Percentage of Flight with other Airlines'))
setnames(project, old=c('Percentage of Flight with other Airlines'), new = c('Percentage.of.Flight.with.other.Airlines'))
setnames(project, old=c('Orgin.City','No..of.other.Loyalty.Cards'), new = c('Origin.City','No.of.other.Loyalty.Cards'))

project$Flight.date=gsub("-", "/", project$Flight.date, fixed=T )
project$Flight.date=as.Date(project$Flight.date, "%m/%d/%Y")
project$Flight.date= format(project$Flight.date, "%m-%d-%y")
project$Destination.City=gsub(",","", project$Destination.City)
project$Origin.City=gsub(",","", project$Origin.City)
project=project[order(project$Satisfaction),]

trimws(project$Type.of.Travel)
trimws(project$Class)
trimws(project$Airline.Code)
trimws(project$Airline.Name)
trimws(project$Origin.City)
trimws(project$Origin.State)
trimws(project$Destination.City)
trimws(project$Destination.State)

check<-which((is.na(project$Flight.time.in.minutes)&project$Flight.cancelled=="No"))
check
project<-project[-check,]

unique(project$Satisfaction)
project$Satisfaction[(project$Satisfaction == "2.5")]=3
project$Satisfaction[(project$Satisfaction == "4.00.2.00")|(project$Satisfaction == "4.00.5")]=4
project$Satisfaction=factor(project$Satisfaction, levels = c("1", "2","3","4","5"))
project$Satisfaction[(project$Satisfaction == "4.5")]=5
unique(project$Satisfaction)
num=project$Satisfaction
num
project$Satisfaction=as.numeric(levels(num))[num]
project$Price.Sensitivity <- as.numeric(as.character(project$Price.Sensitivity))
project$Year.of.First.Flight <- as.numeric(as.character(project$Year.of.First.Flight))
project$No.of.Flights.p.a. <- as.numeric(as.character(project$No.of.Flights.p.a.))
project$Percentage.of.Flight.with.other.Airlines <- as.numeric(as.character(project$Percentage.of.Flight.with.other.Airlines))
project$No.of.other.Loyalty.Cards <- as.numeric(as.character(project$No.of.other.Loyalty.Cards))
project$Shopping.Amount.at.Airport <- as.numeric(as.character(project$Shopping.Amount.at.Airport))
project$Eating.and.Drinking.at.Airport <- as.numeric(as.character(project$Eating.and.Drinking.at.Airport))
project$Day.of.Month <- as.numeric(as.character(project$Day.of.Month))
project$Scheduled.Departure.Hour <- as.numeric(as.character(project$Scheduled.Departure.Hour))
project$Departure.Delay.in.Minutes <- as.numeric(as.character(project$Departure.Delay.in.Minutes))
project$Arrival.Delay.in.Minutes <- as.numeric(as.character(project$Arrival.Delay.in.Minutes))
project$Flight.time.in.minutes <- as.numeric(as.character(project$Flight.time.in.minutes))
project$Flight.Distance <- as.numeric(as.character(project$Flight.Distance))

project=subset(project, Flight.cancelled=="No")


project$Arrival.Delay.in.Minutes[is.na(project$Arrival.Delay.in.Minutes)]=round(median(project$Arrival.Delay.in.Minutes, na.rm=TRUE))
project$Flight.time.in.minutes[is.na(project$Flight.time.in.minutes)]=round(median(project$Flight.time.in.minutes, na.rm=TRUE))

dim(project)
sum(is.na(project))

na<-which(!(project$Satisfaction %in% c(1,2,3,4,5)))
project<-project[-na,]

View(project)
str(project)
summary(project)

new=replicate(length(project$Satisfaction), "nil")
new[project$Satisfaction >= 4]="Happy"
new[project$Satisfaction < 4]="notHappy"
project$happycustomers=new
project$happycustomers

View(project)
str(project)
summary(project)
sum(is.na(project))
dim(project)

table(project$Airline.Name)

stddev_project=sapply(project, sd)
stddev_project
original=data.frame(stddev_project)
colnames(original)=c("original")
View(original)
dim(original)

set.seed(1)
index=sample(1:dim(project)[1], 60000 )
project_samp=project[index, ]
summary(project_samp)
str(project_samp)
View(project_samp)
dim(project_samp)
dim(project)

stddev_project_samp=sapply(project_samp, sd)
stddev_project_samp
sample=data.frame(stddev_project_samp)
colnames(sample)=c("sample")
View(sample)
dim(sample)

std=cbind(sample, original)
View(std)

summary(project_samp)
summary(project)

project=project[,-25]
project_samp=project_samp[,-25]

summary(project_samp)
summary(project)
dim(project_samp)
dim(project)

View(project_samp)

#DATA VISUALIZATION
options(scipen=999)

library(ggplot2)

#1.Customer Satisfaction Based on Airline
sat=table(project$Satisfaction, project$Airline.Name)
sat
airlinenamesat=barplot(sat, main="How many customers are Satisfied with an Airline?", cex.names = 0.45,
                          col=c("darkblue","red", "yellow", "orange", "pink"),ylab="Number of Customers",
                          las=2, legend=rownames(counts2), args.legend = list(x = "top", ncol=5, inset=c(0, 0), cex=.4))


#2. PriceSensitivity with respect to AirlineName
Count1=nrow(project[which(project$Price.Sensitivity == "1"),])
Count2=nrow(project[which(project$Price.Sensitivity == "2"),])
Count3=nrow(project[which(project$Price.Sensitivity == "3"),])
Count4=nrow(project[which(project$Price.Sensitivity == "4"),])
Count5=nrow(project[which(project$Price.Sensitivity == "5"),])
NewData1=table(project$Price.Sensitivity, project$Airline.Name)
NewData1
NewData1=data.frame(NewData1)
colnames(NewData1)=c("PriceSensitivity","AirlineName","Frequency")
View(NewData1)
x=NewData1
x$Satisfaction[which(x$PriceSensitivity=="1")]=x$Satisfaction[which(x$PriceSensitivity=="1")]/Count1*100
x$Satisfaction[which(x$PriceSensitivity=="2")]=x$Satisfaction[which(x$PriceSensitivity=="2")]/Count2*100
x$Satisfaction[which(x$PriceSensitivity=="3")]=x$Satisfaction[which(x$PriceSensitivity=="3")]/Count3*100
x$Satisfaction[which(x$PriceSensitivity=="4")]=x$Satisfaction[which(x$PriceSensitivity=="4")]/Count4*100
x$Satisfaction[which(x$PriceSensitivity=="5")]=x$Satisfaction[which(x$PriceSensitivity=="5")]/Count5*100
NewData1=x
ggplot(NewData1,aes(x = PriceSensitivity, fill=AirlineName, y = Frequency))+geom_col(position = "dodge")

#3. Satisfaction vs Age 
ggplot(project,aes(x=project$Age,y=project$Satisfaction))+geom_count()+
stat_summary(aes(y =project$Satisfaction,group=1), fun.y=mean, colour="red", geom="point",group=1)

#4.Class of Travel with respect to Type of travel
Count6=nrow(project[which(project$Type.of.Travel == "Personal Travel"),])
Count7=nrow(project[which(project$Type.of.Travel == "Business travel"),])
Count8=nrow(project[which(project$Type.of.Travel == "Mileage tickets"),])
NewData2=table(project$Type.of.Travel, project$Class)
NewData2=data.frame(NewData2)
colnames(NewData2)=c("TypeofTravel","ClassofTravel","Frequency")
View(NewData2)
x=NewData2
x$Satisfaction[which(x$Type.of.Travel=="Personal Travel")]=x$Satisfaction[which(x$Type.of.Travel=="Personal Travel")]/TypeCount1*100
x$Satisfaction[which(x$Type.of.Travel=="Business travel")]=x$Satisfaction[which(x$Type.of.Travel=="Business Travel")]/TypeCount2*100
x$Satisfaction[which(x$Type.of.Travel=="Mileage tickets")]=x$Satisfaction[which(x$Type.of.Travel=="Mileage Travel")]/TypeCount3*100
NewData2=x
ggplot(NewData2,aes(x = TypeofTravel, fill=ClassofTravel, y = Frequency))+geom_col(position = "dodge")

#5.Satisfaction vs. Gender
GenderSatisfaction=ggplot(project, aes(Gender, Satisfaction))+geom_boxplot(aes(fill = Gender))+labs(x="Gender", y="Satisfaction")
GenderSatisfaction=GenderSatisfaction + stat_summary(fun.y=mean, colour="darkred", geom="point", 
                                shape=18, size=3,show_guide = FALSE)
GenderSatisfaction

#6.Satisfaction vs. Type of Travel
TypeoftravelSat=ggplot(project, aes(x=Type.of.Travel, y=Satisfaction, colour= "red")) + geom_boxplot(fill="yellow")
TypeoftravelSat=TypeoftravelSat + stat_summary(fun.y=mean, colour="darkred", geom="point", 
                                               shape=18, size=3,show_guide = FALSE)
TypeoftravelSat

#7.Satisfaction with respect to airline status
AirlineStatSatisfaction=ggplot(project, aes(x=Airline.Status, y= Satisfaction)) + geom_col(aes(fill=Satisfaction))
AirlineStatSatisfaction=AirlineStatSatisfaction +xlab("Airline Status") + ylab("Count")
AirlineStatSatisfaction

#8.Satisfaction with respect to price Sensitivity
PriceSensitivitySatisfaction=ggplot(project, aes(x=Price.Sensitivity, y= Satisfaction)) + geom_col(aes(fill=Satisfaction))
PriceSensitivitySatisfaction=PriceSensitivitySatisfaction + xlab("Price Sensitivity") + ylab("Count")
PriceSensitivitySatisfaction

#9. Satisfaction with respect to Type of Travel
TypeofTravelSatisfaction=ggplot(project, aes(x=Type.of.Travel, y= Satisfaction)) + geom_col(aes(fill=Satisfaction))
TypeofTravelSatisfaction=TypeofTravelSatisfaction + xlab("Type of Travel") + ylab("Count")
TypeofTravelSatisfaction

#10.Satisfaction vs. Shopping Amount
ShoppingAmountSatisfaction=ggplot(project, aes(x=project$Shopping.Amount.at.Airport,y=project$Satisfaction))+geom_count()
ShoppingAmountSatisfaction=ShoppingAmountSatisfaction + xlab("Shopping Amount") + ylab("Satisfaction")
ShoppingAmountSatisfaction

#DATA MODELING
#1.Linear Model on the entire dataset
  sat_orig=lm(formula = Satisfaction ~ Airline.Status + Gender + Age + Price.Sensitivity + Year.of.First.Flight +No.of.Flights.p.a.+ Percentage.of.Flight.with.other.Airlines
            +Type.of.Travel
            +No.of.other.Loyalty.Cards
            +Shopping.Amount.at.Airport
            +Eating.and.Drinking.at.Airport
            +Class
            +Day.of.Month
            +Flight.date
            +Airline.Code
            +Airline.Name
            +Origin.City
            +Origin.State
            +Destination.City
            +Destination.State
            +Scheduled.Departure.Hour
            +Departure.Delay.in.Minutes
            +Arrival.Delay.in.Minutes
            +Flight.time.in.minutes
            +Flight.Distance
            +Arrival.Delay.greater.5.Mins
            , data= project)
  summary(sat_orig)

#Linear Model of each significant variable and corresponding plots

AirlineSat=lm(formula = Satisfaction ~ Airline.Status, data= project)
summary(AirlineSat)
ggplot(project, aes(x=Airline.Status, y= Satisfaction)) +geom_count() + stat_summary(aes(y=project$Satisfaction, group=1),fun.y=mean, color = "red", geom = "point", group = 1) + labs(x = "Airline Status", y = "Satisfaction")


GenderSat=lm(formula = Satisfaction ~ Gender, data= project)
summary(GenderSat)
ggplot(project, aes(x=Gender, y= Satisfaction)) +geom_count() + stat_summary(aes(y=project$Satisfaction, group=1),fun.y=mean, color = "red", geom = "point", group = 1) + labs(x = "Gender", y = "Satisfaction")


AgeSat=lm(formula = Satisfaction ~ Age, data= project)
summary(AgeSat)
ggplot(project, aes(x=Age, y= Satisfaction)) +geom_count() + stat_smooth(method = "lm", col = "red") + labs(x = "Age", y = "Satisfaction")


PriceSat=lm(formula = Satisfaction ~ Price.Sensitivity, data= project)
summary(PriceSat)
ggplot(project, aes(x=Price.Sensitivity, y= Satisfaction)) +geom_count() + stat_smooth(method = "lm", col = "red") + labs(x = "Price Sensitivity", y = "Satisfaction")


YearSat=lm(formula = Satisfaction ~ Year.of.First.Flight, data= project)
summary(YearSat)
ggplot(project, aes(x=Year.of.First.Flight, y= Satisfaction)) +geom_count() + stat_smooth(method = "lm", col = "red") + labs(x = "Year of First Flight", y = "Satisfaction")


FlightNumSat=lm(formula = Satisfaction ~ No.of.Flights.p.a., data= project)
summary(FlightNumSat)
ggplot(project, aes(x=No.of.Flights.p.a., y= Satisfaction)) +geom_count() + stat_smooth(method = "lm", col = "red") + labs(x = "Number of Flights per Airline", y = "Satisfaction")


TravelSat=lm(formula = Satisfaction ~ Type.of.Travel, data= project)
summary(TravelSat)
ggplot(project, aes(x=Type.of.Travel, y= Satisfaction)) +geom_count() + stat_summary(aes(y=project$Satisfaction, group=1),fun.y=mean, color = "red", geom = "point", group = 1) + labs(x = "Type of Travel", y = "Satisfaction")


ShoppingSat=lm(formula = Satisfaction ~ Shopping.Amount.at.Airport, data= project)
summary(ShoppingSat)
ggplot(project, aes(x=Shopping.Amount.at.Airport, y= Satisfaction)) +geom_count() + stat_smooth(method = "lm", col = "red") + labs(x = "Shopping Amount at Airport", y = "Satisfaction")


ClassSat=lm(formula = Satisfaction ~ Class, data= project)
summary(ClassSat)
ggplot(project, aes(x=Class, y= Satisfaction)) +geom_count() + stat_summary(aes(y=project$Satisfaction, group=1),fun.y=mean, color = "red", geom = "point", group = 1) + labs(x = "Class", y = "Satisfaction")

#2. SVM on the sampled values from the dataset

library(kernlab)
randIndex=sample(1:dim(project_samp)[1])
randIndex

cutPoint2_3=floor(2 * dim(project_samp)[1]/3)
cutPoint2_3


trainData=project_samp[randIndex[1:cutPoint2_3],]
trainData
View(trainData)

testData=project_samp[randIndex[(cutPoint2_3+1):dim(project_samp)[1]],]
testData
str(testData)
View(testData)

dim(trainData)
dim(testData)

newtrainData=trainData[,c(2,3,4,5,6,7,9,11,13,28)]
newtestData=testData[,c(2,3,4,5,6,7,9,11,13,28)]
str(newtestData)
str(newtrainData)
svmop=ksvm(happycustomers ~., data=newtrainData, kernel = "vanilladot",kpar="automatic", C=5,cross=3, prob.model=TRUE)
svmop

#Output of support vectors
hist(alpha(svmop)[[1]], main="support vector histogram with C=5",
     xlab="support vector values")

svmPred <- predict(svmop, newtestData, type = "votes")
svmPred
str(svmPred)
head(svmPred)
dim(svmPred)
svmPred[1,]

# Creating a confusion matrix
comTable=data.frame(newtestData[ ,10], svmPred[1, ])
comTable[comTable=="0"]="notHappy"
comTable[comTable=="1"]="Happy"
table(comTable)

#calculating the error rate
t<-table(comTable)
sum(t[1,2]+t[2,1])/sum(t)


#3. Associtaive Rule Mining

library(arules)
library(arulesViz)

project_ar=project
str(project_ar)
View(project_ar)

change1=function(vec)
{
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec > 4]="High"
  vBuckets[vec < 4]="Low"
  return(vBuckets)
}

Satisfaction_ar=change1(project$Satisfaction)
unique(Satisfaction_ar)

Price.Sensitivity_ar=change1(project$Price.Sensitivity)
unique(Price.Sensitivity_ar)


change2=function(v)
{
  q=quantile(v, c(0.4, 0.6))
  vBuckets=replicate(length(v), "Average")
  vBuckets[v <= q[1]]="Low"
  vBuckets[v > q[2]]="High"
  return(vBuckets)
}

Age_ar=change2(project$Age)
unique(Age_ar)

No.of.Flights.p.a_ar=change2(project$No.of.Flights.p.a.)
unique(No.of.Flights.p.a_ar)

Shopping.Amount.at.Airport_ar=change2(project$Shopping.Amount.at.Airport)
unique(Shopping.Amount.at.Airport_ar)

Percentage.of.Flight.with.other.Airlines_ar=change2(project$Percentage.of.Flight.with.other.Airlines)
unique(Percentage.of.Flight.with.other.Airlines_ar)

No.of.other.Loyalty.Cards_ar=change2(project$No.of.other.Loyalty.Cards)
unique(No.of.other.Loyalty.Cards_ar)

Eating.and.Drinking.at.Airport_ar=change2(project$Eating.and.Drinking.at.Airport)
unique(Eating.and.Drinking.at.Airport_ar)

Departure.Delay.in.Minutes_ar=change2(project$Departure.Delay.in.Minutes)
unique(Departure.Delay.in.Minutes_ar)

project_ar$Arrival.Delay.in.Minutes_ar=change2(project$Arrival.Delay.in.Minutes)
unique(project_ar$Arrival.Delay.in.Minutes_ar)

change3=function(v)
{
  q=quantile(v, c(0.4, 0.6))
  vBuckets=replicate(length(v), "Average")
  vBuckets[v <= q[1]]= "Least Recent"
  vBuckets[v > q[2]]= "Most Recent"
  return(vBuckets)
}

Year.of.First.Flight_ar=change3(project$Year.of.First.Flight)
unique(Year.of.First.Flight_ar)


ProjectSurvey=data.frame(Satisfaction_ar, project$Airline.Status, project$Gender, Age_ar, Price.Sensitivity_ar, Year.of.First.Flight_ar, No.of.Flights.p.a_ar, project$Type.of.Travel, Shopping.Amount.at.Airport_ar, project$Class)
ProjectSurveyx=as(ProjectSurvey, "transactions")

ruleset=apriori(ProjectSurveyx, parameter = list(support=0.05, confidence =0.05), appearance = list(default="lhs",rhs=("Satisfaction_ar=High")))

inspect(ruleset)

ruleset_filter=subset(ruleset, subset=rhs %in% c("Satisfaction_ar=High") & lift>2)
ruleset_filter=sort(ruleset_filter, decreasing=T, by="count")
inspect(ruleset_filter)
plot(ruleset_filter, method="graph")

###########################################################
#Using a susbet of data by taking out SouthEast Airlines Co. from the cleaned dataframe 
#using all the other airlines to compare the performance between Southeast Airlines Co.
# and all the other airlines.
#This is done to provide recommendations to SouthEast Airlines co. to improve their business


#DATA CLEANING AND MUNGING
#SouthEast Airlines Co.
project_SE=subset(project, Airline.Name=="Southeast Airlines Co. ")
project_SE
dim(project_SE)

#Subset of SouthEast Airlines Co.
project_SE_samp=subset(project_samp, Airline.Name=="Southeast Airlines Co. ")
project_SE_samp
dim(project_SE_samp)

#All other airlines except SouthEast
project_other=subset(project, Airline.Name!="Southeast Airlines Co. ")
project_other
dim(project_other)

#Subset of all other airlines except SouthEast
project_other_samp=subset(project_samp, Airline.Name!="Southeast Airlines Co. ")
project_other_samp
dim(project_other_samp)

project_SE$Price.Sensitivity <- as.numeric(as.character(project_SE$Price.Sensitivity))
project_SE$Year.of.First.Flight <- as.numeric(as.character(project_SE$Year.of.First.Flight))
project_SE$No.of.Flights.p.a. <- as.numeric(as.character(project_SE$No.of.Flights.p.a.))
project_SE$Percentage.of.Flight.with.other.Airlines <- as.numeric(as.character(project_SE$Percentage.of.Flight.with.other.Airlines))
project_SE$No.of.other.Loyalty.Cards <- as.numeric(as.character(project_SE$No.of.other.Loyalty.Cards))
project_SE$Shopping.Amount.at.Airport <- as.numeric(as.character(project_SE$Shopping.Amount.at.Airport))
project_SE$Eating.and.Drinking.at.Airport <- as.numeric(as.character(project_SE$Eating.and.Drinking.at.Airport))
project_SE$Day.of.Month <- as.numeric(as.character(project_SE$Day.of.Month))
project_SE$Scheduled.Departure.Hour <- as.numeric(as.character(project_SE$Scheduled.Departure.Hour))
project_SE$Departure.Delay.in.Minutes <- as.numeric(as.character(project_SE$Departure.Delay.in.Minutes))
project_SE$Arrival.Delay.in.Minutes <- as.numeric(as.character(project_SE$Arrival.Delay.in.Minutes))
project_SE$Flight.time.in.minutes <- as.numeric(as.character(project_SE$Flight.time.in.minutes))
project_SE$Flight.Distance <- as.numeric(as.character(project_SE$Flight.Distance))

#DATA VISUALIZATION

#1.Satisfaction vs. Age for Southeast
ggplot(project_SE,aes(x=project_SE$Age,y=project_SE$Satisfaction))+geom_count()+
  stat_summary(aes(y =project_SE$Satisfaction,group=1), fun.y=mean, colour="red", geom="point",group=1)

#2.Satisfaction Vs. Age for all other airlines
ggplot(project_other,aes(x=project_other$Age,y=project_other$Satisfaction))+geom_count()+
  stat_summary(aes(y =project_other$Satisfaction,group=1), fun.y=mean, colour="red", geom="point",group=1)

#3.Satisfaction vs. Arrival Delay for SouthEast
ggplot(project_SE,aes(x=project_SE$Arrival.Delay.in.Minutes,y=project_SE$Satisfaction))+geom_count()+
  stat_summary(aes(y =project_SE$Satisfaction,group=1), fun.y=mean, colour="red", geom="point",group=1)

#4.Satisfaction vs. Arrival Delay for Other airlines
ggplot(project_other,aes(x=project_other$Arrival.Delay.in.Minutes,y=project_other$Satisfaction))+geom_count()+
  stat_summary(aes(y =project_other$Satisfaction,group=1), fun.y=mean, colour="red", geom="point",group=1)


#5.Satisfaction vs. Departure Delay for Southeast
ggplot(project_SE,aes(x=project_SE$Departure.Delay.in.Minutes,y=project_SE$Satisfaction))+geom_count()+
  stat_summary(aes(y =project_SE$Satisfaction,group=1), fun.y=mean, colour="red", geom="point",group=1)

#6.Satisfaction vs. Departure Delay for other airlines
ggplot(project_other,aes(x=project_other$Departure.Delay.in.Minutes,y=project_other$Satisfaction))+geom_count()+
  stat_summary(aes(y =project_other$Satisfaction,group=1), fun.y=mean, colour="red", geom="point",group=1)

#7.Satisfaction vs. Airline Status for SE
ggplot(project_SE,aes(x=project_SE$Airline.Status,y=project_SE$Satisfaction))+geom_count()+
  stat_summary(aes(y =project_SE$Satisfaction,group=1), fun.y=mean, colour="red", geom="point",group=1)

#8.Satisfaction vs. Airline Status for other airlines
ggplot(project_other,aes(x=project_other$Airline.Status,y=project_other$Satisfaction))+geom_count()+
  stat_summary(aes(y =project_other$Satisfaction,group=1), fun.y=mean, colour="red", geom="point",group=1)

#9.Satisfaction vs. Gender for SE
ggplot(project_SE,aes(x=project_SE$Gender,y=project_SE$Satisfaction))+geom_count()+
  stat_summary(aes(y =project_SE$Satisfaction,group=1), fun.y=mean, colour="red", geom="point",group=1)

#10.Satisfaction vs. Gender for other airlines
ggplot(project_other,aes(x=project_other$Gender,y=project_other$Satisfaction))+geom_count()+
  stat_summary(aes(y =project_other$Satisfaction,group=1), fun.y=mean, colour="red", geom="point",group=1)

#11.Satisfaction vs. No of Flights per airline for SE
ggplot(project_SE,aes(x=project_SE$No.of.Flights.p.a.,y=project_SE$Satisfaction))+geom_count()+
  stat_summary(aes(y =project_SE$Satisfaction,group=1), fun.y=mean, colour="red", geom="point",group=1)

#12.Satisfaction vs.No. of Flights per airline for all other airlines
ggplot(project_other,aes(x=project_other$No.of.Flights.p.a.,y=project_other$Satisfaction))+geom_count()+
  stat_summary(aes(y =project_other$Satisfaction,group=1), fun.y=mean, colour="red", geom="point",group=1)

#13.Satisfaction vs.class for SE
ggplot(project_SE,aes(x=project_SE$Class,y=project_SE$Satisfaction))+geom_count()+
  stat_summary(aes(y =project_SE$Satisfaction,group=1), fun.y=mean, colour="red", geom="point",group=1)

#14.Satisfaction vs. class for all other airlines
ggplot(project_other,aes(x=project_other$Class,y=project_other$Satisfaction))+geom_count()+
  stat_summary(aes(y =project_other$Satisfaction,group=1), fun.y=mean, colour="red", geom="point",group=1)

#15.Satisfaction vs. Price Sensitivity for SE
ggplot(project_SE,aes(x=project_SE$Price.Sensitivity,y=project_SE$Satisfaction))+geom_count()+
  stat_summary(aes(y =project_SE$Satisfaction,group=1), fun.y=mean, colour="red", geom="point",group=1)

#16.Satisfaction vs.No. of Flights per airline for all other airlines
ggplot(project_other,aes(x=project_other$Price.Sensitivity,y=project_other$Satisfaction))+geom_count()+
  stat_summary(aes(y =project_other$Satisfaction,group=1), fun.y=mean, colour="red", geom="point",group=1)


str(project_SE)
summary(project_SE)


#DATA MODELING
#1.Linear model on Southeast Airlines
SE=lm(formula = Satisfaction ~ Airline.Status + Gender + Age + Price.Sensitivity + Year.of.First.Flight +No.of.Flights.p.a.+ Percentage.of.Flight.with.other.Airlines
            +Type.of.Travel
            +No.of.other.Loyalty.Cards
            +Shopping.Amount.at.Airport
            +Eating.and.Drinking.at.Airport
            +Class
            +Day.of.Month
            +Flight.date
            +Origin.City
            +Origin.State
            +Destination.City
            +Destination.State
            +Scheduled.Departure.Hour
            +Departure.Delay.in.Minutes
            +Arrival.Delay.in.Minutes
            +Flight.time.in.minutes
            +Flight.Distance
            +Arrival.Delay.greater.5.Mins
            ,data= project_SE)
summary(SE)

#2.Linear Model on all other airlines
other=lm(formula = Satisfaction ~ Airline.Status + Gender + Age + Price.Sensitivity + Year.of.First.Flight +No.of.Flights.p.a.+ Percentage.of.Flight.with.other.Airlines
            +Type.of.Travel
            +No.of.other.Loyalty.Cards
            +Shopping.Amount.at.Airport
            +Eating.and.Drinking.at.Airport
            +Class
            +Day.of.Month
            +Flight.date
            +Airline.Code
            +Airline.Name
            +Origin.City
            +Origin.State
            +Destination.City
            +Destination.State
            +Scheduled.Departure.Hour
            +Departure.Delay.in.Minutes
            +Arrival.Delay.in.Minutes
            +Flight.time.in.minutes
            +Flight.Distance
            +Arrival.Delay.greater.5.Mins
            ,data= project_other)
summary(other)

dim(project_SE)
dim(project_other)
View(project_SE)

#3. SVM on SothEast Airlines
randIndex_SE=sample(1:dim(project_SE)[1])
randIndex_SE

cutPoint2_3_SE=floor(2 * dim(project_SE)[1]/3)
cutPoint2_3_SE


trainData_SE=project_SE[randIndex_SE[1:cutPoint2_3_SE],]
trainData_SE
View(trainData_SE)

testData_SE=project_SE[randIndex_SE[(cutPoint2_3_SE+1):dim(project_SE)[1]],]
testData_SE
str(testData_SE)
View(testData_SE)

dim(trainData_SE)
dim(testData_SE)

newtrainData_SE=trainData_SE[,c(2,3,4,7,9,28)]
newtestData_SE=testData_SE[,c(2,3,4,7,9,28)]
str(newtestData_SE)
str(newtrainData_SE)
View(newtestData_SE)

svmop_SE=ksvm(happycustomers ~., data=newtrainData_SE, kernel = "vanilladot",kpar="automatic", C=5,cross=3, prob.model=TRUE)
svmop_SE

#Output of support vectors
hist(alpha(svmop_SE)[[1]], main="support vector histogram with C=5",
     xlab="support vector values")

svmPred_SE=predict(svmop_SE, newtestData_SE, type = "votes")
svmPred_SE
str(svmPred_SE)
head(svmPred_SE)
dim(svmPred_SE)
svmPred_SE[1,]

#Creating a confusion matrix
comTable_SE=data.frame(newtestData_SE[ ,6], svmPred_SE[1, ])
comTable_SE[comTable_SE=="0"]="notHappy"
comTable_SE[comTable_SE=="1"]="Happy"
table(comTable_SE)

#calculating the error rate
t_SE<-table(comTable_SE)
sum(t_SE[1,2]+t_SE[2,1])/sum(t_SE)


#4.Associative Rule Mining on SouthEast Airlines

project_SE_ar=project_SE
str(project_SE_ar)


change1_SE=function(vec)
{
  vBuckets <- replicate(length(vec), "Average")
  vBuckets[vec > 4]="High"
  vBuckets[vec < 4]="Low"
  return(vBuckets)
}

Satisfaction_SE_ar=change1_SE(project_SE$Satisfaction)
unique(Satisfaction_SE_ar)

project_SE_ar$Price.Sensitivity_SE_ar=change1_SE(project_SE$Price.Sensitivity)
unique(project_SE_ar$Price.Sensitivity_SE_ar)


change2_SE=function(v)
{
  q=quantile(v, c(0.4, 0.6))
  vBuckets=replicate(length(v), "Average")
  vBuckets[v <= q[1]]="Low"
  vBuckets[v > q[2]]="High"
  return(vBuckets)
}

project_SE_ar$Age_SE_ar=change2_SE(project_SE$Age)
unique(project_SE_ar$Age_SE_ar)

project_SE_ar$No.of.Flights.p.a_SE_ar=change2_SE(project_SE$No.of.Flights.p.a.)
unique(project_SE_ar$No.of.Flights.p.a_SE_ar)

project_SE_ar$Shopping.Amount.at.Airport_SE_ar=change2_SE(project_SE$Shopping.Amount.at.Airport)
unique(project_SE_ar$Shopping.Amount.at.Airport_SE_ar)

project_SE_ar$Percentage.of.Flight.with.other.Airlines_SE_ar=change2_SE(project_SE$Percentage.of.Flight.with.other.Airlines)
unique(project_SE_ar$Percentage.of.Flight.with.other.Airlines_SE_ar)

project_SE_ar$No.of.other.Loyalty.Cards_SE_ar=change2_SE(project_SE$No.of.other.Loyalty.Cards)
unique(project_SE_ar$No.of.other.Loyalty.Cards_SE_ar)

project_SE_ar$Eating.and.Drinking.at.Airport_SE_ar=change2_SE(project_SE$Eating.and.Drinking.at.Airport)
unique(project_SE_ar$Eating.and.Drinking.at.Airport_SE_ar)

project_SE_ar$Departure.Delay.in.Minutes_SE_ar=change2_SE(project_SE$Departure.Delay.in.Minutes)
unique(project_SE_ar$Departure.Delay.in.Minutes_SE_ar)

project_SE_ar$Arrival.Delay.in.Minutes_SE_ar=change2_SE(project_SE$Arrival.Delay.in.Minutes)
unique(project_SE_ar$Arrival.Delay.in.Minutes_SE_ar)

change3_SE=function(v)
{
  q=quantile(v, c(0.4, 0.6))
  vBuckets=replicate(length(v), "Average")
  vBuckets[v <= q[1]]= "Least Recent"
  vBuckets[v > q[2]]= "Most Recent"
  return(vBuckets)
}

project_SE_ar$Year.of.First.Flight_SE_ar=change3_SE(project_SE$Year.of.First.Flight)
unique(project_SE_ar$Year.of.First.Flight_SE_ar)

ProjectSurvey_SE=data.frame(Satisfaction_SE_ar, project_SE$Airline.Status, project_SE$Gender, project_SE_ar$Age_SE_ar, project_SE_ar$Price.Sensitivity_SE_ar, project_SE_ar$Year.of.First.Flight_SE_ar, project_SE_ar$No.of.Flights.p.a_SE_ar, project_SE$Type.of.Travel,project_SE_ar$Shopping.Amount.at.Airport_SE_ar, project_SE$Class)
ProjectSurveyx_SE=as(ProjectSurvey_SE, "transactions")

ruleset_SE=apriori(ProjectSurveyx_SE, parameter = list(support=0.05, confidence =0.05), appearance = list(default="lhs",rhs=("Satisfaction_SE_ar=High")))
inspect(ruleset_SE)


