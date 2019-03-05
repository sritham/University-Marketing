getwd()
setwd('/Users/julesbarbosa/Business Analytics/project')

#----removing the NA values
RIV_users <- read.csv('RIV Users.csv', na.strings = c(""))
RIV_sources <- read.csv('RIV_sources.csv', na.strings = c(""))
RIV_sourceAndNames <- read.csv("RIV sources ID and names.csv", na.strings = c(""))

#---combining the tables

#based on Element id
data <- merge(RIV_users, RIV_sources, by='merged', by.x = "Element.Id", by.y = "element_id", all.x = T)
data$Birth.Place <- NULL
#subsets
Admited <- subset(data, data$Status == "4-Admits")
AdmitedW <- subset(Admited, Admited$Gender == "Female")

summary(AdmitedW)

Normalize <- function(gpa) {
    if (gpa > 5)
      gpa = (gpa/100)*4
    else if (gpa > 4 & gpa <= 5)
      gpa = 4
    return(gpa)
}


HSC.GPA <- as.numeric(as.character(AdmitedNursing$Hs.Gpa))
GPA2 <- na.omit(HSC.GPA) ##Omit NA value
GPAnormal <- sapply(GPA2, Normalize)


##Xuan Chen
applydata$DOB <- as.POSIXct(strptime(applydata$Date.Of.Birth, "%m/%d/%y %H:%M"))

calc_age <- function(birthDate, refDate = Sys.Date()) {
  
  require(lubridate)
  
  period <- as.period(new_interval(birthDate, refDate),
                      unit = "year")
  
  period$year
  
}

applydata$DOB<-calc_age(applydata$DOB)





#Source code-Source Name

RIV_users <- read.csv('RIV Users.csv', na.strings = c(""))
RIV_sources <- read.csv('RIV_sources.csv', na.strings = c(""))
RIV_sourceAndNames <- read.csv("RIV sources ID and names.csv", na.strings = c(""))


library("plyr")
library("dplyr")

data2<- left_join(RIV_sources,RIV_sourceAndNames, by = c("first_source_code" = "source_id"))
data2$first_source_code<-data2$ssource_name
data3<- left_join(RIV_sources,RIV_sourceAndNames, by = c("second_source_code" = "source_id"))
data2$second_source_code<-data3$ssource_name
data4<- left_join(RIV_sources,RIV_sourceAndNames, by = c("third_source_code" = "source_id"))
data2$third_source_code<-data4$ssource_name
data5<- left_join(RIV_sources,RIV_sourceAndNames, by = c("forth_source_code" = "source_id"))
data2$forth_source_code<-data5$ssource_name
data6<- left_join(RIV_sources,RIV_sourceAndNames, by = c("fifth_source_code" = "source_id"))
data2$fifth_source_code<-data6$ssource_name

data <- left_join(RIV_users,RIV_sources, by = c("Element.Id" = "element_id"))



First source: 
CollegeBoard 86013/131118

Second Source:
250OK  843/4314

Third Source:
Royall Spring Search    3598/7168

Forth Source:
Common App 169/623

Fifth Source:
Common App 74/282






