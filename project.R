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




################################
#descriptive analysis
getwd()
setwd('C:\\Sritham\\sem2_classes\\businessAnalytics\\project1')

#----removing the NA values
RIV_users <- read.csv('RIV Users.csv', na.strings = c(""))
RIV_sources <- read.csv('RIV_sources.csv', na.strings = c(""))
RIV_sourceAndNames <- read.csv("RIV sources ID and names.csv", na.strings = c(""))

#---combining the tables
library("plyr")
library("dplyr")

#based on Element id
data <- left_join(RIV_users,RIV_sources, by = c("Element.Id" = "element_id"))
data$Birth.Place <- NULL
#based on SourceId
#presently not needed
data <- data.frame(lapply(data, function(x) { gsub(RIV_sourceAndNames$ï..source_id[367], RIV_sourceAndNames$ssource_name[367], x)}))
View(data[,"Labels"])


#persona development
data$Hs.Gpa <- as.numeric(as.character(data$Hs.Gpa))
summary(data$Hs.State)
persona1 <- data[(data$Status == "2-Prospect") & (data$Address.Home.State == "NY" | data$Address.Home.State == "ME" | data$Address.Home.State =="RI"  | data$Address.Home.State =="NH" | data$Address.Home.State =="CT" | data$Address.Home.State =="MA"),]
summary(data[data$Status == "3-Applicants" & data$Gender == "Male" & (data$Address.Home.State == "MA" | data$Address.Home.State == "NH"),"Housing"])

#gpa
Normalize <- function(gpa) {
  if (gpa > 5)
    gpa = (gpa/100)*4
  else if (gpa > 4 & gpa <= 5)
    gpa = 4
  return(gpa)
}



?gsub()
HSC.GPA <- as.numeric(as.character(data$Hs.Gpa))
GPA2 <- na.omit(HSC.GPA) ##Omit NA value
GPA <- sapply(GPA2, Normalize)



mean(GPA[GPA!=0.00]) #value was found to be 3.335695
summary(persona1$first_source_code)

#CALCULATING GPA INDIVIDUAL STATE GPA
#VT
vt <- data[data$Address.Home.State == "VT",]
HSC.GPA_vt <- as.numeric(as.character(vt$Hs.Gpa))
GPA2_vt <- na.omit(HSC.GPA_vt) ##Omit NA value
GPA_vt <- sapply(GPA2_vt, Normalize)
mean(GPA_vt[GPA_vt!= 0.00])

#ME / MA / NY / CT / RI
ri <- data[data$Address.Home.State == "RI",]
HSC.GPA_ri <- as.numeric(as.character(ri$Hs.Gpa))
GPA2_ri <- na.omit(HSC.GPA_ri) ##Omit NA value
GPA_ri <- sapply(GPA2_ri, Normalize)
mean(GPA_ri[GPA_ri!= 0.00])
#replacing taxonomy values
#data2 <- data.frame(lapply(data, function(x){
#    gsub(RIV_sourceAndNames$ï..source_id[377], RIV_sourceAndNames$ssource_name[377],RIV_sources)
#}))

#plotting using tableau
tail(sort(summary(persona1$Hs.State)))
#gender
summary(persona1$Gender)
summary(persona1$Address.Home.State)
persona1$Suspects <- as.factor(as.character(persona1$Suspects))


#prospective students application status
summary(ct$Application.Status)
summary(ma$Application.Status)
summary(me$Application.Status)
summary(nh$Application.Status)
summary(ny$Application.Status)
summary(ri$Application.Status)
summary(vt$Application.Status)
summary(data$Status)
summary(persona1$Major.) 
summary(ct$Major.) 
summary(ma$Major.) 
summary(me$Major.) 
summary(nh$Major.) 
summary(ny$Major.) 
summary(ri$Major.) 
summary(vt$Major.) 

x#to get recruitmenr sources
source_1 <- data$first_source_code
source1 <- source1

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


persona1$Population
summary(data$ï..duplicates)
DOB <- as.POSIXct(strptime(persona1$Date.Of.Birth, format="%m/%d/%y %H:%M"))

calc_age <- function(birthDate, refDate = Sys.Date()) {
  
  require(lubridate)
  
  period <- as.period(new_interval(birthDate, refDate),
                      unit = "year")
  
  period$year
  
}

applydata$DOB<-calc_age(applydata$DOB)
t <- data[data$Status =="3-Applicants" | data$Status == "4-Admits" | data$Status == "5-Deposit",]
NROW(t)
tail(sort(summary(data[data$Status =="3-Applicants" | data$Status == "4-Admits" | data$Status == "5-Deposit","Recruitment.Source."])))
temp <- data[data$Inquires == 1,"Recruitment.Source."]
tail(sort(summary(data[data$Inquires == 1,"Recruitment.Source."])),3)
is.na(data$fifth_source_code)
d <- data[]
t1 <- t$first_source_code
t1 <- t[(!is.na(t$first_source_code)) & (is.na(t$second_source_code)) & (is.na(t$third_source_code)) & (is.na(t$forth_source_code)) & (is.na(t$fifth_source_code)),]
NROW(na.omit(t$first_source_code))
NROW(na.omit(t$second_source_code))
NROW(na.omit(t$third_source_code))
NROW(na.omit(t$forth_source_code))
NROW(na.omit(t$fifth_source_code))

