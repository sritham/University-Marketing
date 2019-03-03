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
