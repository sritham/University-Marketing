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
    gpa <- (gpa/100)*4
  else if (gpa > 4 & gpa <= 5)
    gpa <- 4
  return(gpa)
}

getwd()
setwd('/Users/julesbarbosa/Business Analytics/project')

#----removing the NA values
RIV_users <- read.csv('RIV Users.csv', na.strings = c(""))
RIV_sources <- read.csv('RIV_sources.csv', na.strings = c(""))
RIV_sourceAndNames <- read.csv("RIV sources ID and names.csv", na.strings = c(""))


##  $61,372. Pew defines the middle class as those earning between 
## two-thirds and double the median household income.f


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

sd(GPAnormal)
mean(GPAnormal)
boxplot(GPAnormal, outline = FALSE)

Outlier <- subset(GPAnormal, GPAnormal!=0 )
mean(Outlier)
View(table(Outlier))
length(subset(Outlier, Outlier<3))
length(subset(Outlier, Outlier>3 & Outlier<=3.33))
length(subset(Outlier, Outlier > 3.33 & Outlier <= 3.67))
length(subset(Outlier, Outlier<3.67 & Outlier <=4))

outlive <- table(AdmitedNursing$On.Campus.ROL)
x <- na.omit(AdmitedNursing$Household.Income.Median)
meand(x)
mean(x)
sd(x)
boxplot(x, outline = FALSE)
