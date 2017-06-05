#################
## Gender and Diversity Methodology Paper
## Project: How to Research Gender and Diversity in the IR Curriculum
## Authors: GCGMKP
## Data created: 4 February 2017
## Last Update: 18 May 2017
#################
## SYSTEM REQUIREMENTS
## This script was developed and run under R64.app operating under and OS10.12.4.
## R version: R 3.4.0
#################

rm(list=ls())

#Loading libraries
library(foreign)
library(Hmisc)
library(dplyr)
library(stringr)
library(ggplot2)
#library(stargazer)
library(RColorBrewer)

if(Sys.info()["user"]=="gokhan"){pathOUT="~/Dropbox/Projects/GDP/data/outputData";
pathIN="~/Dropbox/Projects/GDP/data/inputData";pathR="~/Dropbox/Projects/GDP/Rcode";
pathRep="~/Dropbox/Projects/GDP/Replication";pathM="~/Dropbox/Projects/GDP/manuscript"}

setwd(pathIN)

#Read in data
gender <- read.csv("gender.csv")
diversity <- read.csv("diversity.csv")
convener <- read.csv("convener.csv")

#Filter unique entries (remove duplicates)
#gender <- unique(gender)
#diversity <- unique(diversity)

#Subset the gender data
want.var <- c("Title","Author","Editor","AutGen","EdGen","AutM","AutF","EdM","EdF","Course","Type",
              "Importance","Year","Publisher")
want <- which(colnames(gender) %in% want.var)
gender <- gender[,want]

#Merge the data
gender <- merge(gender,convener,by="Course",all.x=TRUE)
want.var <- c("Course","Cluster")
want <- which(colnames(convener) %in% want.var)
convener <- convener[,want]
diversity <- merge(diversity,convener,by="Course")

#Clean up and create new variables
gender$Code <- substr(gender$Course,3,5) #Extract course code
gender$Code <- as.integer(gender$Code)
gender$Level <- cut(gender$Code,
                  breaks=c(0,400,500,Inf),
                  labels=c("Undergrad","Masters","PhD")) #Split UG/MA/PhD levels
gender$Importance[gender$Importance==""] <- NA
gender$AutGen[gender$AutGen==""] <- NA
gender$Author[gender$Author==""] <- NA
gender <- gender[!is.na(gender$Author),]
gender <- gender[gender$Type=="Book" | gender$Type=="Article",] #Subset to books and articles only

#Editor subset
gender$Editor[gender$Editor==""] <- NA
editor <- gender[!is.na(gender$Editor),]

#Transform variables for logistic regression
gender$Female <- ifelse(gender$AutF>0,1,0)
editor$Female <- ifelse(editor$EdF>0,1,0)
gender$Female <- as.factor(gender$Female)
editor$Female <- as.factor(editor$Female)

#Graphs by date of publication
ggplot(gender[gender$Year>1965 & gender$Year<2017,],aes(x=Year,fill=Female)) +
  geom_histogram(binwidth=.5,alpha=.5,position="identity") +
  scale_fill_brewer(palette="Set1") +
  scale_x_continuous(name="Date of Publication") +
  scale_y_continuous(name="Times Included in Reading List")

#ggplot(editor[editor$Year>1965,],aes(x=Year,fill=EdF)) +
#  geom_histogram(binwidth=.5,alpha=.5,position="identity") +
#  scale_x_continuous(name="Date of Publication") +
#  scale_y_continuous(name="Times Included in Reading List") +
#  scale_fill_brewer(name="Number of Female Editors",palette="Set1")

#Frequency of female author publications by year
ggplot(gender[gender$Year>1945 & gender$AutF>0,],aes(x=Year)) +
  geom_histogram(binwidth=1,alpha=.5,position="identity") +
  scale_fill_brewer(palette="Set1")

#Summary statistics by course, distribution of female authors
tapply(gender$AutF,gender$Course,describe) #n=total number of works,distribution for females only

#Co-authorship statistics
#One male
nrow(gender[gender$AutM>0,])
nrow(gender[gender$AutM>0,])/nrow(gender) #percentage of overall male involvement
nrow(gender[gender$AutM>0 & gender$Type=="Article",]) #article subset
nrow(gender[gender$AutM>0 & gender$Type=="Article",])/nrow(gender[gender$Type=="Article",]) #article percetage
nrow(gender[gender$AutM>0 & gender$Type=="Book",]) #book subset
nrow(gender[gender$AutM>0 & gender$Type=="Book",])/nrow(gender[gender$Type=="Book",]) #book percentage
#One female
nrow(gender[gender$AutF>0,])
nrow(gender[gender$AutF>0,])/nrow(gender) #percentage of overall female involvement
nrow(gender[gender$AutF>0 & gender$Type=="Article",]) #article subset
nrow(gender[gender$AutF>0 & gender$Type=="Article",])/nrow(gender[gender$Type=="Article",]) #article percetage
nrow(gender[gender$AutF>0 & gender$Type=="Book",]) #book subset
nrow(gender[gender$AutF>0 & gender$Type=="Book",])/nrow(gender[gender$Type=="Book",]) #book percentage
#One male and one female
nrow(gender[gender$AutM>0 & gender$AutF>0,])
nrow(gender[gender$AutM>0 & gender$AutF>0,])/nrow(gender) #percentage
nrow(gender[gender$AutM>0 & gender$AutF>0 & gender$Type=="Article",]) #article subset
nrow(gender[gender$AutM>0 & gender$AutF>0 & gender$Type=="Article",])/nrow(gender[gender$Type=="Article",]) #article percetage
nrow(gender[gender$AutM>0 & gender$AutF>0 & gender$Type=="Book",]) #book subset
nrow(gender[gender$AutM>0 & gender$AutF>0 & gender$Type=="Book",])/nrow(gender[gender$Type=="Book",]) #book percentage
#Female co-authorship preferences
describe(gender$AutF[gender$AutF>0]) #with other females
describe(gender$AutM[gender$AutF>0]) #with males

#Run logistic regression (outcome=female author)

#logit.g <- glm(Female~Type+Importance+Year+Level+Convener+Rank+Cluster,
#             data=gender,family="binomial",subset=gender$Year>1945)
#summary(logit.g)

#logit.ge <- glm(Female~Type+Importance+Year+Level+Convener+Rank+Cluster,
#               data=editor,family="binomial",subset=gender$Year>1945)
#summary(logit.ge)

#logit.d <- glm(Female~Type+Importance+Year+Sexuality+Race+Colonialism+NonWEIRD+Level+Convener+Rank+Cluster,
#             data=diversity,family="binomial",subset=diversity$Year>1945)
#summary(logit.d)

#Diversity models

#Colonialism
#col <- glm(Colonialism~Type+Importance+Year+Female+Level+Convener+Rank,
#           data=diversity,family="binomial",subset=diversity$Year>1945)
#summary(col)

#NonWEIRD
#non <- glm(NonWEIRD~Type+Importance+Year+Female+Level+Convener+Rank,
#           data=diversity,family="binomial",subset=diversity$Year>1945)
#summary(non)

#coef1 <- c("Book","Chapter","Document","Journal","Webpage","Essential","Suggested Student Purchase",
#           "Publication Year","Master's","PhD","Male Convener","Associate Professor","Fellow/Adjunct",
#           "Professor")

#coef2 <- c("Book","Chapter","Document","Journal","Webpage","Essential","Suggested Student Purchase",
#           "Publication Year","Female Author","Master's","PhD","Male Convener",
#           "Associate Professor","Fellow/Adjunct","Professor")

#stargazer(logit.g,logit.ge,
#          title="Predictors of Female Inclusion in the IR Curriculum",
#          align=TRUE,digits=2,omit="ConvenerMF",style="ajps",dep.var.labels=c("Female Author",
#          "Female Editor"),
#          model.names=TRUE,model.numbers=TRUE,object.names=FALSE,multicolumn=TRUE,
#          no.space=TRUE,header=FALSE,notes.label=c("Standard errors in parentheses"),
#          notes.align="r",notes.append=TRUE,covariate.labels=coef1,type="text",out="gender.htm")

#stargazer(sex,race,col,non,
#          title="Predictors of Diversity in the IR Curriculum",
#          align=TRUE,digits=2,omit="ConvenerMF",style="ajps",dep.var.labels=c("Gender","Race","Colonialism","Non-WEIRD"),
#          model.names=FALSE,model.numbers=TRUE,object.names=FALSE,multicolumn=TRUE,
#          no.space=TRUE,header=FALSE,notes.label=c("Standard errors in parentheses"),
#          notes.align="r",notes.append=TRUE,covariate.labels=coef2,type="text",out="div.htm")





#####Hypotheses Testing#####





#1a Male-Female Inclusion
t.test(gender$AutM,gender$AutF)

#1b Essential Readings
essential <- glm(gender$Importance=="Essential"~Female+Type+Year+Level+Convener+Rank+Cluster,
                 data=gender,family="binomial")
summary(essential)

#2 Single-Author
gender$single.female <- ifelse(gender$AutF==1 & gender$AutM==0,1,0)
gender$single.male <- ifelse(gender$AutF==0 & gender$AutM==1,1,0)
gender$single <- ifelse(gender$single.female==1 | gender$single.male==1,1,0)
single <- glm(gender$single~Female+Type+Year+Level+Convener+Rank+Cluster,
              data=gender,family="binomial")
summary(single)

#3 Book
book <- glm(gender$Type=="Book"~Female+Importance+Year+Level+Convener+Rank+Cluster,
                 data=gender,family="binomial")
summary(book)

#4 Top Journals
top.j <- c("International Organization","International security","American Political Science Review",
           "International Studies Quarterly","Foreign Policy Analysis","European Journal of International Relations",
           "The Journal of Conflict Resolution","World Politics","Review of international political economy")
gender$top <- ifelse(is.element(gender$Title,top.j),1,0)
gender$top <- as.factor(gender$top)
journal <- glm(top~Female+Importance+Year+Level+Convener+Rank+Cluster,
            data=gender,family="binomial")
summary(journal)

#5 Publishers
top.uni <- c("Cambridge University Press","Routledge","Oxford University Press","Cornell University Press",
             "Palgrave Macmillan","The MIT Press","Princeton University Press","Columbia University Press")
gender$top.press <- ifelse(is.element(gender$Publisher,top.uni),1,0)
gender$top.press <- as.factor(gender$top.press)
press <- glm(top.press~Female+Importance+Year+Level+Convener+Rank+Cluster,
               data=gender,family="binomial")
summary(press)

#6 First Author & 7 Last Author
#library(genderizeR)
#gender$given <- sub('.*,\\s*', '', gender$Author)
#givenNames <- findGivenNames(gender$given)
#gender$pred <- genderize(gender$given,givenNames)

gender$two <- ifelse(gender$AutM==1 & gender$AutF==1,1,0)
describe(gender$AutGen[gender$two==1])
gender$three <- ifelse(gender$AutM>0 & gender$AutF>0 & gender$AutM+gender$AutF==3,1,0)
describe(gender$AutGen[gender$three==1])

gender$Author <- as.character(gender$Author)
authors <- NA
for(i in 1:dim(gender)[1]){
authors[i] <- strsplit(gender$Author[i],split=";")
}

#8 Co-author gender
gender$female.comale <- ifelse(gender$AutF>0 & gender$AutM>0,1,0)
gender$female.cofemale <- ifelse(gender$AutF>1 & gender$AutM==0,1,0)
t.test(gender$female.comale,gender$female.cofemale)

#9 Co-authoring with men vs. single-author
t.test(gender$female.comale,gender$single==1 & gender$AutM==0)

#10 Gender studies & 17a Time x Female
sex <- glm(Sexuality~Type+Importance+Year*Female+Level+Convener+Rank+Cluster,
          data=diversity,family="binomial",subset=diversity$Year>1945)
summary(sex)

#11 Imperialism/Race
race <- glm(Race~Type+Importance+Year+Female+Level+Convener+Rank+Cluster,
           data=diversity,family="binomial",subset=diversity$Year>1945)
summary(race)

#12 Male Convener & 13 Junior Faculty & 15 Course Level & 16 Non-Core Courses & Self-Cite
core <- c("100","200","202","203","410","436","450","501","509")
gender$Core <- ifelse(is.element(gender$Code,core),1,0)
gender$Core <- as.factor(gender$Core)
gender$Name <- as.character(gender$Name)
gender$Self <- mapply(function(x,y) all(x %in% y), 
                     str_extract_all(gender$Author,"\\w+"),str_extract_all(gender$Name,"\\w+"))
logit.g <- glm(Female~Type+Importance+Year+Level+Convener+Rank+Core+top+top.press+Self,
               data=gender,family="binomial")
summary(logit.g)

#Convert odds to percentages
odds.g <- round(exp(logit.g$coefficients),2)-1
odds.g #these mean "times more likely", i.e. 0 equals no effect, 1 means 100% more likely, -1 100% less

#17 Temporal Patterns
ols.time <- lm(Year~Female+Type+Importance+Year+Level+Convener+Rank+single+top+top.press,
               data=gender)
summary(ols.time)

#5 - 14 Publisher Tables and Yearly Inclusion
library(plyr)
gender$Female <- as.integer(gender$Female)-1
pub <- ddply(gender, .(Publisher), summarize, Total=length(Publisher), Female=sum(Female))
pub$Ratio <- (pub$Female)/(pub$Total)
pub <- pub[pub$Total>9 & pub$Total<1000,] #Require a minimum of ten items
head(arrange(pub,desc(Ratio)),n=25) #Top 25 publishers
tail(arrange(pub,desc(Ratio)),n=25) #Bottom 25 publishers

#jou <- ddply(gender, .(Title), summarize, Total=length(Title), Female=sum(Female))
#jou$Ratio <- (jou$Female)/(jou$Total)

aut <-  ddply(gender, .(Author), summarize, Total=length(Author))

detach(package:plyr)

gender$male.comale <- ifelse(gender$AutM>1 & gender$AutF==0,1,0)
gender$male.cofemale <- ifelse(gender$AutM>0 & gender$AutF>0,1,0)
gender$Male <- ifelse(gender$Female==0,1,0)
gender$book <- ifelse(gender$Type=="Book",1,0)

yearly <- gender %>% group_by(Year) %>% 
            summarise(readings=length(Publisher),#total.female=sum(Female),total.male=sum(Male),
            female.count=sum(AutF),male.count=sum(AutM),
            single.author=sum(single),single.female=sum(single.female),single.male=sum(single.male),
            female.comale=sum(female.comale),female.cofemale=sum(female.cofemale),
            male.comale=sum(male.comale),
            book=sum(book))
yearly$ratio <- yearly$book/yearly$readings