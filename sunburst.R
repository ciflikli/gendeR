library(dplyr)
library(sunburstR)

gender <- read_csv("gender_sb.csv")

gender$topp <- ifelse(gender$Top == 1 | gender$Top.Press == 1, 1, 0)
gender$Decade <- ifelse(gender$Year > 1959 & gender$Year < 1970,1960,
                        ifelse(gender$Year > 1969 & gender$Year < 1980,1970,
                               ifelse(gender$Year > 1979 & gender$Year < 1990,1980,
                                      ifelse(gender$Year > 1989 & gender$Year < 2000,1990,
                                             ifelse(gender$Year > 1999 & gender$Year < 2010,2000,2010)))))

test <- gender[gender$Female==1,] %>%
          group_by_(.dots=c("Decade","Type","topp","Single","female.cofemale")) %>%
            summarise(n=n())

test <- na.omit(test)

test$topp <- ifelse(test$topp==1,"TopPublisher","Other")
test$Single <- ifelse(test$Single==1,"SingleAuthored","CoAuthored")
test$female.cofemale <- ifelse(test$female.cofemale==1,"FemaleCoAuthor",
                               ifelse(test$female.cofemale==0,"MaleCoAuthor","")
                               )
colnames(test)[c(3,5)] <- c("Top","Coauthor")
test$Coauthor <- ifelse(test$Single=="SingleAuthored","",test$Coauthor)

test <- within(test, Path <- paste(Decade, Type, Top, Single, Coauthor, sep="-"))
test$Path <- ifelse(test$Coauthor=="",substr(test$Path,1,nchar(test$Path)-1),test$Path)

patch <- test[,6:7]
patch <- patch[c(2,1)]
sunburst(patch)