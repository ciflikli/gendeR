library(shinydashboard)
library(dygraphs)
library(sunburstR)
library(DT)
library(htmlwidgets)
library(tidyverse)
library(RColorBrewer)
library(rbokeh)
library(stringr)
library(mosaic)
library(shinyjs)
library(bubbles)

total <- read_csv("total.csv")
gender <- read_csv("gender_sb.csv")
pub <- read_csv("pub.csv")
course <- read_csv("course.csv")

# course <- gender[, c("Course", "Convener", "Rank", "Name", "Cluster", "Code", "Level",
#                      "Weighted", "Female", "Core")]
# course1 <- course %>%
#             group_by(Course) %>%
#             summarise(Total = n(), FRatio = sum(as.integer(Female)), FWeighted = sum(Weighted))
# course1$Ratio <- course1$FRatio / course1$Total
# course1$WRatio <- course1$FWeighted / course1$Total
# 
# course2 <- merge(course1, course, by = "Course", all.y = FALSE)
# course2$Female <- NULL
# course2$Weighted <- NULL
# course2 <- unique(course2)

# course <- course1 %>%
#           group_by(Course) %>%
#           summarise(n = n())
# 
# course <- cbind(course, course1[,2:6])

#regexp <- "[[:digit:]]+" 
#course$xcor <- ntiles(course$Ratio, 6)
#course$xcor <- str_extract(course$xcor, regexp)
#course$ycor <- ntiles(course$Code, 18)
#course$ycor <- str_extract(course$ycor, regexp)

course$xcor <- c(0, 1, 2, 1, 2, 2, 3, 3, 3, 4,
                 4, 4, 4, 5, 5, 5, 5, 5, 6, 6,
                 6, 7, 7, 7, 7, 7, 8, 8, 9, 9,
                 9, 9, 10, 11, 10, 10, 11, 11, 12, 12,
                 12, 13, 13)
course$ycor <- c(2, 2, 2, 4, 4, 1, 3, 1, 2, 5,
                 1, 2, 3, 5, 1, 3, 2, 4, 3, 2,
                 5, 4, 1, 2, 5, 3, 3, 1, 2, 5,
                 4, 3, 1, 1, 4, 3, 5, 3, 2, 3,
                 4, 4, 5)
course$xcor <- as.character(course$xcor)
course$ycor <- as.character(course$ycor)

clusters <- c("Theory", "Security/Statecraft", "IO/Law", "IPE", "Regional")
colors <- c('#f0f9e8','#bae4bc','#7bccc4','#43a2ca','#0868ac')
course$color <- colors[match(course$Cluster, clusters)]

course$symx <- paste(course$xcor, ":0.1", sep = "")
course$numbery <- paste(course$ycor, ":0.8", sep = "")
course$massy <- paste(course$ycor, ":0.15", sep = "")
course$namey <- paste(course$ycor, ":0.3", sep = "")
course$symx2 <- paste(course$xcor, ":0.7", sep = "")

course$Core <- ifelse(course$Core==1, "Core", "")
course <- course %>% 
unite(Convener, Rank, Convener, sep = "/")
course$Ratio <- format(round(course$Ratio, 2), nsmall = 2)
course$WRatio <- format(round(course$WRatio, 2), nsmall = 2)
course$Code <- paste0("IR",course$Code)
course$Level <- ifelse(course$Level=="Undergrad", "Undergraduate", course$Level)
course$Readings <- course$Total

pub$Female <- abs(pub$Female)
pub$Female <- pub$Total - pub$Female
pub$Ratio <- round(pub$Female / pub$Total, 2)
pub$Ratio <- ifelse(is.nan(pub$Ratio), 0, pub$Ratio)

gender$Gender <- ifelse(gender$Female==1, "Female", "Male")
gender$Female <- as.factor(gender$Female)
gender$topp <- ifelse(gender$Top == 1 | gender$Top.Press == 1, 1, 0)
gender$Decade <- ifelse(gender$Year > 1959 & gender$Year < 1970,1960,
                        ifelse(gender$Year > 1969 & gender$Year < 1980,1970,
                               ifelse(gender$Year > 1979 & gender$Year < 1990,1980,
                                      ifelse(gender$Year > 1989 & gender$Year < 2000,1990,
                                             ifelse(gender$Year > 1999 & gender$Year < 2010,2000,2010)))))

dat2 <- gender %>%
  group_by(AutGen) %>%
  summarise(n = n())
dat2$Male <- str_count(dat2$AutGen, "M")
dat2$Female <- str_count(dat2$AutGen, "F")
dat2$Total <- str_length(dat2$AutGen)

test <- gender[gender$Female==1, ] %>%
  group_by_(.dots=c("Decade", "Type", "topp", "Single", "female.cofemale")) %>%
  summarise(n = n())

test <- na.omit(test)

test$topp <- ifelse(test$topp == 1 ,"TopPublisher", "Other")
test$Single <- ifelse(test$Single == 1,"SingleAuthored", "CoAuthored")
test$female.cofemale <- ifelse(test$female.cofemale == 1, "FemaleCoAuthor",
                               ifelse(test$female.cofemale == 0, "MaleCoAuthor","")
)
colnames(test)[c(3,5)] <- c("Top", "Coauthor")
test$Coauthor <- ifelse(test$Single=="SingleAuthored", "", test$Coauthor)

test <- within(test, Path <- paste(Decade, Type, Top, Single, Coauthor, sep="-"))
test$Path <- ifelse(test$Coauthor=="",substr(test$Path,1,nchar(test$Path)-1),test$Path)

patch <- test[,6:7]
patch <- patch[c(2,1)]


yearly <- read_csv("yearly.csv")
authors <- cbind(yearly$Year,yearly$FM.Weighted)
authors <- authors[1:99,]
authors <- as.data.frame(authors)
authors$V1 <- paste(authors$V1,"-01-01",sep="")
authors$V1 <- as.Date(authors$V1)
authors$V3 <- 1-authors$V2
authors <- as.matrix(authors)
rownames(authors) = authors[,1]
authors <- authors[,2:3]
authors <- authors[43:99,]