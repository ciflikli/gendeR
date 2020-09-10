######Load required libraries
library(shinydashboard)
library(dygraphs)
library(sunburstR)
library(DT)
library(htmlwidgets)
library(tidyverse)
library(RColorBrewer)
library(rbokeh)
library(stringr)
library(shinyjs)
library(bubbles)
library(showtext)
library(hrbrthemes)
theme_set(theme_ipsum_rc(plot_title_size = 18, axis_title_size = 14))

font_add_google(name = "Roboto Condensed", family = "Roboto Condensed", regular.wt = 400, bold.wt = 700)
showtext_auto()
showtext_opts(dpi = 112)

######Read-in data

total <- read_csv("total.csv")
gender <- read_csv("gender_sb.csv")
pub <- read_csv("pub.csv")
course <- read_csv("course.csv")

######Create xy coordinate system for bokeh

#Automated Way: Splits x and y into roughly-equal 6 and 18 boxes, respectively
#regexp <- "[[:digit:]]+" 
#course$xcor <- ntiles(course$Ratio, 6)
#course$xcor <- str_extract(course$xcor, regexp)
#course$ycor <- ntiles(course$Code, 18)
#course$ycor <- str_extract(course$ycor, regexp)

#Manual Way for precise control

course$xcor <- c(11, 10, 1, 12, 8, 1, 14, 5, 5, 9,
                 0, 2, 9, 6, 6, 6, 7, 7, 11, 0,
                 13, 13, 4, 3, 12, 3, 9, 0, 4, 7,
                 0, 8, 2, 7, 6, 10, 12, 13, 6, 7,
                 3, 10, 9)
course$ycor <- c(2, 2, 3, 3, 4, 2, 3, 2, 3, 4,
                 1, 3, 2, 5, 1, 3, 2, 4, 3, 3,
                 5, 4, 2, 2, 5, 3, 3, 2, 3, 5,
                 4, 3, 2, 1, 4, 3, 4, 3, 2, 3,
                 4, 4, 5)

course$xcor <- as.character(course$xcor)
course$ycor <- as.character(course$ycor)

#Match colours with clusters

clusters <- c("Theory", "Security", "IO/Law", "IPE", "Regional")
colors <- c("#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca", "#0868ac")
course$color <- colors[match(course$Cluster, clusters)]

#New dataframe for cluster boxes
indicator <- data_frame(clusters, colors)
indicator$xcor <- as.character(c(10, 1, 13, 7, 4 ))
indicator$ycor <- as.character(rep(6, 5))

indicator$symx <- paste(indicator$xcor, ":0.5", sep = "")
indicator$namey <- paste(indicator$ycor, ":0", sep = "")
indicator$clusters <- paste(indicator$clusters, paste0(c(.225 , .129, .291, .216, .156), "%"), sep = " ")

indicator$Subfield <- c("IR Theory", "Statecraft/Security Studies", "International Organisations/Law",
                         "International Political Economy", "Area Studies")
indicator$Courses <- c(9, 8, 7, 12, 7)

#Create coordinates for additional info within the rectangles

course$symx <- paste(course$xcor, ":0.1", sep = "")
course$numbery <- paste(course$ycor, ":0.8", sep = "")
course$massy <- paste(course$ycor, ":0.15", sep = "")
course$namey <- paste(course$ycor, ":0.3", sep = "")
course$symx2 <- paste(course$xcor, ":0.65", sep = "")

#Make data user-friendly

#course$Core <- ifelse(course$Core == 1, "Core", "")
course <- course %>% 
unite(Convener, Rank, Convener, sep = "/")
course$Ratio <- format(round(course$Ratio, 2), nsmall = 2)
course$WRatio <- format(round(course$WRatio, 2), nsmall = 2)
course$Level <- ifelse(course$Level == "Undergrad", "Undergraduate", course$Level)
colnames(course)[1] <- "Readings"

######Create data for DT

pub$Female <- abs(pub$Female)
pub$Female <- pub$Total - pub$Female
pub$Ratio <- round(pub$Female / pub$Total, 2)
pub$Ratio <- ifelse(is.nan(pub$Ratio), 0, pub$Ratio)

######Prepare data for ggplot
gender$AutGen <- ifelse(gender$AutGen == "VV", "FF", ifelse(gender$AutGen == "N", "M", gender$AutGen))
gender$Gender <- ifelse(gender$Female == 1, "Female", "Male")
gender$Female <- as.factor(gender$Female)
gender$topp <- ifelse(gender$Top == 1 | gender$Top.Press == 1, 1, 0)
gender$Decade <- ifelse(gender$Year > 1959 & gender$Year < 1970, 1960,
                        ifelse(gender$Year > 1969 & gender$Year < 1980, 1970,
                               ifelse(gender$Year > 1979 & gender$Year < 1990, 1980,
                                      ifelse(gender$Year > 1989 & gender$Year < 2000, 1990,
                                             ifelse(gender$Year > 1999 & gender$Year < 2010, 2000, 2010)))))

######Create Co-Author data

dat2 <- gender %>%
  group_by(AutGen) %>%
  summarise(n = n())
dat2$Male <- str_count(dat2$AutGen, "M")
dat2$Female <- str_count(dat2$AutGen, "F")
dat2$Total <- str_length(dat2$AutGen)

######Create sunburstR data

#Create summaries

test <- gender[gender$Female == 1, ] %>%
  group_by_(.dots = c("Decade", "Type", "topp", "Single", "female.cofemale")) %>%
  summarise(n = n())

test <- na.omit(test)

#Create colour vectors so that later we can drop very light/dark colours (makes it easier to read the text)
blues <- c(brewer.pal(9, "Blues"))
reds <- c(brewer.pal(9, "Reds"))

#Provide user-friendly names

test$topp <- ifelse(test$topp == 1, "TopPublisher", "OtherPublisher")
test$Single <- ifelse(test$Single == 1,"SingleAuthored", "CoAuthored")
test$female.cofemale <- ifelse(test$female.cofemale == 1, "FemaleCoAuthor",
                               ifelse(test$female.cofemale == 0, "MaleCoAuthor",""))
colnames(test)[c(3, 5)] <- c("Top", "Coauthor")
test$Coauthor <- ifelse(test$Single == "SingleAuthored", "", test$Coauthor)

#Create sequence data
#Paste strings and add '-' in between

test <- within(test, Path <- paste(Decade, Type, Top, Single, Coauthor, sep = "-"))

#Remove the last '-' for sequences ending with SingleAuthored

test$Path <- ifelse(test$Coauthor == "", substr(test$Path, 1, nchar(test$Path) - 1), test$Path)

#Subset the end product and reorder

patch <- test[, 6:7]
patch <- patch[c(2, 1)]

######Create time-series data for dygraphs

yearly <- read_csv("yearly.csv")
authors <- cbind(yearly$Year, yearly$FM.Weighted)
authors <- authors[1:99, ]
authors <- as.data.frame(authors)
authors$V1 <- paste(authors$V1, "-01-01", sep = "")
authors$V1 <- as.Date(authors$V1)
authors$V3 <- 1 - authors$V2
authors <- as.matrix(authors)
rownames(authors) = authors[,1]
authors <- authors[, 2:3]
authors <- authors[43:99, ]