# R-Learnings
# Titanic cleaning, feature engineering and analysis - July 18
# Large portions taken from the following kernels
# https://www.kaggle.com/mrisdal/exploring-survival-on-the-titanic

rm(list = ls()); gc()

# Libraries
library(RCurl)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(scales)

# Reading data
data.url <- "https://raw.githubusercontent.com/Mixidiot/Kaggle-Getting-Started/master/Titanic/data/train.csv"
test.url <- "https://raw.githubusercontent.com/Mixidiot/Kaggle-Getting-Started/master/Titanic/data/test.csv"

train.df <- read.csv(text = getURL(data.url),
                    header = T,
                    stringsAsFactors = F)
test.df <- read.csv(text = getURL(test.url),
                    header = T,
                    stringsAsFactors = F)

df <- train.df

# Data overview
glimpse(df)


# Feature engineering

# Name
head(df$Name)

# Finding titles - Replacing everything before a , and space + everything after a . with nothing
df$Title <- gsub("(.*, )|(\\..*)", "", df$Name)

table(df$Title)

# Combining certain titles to be the same
other.titles <- c("Capt",
                  "Col",
                  "Don",
                  "Jonkheer",
                  "Lady",
                  "Major",
                  "Rev",
                  "Sir",
                  "the Countess")

df$Title[df$Title == "Mlle"]         <- "Miss"
df$Title[df$Title == "Ms"]           <- "Miss"
df$Title[df$Title == "Mme"]          <- "Mrs"
df$Title[df$Title %in% other.titles] <- "Other title"

# Taking surnames
df$Surname <- gsub("\\,.*", "", df$Name)
  
# Family size and Family column
df$FamSize <- df$SibSp + df$Parch +1 
df$Family <- paste(df$Surname, df$FamSize, sep = "-")

# Visualisation of Family data
ggplot(df, aes(x = FamSize, fill = factor(Survived))) +
  geom_bar(stat = "count", position = "dodge") +
  scale_x_continuous(breaks = c(1:11)) +
  labs(x = "Family Size") + 
  theme_bw()

# Discretize family size 
df$FamSize.Category[df$FamSize == 1] <- "Single"
df$FamSize.Category[df$FamSize < 5 & df$FamSize> 1] <- "Small"
df$FamSize.Category[df$FamSize > 4] <- "Large"

# Mosaic plot
mosaicplot(table(df$FamSize, df$Survived),
           main = "Family Size by Survival",
           shade = T)

# Feature engineering of passenger cabin $Cabin

# Missingness
# Embarkment
# Predictive imputation of Age


# Child mother feature engineering


