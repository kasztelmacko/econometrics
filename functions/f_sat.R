library("readr")
library("dplyr")
library("sandwich")
library("lmtest")
library("MASS")
library("mfx")
library("htmltools")
library("LogisticDx")
library("aod")
library("logistf")
library('dplyr')
library("readr")
library("maxLik")
library(janitor)

data <- read_csv("train.csv")
glimpse(data)

unique_value_counts <- data %>%
  summarise(across(everything(), ~n_distinct(.)))
glimpse(unique_value_counts)

colSums(is.na(data)) %>% 
  sort()

# change name of the columns to be snake case
data <- clean_names(data)
glimpse(data)

# reomve id and ...1 colums
data <- subset(data, select = -c(id, x1))

# gender as factor
table(data$gender)
data$gender <- factor(data$gender, levels=c("Female", "Male"))

# Customer type
data$customer_type <- factor(data$customer_type, levels=c("disloyal Customer", "Loyal Customer"))

# age
hist(data$age)

# type of travel
table(data$type_of_travel)
data$type_of_travel <- factor(data$type_of_travel, levels=c("Business travel", "Personal Travel"))

# class
table(data$class)
data$class <- factor(data$class, levels=c("Business", "Eco", "Eco Plus"))

# satisfaction (dependent variable)
table(data$satisfaction)
data$satisfaction <- factor(data$satisfaction, levels=c("neutral or dissatisfied", "satisfied"))

#hist for all numeric
par(mfrow = c(1, 1))
hist(data$age)
hist(log(data$flight_distance)) # left skewed normal when log
hist(log(data$departure_delay_in_minutes)) # normal when log
hist(log(data$arrival_delay_in_minutes)) # normal when log

satisf.logit <- glm(satisfaction~., data=data, family=binomial(link = "logit"))
summary(satisf.logit)

null.logit <- glm(satisfaction~1, data=data, family=binomial(link="logit"))
lrtest(satisf.logit,null.logit)
