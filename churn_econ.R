########################################
#                                      #
#               Libraries              #
#                                      #
########################################

# setwd("C:\\Users\\kaszt\\OneDrive\\Dokumenty\\advanced econometric\\projekt")
# setwd("/Users/sergiocarcamo/Dev/econometrics")

library(readr)
library(dplyr)
library("sandwich")
library("lmtest")
library("MASS")
#install.packages("mfx")
library("mfx")
#install.packages("LogisticDx") # error
#library("LogisticDx")
library("aod")
#install.packages("logistf")
library("logistf")
library("car")
library("stargazer")
#install.packages("aods3")
library("aods3")
library("DescTools")


data <- read_csv("dataset.csv")

glimpse(data)

########################################
#                                      #
#               Data Prep              #
#                                      #
########################################

#remove id column
data <- subset(data, select = -customerID)

# count of all unique values
unique_value_counts <- data %>%
  summarise(across(everything(), ~n_distinct(.)))
glimpse(unique_value_counts)

# gender
table(data$gender)
data$gender <- factor(data$gender, levels = c("Female", "Male"))

# SeniorCitizen
table(data$SeniorCitizen)
data$SeniorCitizen <- as.logical(data$SeniorCitizen)

# Partner
table(data$Partner)
data$Partner <- ifelse(data$Partner == "Yes", TRUE, FALSE)

# Dependents
table(data$Dependents)
data$Dependents <- ifelse(data$Dependents == "Yes", TRUE, FALSE)

# tenure
hist(data$tenure)

# Phone Service
data$PhoneService <- ifelse(data$PhoneService == "Yes", TRUE, FALSE)

# Internet Service
table(data$InternetService)
data$InternetService <- factor(data$InternetService, levels = c("DSL", "Fiber optic", "No"))
levels(data$InternetService)[levels(data$InternetService) == "Fiber optic"] <- "Fiber"


# combine "No" and "No internet service" / "No phone service" into "No" 
# as both mean user did not chose this service
combine_levels <- function(x) {
  x <- as.character(x)  # Ensure x is character type
  x[x %in% c("No", "No phone service", "No internet service")] <- "No"
  return(x)
}

# Apply the function to each variable
data$MultipleLines <- combine_levels(data$MultipleLines)
data$OnlineSecurity <- combine_levels(data$OnlineSecurity)
data$OnlineBackup <- combine_levels(data$OnlineBackup)
data$DeviceProtection <- combine_levels(data$DeviceProtection)
data$TechSupport <- combine_levels(data$TechSupport)
data$StreamingTV <- combine_levels(data$StreamingTV)
data$StreamingMovies <- combine_levels(data$StreamingMovies)

# Factorize variables with only two levels: "No" and "Yes"
data$MultipleLines <- factor(data$MultipleLines)
data$OnlineSecurity <- factor(data$OnlineSecurity)
data$OnlineBackup <- factor(data$OnlineBackup)
data$DeviceProtection <- factor(data$DeviceProtection)
data$TechSupport <- factor(data$TechSupport)
data$StreamingTV <- factor(data$StreamingTV)
data$StreamingMovies <- factor(data$StreamingMovies)


# Contract
table(data$Contract)
data$Contract <- factor(data$Contract, levels = c("Month-to-month", "One year", "Two year"))
levels(data$Contract)[levels(data$Contract) == "One year"] <- "One_Year"
levels(data$Contract)[levels(data$Contract) == "Two year"] <- "Two_Year"

# Paperless Billing
data$PaperlessBilling <- ifelse(data$PaperlessBilling == "Yes", TRUE, FALSE)

# PaymentMethod
# treat Electronic Check as a base level and dont use it when creating a model
table(data$PaymentMethod)
data$Bank <- ifelse(data$PaymentMethod == "Bank transfer (automatic)", 1, 0)
data$Credit <- ifelse(data$PaymentMethod == "Credit card (automatic)", 1, 0)
data$ECheck <- ifelse(data$PaymentMethod == "Electronic check", 1, 0)
data$MCheck <- ifelse(data$PaymentMethod == "Mailed check", 1, 0)
data <- subset(data, select = -c(PaymentMethod))
glimpse(data)

# Total Charges
data <- data[!is.na(data$TotalCharges), ]

# Churn
data$Churn <- ifelse(data$Churn == "Yes", TRUE, FALSE)

glimpse(data)

unique_value_counts <- data %>%
  summarise(across(everything(), ~n_distinct(.)))
glimpse(unique_value_counts)

colSums(is.na(data)) %>% 
  sort()

########################################
#                                      #
#               Model                  #
#                                      #
########################################

general <- glm(Churn ~ . - ECheck, data=data, 
               family=binomial(link="probit"))

summary(general)

# likelihood ratio test
# the general model is better than a model only with intercept
null_probit = glm(Churn~1, data=data, family=binomial(link="probit"))
lrtest(general, null_probit)

# Remove insignificant variables from the model
# Step 1 - check if all insignificant variables are jointly significant
insignificant_model_1 <- glm(Churn ~ gender + Partner + Dependents
                             + PhoneService + OnlineSecurity 
                             + OnlineBackup + DeviceProtection + TechSupport 
                             + StreamingTV + MonthlyCharges
                             , data = data
                             , family = binomial(link = "probit"))

anova(general, insignificant_model_1, test = "LRT")
# the variables are jointly significant so we have to take the General-to-specific approach

# Step 2 - remove Partner (p-value: 0.89)
reduced_model_1 <- glm(Churn ~ . - ECheck - Partner
                       , data = data
                       , family = binomial(link = "probit"))
summary(reduced_model_1)

anova(general, reduced_model_1, test = "LRT")
# there are still insignificant variables in reduced_model_1 
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 3
# let's drop "the most insignificant" - gender
reduced_model_2 <- glm(Churn ~ . - ECheck - Partner - gender
                       , data = data
                       , family = binomial(link = "probit"))
summary(reduced_model_2)

anova(general, reduced_model_2, test = "LRT")
# there are still insignificant variables in reduced_model_2
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 4
# let's drop "the most insignificant" - OnlineBackup
reduced_model_3 <- glm(Churn ~ . - ECheck - Partner - gender - OnlineBackup
                       , data = data
                       , family = binomial(link = "probit"))
summary(reduced_model_3)

anova(general, reduced_model_3, test = "LRT")
# there are still insignificant variables in reduced_model_3
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 5
# let's drop "the most insignificant" - PhoneService
reduced_model_4 <- glm(Churn ~ . - ECheck - Partner - gender - OnlineBackup - PhoneService
                       , data = data
                       , family = binomial(link = "probit"))
summary(reduced_model_4)

anova(general, reduced_model_4, test = "LRT")
# there are still insignificant variables in reduced_model_4
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 6
# let's drop "the most insignificant" - DeviceProtection
reduced_model_5 <- glm(Churn ~ . - ECheck - Partner - gender - OnlineBackup - PhoneService - DeviceProtection
                       , data = data 
                       , family = binomial(link = "probit"))
summary(reduced_model_5)

anova(general, reduced_model_5, test = "LRT")
# there is no insignificant variables anymore
# we can stop with the General to specific approach now


########################################
#                                      #
#               Final Model            #
#                                      #
########################################


final_model <- glm(Churn ~ . - ECheck - Partner - gender - OnlineBackup - PhoneService - DeviceProtection
                      , data = data 
                      , family = binomial(link = "probit"))

# quality table presenting general and final model
model_list <- list(general, final_model)
model_names <- c("General Model", "Final Model")
stargazer(model_list, type = "text", title = "Regression Model Comparison",
          align = TRUE, column.labels = model_names)

# marginal effects for average characteristics
probitmfx(formula=Churn ~ . - ECheck - Partner - gender - OnlineBackup - PhoneService - DeviceProtection
          , data = data
          , atmean = T)

# marginal effects for user defined characteristics
# I DONT KNOW 
source("marginaleffects.R")

# linktest
source("linktest.R")
# The linktest evaluates if the functional form specified
# for the predictors in the model  adequatly captures the relationship with the response variable
linktest_result <- linktest(final_model)
# yhat is significant so there is no need to include or omit variable,
# and the predicted yhat is very identical to the real y dependent variable values.

# R-Squared 
PseudoR2(final_model,c("McFadden","Tjur","McKelveyZavoina","VeallZimmermann","Nagelkerke"))

# Hosmer-Lemeshow
# Goodness-of-fit test commonly used to asses the adequacy of logistic regression models
HosmerLemeshowTest(final_model$fitted.values, data$Churn)
# H0: the model is well fitted for the data
# we reject the H0

# Osius-Rojekt test
# Goodness-of-fit test used in logistic regression models

# Hypothesis verification
# use wald.test
