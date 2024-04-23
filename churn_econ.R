########################################
#                                      #
#               Libraries              #
#                                      #
########################################

setwd("C:\\Users\\kaszt\\OneDrive\\Dokumenty\\advanced econometric\\projekt")

library(readr)
library(dplyr)
library("sandwich")
library("lmtest")
library("MASS")
library("mfx")
library("LogisticDx")
library("aod")
library("logistf")
library("car")
library(DescTools)



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

# Paperless Billing
data$PaperlessBilling <- ifelse(data$PaperlessBilling == "Yes", TRUE, FALSE)

# PaymentMethod
table(data$PaymentMethod)
new_labels <- c("Bank", "Credit", "ECheck", "MCheck")
data$PaymentMethod <- factor(data$PaymentMethod, levels = c("Bank transfer (automatic)", "Credit card (automatic)", "Electronic check", "Mailed check"), labels = new_labels)

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

general <- glm(Churn ~ ., data=data, 
                family=binomial(link="probit"))

summary(general)
# NA values are omitted and treated as reference levels

# likelihood ratio test
null_probit = glm(Churn~1, data=data, family=binomial(link="probit"))
lrtest(general, null_probit)

# Remove insignificant variables from the model
# Step 1 - check if all insignificant variables are jointly significant
insignificant_model_1 <- glm(Churn ~ gender + SeniorCitizen + Partner 
                     + Dependents + PhoneService + OnlineSecurity 
                     + OnlineBackup + DeviceProtection + TechSupport 
                     + StreamingTV + PaymentMethod + MonthlyCharges + TotalCharges
                     , data = data
                     , family = binomial(link = "probit"))

anova(general, insignificant_model_1, test = "LRT")
# the variables are jointly significant so we have to take the General-to-specific approach

# Step 2 - remove Partner (p-value: 0.89)
reduced_model_1 <- glm(Churn ~ . - Partner
                       , data = data
                       , family = binomial(link = "probit"))
summary(reduced_model_1)

anova(general, reduced_model_1, test = "LRT")
# there are still insignificant variables in reduced_model_1 
# We fail to reject H0 that the reduced model is better than general 
# both models are equaly good, so we continue with removing variables

# Step 3
# let's drop "the most insignificant" - gender
reduced_model_2 <- glm(Churn ~ . - Partner - gender
                       , data = data
                       , family = binomial(link = "probit"))
summary(reduced_model_2)

anova(general, reduced_model_2, test = "LRT")
# there are still insignificant variables in reduced_model_2
# We fail to reject H0 that the reduced model is better than general 
# both models are equaly good, so we continue with removing variables

# Step 4
# let's drop "the most insignificant" - OnlineBackup
reduced_model_3 <- glm(Churn ~ . - Partner - gender - OnlineBackup
                       , data = data
                       , family = binomial(link = "probit"))
summary(reduced_model_3)

anova(general, reduced_model_3, test = "LRT")
# there are still insignificant variables in reduced_model_3
# We fail to reject H0 that the reduced model is better than general 
# both models are equaly good, so we continue with removing variables

# Step 5
# let's drop "the most insignificant" - TechSupport
reduced_model_4 <- glm(Churn ~ . - Partner - gender - OnlineBackup - TechSupport
                       , data = data
                       , family = binomial(link = "probit"))
summary(reduced_model_4)

anova(general, reduced_model_4, test = "LRT")
# there are still insignificant variables in reduced_model_4
# We fail to reject H0 that the reduced model is better than general 
# both models are equaly good, so we continue with removing variables

# Step 6
# let's drop "the most insignificant" - PaymentMethod
reduced_model_5 <- glm(Churn ~ . - Partner - gender - OnlineBackup - TechSupport - PaymentMethod
                       , data = data 
                       , family = binomial(link = "probit"))
summary(reduced_model_5)

anova(general, reduced_model_5, test = "LRT")
# there are still insignificant variables in reduced_model_5
# We reject H0 that the reduced model is better than general
# we can conclude that PaymentMethod improves the quality of the model

# Step 7
# check if dropping OnlineSecurity will change quality
reduced_model_6 <- glm(Churn ~ . - Partner - gender - OnlineBackup - TechSupport - OnlineSecurity
                       , data = data
                       , family = binomial(link = "probit"))
summary(reduced_model_6)

anova(general, reduced_model_6, test = "LRT")
# We fail to reject H0 that the reduced model is better than general 
# both models are equaly good, and we are left with only significant variables


########################################
#                                      #
#               Final Model            #
#                                      #
########################################

final_model <- glm(Churn ~ . - Partner - gender - OnlineBackup - TechSupport - OnlineSecurity
                , data = data
                , family = binomial(link = "probit"))
summary(final_model)

# marginal effects
source("marginaleffects.R")
source("linktest.R")

# for average characteristics
probitmfx(formula=Churn ~ . - Partner - gender - OnlineBackup - TechSupport - OnlineSecurity
                              , data = data
                              , atmean = T)

# R-Squared statistics
PseudoR2(final_model,c("McFadden","Tjur","McKelveyZavoina","VeallZimmermann","Nagelkerke"))

# linktest
# The linktest evaluates if the functional form specified
# for the predictors in the model  adequatly captures the relationship with the response variable
linktest_result <- linktest(final_model)
# yhat is significant so there is no need to include or omit variable,
# and the predicted yhat is very identical to the real y dependent variable values.

