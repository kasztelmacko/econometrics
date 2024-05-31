########################################
#                                      #
#               Libraries              #
#                                      #
########################################

setwd("C:\\Users\\kaszt\\OneDrive\\Dokumenty\\advanced econometric\\projekt\\econometrics")
# setwd("/Users/sergiocarcamo/Dev/econometrics")

library(readr)
library(dplyr)
library("sandwich")
library("lmtest")
library("MASS")
#install.packages("mfx")
library("mfx")
#install.packages("LogisticDx") # error
library("LogisticDx")
library("aod")
#install.packages("logistf")
library("logistf")
library("car")
library("stargazer")
#install.packages("aods3")
library("aods3")
library("DescTools")
library(ggplot2)
library(AER)


data <- read_csv("data/dataset.csv")

glimpse(data)
colnames(data)
table(data$Churn)

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
data$gender <- ifelse(data$gender == "Male", 1, 0)

# SeniorCitizen
table(data$SeniorCitizen)
# already binary

# Partner
table(data$Partner)
data$Partner <- ifelse(data$Partner == "Yes", 1, 0)

# Dependents
table(data$Dependents)
data$Dependents <- ifelse(data$Dependents == "Yes", 1, 0)

# tenure
hist(data$tenure)

# Phone Service
data$PhoneService <- ifelse(data$PhoneService == "Yes", 1, 0)

# Internet Service
table(data$InternetService)
data$Internet_DSL <- ifelse(data$InternetService == "DSL", 1, 0)
data$Internet_Fiber <- ifelse(data$InternetService == "Fiber optic", 1, 0)
data$Internet_NoInternet <- ifelse(data$InternetService == "No", 1, 0)
data <- subset(data, select = -InternetService)

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
data$MultipleLines <- ifelse(data$MultipleLines == "Yes", 1, 0)
data$OnlineSecurity <- ifelse(data$OnlineSecurity == "Yes", 1, 0)
data$OnlineBackup <- ifelse(data$OnlineBackup == "Yes", 1, 0)
data$DeviceProtection <- ifelse(data$DeviceProtection == "Yes", 1, 0)
data$TechSupport <- ifelse(data$TechSupport == "Yes", 1, 0)
data$StreamingTV <- ifelse(data$StreamingTV == "Yes", 1, 0)
data$StreamingMovies <- ifelse(data$StreamingMovies == "Yes", 1, 0)


# Contract
table(data$Contract)
data$Contract_MonthToMonth <- ifelse(data$Contract == "Month-to-month", 1, 0)
data$Contract_OneYear <- ifelse(data$Contract == "One year", 1, 0)
data$Contract_TwoYear <- ifelse(data$Contract == "Two year", 1, 0)

# Monthly charges and COntract interactions
data$MCharges_MonthToMonth_interaction <- data$MonthlyCharges * data$Contract_MonthToMonth
data$MCharges_OneYear_interaction <- data$MonthlyCharges * data$Contract_OneYear
data$MCharges_TwoYear_interaction <- data$MonthlyCharges * data$Contract_TwoYear

data <- subset(data, select = -Contract)

# Paperless Billing
data$PaperlessBilling <- ifelse(data$PaperlessBilling == "Yes", 1, 0)

# PaymentMethod
# treat Electronic Check as a base level and dont use it when creating a model
table(data$PaymentMethod)
data$Payment_Bank <- ifelse(data$PaymentMethod == "Bank transfer (automatic)", 1, 0)
data$Payment_Credit <- ifelse(data$PaymentMethod == "Credit card (automatic)", 1, 0)
data$Payment_ECheck <- ifelse(data$PaymentMethod == "Electronic check", 1, 0)
data$Payment_MCheck <- ifelse(data$PaymentMethod == "Mailed check", 1, 0)
data <- subset(data, select = -c(PaymentMethod))
glimpse(data)

# Total Charges
data <- data[!is.na(data$TotalCharges), ]

# Churn
data$Churn <- ifelse(data$Churn == "Yes", 1, 0)

# Interaction
data$MCharges_tenure_interaction <- data$MonthlyCharges * data$tenure

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
# remove one level from all variable levels
# treat them as base level
final_data <- subset(data, select = -c(Internet_DSL, Contract_MonthToMonth, Payment_Bank, MCharges_MonthToMonth_interaction))


logit_model <- glm(Churn ~ ., data=final_data, 
                   family=binomial(link="logit"))

summary(logit_model)

probit_model <- glm(Churn ~ ., data=final_data, 
                    family=binomial(link="probit"))
summary(probit_model)

model_list <- list(logit_model, probit_model)
model_names <- c("Logit Model", "Probit Model")
stargazer(model_list, type = "text", title = "Regression Model Comparison",
          align = TRUE, column.labels = model_names, out = "models.txt")


general <- glm(Churn ~ ., data=final_data, 
               family=binomial(link="logit"))
summary(general)
# likelihood ratio test
# the general model is better than a model only with intercept
null_probit = glm(Churn~1, data=data, family=binomial(link="logit"))
lr_test <- lrtest(general, null_probit)
stargazer(lr_test, type = "text", summary = FALSE, title = "Likelihood Ratio Test Results", out="lrtest.txt")


# Remove insignificant variables from the model
# Step 1 - check if all insignificant variables are jointly significant
insignificant_model_1 <- glm(Churn ~ gender + Partner + Dependents
                             + PhoneService + OnlineSecurity + OnlineBackup
                             + DeviceProtection + TechSupport + StreamingTV
                             + StreamingMovies + MonthlyCharges + Payment_Credit
                             + Payment_MCheck + TotalCharges + MCharges_TwoYear_interaction
                             + MCharges_tenure_interaction
                             , data = data
                             , family = binomial(link = "logit"))

anova_results <- anova(general, insignificant_model_1, test = "LRT")
stargazer(anova_results, type = "text", summary = FALSE, title = "ANOVA Likelihood Ratio Test Results", 
          rownames = TRUE, digits = 3, out = "anov_insig.txt")
# the variables are jointly significant so we have to take the General-to-specific approach

# Step 2 - remove Partner (p-value: 0.95)
reduced_model_1 <- glm(Churn ~ . -Partner
                       , data = final_data
                       , family = binomial(link = "logit"))
summary(reduced_model_1)

anova(general, reduced_model_1, test = "LRT")
# there are still insignificant variables in reduced_model_1 
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 3
# let's drop "the most insignificant" - OnlineBackup (p-value: 0.86)
reduced_model_2 <- glm(Churn ~ . -Partner -OnlineBackup
                       , data = final_data
                       , family = binomial(link = "logit"))
summary(reduced_model_2)

anova(general, reduced_model_2, test = "LRT")
# there are still insignificant variables in reduced_model_2
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 4
# let's drop "the most insignificant" - MCharges_tenure_interaction (p-value: 0.81)
reduced_model_3 <- glm(Churn ~ . -Partner -OnlineBackup -MCharges_tenure_interaction
                       , data = final_data
                       , family = binomial(link = "logit"))
summary(reduced_model_3)

anova(general, reduced_model_3, test = "LRT")
# there are still insignificant variables in reduced_model_3
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 5
# let's drop "the most insignificant" - gender (p-value: 0.76)
reduced_model_4 <- glm(Churn ~ . -Partner -OnlineBackup -MCharges_tenure_interaction -gender
                       , data = final_data
                       , family = binomial(link = "logit"))
summary(reduced_model_4)

anova(general, reduced_model_4, test = "LRT")
# there are still insignificant variables in reduced_model_4
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 6
# let's drop "the most insignificant" - Phone service (p-value: 0.61)
reduced_model_5 <- glm(Churn ~ . -Partner -OnlineBackup -MCharges_tenure_interaction -gender -PhoneService
                       , data = final_data
                       , family = binomial(link = "logit"))
summary(reduced_model_5)

anova(general, reduced_model_5, test = "LRT")
# there are still insignificant variables in reduced_model_5
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 7
# let's drop "the most insignificant" - Payment_MCheck (p-value: 0.66)
reduced_model_6 <- glm(Churn ~ . -Partner -OnlineBackup -MCharges_tenure_interaction -gender -PhoneService -Payment_MCheck
                       , data = final_data
                       , family = binomial(link = "logit"))
summary(reduced_model_6)

anova(general, reduced_model_6, test = "LRT")
# there are still insignificant variables in reduced_model_6
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 8
# let's drop "the most insignificant" - Payment_Credit (p-value: 0.55)
reduced_model_7 <- glm(Churn ~ . -Partner -OnlineBackup -MCharges_tenure_interaction -gender -PhoneService -Payment_MCheck -Payment_Credit
                       , data = final_data
                       , family = binomial(link = "logit"))
summary(reduced_model_7)

anova(general, reduced_model_7, test = "LRT")

# there are still insignificant variables in reduced_model_7
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 9
# let's drop "the most insignificant" - MCharges_TwoYear_interaction (p-value: 0.2)
reduced_model_8 <- glm(Churn ~ . -Partner -OnlineBackup -MCharges_tenure_interaction -gender -PhoneService -Payment_MCheck -Payment_Credit -MCharges_TwoYear_interaction
                       , data = final_data
                       , family = binomial(link = "logit"))
summary(reduced_model_8)

anova(general, reduced_model_8, test = "LRT")
# there are still insignificant variables in reduced_model_8
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 10
# let's drop "the most insignificant" - DeviceProtection (p-value: 0.19)
reduced_model_9 <- glm(Churn ~ . -Partner -OnlineBackup -MCharges_tenure_interaction -gender -PhoneService -Payment_MCheck -Payment_Credit -MCharges_TwoYear_interaction -DeviceProtection
                       , data = final_data
                       , family = binomial(link = "logit"))
summary(reduced_model_9)

anova(general, reduced_model_9, test = "LRT")

# Step 11
# let's drop "the most insignificant" - DeviceProtection (p-value: 0.19)
reduced_model_10 <- glm(Churn ~ . -Partner -OnlineBackup -MCharges_tenure_interaction -gender -PhoneService -Payment_MCheck -Payment_Credit -MCharges_TwoYear_interaction -DeviceProtection -Dependents
                       , data = final_data
                       , family = binomial(link = "logit"))
summary(reduced_model_10)

anova(general, reduced_model_10, test = "LRT")
# there is no insignificant variables anymore
# we can stop with the General to specific approach now


########################################
#                                      #
#               Final Model            #
#                                      #
########################################

###### Probit / Logit ###########
# Fit logit and probit models
x_values <- data.frame(x = seq(-8, 8, length.out = 100))
ggplot(data = x_values, aes(x = x)) +
  stat_function(fun = plogis, aes(color = "Logit"), size = 1.2, linetype = 1) +
  stat_function(fun = pnorm, aes(color = "Probit"), size = 1.2, linetype = 2) +
  scale_color_manual(name = "Function",
                     values = c("Logit" = "blue", "Probit" = "red")) +
  labs(title = "Logistic vs Probit",
       x = "x") +
  theme_minimal()

############ We choose logit model ###########
final_model <- glm(Churn~SeniorCitizen + tenure + MultipleLines 
                   + OnlineSecurity + TechSupport + StreamingTV
                   + StreamingMovies + PaperlessBilling + MonthlyCharges
                   + TotalCharges + Internet_Fiber + Internet_NoInternet
                   + Contract_OneYear + Contract_TwoYear + Payment_ECheck
                   + MCharges_OneYear_interaction
                   , data = final_data
                   , family = binomial(link = "logit"))
stargazer(final_model, type = "text", title = "final model", align=T, out = "final.txt")

summary(final_model)
# quality table presenting general and final model
model_list <- list(general, final_model)
model_names <- c("General Model", "Final Model")
stargazer(model_list, type = "text", title = "Regression Model Comparison",
          align = TRUE, column.labels = model_names, out = "final_general.txt")

# marginal effects for average characteristics
probitmfx(formula=Churn~SeniorCitizen + tenure + MultipleLines 
          + OnlineSecurity + TechSupport + StreamingTV
          + StreamingMovies + PaperlessBilling + MonthlyCharges
          + TotalCharges + Internet_Fiber + Internet_NoInternet
          + Contract_OneYear + Contract_TwoYear + Payment_ECheck
          + MCharges_OneYear_interaction
          , data = final_data
          , atmean = T)

# marginal effects for user defined characteristics
source("functions/marginaleffects.R")
glimpse(final_model)
# c(intercept, x1, x2, ...)
user.def.obs = c(1, 0, 4, 1, 0, 1, 1, 1, 1, 66, 300, 1, 0, 0, 1, 0, 60)
marginaleffects(final_model, user.def.obs)

# linktest
source("functions/linktest.R")
# The linktest evaluates if the functional form specified
# for the predictors in the model  adequatly captures the relationship with the response variable
linktest_result <- linktest(final_model)
# yhat is significant so there is no need to include or omit variable,
# and the predicted yhat is very identical to the real y dependent variable values.

# R-Squared 
PseudoR2(final_model,c("Tjur","McKelveyZavoina"))

# count r2
predicted_probabilities <- predict(final_model, type = "response")
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)
actual_classes <- final_model$y
correct_predictions <- sum(predicted_classes == actual_classes)
total_observations <- length(actual_classes)
count_R2 <- correct_predictions / total_observations
count_R2

# adj count r2
most_frequent_outcome_count <- max(table(actual_classes))
adjusted_count_R2 <- (correct_predictions - most_frequent_outcome_count) / (total_observations - most_frequent_outcome_count)
adjusted_count_R2

# Hosmer-Lemeshow and Osius-Rojekt test 
# Goodness-of-fit test used in logistic regression models
gof.results <- LogisticDx::gof(final_model)
gof.results$gof
#         test    stat       val      df      pVal
#1:         HL    chiSq      25.97    8       0.001
#2:       OsRo    Z          2.71     NA      0.007


# Hypothesis verification
# use wald.test 
final_model_with_omited <- glm(Churn~SeniorCitizen + tenure + MultipleLines 
                               + OnlineSecurity + TechSupport + StreamingTV
                               + StreamingMovies + PaperlessBilling + MonthlyCharges
                               + TotalCharges + Internet_Fiber + Internet_NoInternet
                               + Contract_OneYear + Contract_TwoYear + Payment_ECheck
                               + Dependents
                               , data = final_data
                               , family = binomial(link = "logit"))
waldtest(final_model, general, test="F")
# p-value of wald test is > 0.05 so we fail to reject H0, and can conclude
# that the coefficients of the omitted variables are significantly different from zero.