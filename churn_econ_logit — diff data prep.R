########################################
#                                      #
#               Libraries              #
#                                      #
########################################

setwd("C:\\Users\\kaszt\\OneDrive\\Dokumenty\\advanced econometric\\projekt")
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
hist(log(data$tenure))

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
final_data <- subset(data, select = -c(Internet_DSL, Contract_MonthToMonth, Payment_Bank))


general <- glm(Churn ~ ., data=final_data, 
               family=binomial(link="logit"))

summary(general)

# likelihood ratio test
# the general model is better than a model only with intercept
null_probit = glm(Churn~1, data=data, family=binomial(link="logit"))
lrtest(general, null_probit)

# Remove insignificant variables from the model
# Step 1 - check if all insignificant variables are jointly significant
insignificant_model_1 <- glm(Churn ~ gender + Partner + Dependents
                             + PhoneService + OnlineSecurity + OnlineBackup
                             + DeviceProtection + TechSupport + StreamingTV
                             + StreamingMovies + MonthlyCharges + Payment_Credit
                             + Payment_MCheck
                             , data = data
                             , family = binomial(link = "logit"))

anova(general, insignificant_model_1, test = "LRT")
# the variables are jointly significant so we have to take the General-to-specific approach

# Step 2 - remove Partner (p-value: 0.99)
reduced_model_1 <- glm(Churn ~ . -Partner
                       , data = final_data
                       , family = binomial(link = "logit"))
summary(reduced_model_1)

anova(general, reduced_model_1, test = "LRT")
# there are still insignificant variables in reduced_model_1 
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 3
# let's drop "the most insignificant" - OnlineBackup (p-value: 0.88)
reduced_model_2 <- glm(Churn ~ . -Partner -OnlineBackup
                       , data = final_data
                       , family = binomial(link = "logit"))
summary(reduced_model_2)

anova(general, reduced_model_2, test = "LRT")
# there are still insignificant variables in reduced_model_2
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 4
# let's drop "the most insignificant" - PhoneService (p-value: 0.77)
reduced_model_3 <- glm(Churn ~ . -Partner -OnlineBackup -PhoneService
                       , data = final_data
                       , family = binomial(link = "logit"))
summary(reduced_model_3)

anova(general, reduced_model_3, test = "LRT")
# there are still insignificant variables in reduced_model_3
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 5
# let's drop "the most insignificant" - gender (p-value: 0.73)
reduced_model_4 <- glm(Churn ~ . -Partner -OnlineBackup -PhoneService -gender
                       , data = final_data
                       , family = binomial(link = "logit"))
summary(reduced_model_4)

anova(general, reduced_model_4, test = "LRT")
# there are still insignificant variables in reduced_model_4
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 6
# let's drop "the most insignificant" - Payment_MCheck (p-value: 0.61)
reduced_model_5 <- glm(Churn ~ . -Partner -OnlineBackup -PhoneService -gender -Payment_MCheck
                       , data = final_data 
                       , family = binomial(link = "logit"))
summary(reduced_model_5)

anova(general, reduced_model_5, test = "LRT")
# there are still insignificant variables in reduced_model_5
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 7
# let's drop "the most insignificant" - Payment_Credit (p-value: 0.54)
reduced_model_6 <- glm(Churn ~ . -Partner -OnlineBackup -PhoneService -gender -Payment_MCheck -Payment_Credit
                       , data = final_data 
                       , family = binomial(link = "logit"))
summary(reduced_model_6)

anova(general, reduced_model_6, test = "LRT")
# there are still insignificant variables in reduced_model_6
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 8
# let's drop "the most insignificant" - DeviceProtection (p-value: 0.19)
reduced_model_7 <- glm(Churn ~ . -Partner -OnlineBackup -PhoneService -gender -Payment_MCheck -Payment_Credit -DeviceProtection
                       , data = final_data 
                       , family = binomial(link = "logit"))
summary(reduced_model_7)

anova(general, reduced_model_7, test = "LRT")

# there are still insignificant variables in reduced_model_7
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 9
# let's drop "the most insignificant" - Dependents (p-value: 0.06)
reduced_model_8 <- glm(Churn ~ . -Partner -OnlineBackup -PhoneService -gender -Payment_MCheck -Payment_Credit -DeviceProtection -Dependents
                       , data = final_data 
                       , family = binomial(link = "logit"))
summary(reduced_model_8)

anova(general, reduced_model_8, test = "LRT")
# there is no insignificant variables anymore
# we can stop with the General to specific approach now


########################################
#                                      #
#               Final Model            #
#                                      #
########################################

###### Probit / Logit ###########
# Fit logit and probit models
newdata <- data
newdata$Ratio <- data$MonthlyCharges / data$tenure
ratio_seq <- seq(min(newdata$Ratio), max(newdata$Ratio), length.out = 100)

ggplot(newdata, aes(x = Ratio, y = Churn)) +
  geom_point(shape = 16, size = 2) +
  stat_function(fun = plogis, aes(color = "Logit"), size = 1.2, linetype = 1) +
  stat_function(fun = pnorm, aes(color = "Probit"), size = 1.2, linetype = 2) +
  labs(
    x = "Ratio (MonthlyCharges / tenure)",
    y = "Churn",
    title = "Probit and Logit Models of the Probability of Churn, Given Charges/Tenure ratio"
  ) +
  theme_minimal() +
  xlim(-25, 100) +
  geom_text(aes(x = 90, y = 0.03, label = "Churn = False"), color = "black", size = 4) +
  geom_text(aes(x = 90, y = 1.03, label = "Churn = True"), color = "black", size = 4) +
  scale_color_manual(values = c("Logit" = "blue", "Probit" = "red"), 
                     labels = c("Logit", "Probit")) +
  labs(color = "Model") +
  guides(color = guide_legend(title = "Model")) +
  scale_y_continuous(breaks = c(0, 0.5, 1), expand = c(0, 0.1))

############ We choose logit model ###########
final_model <- glm(Churn~SeniorCitizen + tenure + MultipleLines 
                   + OnlineSecurity + TechSupport + StreamingTV
                   + StreamingMovies + PaperlessBilling + MonthlyCharges
                   + TotalCharges + Internet_Fiber + Internet_NoInternet
                   + Contract_OneYear + Contract_TwoYear + Payment_ECheck
                   , data = final_data
                   , family = binomial(link = "logit"))

summary(final_model)
# quality table presenting general and final model
model_list <- list(general, final_model)
model_names <- c("General Model", "Final Model")
stargazer(model_list, type = "text", title = "Regression Model Comparison",
          align = TRUE, column.labels = model_names)

# marginal effects for average characteristics
probitmfx(formula=Churn~SeniorCitizen + tenure + MultipleLines 
          + OnlineSecurity + TechSupport + StreamingTV
          + StreamingMovies + PaperlessBilling + MonthlyCharges
          + TotalCharges + Internet_Fiber + Internet_NoInternet
          + Contract_OneYear + Contract_TwoYear + Payment_ECheck
          , data = final_data
          , atmean = T)

# marginal effects for user defined characteristics
source("functions/marginaleffects.R")
glimpse(final_data)
# c(intercept, x1, x2, ...)
user.def.obs = c(0.804, 0, 4, 0, 1, 1, 1, 1, 1, 65, 260, 1, 0, 0, 1, 1)
marginaleffects(final_model, user.def.obs)

# linktest
source("functions/linktest.R")
# The linktest evaluates if the functional form specified
# for the predictors in the model  adequatly captures the relationship with the response variable
linktest_result <- linktest(final_model)
# yhat is significant so there is no need to include or omit variable,
# and the predicted yhat is very identical to the real y dependent variable values.

# R-Squared 
PseudoR2(final_model,c("McFadden","Tjur","McKelveyZavoina","VeallZimmermann","Nagelkerke"))

# Hosmer-Lemeshow and Osius-Rojekt test 
# Goodness-of-fit test used in logistic regression models
gof.results <- LogisticDx::gof(final_model)
gof.results$gof
#         test    stat       val      df      pVal
#1:         HL    chiSq      25.97    8       0.0106
#2:       OsRo    Z          2.71     NA      0.067


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