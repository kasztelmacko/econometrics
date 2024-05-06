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




general <- glm(Churn ~ . - ECheck, data=data, 
               family=binomial(link="logit"))

summary(general)

# likelihood ratio test
# the general model is better than a model only with intercept
null_probit = glm(Churn~1, data=data, family=binomial(link="logit"))
lrtest(general, null_probit)

# Remove insignificant variables from the model
# Step 1 - check if all insignificant variables are jointly significant
insignificant_model_1 <- glm(Churn ~ gender + Partner + Dependents
                             + PhoneService + OnlineSecurity 
                             + OnlineBackup + DeviceProtection + TechSupport 
                             + StreamingTV + StreamingMovies + MonthlyCharges
                             , data = data
                             , family = binomial(link = "logit"))

anova(general, insignificant_model_1, test = "LRT")
# the variables are jointly significant so we have to take the General-to-specific approach

# Step 2 - remove Partner (p-value: 0.99)
reduced_model_1 <- glm(Churn ~ . - ECheck - Partner
                       , data = data
                       , family = binomial(link = "logit"))
summary(reduced_model_1)

anova(general, reduced_model_1, test = "LRT")
# there are still insignificant variables in reduced_model_1 
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 3
# let's drop "the most insignificant" - OnlineBackup (p-value: 0.88)
reduced_model_2 <- glm(Churn ~ . - ECheck - Partner - OnlineBackup
                       , data = data
                       , family = binomial(link = "logit"))
summary(reduced_model_2)

anova(general, reduced_model_2, test = "LRT")
# there are still insignificant variables in reduced_model_2
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 4
# let's drop "the most insignificant" - gender (p-value: 0.73)
reduced_model_3 <- glm(Churn ~ . - ECheck - Partner - gender - OnlineBackup
                       , data = data
                       , family = binomial(link = "logit"))
summary(reduced_model_3)

anova(general, reduced_model_3, test = "LRT")
# there are still insignificant variables in reduced_model_3
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 5
# let's drop "the most insignificant" - PhoneService (p-value: 0.78)
reduced_model_4 <- glm(Churn ~ . - ECheck - Partner - gender - OnlineBackup - PhoneService
                       , data = data
                       , family = binomial(link = "logit"))
summary(reduced_model_4)

anova(general, reduced_model_4, test = "LRT")
# there are still insignificant variables in reduced_model_4
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 6
# let's drop "the most insignificant" - DeviceProtection
reduced_model_5 <- glm(Churn ~ . - ECheck - Partner - gender - OnlineBackup - PhoneService - DeviceProtection
                       , data = data 
                       , family = binomial(link = "logit"))
summary(reduced_model_5)

anova(general, reduced_model_5, test = "LRT")
# there are still insignificant variables in reduced_model_5
# We fail to reject H0 that the reduced model is better than general 
# both models are equally good, so we continue with removing variables

# Step 7
# let's drop "the most insignificant" - Dependents (p-value: 0.06)
reduced_model_6 <- glm(Churn ~ . - ECheck - Partner - gender - OnlineBackup - PhoneService - DeviceProtection - Dependents
                       , data = data 
                       , family = binomial(link = "logit"))
summary(reduced_model_6)

anova(general, reduced_model_6, test = "LRT")
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
data.significant <- data[, !(names(data) %in% c("ECheck", "Partner", "gender", "OnlineBackup", "PhoneService", "DeviceProtection", "Dependents"))]

final_model <- glm(Churn ~ SeniorCitizen + tenure + MultipleLines + InternetService 
                   + OnlineSecurity + TechSupport + StreamingTV + StreamingMovies 
                   + Contract + PaperlessBilling + MonthlyCharges + TotalCharges 
                   + Bank + Credit + MCheck
                   , data = data.significant
                   , family = binomial(link = "logit"))


# quality table presenting general and final model
model_list <- list(general, final_model)
model_names <- c("General Model", "Final Model")
stargazer(model_list, type = "text", title = "Regression Model Comparison",
          align = TRUE, column.labels = model_names)

# marginal effects for average characteristics
probitmfx(formula=Churn ~ .
          , data = data.significant
          , atmean = T)

# marginal effects for user defined characteristics

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
HosmerLemeshowTest(final_model$fitted.values, data.significant$Churn)
# H0: the model is well fitted for the data
# we reject the H0

# Osius-Rojekt test
# Goodness-of-fit test used in logistic regression models
gof.results <- gof(final_model)
gof.results$gof
## test     stat      val       df   pVal
## OsRo     Z         2.787577  NA   5.310388e-03
# H): the model is well fitted for the data
# we reject the H0


# Hypothesis verification
# use wald.test



