# load the required libraries
library(tidyverse)

# Scenario One
# QUESTION 1 (answered after the model has been trained below)

#load/view the dataset
coffee_df <- read.csv("coffee_bloodPressure.csv")

# data wrangling

coffee_df<- coffee_df %>% 
  rename("num_cups" = "Cups.of.Coffee") %>% 
  rename("bp" = "Blood.Pressure..Systolic.Pressure.")

# visualize the target and feature column(s)
# target column: bp
# feature column(s): num_cups

coffee_df %>% 
  ggplot(aes(num_cups, bp)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(title="Cups of Coffee vs. Blood Pressure",
       x="# of Cups",
       y="Blood Pressure")

# relationship: positive, but data has a very large spread.

# split the dataset

set.seed(1)
coffee_df$id <- 1:nrow(coffee_df)
coffee_train <- coffee_df %>% sample_frac(0.80)
coffee_test <- coffee_df %>% anti_join(coffee_train, by = 'id')

# create the model

coffee_model <- lm(bp ~ num_cups, data=coffee_train)
summary(coffee_model)

r <- sqrt(0.1083)
print(r)

# QUESTION 1 ANSWER:
# The coefficient for bp is 2.0352, which suggests a positive relationship.
# The P-Value is 0.0436, which is below 0.05 and statistically significant
# R is 0.33, which suggests a weak relationship.
# Therefore, there is a weak positive correlation between cups of coffee and blood pressure.

# QUESTION 2 ANSWER
# If someone's doctor said their blood pressure was a concern, the patient could consider
# looking to reduce their coffee consumption. However, the correlation between coffee 
# consumption and blood pressure is pretty weak, and so this should only be one factor 
# the patient looks at among others.

# predict on values of 8 and 6 cups of coffee.

predicted_vals <- predict(coffee_model, data.frame(num_cups=c(8, 6)))
print(predicted_vals)

# QUESTION 3 ANSWER: 
# The model predicted that a person who drinks 8 cups of coffee a day will have
# a blood pressure of ~152, whereas a person who drinks 6 cups of coffee a day will have
# a blood pressure of 148.

# Scenario Two

#load/view the dataset
dental_df <- read.csv("dentalService.csv")

# data wrangling

# rename columns
dental_df<- dental_df %>% 
  rename("income" = "Income..K.") %>% 
  rename("num_visits" = "Dentist....Visits.") %>% 
  rename("price" = "Price")

# remove $ from values
# code found on tutorialspoint.com, see sources below
dental_df$income<-gsub("[$]","",as.character(dental_df$income))
dental_df$price<-gsub("[$]","",as.character(dental_df$price))

# convert to numerical values
dental_df$income <- as.numeric(dental_df$income)
dental_df$price <- as.numeric(dental_df$price)

# visualize the target and feature column(s)
# target column: income
# feature column(s): price
# ignored column(s): num_visits 

# QUESTION 1 & 2 ANSWERS  (analysis is found after model is trained):

dental_df %>% 
  ggplot(aes(price, income)) +
  geom_point() +
  geom_smooth(method = "lm")

# relationship: positive; data has a somewhat smaller spread

# split the dataset

set.seed(1)
dental_df$id <- 1:nrow(dental_df)
dental_train <- dental_df %>% sample_frac(0.80)
dental_test <- dental_df %>% anti_join(dental_train, by = 'id')

# create the model

dental_model <- lm(income ~ price, data=dental_train)
summary(dental_model)

r <- sqrt(0.5594)
print(r)

# QUESTION 2 ANSWER (cont'd):
# The coefficient for price is 1.5329, which suggests a positive relationship.
# The P-Value is 4.44e-08, which is well below 0.05 and statistically significant
# R is 0.75, which suggests a moderate relationship.
# Therefore, there is a moderate positive correlation between price and income.

# predict on price points of $200 and $50

dental_predicted_vals <- predict(dental_model, data.frame(price=c(200, 50)))
print(dental_predicted_vals)

# QUESTION 3 ANSWER:
# The next two predicted incomes are $297,000 (at a price point of $200) and $67,000 (at a price point of $50).

# QUESTION 4 ANSWER

# combine predicted values with data
# select columns to plot
dental_predicted <- dental_test %>% 
  select("price", "income")

# add a column with "Actual" or "Predicted" 
actual_pred = rep("Actual", times=10)
dental_predicted <- cbind(dental_predicted, actual_pred)

# add new rows to dataframe
new_record1 <- data.frame(price=200, income=dental_predicted_vals[1], actual_pred='Predicted')
new_record2 <- data.frame(price=50, income=dental_predicted_vals[2], actual_pred='Predicted')

dental_predicted <- rbind(dental_predicted, new_record1)
dental_predicted <- rbind(dental_predicted, new_record2)

# re-plot with new data points
# code to colour points independently of geom_smooth so 
# only one line is produced was adapted from stackoverflow.com

dental_predicted %>% 
  ggplot(aes(price, income)) +
  geom_point(aes(color=actual_pred)) +
  geom_smooth(method="lm") + 
  labs(color="")


# CODE SOURCES
# removing $ from values: 
# https://www.tutorialspoint.com/how-to-remove-a-character-in-an-r-data-frame-column
# coloring independent of geom_smooth: 
# https://stackoverflow.com/questions/33293606/how-to-get-geom-smooth-ignore-my-colour-grouping
