## DATA PREPARATION & CLEANING

# load libraries

library(tidyverse)

# read in csv file
titanic_df = read_csv("titanic.csv")

# find information about df

glimpse(titanic_df)
summary(titanic_df)

# age has 177 Nulls

# code to check for nulls for categorical cols
# from Analytics Vidhya (see bottom of code for link)
# I checked all categorical columns, ending with alone
sum(is.na(titanic_df$alone))

# embarked has 2 Nulls
# deck has 688 Nulls
# embark_town has 2 Nulls

# filling null values

# age: mean (29.70)
titanic_df <- titanic_df %>% 
  mutate(age = replace_na(age, 29.70))

# embarked, embark_town: "Unknown"

titanic_df <- titanic_df %>%
  mutate(embarked = replace_na(embarked, "Unknown"))

titanic_df <- titanic_df %>% 
  mutate(embark_town = replace_na(embark_town, "Unknown"))

# deck is missing the majority of its values, 
# so I will just drop it rather than fill the values

titanic_df <- titanic_df %>% 
  select(1:11, 13:15)

# renaming columns
# I will rename the sibsp and parch columns 
# so their meaning is more clear

titanic_df <- titanic_df %>% 
  rename("num_sib_spouse" = "sibsp")

titanic_df <- titanic_df %>% 
  rename("num_parent_child" = "parch")

# changing data types

# changing logical values to 0 and 1

titanic_df <- titanic_df %>% 
  mutate(adult_male = as.integer(adult_male))

titanic_df <- titanic_df %>% 
  mutate(alone = as.integer(alone))

# re-code survived, adult_male, and alone columns to "Yes" and "No" 
# for ease of visualization.
# I'm sure R can plot columns with 0's and 1's but I think "Yes"
# and "No" is easier to understand in a graph.

titanic_df <- titanic_df %>% 
  mutate(survived = recode(survived, '0' = "no", '1' = "yes"))
titanic_df <- titanic_df %>% 
  mutate(adult_male = recode(survived, '0' = "no", '1' = "yes"))
titanic_df <- titanic_df %>% 
  mutate(alone = recode(survived, '0' = "no", '1' = "yes"))

# change survived, pclass, sex, embarked, class, who, and alive to factor

titanic_df$survived <- as.factor(titanic_df$survived)
titanic_df$pclass <- as.factor(titanic_df$pclass)
titanic_df$sex <- as.factor(titanic_df$sex)
titanic_df$embarked <- as.factor(titanic_df$embarked)
titanic_df$class <- as.factor(titanic_df$class)
titanic_df$who <- as.factor(titanic_df$who)
titanic_df$adult_male <- as.factor(titanic_df$adult_male)
titanic_df$alive <- as.factor(titanic_df$alive)
titanic_df$alone <- as.factor(titanic_df$alone)

# recording columns to put related columns beside each other

titanic_df <- titanic_df %>% 
  select(survived, pclass, age, sex, who, adult_male, num_sib_spouse, 
         num_parent_child, alone, class, fare, embarked, embark_town, alive)

## DATA VISUALIZATION

# histograms

# fare
# how is the fare data distributed in each class?
titanic_df %>% 
  # remove > 500 as they seem to be outliers
  filter(fare < 500) %>% 
  # Even though the data is already split up by class
  # using facet_wrap, I set fill to pclass just to colour-code
  ggplot(aes(fare, fill=pclass)) +
  geom_histogram(bins=40, alpha=0.9) +
  facet_wrap(~pclass) +
  labs(title = "Distribution of Fare by Class",
       y="",
       x="fare (pounds)") +
  theme_light() +
  theme(legend.position = "none")

# age
# what is the distribution of age by sex?
titanic_df %>% 
  ggplot(aes(age, fill=sex)) +
  facet_wrap(~sex) +
  geom_histogram() +
  labs(title = "Distribution of Age by Sex",
       y="",
       x="age (years)") +
  theme_light() +
  theme(legend.position = "none") 

# bar charts

# embark town
# how many people are still alive from each town?
titanic_df %>% 
  # remove "Unknown" because it is so miniscule
  filter(embark_town %in% c("Cherbourg", "Queenstown", "Southampton")) %>%
  # filter to alive only
  filter(alive == "yes") %>% 
  ggplot(aes(embark_town)) +
  geom_bar(alpha = 0.8) +
  labs(title = "Alive Passengers from Each Departure Town",
       x="departure town",
       y="# of passengers") +
  theme_light()

# who
# how many men, women, and children were in each class?
titanic_df %>% 
  ggplot(aes(who, fill=class)) +
  geom_bar(alpha = 0.7) +
  labs(title="Count of Men, Women, and Children by Class",
       x="",
       y="count of passengers") +
  theme_light()+
  theme(legend.position = "top")

  
# scatter plots
  
# age and fare
# did a passenger's age affect the fare they paid?
# does this look different for passengers who were alone?
titanic_df %>% 
  # remove outlier fare of > 500
  filter(fare < 500) %>% 
  ggplot(aes(age, fare)) +
  # lower opacity to better see overlapping points
  geom_point(alpha = 0.5) +
  geom_smooth(method="lm") +
  facet_wrap(~alone) +
  labs(title = "Fare of Passengers by Age",
       subtitle = "facetted by whether they were alone or not",
       x="age (years)",
       y="fare (pounds)") +
  theme_light()

# age and num_parent_child
# did a passenger's age affect 
# the number of siblings and spouses they had
titanic_df %>% 
  ggplot(aes(age, num_parent_child)) +
  # trying out a different shape
  geom_point(shape = 5) +
  # looked at geom_smooth without "lm" method
  # just to see the effect
  geom_smooth() +
  labs(title = "Number of Parents/Children Aboard by Age",
       y="# of parents/children",
       x="age (years)") +
  theme_light()

# box plots

# fare
# are there any outliers in this column?
# i won't remove the outliers for this plot
# since I am curious to see what they are
titanic_df %>% 
  ggplot(aes(fare)) +
  geom_boxplot(alpha = 0.9) +
  labs(title = "Fare Boxplot",
       x='fare (pounds)') +
  theme_light()

# num_sib_spouse
# What is the distribution of # of siblings/spouses 
# for passengers from each departure port?
titanic_df %>% 
  # again I'll remove unknown since it is
  # a few number of passengers
  filter(embarked %in% c("C", "Q", "S")) %>% 
  ggplot(aes(num_sib_spouse, fill=embarked)) +
  geom_boxplot(alpha = 0.8) + 
  labs(title = "Boxplot of # of Siblings/Spouses Aboard",
       x="# of siblings/spouses") +
  theme_light()

# age
# what is the age distribution for men and women
# in each class?
titanic_df %>% 
  ggplot(aes(age, fill=class)) +
  facet_wrap(~sex) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Boxplots of Age by Class for Men and Women",
       x="age (years)") +
  theme_light()


# Code Sources
# Analytics Vidhya:  https://discuss.analyticsvidhya.com/t/how-to-count-the-missing-value-in-r/2949