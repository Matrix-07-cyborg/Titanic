# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Load the dataset
titanic_data <- read_csv("C:/Users/Rahul Shaw/Downloads/train.csv")

# Inspect the dataset
head(titanic_data)
summary(titanic_data)
str(titanic_data)

# Check for missing values
colSums(is.na(titanic_data))

# Data Cleaning
# Fill missing 'Age' values with median age
titanic_data$Age[is.na(titanic_data$Age)] <- median(titanic_data$Age, na.rm = TRUE)
titanic_data$Age[is.na(titanic_data$Age)]

# Fill missing 'Embarked' values with mode (most frequent value)
titanic_data$Embarked[is.na(titanic_data$Embarked)] <- as.character(titanic_data$Embarked) %>% 
  table() %>% which.max() %>% names()
titanic_data$Embarked[is.na(titanic_data$Embarked)]

# Convert 'Survived', 'Pclass', 'Sex', 'Embarked' to factors
titanic_data <- titanic_data %>%
  mutate(Survived = as.factor(Survived),
         Pclass = as.factor(Pclass),
         Sex = as.factor(Sex),
         Embarked = as.factor(Embarked))
titanic_data

# Drop irrelevant columns (e.g., 'PassengerId', 'Name', 'Ticket', 'Cabin')
titanic_data <- titanic_data %>%
  select(-PassengerId, -Name, -Ticket, -Cabin)
titanic_data

# Check cleaned data
summary(titanic_data)

# Exploratory Data Analysis (EDA)
#Distribution of Age
library(ggplot2)
ggplot(titanic_data, aes(x=Age))+
  geom_histogram(binwidth=5, fill="lightblue",color="black")+
  labs(title="Distribution of Age")

#Distribution of Survived
library(ggplot2)
ggplot(titanic_data, aes(factor(Survived)))+
  geom_bar(fill="lightgreen")+
  labs(title="Distribution of Survived")+
  scale_x_discrete(labels=c("No","Yes"))

#Distribution of Pclass
library(ggplot2)
ggplot(titanic_data, aes(factor(Pclass)))+
  geom_bar(fill="orange")+
  labs(title="Distribution of Pclass")

#Distribution of Sex
library(ggplot2)
ggplot(titanic_data, aes(factor(Sex)))+
  geom_bar(fill="pink")+
  labs(title="Distribution of Sex")

# Survival rate by sex
ggplot(titanic_data, aes(x = Sex, fill = Survived)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", title = "Survival Rate by Sex")

# Survival rate by class
ggplot(titanic_data, aes(x = Pclass, fill = Survived)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", title = "Survival Rate by Class")

# Age distribution by survival
ggplot(titanic_data, aes(x = Age, fill = Survived)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(title = "Age Distribution by Survival")

# Survival rate by embarkation
ggplot(titanic_data, aes(x = Embarked, fill = Survived)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", title = "Survival Rate by Embarkation")

#Boxplot of Age by Survived
ggplot(titanic,aes(x = factor(Sex),y=Age,fill=factor(Survived)))+
  geom_boxplot()+
  labs(title="Age Distribution by Survived")+
  scale_fill_manual(values=c("orange","yellow"),
                    labels=c("Survived","Not survived"))+
  guides(fill=guide_legend(title="Survived"))

# Correlation between age and fare
ggplot(titanic_data, aes(x = Age, y = Fare)) +
  geom_point(aes(color = Survived), alpha = 0.7) +
  labs(title = "Correlation between Age and Fare")


