# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(randomForest)
library(caret)

# Load the datasets
train_data <- read.csv("C:/Users/Rahul Shaw/Downloads/train.csv")
test_data <- read.csv("C:/Users/Rahul Shaw/Downloads/test.csv")

# View the first few rows of the train dataset
head(train_data)
head(test_data)



# Fill missing 'Age' values with the median age
train_data$Age[is.na(train_data$Age)] <- median(train_data$Age, na.rm = TRUE)

# Fill missing 'Embarked' values with the most frequent value
train_data$Embarked[is.na(train_data$Embarked)] <- as.character(train_data$Embarked) %>% 
  table() %>% which.max() %>% names()

# Drop the 'Cabin' column due to many missing values
train_data <- train_data %>% select(-Cabin)

# Fill missing 'Fare' values with the median fare
train_data$Fare[is.na(train_data$Fare)] <- median(train_data$Fare, na.rm = TRUE)

# Convert 'Pclass', 'Sex', 'Embarked' to factors
train_data <- train_data %>%
  mutate(Pclass = as.factor(Pclass),
         Sex = as.factor(Sex),
         Embarked = as.factor(Embarked))

# View the cleaned training data
head(train_data)



# Create a new feature 'FamilySize'
train_data <- train_data %>%
  mutate(FamilySize = SibSp + Parch + 1)

# Create age groups
train_data <- train_data %>%
  mutate(AgeGroup = cut(Age, breaks = c(0, 12, 18, 35, 60, 100), 
                        labels = c("Child", "Teenager", "Adult", "Middle-aged", "Senior")))

# Create fare groups
train_data <- train_data %>%
  mutate(FareGroup = cut(Fare, breaks = quantile(train_data$Fare, probs = seq(0, 1, 0.25), na.rm = TRUE),
                         include.lowest = TRUE, labels = c("Low", "Medium", "High", "Very High")))

# Extract titles from names
train_data <- train_data %>%
  mutate(Title = sub("^.*, (.*?)\\..*$", "\\1", Name))

# Simplify titles
train_data$Title <- gsub("Mme|Mlle", "Miss", train_data$Title)
train_data$Title <- gsub("Ms", "Miss", train_data$Title)
train_data$Title <- gsub("Lady|Countess|Dona", "Lady", train_data$Title)
train_data$Title <- gsub("Sir|Don|Jonkheer", "Sir", train_data$Title)
train_data$Title <- gsub("Col|Capt|Major", "Officer", train_data$Title)
train_data$Title <- gsub("Dr", "Dr", train_data$Title)

# Create fare per person
train_data <- train_data %>%
  mutate(FarePerPerson = Fare / FamilySize)

# Create family size group
train_data <- train_data %>%
  mutate(FamilySizeGroup = case_when(
    FamilySize == 1 ~ "Alone",
    FamilySize <= 4 ~ "Small",
    TRUE ~ "Large"
  ))

# Drop irrelevant columns
train_data <- train_data %>%
  select(-PassengerId, -Name, -Ticket)

# View the engineered training data
head(train_data)




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

head(test_data)

# Split the data into training and validation sets
set.seed(123)
trainIndex <- createDataPartition(train_data$Survived, p = .8, 
                                  list = FALSE, 
                                  times = 1)
Train <- train_data[ trainIndex,]
Test  <- train_data[-trainIndex,]

# Train a random forest model
#model <- randomForest(Survived ~ ., data = Train, ntree = 100, mtry = 3, importance = TRUE)

# View model summary
#print(model)

# Separate features and target variable
X_train <- Train %>% select(-Survived)
y_train <- Train$Survived
X_test <- Test %>% select(-Survived)
y_test <- Test$Survived

# Train a random forest model
set.seed(42)
rf_model <- randomForest(x = X_train, y = as.factor(y_train), ntree = 100)

# Make predictions on the test dataset
predictions <- predict(rf_model, newdata = X_test)

# Evaluate the model
conf_matrix <- confusionMatrix(predictions, as.factor(y_test))
print(conf_matrix)

# Accuracy of the model
accuracy <- conf_matrix$overall['Accuracy']
print(paste('Accuracy:', round(accuracy, 2)))

# Create a heatmap of the confusion matrix
conf_matrix_df <- as.data.frame(conf_matrix$table)
conf_matrix_df$Prediction <- factor(conf_matrix_df$Prediction, levels = rev(levels(conf_matrix_df$Prediction)))

ggplot(data = conf_matrix_df, aes(x = Prediction, y = Reference)) +
  geom_tile(aes(fill = Freq), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  geom_text(aes(label = Freq), vjust = 1) +
  labs(x = "Predicted", y = "Actual", fill = "Frequency") +
  ggtitle("Confusion Matrix Heatmap")
