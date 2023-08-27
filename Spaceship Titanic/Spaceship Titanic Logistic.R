# Here I am trying to understand better how to approach a Machine Learning Project. 
# Much of the code is directly copied from DUNCAN MCKINNON in his Kaggle collaboration that you can find here: https://www.kaggle.com/code/duncankmckinnon/spaceship-titanic-r-feature-engineering-analysis
# I just apply his feature engineering to a logistic regression model. Nonetheless, the comments on this code and the implementation
# of the model are of my creation. 

# Libraries

library(tidyverse)
library(forecast)
library(xlsx)
library(stargazer)
library(readr)
library(caTools)
library(ROCR)



#setwd("C:/Users/Oscar Rosales/Desktop/Gazer Analytica/Spaceship Titanic")

#getwd()

# Libraries

ss <- read_csv("sample_submission.csv")


train <- read_csv("train.csv")


test <- read_csv("test.csv")

# Creating an object that contains in a lists the dataframes

my_list <- list()

my_list[["train"]] <- train
my_list[["test"]] <- test


my_list$train

# Data cleaning and transformations

# I separate the PassengerId column since the first part has information about people that could be related.
# I separate the name into first name and second name.
# I separate the cabin column into the sub components that reveal the information about the cabin.

split_cols <- function(df){
  df %>% 
    separate(PassengerId, into=c("PID_1", "PID_2"), sep="_", remove=FALSE, convert=TRUE) %>%
    separate(Name, into = c("FN", "LN"), sep=" ", extra="merge", remove=TRUE) %>%
    separate(Cabin, into = c("Deck", "CabinNum", "PortOrSB"), sep = "/", extra = "merge", remove = TRUE, convert = TRUE)
}


# I full the NA values with hard coded NA al later turn the columns into factor variables to help the model understand them.


update_factors <- function(df){
  df %>%
    mutate(HomePlanet = ifelse(is.na(HomePlanet), "NA", HomePlanet),
           Destination = ifelse(is.na(Destination), "NA", Destination),
           Deck = ifelse(is.na(Deck), "NA", Deck),
           PortOrSB = ifelse(is.na(PortOrSB), "NA", PortOrSB)
    ) %>%
    mutate(HomePlanet = factor(HomePlanet),
           Destination = factor(Destination),
           Deck = factor(Deck),
           PortOrSB = factor(PortOrSB)
    )
}


# I make columns that keep track of the missing values that were not present in original data because the fact
# that they were missing could be reveling information.

encode_missing <- function(df){
  df %>%
    mutate(
      RoomService = ifelse(is.na(RoomService), 0, RoomService),
      FoodCourt = ifelse(is.na(FoodCourt), 0, FoodCourt),
      ShoppingMall = ifelse(is.na(ShoppingMall), 0, ShoppingMall),
      Spa = ifelse(is.na(Spa), 0, Spa),
      VRDeck = ifelse(is.na(VRDeck), 0, VRDeck),
      across(
        where(~!is.factor(.) & any(is.na(.))),
        .fns = function(x) as.numeric(is.na(x)),
        .names="{.col}_isna"
      ),
      across(
        where(~!is.factor(.) & any(is.na(.))),
        .fns = function(x) ifelse(is.na(x), 0, x),
        .names="{.col}"
      ),
      across(
        where(~is.factor(.)),
        .fns = function(x) ifelse(x == "NA", 1, 0),
        .names = "{.col}_isna"
      )
    )
}


# I add columns that keep track of people wth the same last name and that were related by the PassangerId

add_counts <- function(df){
  LN_count <- df %>% group_by(LN) %>% summarize(LN_C = n())
  Group_count <- df %>% group_by(PID_1) %>% summarize(PID_C = min(4, max(PID_2)))
  
  df %>%
    left_join(LN_count, by = "LN", keep=FALSE) %>%
    left_join(Group_count, by = "PID_1", keep=FALSE) %>%
    select(-LN, -FN, -PID_2, -CabinNum)
}

# I create the function that joins all the previous functions

clean_and_engineer <- function(df){
  df %>%
    split_cols() %>%
    update_factors() %>% 
    encode_missing() %>%
    add_counts()
}

# I execute it on the train and test data

my_list$train_clean <- my_list$train %>%
  clean_and_engineer()


my_list$test_clean <- my_list$test %>%
  clean_and_engineer()

# Check how it is going

my_list$train_clean %>% summary()


# Chart that shows the % of people transported by their deck

my_list$train_clean %>%
  left_join(my_list$train_clean %>%
              group_by(Deck) %>%
              summarize(DeckTotal = n(), DeckTransported = sum(Transported)), by = "Deck", keep = F) %>%
  ggplot() +
  geom_histogram(aes(x=Deck, fill=Transported), stat="count", position="stack") +
  geom_text(aes(x=Deck, y = DeckTransported + 10, label = paste0(round(100 * DeckTransported / DeckTotal, 0), "%"))) +
  labs(title = "Transported Rate by Deck", x = "Deck", y = "Passengers", fill = "Transported") + 
  theme_bw()

# There are several columns that reveal the level of money spent by passanger. I will create spending bins 
# to help me analyse the information.



levels <- c("<1K", "<5K", "<10K", ">10K")
spend_category <- function(x){
  if(x < 1000){
    levels[1]
  } else if(x < 5000){
    levels[2]
  } else if(x < 10000){
    levels[3]
  }else{
    levels[4]
  }
}

# I create a function to sum the spending levels in the different columns and bucket it on the preveously created bins

AddSpendingCategory <- function(df){
  df %>%
    mutate(TotalCost = Spa + ShoppingMall + RoomService + FoodCourt + VRDeck) %>%
    mutate(SpendingCategory = sapply(TotalCost, spend_category) %>% factor(levels = levels, ordered = T))
}

# I applay the function to the train and test data


my_list$train_clean  <- my_list$train_clean %>% 
  AddSpendingCategory()

my_list$test_clean <- my_list$test_clean %>%
  AddSpendingCategory()


# Charting transported people by level of spending

my_list$train_clean %>%
  group_by(SpendingCategory) %>%
  summarize(CatCount = n(), TransportedCount = sum(Transported, na.rm = T), NotTransported = CatCount - TransportedCount) %>%
  ggplot() +
  geom_col(aes(x = SpendingCategory, y = NotTransported + TransportedCount), fill = "lightcoral",position = "dodge2") +
  geom_col(aes(x = SpendingCategory, y = TransportedCount), fill = "skyblue",position = "dodge2") +
  
  geom_text(aes(x = SpendingCategory, y = 100 + TransportedCount, label = paste0(round(100 * TransportedCount / CatCount, 0), "%"))) +
  labs(title = "Transported Rate by Spend Grouping", x = "Spend Grouping", y = "Count", fill = "Transported") + 
  theme_bw()

# rich people are transported less

# Check how the data looks

View(my_list$train_clean)


# To train the model I need to leave the PassangerId column out of the train data

my_list$train_clean <- my_list$train_clean[, !(names(my_list$train_clean) == "PassengerId")]

View(my_list$train_clean)

# Here I train the logistc model to predict Transported bease of the other columns

logistic_model <- glm(Transported ~., data = my_list$train_clean, family = "binomial" ) 

logistic_model$fitted.values

# Here look how the models looks

stargazer(logistic_model, type = "text")

summary(logistic_model)

# Confusion matrix

# A confusion matrix allow us to test how the model performed since it compare the predicted values with the
# actual values


# I need to create the dataframes that allow a confusion matrix

# I create a dataframe for the fitted values(values predicted by the model)
# Since this is a logistic model the fitted values are probabilities, values between 0 and 1.   
# For this instance I will use 0.5 as the cutoff value to decide if the person was transfered(>0.5) or not(0.5<)

fitted_values <- logistic_model$fitted.values %>% as.data.frame() %>% rename(Transported = ".") %>% mutate(Transported = ifelse(Transported <0.5, F,T ))


# I create a dataframe for the original values

original_values <- my_list$train_clean %>% 
  select(Transported) %>% 
  as.data.frame()

# The confusion matrix

table(Actual = original_values$Transported,
      Predicted = fitted_values$Transported)

# the accuracy of the model

(3319+3578)/(3319+3578+996+800)


# I can try to find a better cutooff value that improve the accuracy of the model


# ROCOpred will create an object that contains information about the predictions,a predictions object.
# We later use the ROCRpred information and evalate the performance, creating the ROCRpref



ROCRpred = ROCR::prediction(logistic_model$fitted.values, my_list$train_clean$Transported)
ROCRpref <-performance(ROCRpred, "tpr", "fpr")

#

# Here we plot how the true positive rate and the false positive rate change at different cutoff values.
# Depending on the objective of the model we could pick different cutoff values, but each decision will have impact on the model performance.

plot(ROCRpref, colorize =T, print.cutoff.at = seq(0.1, by  = 0.01))




#Perhaps picking 0.4 as a cutoff value instead of 0.5 will improve the accuracy

# New confusion matrix with cutoff at 0.47


fitted_valuesb <- logistic_model$fitted.values %>% as.data.frame() %>% rename(Transported = ".") %>% mutate(Transported = ifelse(Transported <0.47, F,T ))


table(Actual = original_values$Transported,
      Predicted = fitted_valuesb$Transported)

(3231+3672)/(3231+3672+706+1084)

# Making the forecast

predictions <- data.frame(PassengerId =  my_list$test_clean, predicted_value = predict(logistic_model, newdata = my_list$test_clean, type = "response"))

# I make submission with the 2 cutoff point. The difference is only slight but the best scored is the 0.47 cutoff point. 

submission.05 <- predictions %>% 
  select(PassengerId.PassengerId, predicted_value) %>% 
  rename(PassengerId = "PassengerId.PassengerId") %>% 
  rename(Transported = "predicted_value") %>% 
  mutate(Transported = ifelse( Transported < 0.5,"False", "True"))


submission.047 <- predictions %>% 
  select(PassengerId.PassengerId, predicted_value) %>% 
  rename(PassengerId = "PassengerId.PassengerId") %>% 
  rename(Transported = "predicted_value") %>% 
  mutate(Transported = ifelse( Transported < 0.47,"False", "True"))


head(submission.05)

# Eporting the results in a csv formant

write_csv(submission.05 ,"submission05.csv")


write_csv(submission.047 ,"submission047.csv")

