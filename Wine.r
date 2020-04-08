
install.packages('ggcorrplot')

library(rvest)
library(repr)
library(tidyverse)
library(stringr)
library(forcats)
library(DBI)
library(RPostgres)
library(lubridate)
library(dplyr)
library(caret)
library(ggcorrplot)
library(GGally)

wine<-read_csv('wine_dataset.csv')
head(wine)

options(repr.plot.width = 6, repr.plot.height = 4) 
hist(wine$quality) 

wine$rating <- ifelse(wine$quality < 6, 'low', 'high')
#wine$rating[wine$quality == 6] <- 'mediocre'
wine$rating <- as.factor(wine$rating)
red<-wine%>%
     filter(style=='red')
white<-wine%>%
       filter(style=='white')



set.seed(10) # makes the random selection of rows reproducible
set_rows_red <- red %>% 
  select(rating) %>% 
  unlist() %>% # converts Class from a tibble to a vector
  createDataPartition(p = 0.75, list = FALSE)

#
training_set_red<- red %>% slice(set_rows_red)
test_set_red<- red %>% slice(-set_rows_red)

set_rows_white <- white %>% 
  select(rating) %>% 
  unlist() %>% # converts Class from a tibble to a vector
  createDataPartition(p = 0.75, list = FALSE)

#
training_set_white<- white %>% slice(set_rows_white)
test_set_white<- white %>% slice(-set_rows_white)





glimpse(training_set_red)
glimpse(training_set_white)

glimpse(test_set_red)
glimpse(test_set_white)

#TRANSFORM Red

scale_transformer <- preProcess(training_set_red, method = c("center", "scale")) 
training_set_red<- predict(scale_transformer, training_set_red)
test_set_red<- predict(scale_transformer, test_set_red)

#TRANSFORM White
scale_transformer <- preProcess(training_set_white, method = c("center", "scale")) 
training_set_white<- predict(scale_transformer, training_set_white)
test_set_white<- predict(scale_transformer, test_set_white)


options(repr.plot.width = 12, repr.plot.height = 10)
corr_red<- training_set_red%>%
        select(-style,-rating)%>%
        cor() 
corr_white<- training_set_white%>%
        select(-style,-rating)%>%
        cor() 
ggcorrplot(corr_red,lab = TRUE,title='Correlation Red')
ggcorrplot(corr_white,lab = TRUE,title='Correlation white')

#ggpairs(training_set_red%>%select(-rating))
#ggpairs(training_set_white%>%select(-rating))

X_train_r<- training_set_red%>% 
  select(volatile_acidity, citric_acid, sulphates, alcohol) %>% 
  data.frame()
Y_train_r<- training_set_red%>% 
  select(rating) %>% 
  unlist()
X_train_w<- training_set_white%>% 
  select( chlorides, density, alcohol,volatile_acidity,pH) %>% 
  data.frame()
Y_train_w<- training_set_white%>% 
  select(rating) %>% 
  unlist()

train_control <- trainControl(method="cv", number = 5)


set.seed(1234)

ks<-data.frame(k = seq(from = 10, to = 250, by = 5))




choose_k_r <- train(x = X_train_r, y = Y_train_r, method = "knn", tuneGrid = ks, trControl=train_control)

choose_k_w <- train(x = X_train_w, y = Y_train_w, method = "knn", tuneGrid = ks, trControl=train_control)




k_accuracies_r<- choose_k_r$results %>%
                    select(k, Accuracy)
k_accuracies_w<- choose_k_w$results %>%
                    select(k, Accuracy)
choose_k_plot_r<-ggplot(k_accuracies_r, aes(x = k, y = Accuracy)) +
                  geom_point(color='red') +
                  geom_line(alpha=.3)
choose_k_plot_w<-ggplot(k_accuracies_w, aes(x = k, y = Accuracy)) +
                  geom_point(color='cyan') +
                  geom_line(alpha=.3)
choose_k_plot_r
choose_k_plot_w


best_k_r<- choose_k_r$results %>%
    filter(Accuracy == max(Accuracy)) %>%
    select(k) %>%
    unlist()

best_k_r

best_k_w<-choose_k_w$results %>%
      filter(Accuracy == max(Accuracy)) %>%
        select(k) %>%
        unlist()


best_k_w


model_red <-train(x = X_train_r, 
                   y = Y_train_r, 
                  method = "knn", 
                  tuneGrid = data.frame(k=20),trControl=train_control)

model_white <-train(x = X_train_w, 
                   y = Y_train_w, 
                  method = "knn", 
                  tuneGrid = data.frame(k=25),trControl=train_control)



model_white

model_red

X_test_white <- test_set_white%>% 
  select(chlorides, density, alcohol,volatile_acidity,pH) %>% 
  data.frame()
Y_test_predicted_white<- predict(object = model_white, X_test_white)
head(Y_test_predicted_white)

X_test_red<- test_set_red %>% 
  select(volatile_acidity, citric_acid, sulphates, alcohol) %>% 
  data.frame()
Y_test_predicted_red<- predict(object = model_red, X_test_red)
head(Y_test_predicted_red)

Y_test_red<- test_set_red%>% 
  select(rating) %>% 
  unlist()

model_quality_red<- confusionMatrix(data = Y_test_predicted_red, reference = Y_test_red)
model_quality_red


Y_test_white<- test_set_white%>% 
  select(rating) %>% 
  unlist()

model_quality_white<- confusionMatrix(data = Y_test_predicted_white, reference = Y_test_white)
model_quality_white



