
#Load some more packages#
Packages <- c("tidyverse","here", "caret", "rpartScore", "xgboost", "recipes","MASS","kernlab","C50","naivebayes","ranger","nnet")
lapply(Packages, library, character.only = TRUE)

#Create categories for each of the clusters
summary
clust <- c(1:5)
clust1 <- c("Blowout","Good","Great","Average","Poor")
lookup <- data.frame(clust,clust1)
lookup$clust <- as.character(lookup$clust)

#join the named clusters
c <- class %>% inner_join(lookup,by=c("cluster"="clust"))
c <- c %>% dplyr::select(-cluster)
c <- c %>% dplyr::select(-clust1)

#keep necessary variables for model
class_select <- c %>% dplyr::select(elo_favor_team:cluster)

#hold 2021 data as test set
test <- c %>% filter(season==2021)

test$cluster <- as.factor(test$cluster)
class_select %>% group_by(cluster) %>% count()

# Split the data into training and testing sets (not a typical way to do this)
training <- class_select
training$cluster <- as.factor(training$cluster)
training %>% group_by(cluster) %>% count()

#build the model
class_rec <- recipe(cluster ~ elo_favor_team + elo_under_team + 
                      elo_prob1 + elo_avg, data= training) %>% 
  step_scale(all_predictors())

#set the training controls
ctrl <- trainControl(method = "repeatedcv",number=10,repeats=3,sampling="down",classProbs = TRUE,
                     summaryFunction = multiClassSummary,
                     savePredictions = "final",allowParallel = TRUE)

#Run 4 algorithms, random forest, neural net, regression tree, fancy regression tree (c5.0)
class_ranger <- train(class_rec, data = training, method = "ranger",  trControl = ctrl,metric="logLoss",importance = 'impurity') #random forest 2 
class_nn <- train(class_rec, data = training, method = "nnet",  trControl = ctrl,metric="logLoss") #neural net
class_cart <- train(class_rec, data = training, method = "rpart2",  trControl = ctrl,metric="logLoss") #regression tree 
class_50 <- train(class_rec, data = training, method = "C5.0",  trControl = ctrl,metric="logLoss")  #boosting

#review performance
a <- getTrainPerf(class_50)
b <- getTrainPerf(class_ranger)
d <- getTrainPerf(class_nn)
e <- getTrainPerf(class_cart)

train_perf <- bind_rows(a,b,d,e) %>% arrange(TrainlogLoss) %>% dplyr::select(method,
                                                                                   TrainlogLoss,TrainMean_Sensitivity,TrainMean_Specificity)

# Create new columns
results <- training %>%
  mutate(C50 = predict(class_50, training),
         Neural_Net = predict(class_nn,training),
         Random_Forest2 = predict(class_ranger, training),
         CART = predict(class_cart,training))

# Load yardstick
library(yardstick)
# Evaluate the performance with confusion matrix
confusionMatrix(results$cluster,results$C50)
confusionMatrix(results$cluster,results$Random_Forest2)
confusionMatrix(results$cluster,results$CART)
confusionMatrix(results$cluster,results$Neural_Net)

test$cluster <- as.factor(test$cluster)
# Create the new columns
testing_results <- test %>%
  mutate(C50 = predict(class_50, test),
         Neural_Net = predict(class_nn,test),
         Random_Forest2 = predict(class_ranger, test),
         CART = predict(class_cart,test))

# Evaluate the performance on test
confusionMatrix(testing_results$cluster,testing_results$C50)
confusionMatrix(testing_results$cluster,testing_results$Random_Forest2)
confusionMatrix(testing_results$cluster,testing_results$CART)
confusionMatrix(testing_results$cluster,testing_results$Neural_Net)

#do variable importance scores
# ranger_imp <- varImp(class_ranger, scale = FALSE, 
#                    surrogates = FALSE, 
#                    competes = FALSE)
# ggplot(ranger_imp) + xlab("")
# Add predictions to validation set

#new %>% group_by(cluster) %>% count()

#write data to file
testing_results %>% dplyr::select(date:elo_avg,C50) %>% write_csv('data/game_class.csv')

