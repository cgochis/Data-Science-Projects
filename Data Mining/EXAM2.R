terabyte_purchase_data <- read.csv('terabyte_purchase_data.csv', stringsAsFactors = TRUE)
options(scipen = 999)
attach(terabyte_purchase_data)

#1
table(purchase)

#2
addmargins(table(purchase, gender))
(66/196) * 100
(77/204) * 100

#3
set.seed(777)
TERA_RANDOMIZED_DATA <- terabyte_purchase_data[order(runif(400)),]

TERA_TRAINING_SET <- TERA_RANDOMIZED_DATA[1:300,]

TERA_TEST_SET <- TERA_RANDOMIZED_DATA[301:400,]

library(rpart)
library(rpart.plot)

attach(TERA_TRAINING_SET)

TERA_TREE<-rpart(purchase~., data = TERA_TRAINING_SET, method="class", control = rpart.control(cp=0))
summary(TERA_TREE)

TERA_TREE$cptable

#4
TERA_PRUNED_TREE <- prune(TERA_TREE, cp=.0189)
TERA_PRUNED_TREE

TERA_PRUNED_TREE_DIAGRAM <- rpart.plot(TERA_PRUNED_TREE, digits = 5, fallen.leaves = TRUE, type=2, extra = 1)

#5
TERA_PREDICTED_TREE<- predict(TERA_PRUNED_TREE,TERA_TEST_SET, type="class")
TERA_PREDICTED_TREE

TERA_TREE_CONFMATRIX <- table(TERA_PREDICTED_TREE, TERA_TEST_SET$purchase)
TERA_TREE_CONFMATRIX
(57+35)/100

#6
attach(TERA_TRAINING_SET)

contrasts(purchase)

TERA_LOGITMODEL<-glm(purchase~., family=binomial(), 
                data=TERA_TRAINING_SET)
summary(TERA_LOGITMODEL)

#7
attach(TEST_SET)

TERA_PROBS_LOGIT <- predict(TERA_LOGITMODEL, TERA_TEST_SET, type="response")
TERA_PROBS_LOGIT

TERA_PREDICTIONS<- ifelse(TERA_PROBS_LOGIT>.50, "yes", "no")
TERA_PREDICTIONS

TERA_LOGIT_CONFMATRIX <- table(TERA_PREDICTIONS, TERA_TEST_SET$purchase)
TERA_LOGIT_CONFMATRIX
(58+25)/100


############################################
boston_housing_data <- read.csv('boston_housing_data.csv', stringsAsFactors = TRUE)
attach(boston_housing_data)
options(scipen = 999)

#1
set.seed(555)
BOSTON_RANDOMIZED_DATA <- boston_housing_data[order(runif(506)),]

BOSTON_TRAINING_SET <- BOSTON_RANDOMIZED_DATA[1:400,]

BOSTON_TEST_SET <- BOSTON_RANDOMIZED_DATA[401:506,]

BOSTON_TREE<-rpart(medv~., 
                      control = rpart.control(minbucket = 1, cp=0),
                      data = BOSTON_TRAINING_SET, method="anova")

tail(BOSTON_TREE$cptable)

#2
BOSTON_PRUNED_TREE <- rpart(medv~., 
                            control = rpart.control(minbucket = 100, cp=0),
                            data = BOSTON_TRAINING_SET, method="anova")

BOSTON_PRUNED_TREE_DIAGRAM <- rpart.plot(BOSTON_PRUNED_TREE, digits = 5, fallen.leaves = TRUE, type=2, extra = 1)

#3
BOSTON_PREDICT_TREE <- predict(BOSTON_PRUNED_TREE, BOSTON_TEST_SET)
BOSTON_PREDICT_TREE

library(Metrics)
rmse(BOSTON_TEST_SET$medv, BOSTON_PREDICT_TREE)
mae(BOSTON_TEST_SET$medv, BOSTON_PREDICT_TREE)

#4
attach(BOSTON_TRAINING_SET)
BOSTON_MLR<- lm(medv~., data = BOSTON_TRAINING_SET)
summary(BOSTON_MLR)

MLR_PREDICTIONS<-predict(BOSTON_MLR, BOSTON_TEST_SET)
MLR_PREDICTIONS

rmse(BOSTON_TEST_SET$medv, MLR_PREDICTIONS)
mae(BOSTON_TEST_SET$medv, MLR_PREDICTIONS)

#5
library(randomForest)
BOSTON_RF<- randomForest(medv~., 
                       data = BOSTON_TRAINING_SET, 
                       ntree = 100, mtry = 13,
                       importance=TRUE)

BOSTON_RF_PREDICT<-predict(BOSTON_RF, BOSTON_TEST_SET)

varImpPlot(BOSTON_RF, type = 1)

rmse(BOSTON_TEST_SET$medv, BOSTON_RF_PREDICT)
mae(BOSTON_TEST_SET$medv, BOSTON_RF_PREDICT)


257+143







