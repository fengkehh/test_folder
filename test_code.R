source('Testing_Model_Tuning.R')
source('cv_functions.R')

set.seed(123)
toss <- rbinom(2000, 1, 0.5)
inst <- rnorm(2000) + toss
volt <- rnorm(2000)
water <- rnorm(2000)

toss_fac <- factor(toss, labels = c("tail", "head"))

data <- data.frame(inst = inst, volt = volt, water = water, response = toss_fac)

set.seed(1)
inTrain <- createDataPartition(data$response, p = 0.5, list = FALSE)

data.train <- data[inTrain, ]
data.test <- data[-inTrain, ]

set.seed(1)
inTrain2 <- createDataPartition(data.train$response, p = 0.7, list = FALSE)
data.train2 <- data.train[inTrain2, ]
data.validation <- data.train[-inTrain2, ]

# Bias Variance Analysis for Performance Estimation

# create cv folds
set.seed(123)
folds_2 <- createFolds(data.train$response, k = 2)
set.seed(123)
folds_10 <- createFolds(data.train$response, k = 10)
set.seed(123)
folds_loo <- createFolds(data.train$response, k = nrow(data.train))

# create repeated cv folds
repeat_folds <- my_repeat_CV_folds(data.train$response, j = 10, k = 10, seed = 123)


tunegrid <- expand.grid(minsplit = 2:20, 
                        minbucket = 1:10, 
                        cp = c(0.001, 0.01, 0.1))

result <- rpart_grid_search(response ~ inst, 
                  data.train = data.train2, 
                  data.test = data.validation, 
                  tuneGrid = tunegrid, 
                  seed = 123)

result[which.max(result$accuracies),]
