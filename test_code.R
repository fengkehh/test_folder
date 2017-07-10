source('Testing_Model_Tuning.R')

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
data.train <- data[inTrain2, ]
data.validation <- data[-inTrain2, ]

tunegrid <- expand.grid(minsplit = 2:20, 
                        minbucket = 1:10, 
                        cp = exp(seq(log(0.001), log(0.5), length.out = 10)))

result <- rpart_grid_search(response ~ inst, 
                  data.train = data.train, 
                  data.test = data.validation, 
                  tuneGrid = tunegrid, 
                  seed = 123)

result[which.max(result$accuracies),]
