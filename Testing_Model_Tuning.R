library('caret')
library('rpart')

# Tune using grid Search on a fixed set of data (and evaluate on a fixed validation set)
rpart_grid_search <- function(formula, data.train, data.test, tuneGrid, seed = NULL) {
    resp <- all.vars(formula)[1]
    accuracies <- rep(0, nrow(tuneGrid))
    
    for (i in 1:nrow(tuneGrid)) {
        treeCon <- do.call(rpart.control, as.list(tuneGrid[i, ]))
        
        if (!is.null(seed)) {
            set.seed(seed)
        }
        model <- rpart(formula, data.train, control = treeCon)
        
        pred <- predict(model, newdata = data.test, type = 'class')
        
        accuracies[i] <- sum(pred == data.test[, resp])/nrow(data.test)
    }
    
    ans <- data.frame(tuneGrid, accuracies = accuracies)
    
    return(ans)
}

# Tune model parameters using cross validation
rpart_tune <- function(formula, data, k, tuneGrid, seed = NULL) {
    if (!is.null(seed)) {
        set.seed(seed)
    }
    folds <- createFolds(data$response, k, list = FALSE)
    
    max_params_inFold <- data.frame()

    for (i in 1:k) {
        inFold <- folds == i
        data.inFold <- data[inFold, ]
        data.outside <- data[!inFold, ]
        
        result <- rpart_grid_search(formula, 
                                    data.outside, 
                                    data.inFold, 
                                    tuneGrid, 
                                    seed)
        
        max_params_inFold <- rbind(result[which.max(result$accuracies), ])
    }
    
    return(max_params_inFold)
}

# Tune model parameters using nested cross validation
# rpart_tune_nested <- function(data, k, tuneGrid, seed) {
#     # Generate folds for outer cross validation
#     outer_folds <- createFolds(data$response, k, list = FALSE)
#     
#     for (i in 1:k) {
#         data.inFold <- data[outer_folds == i,]
#         
#     }
# }