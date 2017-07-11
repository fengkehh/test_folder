library('caret')
library('rpart')

# Function to Generate n performance estimations 
# (underlying model is hard coded to rpart)

# Arguments:
# FUN: function to use for performance estimation (ie: tree_CV() or 
# repeat_tree_CV())

# formula: formula object describing relations between dependent and independent 
# variables

# data: the data to be used for performance estimation

# control: tree control object

# n: number of performance estimations to be generated

# seed: optional random number seed for reproducibility

# ...: arguments to be past to the performance estimation function, FUN 
# (ie: folds)

# Returns:
# estimates: a size n vector containing the performance estimates
generate_estimates <- function(FUN, formula, data, control, n = 30, 
                               seed = NULL, ...) {
    
    estimates <- rep(0, n)
    
    for (i in 1:n) {
        estimates[i] <- FUN(formula, data, control, ...)    
    }
    
    return(estimates)
}


# Function to compute performance estimation bias.

# Arguments:
# model: constructed model fit object

# data.heldout: held-out data set to be used to compute real performance

# estimates: vector of model performance estimates from whatever method chosen

# ...: any additional arguments to be passed to the predict function for model

# Returns:
# bias: the bias between estimated fitting performance and real performance
compute_bias <- function(model, data.heldout, estimates, ...) {
    
    pred <- predict(model, data.heldout, ...)
    
    response <- all.vars(terms(model))[1]
    
    # true model fitting performance
    true <- sum(pred == data.heldout[, response])/nrow(data.heldout)
    
    # compute bias
    bias <- mean(estimates - true)
    
    return(bias)
}

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