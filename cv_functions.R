library('caret')
library('rpart')
library('foreach')
library('parallel')
library('doParallel')

# Function to generate folds for repeated CV folds

# Arguments:
# y: vector representing the response data from the data set

# j: number of repeat (default to 10)

# k: number of folds (default to 10)

# seed: optional random number seed for reproducibility
my_repeat_CV_folds <- function(y, j = 10, k = 10, seed = NULL) {
    repeat_folds <- matrix(rep(0, j*k), nrow = k, ncol = j)
    
    for (i in 1:j) {
        if (!is.null(seed)) {
            set.seed(seed + i - 1)
        }
        repeat_folds[,i] <- createFolds(y, k = k, list = FALSE)    
    }
    
    return(repeat_folds)
}

# Function to train tree models and evaluate model performance using CV

# Arguments: 
# formula: formula object that describes dependent and independent variables
# data: data to train the model on

# control: control objects that contains hyper-parameters for rpart.

# folds: optional cross validation folds for reproducibility. Length must be 
# the number of rows in data.

# k: Integer used in the case folds is not given. Indicates the number of folds to 
# be generated. Special case: k is a string -> leave one out CV, number of 
# folds is nrow(data)

# seed: optional random number seed for reproducibility

# Returns: Average model performance from all cross validation folds.
tree_CV <- function(formula, data, control, folds = NULL, k = NULL, seed = NULL) {
    if (is.null(folds)) {
        # No cv folds provided. Generate k random folds.
        if (is.null(k)) {
            stop('Missing both folds and k values!')
        }
        if (is.character(k)) {
            # LOOCV
            k <- nrow(data)
        }
        y <- all.vars(formula)[1]
        
        if(!is.null(seed)) {
            set.seed(seed)
        }
        folds <- createFolds(data[, y], k = k, list = FALSE)
    } else {
        k <- max(folds)
    }
    n <- nrow(data)
    index <- 1:n
    accuracies <- rep(0, k)
    
    for (i in 1:k) {
        inFold <- index[folds == i]
        data.infold <- data[inFold, ]
        data.outside <- data[-inFold, ]
        
        # Train model on data outside of fold, predict on data in the fold, compute
        # accuracy.
        #set.seed(1)
        model <- rpart(formula, data.outside, control = control)
        
        pred <- predict(model, newdata = data.infold, type = "class")
        
        accuracies[i] <- sum(pred == data.infold[, all.vars(formula)[1]])/nrow(data.infold)
        
    }
    
    return(mean(accuracies))
}

# Function to estimate tree model performance using repeated cross validation
# Arguments: 
# formula: formula object that describes dependent and independent variables
# data: data to train the model on

# control: control objects that contains hyper-parameters for rpart.

# repeat_folds: optional repeated cross validation fold matrix for reproducibility. 
# Number of rows must be equal to the number of rows in data. Each column must be a different but complete set of folds. ncol = j repetitions.

# j: only used if repeat_folds is not given. Specify the number of repetitions.

# k: only used if repeat_folds is not given. Specify the number of folds in each 
# repetition.

# seed: optional random number seed for reproducibility. Only used if repeat_folds 
# is not provided.

# Returns: Average model performance from all repetitions of cross validation estimates.
repeat_tree_CV <- function(formula, data, control, repeat_folds = NULL, j = NULL, 
                           k = NULL, seed = NULL) {
    if (is.null(repeat_folds)) {
        # no repeat_folds provided. Generate repeat folds (j X k).
        if (is.null(j) | is.null(k)) {
            y <- all.vars(formula)[1]
            repeat_folds <- my_repeat_CV_folds(data[, y], j = j, k = k, seed)
        }
    } else {
        j <- ncol(repeat_folds)
    }
    
    accuracies <- rep(0, j)
    
    for (i in 1:j) {
        folds <- repeat_folds[,i]
        
        accuracies[i] <- tree_CV(formula, data, control, folds)
    }
    
    return(mean(accuracies))
    
}

# Function to generate the true estimate of performance by checking model accuracy using the held out test set.
true_estimate <- function(formula, data.train, control, data.test) {
    #set.seed(1)
    model <- rpart(formula, data.train, control = control)
    
    pred <- predict(model, newdata = data.test, type = "class")
    
    accuracy <- sum(pred == data.test[, all.vars(formula)[1]])/nrow(data.test)
    
    return(accuracy)
}

# Function to generate a vector of performance estimate by repeating specified estimation technique n times.

# Arguments:
# FUN: function to use for performance estimation (ie: tree_CV() or 
# repeat_tree_CV())

# formula: formula object describing relations between dependent and independent 
# variables

# data.list: a list of data to be used for performance estimation. Each element is a complete data frame containing the variables specified in formula. The number of such data frames, n, is the number of performance estimations to be carried out.

# control: tree control object

# ...: additional parameters to be passed to the estimation function

# Returns:
# estimates: a size n vector containing the performance estimates
n_estimates <- function(FUN, formula, data.list, control, ...) {
    n <- length(data.list)
    FUN <- match.fun(FUN)
    
    
    # for (i in 1:n) {
    #     data <- data.list[[i]]
    #     
    #     if (!is.null(seed)) {
    #         set.seed(seed)
    #     }
    #     estimates[i] <- FUN(formula, data, control, ...)    
    # }
     
    # Set up parallel computation
    cl <- makeCluster(detectCores())
    registerDoParallel(cl)
    
    estimates <- foreach(i = 1:n, 
                         .combine = 'c',
                         .packages = c('caret', 'rpart')) %dopar% {
        data <- data.list[[i]]
        
        ans <- FUN(formula, data, control, ...)
                         }
    
    stopCluster(cl)
    
    return(estimates)
}

# Function to compute performance estimation bias.

# Arguments:
# true: a vector of the true performances (model built on n training sets and tested on the test set)

# estimates: vector of model performance estimates from whatever method chosen, computed on the same training sets with the same order as the true estimates

# Returns:
# bias: the bias between estimated fitting performance and real performance by subtracting true values from the estimates. ie: negative means downward bias
compute_bias <- function(true, estimates) {
    # compute bias
    bias <- mean(estimates- true)
    
    return(bias)
}