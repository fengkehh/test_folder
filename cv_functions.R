# support functions for cross validation
library('caret')

# Function to create CV folds
my_createFolds <- function(data, k, seed) {
    n <- nrow(data)
    folds <- rep(0, n)
    fold_size <- floor(n/k)
    # index of data points that haven't been assigned a fold
    index_left <- 1:n
    
    for (i in 1:k) {
        if (i < k) {
            set.seed(seed)
            selected <- sample(1:length(index_left), fold_size)
            folds[index_left[selected]] <- i
            index_left <- index_left[-selected]
            
        } else {
            # Last fold. Assign everything left here.
            folds[index_left] <- k
        }
    }
    
    return(folds)
}

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

# Arguments: data: data to train the model on

# control: control objects that contains hyper-parameters for rpart.

# folds: optional cross validation folds for reproducibility. Length must be 
# the number of rows in data.

# Returns: Model performance from each cross validation fold. Average
# returned vector to obtain final performance estimate!
tree_cv <- function(formula, data, control, folds = NULL) {
    if (is.null(folds)) {
        # No cv folds provided. Generate 10 irreproducible random folds.
        response <- all.vars(formula)[1]
        folds <- createFolds(data[, y], k = 10, list = FALSE)
    }
    k <- max(folds)
    n <- nrow(data)
    index <- 1:n
    accuracies <- rep(0, k)
    
    for (i in 1:k) {
        inFold <- index[folds == i]
        data.infold <- data[inFold, ]
        data.outside <- data[-inFold, ]
        
        # Train model on data outside of fold, predict on data in the fold, compute
        # accuracy.
        set.seed(1)
        model <- rpart(formula, data.outside, control = control)
        
        pred <- predict(model, newdata = data.infold, type = "class")
        
        accuracies[i] <- sum(pred == data.infold[, all.vars(formula)[1]])/nrow(data.infold)
    }
    
    return(mean(accuracies))
}

repeat_tree_CV <- function(formula, data, control, repeat_folds = NULL) {
    if (is.null(repeat_folds)) {
        # no repeat_folds provided. Generate irreproducible 10-10 repeat folds.
        response <- all.vars(formula)[1]
        repeat_folds <- my_repeat_CV_folds(data[, y])
    }
    n <- ncol(repeat_folds)
    
    accuracies <- rep(0, n)
    
    for (i in 1:n) {
        folds <- repeat_folds[,i]
        
        accuracies[i] <- tree_CV(formula, data, control, folds)
    }
    
    return(mean(accuracies))
    
}