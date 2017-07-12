true_estimate <- function(formula, data.train, data.test, control) {
    set.seed(1)
    model <- rpart(formula, data.train, control = control)
    
    pred <- predict(model, newdata = data.test, type = "class")
    
    accuracy <- sum(pred == data.test[, all.vars(formula)[1]])/nrow(data.test)
    
    return(accuracy)
}
