split.optimize <- function(data, response = "y", formula, ...){
  
  require(rpart)
  require(pROC)

  perc.split <- seq(0.2, 0.8, by = 0.05)
  
  aucs <- data.frame(train = 1:length(perc.split), test = 1:length(perc.split))
  
  for(i in 1:length(perc.split)){
    
    # split data
    row.samp <- sample(1:nrow(data), perc.split[i]*nrow(data))
    data.train <- data[row.samp, ]
    data.test <- data[-row.samp, ]
    
    # model and predict
    rt <- rpart(formula = formula, data = data.train, ...)
    pred.train <- predict(rt, data.train)
    pred.test <- predict(rt, data.test)
    
    aucs[i, 'train'] <- as.numeric(auc(data.train[, response], pred.train))
    aucs[i, 'test'] <- as.numeric(auc(data.test[, response], pred.test))
    
  }
  
  return(aucs)
}
  