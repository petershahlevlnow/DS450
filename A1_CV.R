data(swiss)
data(mtcars)

## First the user defined functions 
cv.rpartXse <- function(form, train, test, ...) {
  require(DMwR)
  t <- rpartXse(form, train, ...)
  p <- predict(t, test)
  mse <- mean((p - resp(form, test))^2)
  c(nmse = mse/mean((mean(resp(form, train)) - resp(form, test))^2), 
    mse = mse)
}

## run the experimental comparison
results <- experimentalComparison(
  c(dataset(Infant.Mortality ~ ., swiss)),
    #dataset(mpg ~ ., mtcars)),
  c(variants('cv.rpartXse',se=c(0,0.5,1))),
  cvSettings(1,10,1234)
)
## get the best scores for dataset and statistic
b <- bestScores(results)

bestModelNames <- lapply(bestScores(results), function(x) x['nmse', 'system'])
learners <- c(rf = 'randomForest', rpart = 'rpartXse')
funcs <- learners[lapply(strsplit(bestModelNames, '\\.'), function(x) x[2])]
parSetts <- lapply(bestModelNames, function(x) getVariant(x, resAll)@pars)

t <- getVariant('cv.rpartXse.v1',results)

preds <- predict(t, swiss)
getVariant(bestScores(results)$swiss['nmse','system'],results)@pars

