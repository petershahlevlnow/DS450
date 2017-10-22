# Get Data from CSV file
bank <- read.csv("Bank Data.csv")
head(bank)
str(bank)
summary(bank)

# create dummy variables for region
require(dplyr)
bank <- bank %>% mutate(inner.city = region == "INNER_CITY")
bank <- bank %>% mutate(rural = region == "RURAL")
bank <- bank %>% mutate(suburban = region == "SUBURBAN")
bank <- bank %>% mutate(town = region == "TOWN")

# remove region
bank <- bank %>% select(-region)

# scatter plot matrix
require(car)
scatterplotMatrix(bank)

# correlation matrix
require(corrplot)

# change logical and factor columns to numerics
factor.nums <- sapply(bank, is.factor)
bank.numeric <- bank
bank.numeric[, factor.nums] <- sapply(bank.numeric[, factor.nums], as.numeric)

logical.nums <- sapply(bank.numeric, is.logical)
bank.numeric[, logical.nums] <- sapply(bank.numeric[, logical.nums], as.numeric)

# get correlations
cors <- cor(bank.numeric, method = "pearson")

cor.mtest.2 <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

corm <- cor.mtest.2(cors)

# corrplot
cex.before <- par("cex")
par(cex = 0.7)
corrplot(cors,  p.mat = corm[[1]], insig = "blank", method = "color",
         addCoef.col="grey", 
         order = "AOE", tl.cex = 0.8,
         cl.cex = 1/par("cex"), addCoefasPercent = FALSE)
par(cex = cex.before)

# not much seems to be correlated, especially to PEP

## Run a baseline rpart

# split data 50% - 50% to start
set.seed(1357)
perc.split <- 0.5
row.samp <- sample(1:nrow(bank), perc.split*nrow(bank))
bank.train <- bank[row.samp, ]
bank.test <- bank[-row.samp, ]

require(rpart)
require(rpart.plot)
bank.base.rt <- rpart(pep ~., data = bank.train)
printcp(bank.base.rt)

rpart.plot(bank.base.rt, extra = 1)

# predict baseline test set for accuracy statistics
bank.base.rt.preds <- predict(bank.base.rt, bank.test)

require(pROC)
require(caret)
bank.roc <- roc(bank.test$pep, bank.base.rt.preds[,2])
plot(bank.roc, print.thres = TRUE)
auc(bank.roc)

threshold <- .605
pred.class <- ifelse(bank.base.rt.preds[,2] > threshold, "YES", "NO")
cm.base <- confusionMatrix(pred.class, bank.test$pep, positive = "YES")
cm.base

# Prune tree based on minimum xerror from cp table
prune.cp <- bank.base.rt$cptable[which.min(bank.base.rt$cptable[,"xerror"]),"CP"]
bank.base.rtp <- prune(bank.base.rt, cp = prune.cp)
rpart.plot(bank.base.rtp, extra = 1)

# this results in the same tree. 

# Try automated post process.
require(DMwR)
bank.base.rtxse <- rpartXse(pep ~., data = bank.train)
printcp(bank.base.rtxse)

rpart.plot(bank.base.rtxse, extra = 1)

bank.base.rtxse.preds <- predict(bank.base.rtxse, bank.test)
bank.roc.rtxse <- roc(bank.test$pep, bank.base.rtxse.preds[,2])
plot(bank.roc.rtxse, print.thres = TRUE)
auc(bank.roc.rtxse)

threshold <- .512
pred.class <- ifelse(bank.base.rtxse.preds[,2] > threshold, "YES", "NO")
cm.base.rtxse <- confusionMatrix(pred.class, bank.test$pep, positive = "YES")
cm.base.rtxse

# build table of control parameters
rt.ctrl <- as.data.frame(bank.base.rt$control)
rt.ctrl <- rbind(rt.ctrl, as.data.frame(bank.base.rtxse$control))
row.names(rt.ctrl) <- c("rpart.base", "rpartXse")
rt.ctrl
