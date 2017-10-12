# Get Data from CSV
wine <- read.csv("~/Dropbox/UW_DS/DS450/Lecture2/RedWhiteWine.csv")
head(wine)
str(wine)
summary(wine)

# remove quality column 
require(dplyr)
wine <- wine %>% select(-quality)

# explore the correlations and relationships
require(car)
scatterplotMatrix(x = wine, var.labels = colnames(wine))

# correllation matrix
require(corrplot)
cors <- cor(wine, method = "pearson")

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

# ggplot of a few correlated attributes to class
require(ggplot2)

ggplot(wine, aes(total.sulfur.dioxide, volatile.acidity)) +
  geom_point(aes(color = factor(Class, labels = c("White", "Red"))), alpha = 0.3) +
  xlab('Total SO2') + ylab('Volatile Acidity') + labs(color = "Wine") +
  ggtitle('Total SO2 vs Volatile Acidity by Class')

ggplot(wine, aes(total.sulfur.dioxide, volatile.acidity)) +
  geom_point(aes(color = factor(Class, labels = c("White", "Red"))), alpha = 0.3) +
  facet_grid(Class ~ .) +
  xlab('Total SO2') + ylab('Volatile Acidity') + labs(color = "Wine") +
  ggtitle('Total SO2 vs Volatile Acidity by Class') 

ggplot(wine, aes(chlorides, fixed.acidity)) +
  geom_point(aes(color = factor(Class, labels = c("White", "Red"))), alpha = 0.3) +
  xlab('chlorides') + ylab('Fixed Acidity') + labs(color = "Wine") +
  ggtitle('Chlorides vs Fixed Acidity by Class')

# Train and test split
perc.split <- 0.5
row.samp <- sample(1:nrow(wine), perc.split*nrow(wine))
wine.train <- wine[row.samp, ]
wine.test <- wine[-row.samp, ]

# baseline rpart with all attributes
require(rpart)
require(rpart.plot)
rt.base <- rpart(Class ~., data = wine.train)
rpart.plot(rt.base, extra = 1, digits = 4)

rt.base.pred <- predict(rt.base, wine.test)

require(pROC)
require(caret)
wine.roc <- roc(wine.test$Class, rt.base.pred)
plot(wine.roc, print.thres = TRUE)
auc(wine.roc)
threshold <- .461
pred.class <- ifelse(rt.base.pred > threshold, 1, 0)
confusionMatrix(pred.class, wine.test$Class, positive = "1")
