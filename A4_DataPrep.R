# Get Data from CSV file
# some comments are in relation to the assignment pdf. 
video <- read.csv("H:/R/DS450/DS450/Video_Store.csv")
head(video)
str(video)
summary(video)

# bin age with a depth of 4 (interval is every four years.)
age.cuts <- cut(video$Age, breaks =  seq(12, 72, 4), labels = seq(12, 72-4, 4))
summary(age.cuts)

# min/max normalization.
nums <- sapply(video, is.numeric)
head(video[, nums])

video.01.scale <- sapply(video[,nums], function(x) ((x-min(x))/diff(range(x))))
colnames(video.01.scale) <- c("mm.custid", "mm.income", "mm.age", "mm.rentals", "mm.avg.p.visit")
summary(video.01.scale)

# z-score normalization

video.z.scale <- scale(video[,nums])
colnames(video.z.scale) <- c("z.custid", "z.income", "z.age", "z.rentals", "z.avg.p.visit")
summary(video.z.scale)

# Discretize income 

income.labels <- c("<$25K", "$25-$59K", ">$60K")
income.breaks <- c(min(video$Income)-1, 25000, 60000, max(video$Income)+1)
income.cuts <- cut(video$Income, breaks = income.breaks, labels = income.labels)
summary(income.cuts)

# Combine columns to original
video.bind <- video
video.bind <- cbind(video.bind, age.cuts)
video.bind <- cbind(video.bind, video.01.scale)
video.bind <- cbind(video.bind, video.z.scale)
video.bind <- cbind(video.bind, income.cuts)
head(video.bind)

# Model Matrix
video.model <- model.matrix(~.- 1, video[, -1])
head(video.model)

# make dummy variables
require(dummies)
facts <- sapply(video, is.factor)
dummy.vars <- as.data.frame(sapply(video[,facts], dummy))
str(dummy.vars)
colnames(dummy.vars) <- c("female", "male", "no.inc", "yes.inc", "action", "comedy", "drama")

nums <- sapply(video, is.numeric)
numeric.vars <- video[, nums]

video.combined <- cbind(numeric.vars[,-1], dummy.vars)
str(video.combined)

# correlation matrix
require(corrplot)

cors <- cor(video.combined, method = "pearson")

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
F. 
corm <- cor.mtest.2(cors)

# corrplot
cex.before <- par("cex")
par(cex = 0.7)
corrplot(cors,  p.mat = corm[[1]], insig = "blank", method = "color",
         addCoef.col="grey", 
         order = "AOE", tl.cex = 0.8,
         cl.cex = 1/par("cex"), addCoefasPercent = FALSE)
par(cex = cex.before)

# cross tabulation on gender variables and genre
table(video$Gender, video$Genre)

require(ggplot2)
ggplot(video, aes(x = Gender)) + geom_bar() + facet_grid(.~ Genre)

# good customers rentals > 30

require(dplyr)
video.ge.30 <- video %>% filter(Rentals >= 30)
video.lt.30 <- video %>% filter(Rentals < 30)

# customers with >= 30 rentals are typically younger and have a lower income

source('~/R/DataAnalytics/PlotFunctions.R', echo=TRUE)
summary(video.ge.30)
summary(video.lt.30)
summary(video)

labels <- c("Mean Income for 30+ Rentals", "< 30 Rentals")
plot.dists.cut(video.ge.30$Income,video.lt.30$Income, cols = labels, nbins = 10)

labels <- c("Mean Age for 30+ Rentals", "< 30 Rentals")
plot.dists.cut(video.ge.30$Age,video.lt.30$Age, cols = labels, nbins = 16)

# action with incidentals. not comedy. potentially male although insignificant.
