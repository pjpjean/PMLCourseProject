# read dataset
setwd("~/GitHub/PMLCourseProject")
pml <- read.csv("data\\pml-training.csv")

# check it out
str(pml)
# variable classes
table(sapply(pml, class))
# factor integer numeric 
#     37      35      88 

# factors
summary(pml[names(which(sapply(pml, class)=="factor"))])

# there are many NA's, Excel error codes (#DIV/0!) and empty strings
# reread dataset, taking care of these problems
pml <- read.csv("data\\pml-training.csv",
                stringsAsFactors = FALSE,
                na.strings = c("","NA", "#DIV/0!"))
pml.submit <- read.csv("data\\pml-testing.csv",
                       stringsAsFactors = FALSE,
                       na.strings = c("","NA", "#DIV/0!"))

table(sapply(pml, class))
# character   integer   logical   numeric 
#        4        35         6       115 
# check strings
head(pml[names(which(sapply(pml, class)=="character"))])
# check logicals
summary(pml[names(which(sapply(pml, class)=="logical"))])
# they're all NA's. get rid of them.
# for (varname in names(which(sapply(pml, function(x) all(is.na(x))))))
#      pml[varname] <- NULL

# check NA's left
# remove all variables with more than 90% NA's
na.count <- sapply(pml, function(x) sum(is.na(x)))
na.count[na.count/nrow(pml) > .9]
summary(na.count)
hist(na.count)
hist(na.count[na.count/nrow(pml) > .9])
table(na.count)
table(na.count[na.count/nrow(pml) > .9])

# transform some variables
pml <- within(pml, {
  classe <- factor(classe)
  new_window <- as.numeric(new_window == "yes") # convert to logic (0 or 1)
})
              
pml.submit <- within(pml.submit, {
  # classe <- factor(classe, levels = levels(pml$classe))
  new_window <- as.numeric(new_window == "yes") # convert to logic (0 or 1)
})

table(sapply(pml, class))
head(pml)

# it seems that num_window is an id
table(pml$user_name, pml$num_window)
nrow(unique(pml[c("user_name", "num_window")]))
length(unique(pml$num_window))

# we're not going to perform time-series analysis
# this data would likely benefit of time series analysis since activities
# have a logical connection (suggested variables: previous_activity, etc.)
# Unfortunately, test set observations are isolated cases and thus have no
# time series context (well, technically they have, because they have
# timestamp, num_window, etc., but as they are isolated, they shouldn't)

# user_name, 

# categorize variables
id.vars <- c("X", "num_window")
rem.vars <- c("raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp")
#all.na.vars <- names(which(sapply(pml, function(x) all(is.na(x)))))
na.vars <- names(which(sapply(pml, function(x) sum(is.na(x))/length(x)) > 0.9))
#almost.na.vars <- almost.na.vars[!almost.na.vars %in% all.na.vars]
  
y.var <- "classe"
x.vars <- names(pml)[!names(pml) %in% c(id.vars, rem.vars, na.vars, y.var)]
d.vars <- c(x.vars, y.var)

library(caret)
library(ggplot2)
library(randomForest)
source("code\\utils.R")

set.seed(40187)
inTrain <- createDataPartition(pml$classe, p=0.6, list=FALSE)
pml.train <- pml[inTrain, d.vars]
#
inCV <- createDataPartition(pml[-inTrain,]$classe, p=0.5, list=FALSE)
pml.cv <- pml[-inTrain, d.vars][inCV, ]
pml.test <- pml[-inTrain, d.vars][-inCV, ]


## random forest
# In random forests, there is no need for cross-validation or a separate test
# set to get an unbiased estimate of the test set error. It is estimated
# internally, during the run, as follows:
rf.basemodel <- train(classe ~ ., 
                  data = pml.train, 
                  method = "rf", 
                  trControl = trainControl(
                    method="oob"),
                  tuneLength = 5)
rf.basemodel
rf.basemodel$finalModel

pred.basemodel <- predict(rf.basemodel,newdata = pml.cv)
confusionMatrix(pml.cv$classe, pred.basemodel)
varImp(rf.basemodel)

predict(rf.basemodel, newdata=pml.submit)


# test learning curves with multinomial logistic regression
library(nnet)
table(sapply(pml.train, class))

m1 <- multinom(classe ~ ., pml.train, family="binomial")
lc <- learningCurves(
  pml.train[sample(nrow(pml.train), 1000),], pml.cv, 
  start = 200,
  modelCall = function(d) multinom(classe ~ ., d, family="binomial"),
  modelErrorCall = function(fit, d) {
    pred <- predict(fit, newdata = d, type="class")
    sum(pred != d$classe)/nrow(d)                 
  }, plot=TRUE)
lc

