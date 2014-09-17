---
title: "Course Project Analysis"
author: "pjpjean"
output: html_document
---


# Initializing

```r
library(caret)
library(ggplot2)
library(randomForest)
source(wd("code\\utils.R"))
```



# Read datasets

```r
pml <- read.csv(wd("data/pml-training.csv"), stringsAsFactors = FALSE, na.strings = c("", 
    "NA", "#DIV/0!"))
pml.submit <- read.csv(wd("data/pml-testing.csv"), stringsAsFactors = FALSE, 
    na.strings = c("", "NA", "#DIV/0!"))
```

# Examining variables

```r
table(sapply(pml, class))
```

```
## 
## character   integer   logical   numeric 
##         4        35         6       115
```

```r
# check strings
head(classSubset(pml, "character"))
```

```
##   user_name   cvtd_timestamp new_window classe
## 1  carlitos 05/12/2011 11:23         no      A
## 2  carlitos 05/12/2011 11:23         no      A
## 3  carlitos 05/12/2011 11:23         no      A
## 4  carlitos 05/12/2011 11:23         no      A
## 5  carlitos 05/12/2011 11:23         no      A
## 6  carlitos 05/12/2011 11:23         no      A
```

```r
# check logicals
summary(classSubset(pml, "logical"))
```

```
##  kurtosis_yaw_belt skewness_yaw_belt kurtosis_yaw_dumbbell
##  Mode:logical      Mode:logical      Mode:logical         
##  NA's:19622        NA's:19622        NA's:19622           
##  skewness_yaw_dumbbell kurtosis_yaw_forearm skewness_yaw_forearm
##  Mode:logical          Mode:logical         Mode:logical        
##  NA's:19622            NA's:19622           NA's:19622
```

```r
# will remove all variables with more than 90% NA's
na.count <- sapply(pml, function(x) sum(is.na(x)))
na.count[na.count/nrow(pml) > 0.9]
```

```
##       kurtosis_roll_belt      kurtosis_picth_belt        kurtosis_yaw_belt 
##                    19226                    19248                    19622 
##       skewness_roll_belt     skewness_roll_belt.1        skewness_yaw_belt 
##                    19225                    19248                    19622 
##            max_roll_belt           max_picth_belt             max_yaw_belt 
##                    19216                    19216                    19226 
##            min_roll_belt           min_pitch_belt             min_yaw_belt 
##                    19216                    19216                    19226 
##      amplitude_roll_belt     amplitude_pitch_belt       amplitude_yaw_belt 
##                    19216                    19216                    19226 
##     var_total_accel_belt            avg_roll_belt         stddev_roll_belt 
##                    19216                    19216                    19216 
##            var_roll_belt           avg_pitch_belt        stddev_pitch_belt 
##                    19216                    19216                    19216 
##           var_pitch_belt             avg_yaw_belt          stddev_yaw_belt 
##                    19216                    19216                    19216 
##             var_yaw_belt            var_accel_arm             avg_roll_arm 
##                    19216                    19216                    19216 
##          stddev_roll_arm             var_roll_arm            avg_pitch_arm 
##                    19216                    19216                    19216 
##         stddev_pitch_arm            var_pitch_arm              avg_yaw_arm 
##                    19216                    19216                    19216 
##           stddev_yaw_arm              var_yaw_arm        kurtosis_roll_arm 
##                    19216                    19216                    19294 
...
```

```r
summary(na.count)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0       0   19200   12000   19200   19600
```

```r
hist(na.count)
```

![plot of chunk examine-variables](figure/examine-variables1.png) 

```r
hist(na.count[na.count/nrow(pml) > 0.9])
```

![plot of chunk examine-variables](figure/examine-variables2.png) 

```r
table(na.count)
```

```
## na.count
##     0 19216 19217 19218 19220 19221 19225 19226 19227 19248 19293 19294 
##    60    67     1     1     1     4     1     4     2     2     1     1 
## 19296 19299 19300 19301 19622 
##     2     1     4     2     6
```

```r
table(na.count[na.count/nrow(pml) > 0.9])
```

```
## 
## 19216 19217 19218 19220 19221 19225 19226 19227 19248 19293 19294 19296 
##    67     1     1     1     4     1     4     2     2     1     1     2 
## 19299 19300 19301 19622 
##     1     4     2     6
```

```r
# it seems that num_window is an id
table(pml$user_name, pml$num_window)
```

```
##           
##             1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
##   adelmo    0  0  0 28 21 21 21 24 24 12  0  0  0  0  0  0  0  0  0  0  0
##   carlitos  0  0  0  0  0  0  0  0  0  0  3 21 28 24 24 29 17 19 21 28 23
##   charles   0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
##   eurico   20 21  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
##   jeremy    0  0  3  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
##   pedro     0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
##           
##            22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42
##   adelmo    0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
##   carlitos 20 32 22  1 28 15 34 35 27 30 20 17 22 30 29 28 18 25 28 25 24
##   charles   0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
##   eurico    0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
##   jeremy    0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
##   pedro     0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
##           
##            43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
##   adelmo    0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
##   carlitos 33 22 17  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
##   charles   0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
##   eurico    0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
##   jeremy    0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
##   pedro     0  0  0 17 28 31 18 35 20 15 32 18 22 24 38 13 30 26 20 29 17
...
```

```r
nrow(unique(pml[c("user_name", "num_window")]))
```

```
## [1] 858
```

```r
length(unique(pml$num_window))
```

```
## [1] 858
```

# Some transformations

```r
# transform some variables
pml <- within(pml, {
    classe <- factor(classe)
    new_window <- as.numeric(new_window == "yes")  # convert to logic (0 or 1)
})
pml.submit <- within(pml.submit, {
    # classe <- factor(classe, levels = levels(pml$classe))
    new_window <- as.numeric(new_window == "yes")  # convert to logic (0 or 1)
})
table(sapply(pml, class))
```

```
## 
## character    factor   integer   logical   numeric 
##         2         1        35         6       116
```

```r
# head(pml, 1)
```

this data would likely benefit of time series analysis since activities
have a logical connection (suggested variables: previous_activity, etc.)
Unfortunately, test set observations are isolated cases and thus have no
time series context (well, technically they have, because they have
timestamp, num_window, etc., but as they are isolated, they shouldn't)

user_name, otherwise, although is not informative as such, might
give us indirect information on age, height, weigth of our subjects.
user_name is a proxy for age, height, weigth ...

## Tidy dataset

```r
# categorize variables
id.vars <- c("X", "num_window")
rem.vars <- c("raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp")
na.vars <- names(which(sapply(pml, function(x) sum(is.na(x))/length(x)) > 0.9))
y.var <- "classe"
x.vars <- names(pml)[!names(pml) %in% c(id.vars, rem.vars, na.vars, y.var)]
d.vars <- c(x.vars, y.var)
```

# Split dataset
We'll divide the dataset in two: training and test set.
training will be used to train the models and tuning will be done with cross-validation (or, in the case of random forest, with out-of-bag estimation)
test will be used to estimate each model's performance.


```r
set.seed(40187)
inTrain <- createDataPartition(pml$classe, p = 0.7, list = FALSE)
pml.train <- pml[inTrain, d.vars]
pml.test <- pml[-inTrain, d.vars]
```



```r
print.cor(cor(classSubset(pml.train, c("numeric", "integer"))), min.coef = 0.6, 
    lower.t = TRUE)
```

```
##                   roll_belt pitch_belt yaw_belt total_accel_belt
## yaw_belt               0.82      -0.71        .                .
## total_accel_belt       0.98          .     0.76                .
## accel_belt_x              .      -0.97     0.71                .
## accel_belt_y           0.93          .     0.61             0.94
## accel_belt_z          -0.99          .    -0.78            -0.98
## magnet_belt_x             .      -0.92     0.77                .
## accel_arm_y           -0.79          .    -0.67            -0.76
## yaw_dumbbell              .       0.67        .                .
##                   gyros_belt_x accel_belt_x accel_belt_y accel_belt_z
## accel_belt_z                 .            .        -0.94            .
## magnet_belt_x                .         0.92            .            .
## accel_arm_y                  .            .        -0.73         0.78
## yaw_dumbbell                 .        -0.66            .            .
## magnet_dumbbell_x        -0.76            .            .            .
## magnet_dumbbell_y         0.81            .            .            .
##                   magnet_belt_x magnet_belt_y total_accel_arm gyros_arm_x
## magnet_belt_z                 .          0.85               .           .
## gyros_arm_y                   .             .               .       -0.92
## accel_arm_z                   .             .           -0.61           .
## yaw_dumbbell              -0.64             .               .           .
##                   accel_arm_x accel_arm_z magnet_arm_x magnet_arm_y
## magnet_arm_x             0.81           .            .            .
## magnet_arm_y            -0.70           .        -0.78            .
## magnet_arm_z            -0.66        0.79            .         0.81
##                   roll_dumbbell pitch_dumbbell yaw_dumbbell
## accel_dumbbell_x              .           0.79            .
## accel_dumbbell_y           0.72              .            .
## accel_dumbbell_z              .              .         0.85
##                   total_accel_dumbbell gyros_dumbbell_x accel_dumbbell_x
## gyros_dumbbell_z                     .            -0.62                .
## accel_dumbbell_x                 -0.66                .                .
## accel_dumbbell_y                  0.79                .                .
## accel_dumbbell_z                 -0.63                .              0.7
##                   accel_dumbbell_y magnet_dumbbell_x magnet_dumbbell_z
## accel_dumbbell_z             -0.67                 .                 .
## magnet_dumbbell_y                .             -0.77                 .
## accel_forearm_z                  .                 .              0.62
## magnet_forearm_z                 .                 .             -0.60
##                   gyros_forearm_y accel_forearm_x accel_forearm_y
## gyros_forearm_z              0.74               .               .
## magnet_forearm_x                .            0.68               .
## magnet_forearm_y                .               .            0.75
```


```r
if (draft) {
    default.trControl <- trainControl(method = "cv", number = 10)
} else {
    default.trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
}
```


```r
set.seed(43443)
mnom.trControl <- default.trControl
mnom.time <- system.time(mnom.basemodel <- train(classe ~ ., data = pml.train, 
    method = "multinom", trControl = mnom.trControl, tuneLength = 5))
```


```r
mnom.time
```

```
##    user  system elapsed 
##   57.05    0.04   57.10
```

```r
mnom.basemodel
```

```
## Penalized Multinomial Regression 
## 
## 1373 samples
##   54 predictor
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## 
## Summary of sample sizes: 1235, 1235, 1236, 1237, 1234, 1237, ... 
## 
## Resampling results across tuning parameters:
## 
##   decay  Accuracy  Kappa  Accuracy SD  Kappa SD
##   0e+00  0.6       0.6    0.05         0.06    
##   1e-04  0.6       0.6    0.05         0.06    
##   1e-03  0.6       0.6    0.05         0.06    
##   1e-02  0.6       0.6    0.05         0.06    
##   1e-01  0.6       0.6    0.05         0.06    
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was decay = 0.1.
```

```r
t(coef(mnom.basemodel$finalModel))
```

```
##                               B          C          D          E
## (Intercept)          -1.704e-03  9.217e-04  3.200e-03  8.416e-04
## user_namecarlitos    -3.549e-03  1.331e-02 -5.397e-03  1.569e-02
## user_namecharles     -3.887e-03  7.857e-04  5.471e-03  1.388e-03
## user_nameeurico      -3.823e-03 -3.334e-03  3.254e-03 -7.217e-03
## user_namejeremy       5.336e-03 -1.165e-02 -3.046e-05  4.636e-03
## user_namepedro       -3.234e-03  7.538e-05  1.473e-03  8.238e-04
## new_window            2.243e-02 -2.978e-03  2.087e-03 -8.417e-03
## roll_belt             1.281e-01  8.689e-02 -8.049e-02  1.420e-01
## pitch_belt           -2.611e-01 -9.409e-02 -9.274e-02  2.507e-01
## yaw_belt             -1.044e-01 -9.541e-02 -7.871e-02 -2.321e-02
## total_accel_belt     -1.866e-01 -4.382e-02  1.929e-01 -2.926e-03
## gyros_belt_x         -4.816e-03 -2.106e-03  4.246e-03  6.444e-03
## gyros_belt_y          1.118e-03  2.400e-03 -3.018e-03 -2.029e-03
## gyros_belt_z          7.375e-03 -1.868e-02 -1.746e-02  4.975e-02
## accel_belt_x          8.303e-02  3.029e-02  1.113e-02  4.774e-02
## accel_belt_y          2.368e-02 -2.272e-02 -4.958e-03 -1.774e-01
## accel_belt_z          3.043e-04 -4.392e-02 -7.916e-02 -1.562e-03
## magnet_belt_x        -6.666e-02 -1.134e-02 -4.318e-03 -4.092e-03
## magnet_belt_y        -1.278e-03 -1.186e-03 -3.801e-03 -1.662e-02
## magnet_belt_z         1.456e-02  3.344e-02  2.102e-02  8.270e-03
## roll_arm              6.499e-03  1.734e-03  1.482e-03  1.468e-03
## pitch_arm             1.667e-02  9.093e-04 -5.674e-03 -1.539e-03
## yaw_arm               2.406e-03  4.049e-03  5.889e-03 -3.341e-05
## total_accel_arm      -3.496e-02 -1.416e-02  7.345e-02  9.652e-03
## gyros_arm_x           8.297e-02 -6.731e-02  2.146e-03  2.360e-01
## gyros_arm_y          -5.177e-02  5.483e-02  3.193e-03 -8.427e-02
## gyros_arm_z          -2.278e-02  1.997e-02  9.442e-03  1.280e-02
## accel_arm_x          -1.615e-02 -8.373e-03  1.301e-03 -1.522e-02
## accel_arm_y          -1.285e-02 -1.005e-02 -2.732e-02 -1.288e-02
## accel_arm_z           1.990e-02  5.663e-03  1.851e-02  2.547e-02
## magnet_arm_x          5.907e-03  1.823e-03  1.361e-03  2.148e-03
## magnet_arm_y          1.555e-02  3.639e-03  4.806e-04 -4.137e-03
## magnet_arm_z         -1.810e-02 -2.502e-03 -1.273e-04 -8.883e-03
## roll_dumbbell         1.521e-03  6.873e-04  1.545e-02  2.836e-03
## pitch_dumbbell       -6.457e-03 -5.799e-03  7.355e-04 -6.756e-03
## yaw_dumbbell         -1.535e-02 -1.326e-02 -1.830e-02 -1.221e-02
## total_accel_dumbbell  1.575e-01 -9.143e-03  2.588e-02  2.497e-01
## gyros_dumbbell_x      1.346e-05  7.580e-03  1.592e-02 -1.166e-02
## gyros_dumbbell_y     -1.699e-02  1.509e-02  2.615e-02  2.416e-02
## gyros_dumbbell_z      1.805e-02 -1.532e-02  1.290e-02 -2.793e-03
## accel_dumbbell_x      1.482e-02  9.700e-03  6.832e-03  4.481e-02
## accel_dumbbell_y      6.168e-03  3.483e-03  1.923e-03  8.012e-03
## accel_dumbbell_z      1.376e-02  2.847e-03  2.680e-03  5.900e-03
## magnet_dumbbell_x    -2.376e-03 -7.520e-03 -5.358e-03 -1.259e-02
## magnet_dumbbell_y    -2.744e-03 -8.166e-03 -5.111e-03 -7.521e-03
## magnet_dumbbell_z     6.680e-03  3.838e-02  3.014e-02  2.504e-02
## roll_forearm          1.879e-03 -1.118e-03 -1.793e-03 -2.572e-04
## pitch_forearm         1.931e-02  8.283e-03  4.073e-02  4.677e-02
## yaw_forearm          -4.472e-04 -1.116e-03 -1.136e-03 -3.919e-04
## total_accel_forearm   7.580e-02  4.210e-03  2.090e-02  1.130e-01
## gyros_forearm_x       2.485e-02  1.070e-02 -2.170e-02 -5.882e-02
## gyros_forearm_y      -6.260e-02 -4.619e-02  2.075e-02  4.605e-02
## gyros_forearm_z       7.339e-03 -2.042e-02 -4.330e-03  1.586e-02
## accel_forearm_x       6.026e-03 -1.160e-03 -9.649e-03  1.150e-02
## accel_forearm_y      -7.156e-04  1.977e-03  2.016e-03 -4.806e-04
## accel_forearm_z       2.681e-03 -1.469e-02 -9.987e-03 -1.078e-02
## magnet_forearm_x     -1.889e-03 -2.054e-04  1.670e-03 -4.329e-03
## magnet_forearm_y     -8.562e-04  4.021e-05  8.863e-04 -8.830e-04
## magnet_forearm_z      2.861e-04  1.901e-03  6.297e-04  3.166e-03
```


```r
num.vars <- classVars(pml.train, c("numeric", "integer"))
cor.vars <- num.vars[findCorrelation(cor(classSubset(pml.train, c("numeric", 
    "integer"))), cutoff = 0.75)]
lowcor.vars <- names(pml.train)[!d.vars %in% cor.vars]
mnom.lowcortime <- system.time(mnom.lowcormodel <- train(classe ~ ., data = pml.train[, 
    lowcor.vars], method = "multinom", trControl = mnom.trControl, tuneLength = 5))
```


```r
mnom.lowcortime
```

```
##    user  system elapsed 
##   42.02    0.05   42.18
```

```r
mnom.lowcormodel
```

```
## Penalized Multinomial Regression 
## 
## 1373 samples
##   36 predictor
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (10 fold) 
## 
## Summary of sample sizes: 1236, 1235, 1235, 1236, 1236, 1235, ... 
## 
## Resampling results across tuning parameters:
## 
##   decay  Accuracy  Kappa  Accuracy SD  Kappa SD
##   0e+00  0.5       0.4    0.04         0.05    
##   1e-04  0.5       0.4    0.04         0.05    
##   1e-03  0.5       0.4    0.04         0.05    
##   1e-02  0.5       0.4    0.04         0.05    
##   1e-01  0.5       0.4    0.04         0.05    
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was decay = 0.01.
```

```r
mnom.lowcormodel$finalModel
```

```
## Call:
## multinom(formula = .outcome ~ ., data = dat, decay = param$decay)
## 
## Coefficients:
##   (Intercept) user_namecarlitos user_namecharles user_nameeurico
## B    -0.35780            0.3604           1.6436         -1.3346
## C    -0.36892           -0.3658           0.1635         -0.3008
## D     0.08313           -0.9837          -0.8099          0.5454
## E     0.72535            3.6858           2.1498         -0.6936
##   user_namejeremy user_namepedro new_window pitch_belt total_accel_belt
## B         -0.3015        -0.6377    0.90646    0.01984          0.17632
## C         -0.6382         0.3832   -0.01668    0.02995          0.10648
## D          0.3999         1.0188   -0.08722    0.02036          0.02684
## E         -0.9251        -2.6875    0.07525    0.06863          0.39686
##   gyros_belt_x gyros_belt_y gyros_belt_z magnet_belt_y  roll_arm pitch_arm
## B    -0.085595       0.3999       0.6978     -0.006745  0.007230  0.012027
## C    -0.005689       0.2693      -0.3928     -0.007470  0.002581 -0.001572
## D    -0.416638      -0.3633      -1.0131     -0.015557 -0.001041 -0.001591
## E     1.694552      -0.1198       1.3306     -0.021553  0.003085 -0.002889
##    yaw_arm total_accel_arm gyros_arm_x gyros_arm_z magnet_arm_x
## B 0.007861       -0.039247     0.02431    -0.38804   -8.396e-04
## C 0.003130       -0.002747    -0.15590     0.55203    7.796e-05
## D 0.004087        0.078757    -0.13256     0.25521    2.678e-03
## E 0.001064       -0.014523     0.04923    -0.01063    8.279e-05
##   magnet_arm_z roll_dumbbell pitch_dumbbell yaw_dumbbell
## B   -0.0041979      0.004164      0.0068498    -0.004524
## C    0.0011593     -0.004535     -0.0006552    -0.016279
## D    0.0024217      0.017844     -0.0072530    -0.011987
## E   -0.0008554      0.002289     -0.0060472    -0.011003
##   total_accel_dumbbell gyros_dumbbell_x gyros_dumbbell_y gyros_dumbbell_z
## B              0.09263          -0.5161           0.3736           0.6684
## C             -0.04879          -0.4725           0.5279           0.2761
## D             -0.14990           0.3285           0.9302           0.9097
## E              0.12706          -0.7256           0.4589           0.4445
##   magnet_dumbbell_z roll_forearm pitch_forearm yaw_forearm
## B           0.01063     0.003212       0.03696   -0.003156
## C           0.02131     0.001678       0.02880   -0.003773
## D           0.02158     0.002711       0.04880   -0.005539
## E           0.02036     0.007210       0.05155   -0.001514
##   total_accel_forearm gyros_forearm_x gyros_forearm_y gyros_forearm_z
## B             0.07329         -0.4054       -0.079768        -0.02695
## C             0.01628         -0.3363       -0.001626        -0.20109
## D             0.06262         -0.5115       -0.075290        -0.15950
## E             0.12035         -0.5984       -0.054835         0.15747
##   accel_forearm_x accel_forearm_z magnet_forearm_x magnet_forearm_y
## B        0.002645       -0.002199       -0.0012913        -0.000876
## C       -0.005305       -0.014583        0.0004504         0.001188
## D       -0.014468       -0.014793        0.0030090         0.002043
## E        0.007371       -0.007617       -0.0029237        -0.001049
##   magnet_forearm_z
## B       -0.0002339
## C        0.0018327
## D        0.0021965
## E       -0.0006916
## 
## Residual Deviance: 2896 
## AIC: 3224
```

## random forest
In random forests, there is no need for cross-validation or a separate test
set to get an unbiased estimate of the test set error. It is estimated
internally, during the run, as follows:


```r
set.seed(43443)
rf.trControl <- trainControl(method = "oob")
rf.time <- system.time(rf.basemodel <- train(classe ~ ., data = pml.train, method = "rf", 
    trControl = rf.trControl, tuneLength = 5))
```


```r
rf.time
```

```
##    user  system elapsed 
##   37.42    0.13   37.66
```

```r
rf.basemodel
```

```
## Random Forest 
## 
## 1373 samples
##   54 predictor
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Out of Bag Resampling 
## 
## Summary of sample sizes:  
## 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy  Kappa
##    2    0.9       0.9  
##   16    0.9       0.9  
##   30    0.9       0.9  
##   44    0.9       0.9  
##   58    0.9       0.9  
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 16.
```

```r
rf.basemodel$finalModel
```

```
## 
## Call:
##  randomForest(x = x, y = y, mtry = param$mtry) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 16
## 
##         OOB estimate of  error rate: 6.7%
## Confusion matrix:
##     A   B   C   D   E class.error
## A 398   2   1   1   0     0.00995
## B  20 233  14   0   2     0.13383
## C   0  13 234   2   1     0.06400
## D   2   2  15 184   0     0.09360
## E   1   6   5   5 232     0.06827
```

```r
# pred.basemodel <- predict(rf.basemodel,newdata = pml.validation)
# confusionMatrix(pml.validation$classe, pred.basemodel)
varImp(rf.basemodel)
```

```
## rf variable importance
## 
##   only 20 most important variables shown (out of 58)
## 
##                      Overall
## roll_belt              100.0
## pitch_forearm           76.2
## magnet_dumbbell_z       61.1
## yaw_belt                56.1
## magnet_dumbbell_y       54.6
## pitch_belt              41.7
## roll_forearm            39.4
## roll_dumbbell           34.9
## magnet_dumbbell_x       33.1
## accel_dumbbell_y        31.8
## accel_belt_z            29.9
## gyros_belt_z            29.2
## magnet_belt_z           28.6
## accel_forearm_x         27.0
## magnet_belt_y           23.2
## gyros_dumbbell_y        22.8
## total_accel_dumbbell    21.9
## accel_dumbbell_z        21.1
## magnet_forearm_z        19.1
## yaw_dumbbell            18.4
```

```r
# pml_write_files(as.character(predict(rf.basemodel, newdata=pml.submit)),
# 'rf-mtry_16')
```


```r
set.seed(43443)
svm.trControl <- default.trControl
svm.time <- system.time(svm.basemodel <- train(classe ~ ., data = pml.train, 
    method = "svmRadialCost", preProc = c("center", "scale"), trControl = svm.trControl, 
    tuneLength = 9))
```


```r
svm.time
```

```
##    user  system elapsed 
##   89.25    0.40   89.88
```

```r
svm.basemodel
```

```
## Support Vector Machines with Radial Basis Function Kernel 
## 
## 1373 samples
##   54 predictor
##    5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## Pre-processing: centered, scaled 
## Resampling: Cross-Validated (10 fold) 
## 
## Summary of sample sizes: 1235, 1235, 1236, 1237, 1234, 1237, ... 
## 
## Resampling results across tuning parameters:
## 
##   C     Accuracy  Kappa  Accuracy SD  Kappa SD
##    0.2  0.6       0.4    0.03         0.04    
##    0.5  0.7       0.6    0.04         0.05    
##    1.0  0.8       0.7    0.04         0.05    
##    2.0  0.8       0.7    0.03         0.04    
##    4.0  0.8       0.8    0.03         0.04    
##    8.0  0.9       0.8    0.02         0.03    
##   16.0  0.9       0.9    0.02         0.03    
##   32.0  0.9       0.9    0.02         0.03    
##   64.0  0.9       0.9    0.02         0.03    
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was C = 64.
```

```r
svm.basemodel$finalModel
```

```
## Support Vector Machine object of class "ksvm" 
## 
## SV type: C-svc  (classification) 
##  parameter : cost C = 64 
## 
## Gaussian Radial Basis kernel function. 
##  Hyperparameter : sigma =  0.010811317246045 
## 
## Number of Support Vectors : 926 
## 
## Objective Function Value : -1724 -915.2 -697.5 -360.8 -1192 -385.2 -559.1 -1722 -797.8 -623.7 
## Training error : 0.005827
```


```r
sel.models <- list(mnom = mnom.basemodel, mnom.lc = mnom.lowcormodel, rf = rf.basemodel, 
    svm = svm.basemodel)
test.pred <- predict(sel.models, newdata = pml.test)
t(sapply(test.pred, pml.test$classe, FUN = function(pred, obs) {
    cm <- confusionMatrix(pred, obs)
    round(cm$overall[1:4] * 100, 1)
}))
```

```
##         Accuracy Kappa AccuracyLower AccuracyUpper
## mnom        64.3  54.6          60.3          68.2
## mnom.lc     55.3  43.1          51.2          59.3
## rf          93.5  91.8          91.2          95.4
## svm         88.3  85.1          85.4          90.8
```

