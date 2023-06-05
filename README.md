``{r setup, include=TRUE}



library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(corrplot)
library(rattle)
library(randomForest)
library(RColorBrewer)

set.seed(222)

# Download the training and testing datasets
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv","training.csv",method = "curl")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv","testing.csv",method = "curl")

url_train <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url_quiz  <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

data_train <-read.csv(url(url_train), strip.white = TRUE, na.strings = c("NA",""))
data_quiz  <-read.csv(url(url_quiz),  strip.white = TRUE, na.strings = c("NA",""))

dim(data_train)

dim(data_quiz)

##Create two partitions (75% and 25%) within the original training dataset.

in_train  <-createDataPartition(data_train$classe, p=0.75, list=FALSE)
train_set <-data_train[ in_train, ]
test_set  <-data_train[-in_train, ]

dim(train_set)
dim(test_set)

## The two datasets (train_setandtest_set) have a large number ofNAvalues as well as near-zero-variance (NZV) variables. Both will be removed together with their ID variables

nzv_var <-nearZeroVar(train_set)
train_set <-train_set[ , -nzv_var]
test_set  <-test_set [ , -nzv_var]

dim(train_set)

dim(test_set)

## Remove variables that are mostly NA. A threshlod of 95 % is selected.

na_var <-sapply(train_set, function(x) mean(is.na(x))) > 0.95
train_set <-train_set[ , na_var == FALSE]
test_set  <-test_set [ , na_var == FALSE]

dim(train_set)

dim(test_set)

## Since columns 1 to 5 are identification variables only, they will be removed as well.

train_set <-train_set[ , -(1:5)]
test_set  <-test_set [ , -(1:5)]
dim(train_set)
dim(test_set)

## The number of variables for the analysis has been reduced from the original 160 down to 54

# Correlation Analysis

## Correlation analysis between the variables before the modeling work itself is done. The “FPC” is used as the first principal component order.

corr_matrix <-cor(train_set[ , -54])
corrplot(corr_matrix, order = "FPC", method = "circle", type = "lower",
         tl.cex = 0.6, tl.col = rgb(0, 0, 0))

## If two variables are highly correlated their colors are either dark blue (for a positive correlation) or dark red (for a negative correlations). Because there are only few strong correlations among the input variables, the Principal Components Analysis (PCA) will not be performed in this analysis. Instead, a few different prediction models will be built to have a better accuracy.

# Prediction Models

## Decision Tree Model

set.seed(2222)
fit_decision_tree <-rpart(classe ~ ., data = train_set, method="class")
fancyRpartPlot(fit_decision_tree)

##Predictions of the decision tree model ontest_set

predict_decision_tree <-predict(fit_decision_tree, newdata = test_set, type="class")
conf_matrix_decision_tree <-confusionMatrix(predict_decision_tree, factor(test_set$classe))
conf_matrix_decision_tree

## The predictive accuracy of the decision tree model is relatively low at75.2 %.Plot the predictive accuracy of the decision tree model.

