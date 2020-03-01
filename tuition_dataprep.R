# load packages & data ----
library(dplyr, readr, ggplot2, naniar)
library(caret, tidymodels, readr, tidyverse)

set.seed(0726)
# importing by-year tuition data
t11 = read.csv("C:/My_R/524.proj/tui_11.csv")
t12 = read.csv("C:/My_R/524.proj/tui_12.csv")
t13 = read.csv("C:/My_R/524.proj/tui_13.csv")
t14 = read.csv("C:/My_R/524.proj/tui_14.csv")
t15 = read.csv("C:/My_R/524.proj/tui_15.csv")
t16 = read.csv("C:/My_R/524.proj/tui_16.csv")


# combining to make overall tuition df
tui_clean <- as.data.frame(rbind(t11, t12, t13, t14, t15, t16))

write.csv(tui_clean, file = "C:/My_R/524.proj/tuition_clean.csv") 


# PREP by resolving NAs & initial_split() ------------------------
#replacing missing values with the median of each variable usinf preProcess function

gg_miss_var(tui_clean) # visual of the missing data 

tui_clean <- preProcess(
  x = tui_clean,
  method = c("medianImpute")
) %>% predict(tui_clean)

summary(tui_clean)
sum(is.na(tui_clean))

# Create SPLITS for training vs testing

split = initial_split(tui_clean, prop = 4/5)
train = training(split)
test = testing(split)


#transform dataframe into matrix to use lasso/ridge and remove id and name of institution 
train_mat <- as.matrix(train[,3:23])
test_mat <- as.matrix(test[,3:23])



# Elasticnet - START HERE ----------------------
## using caret


## Our range of λ
lambdas_net = 10^seq(from = 5, to = -2, length = 100)

## Our range of α
alphas = seq(from = 0, to = 1, by = 0.1)

## Elasticnet - Train with cross validation -------
net_cv = train(
  # The formula
  y = train_mat[,21], # wants factor outcome for classification... but trying with numeric for this
  x = train_mat[,1:20], # matrix, omit id & is_train columns
  # The dataset
  data = train_mat[,1:20], # matrix, omit id & is_train columns
  # The 'glmnet' package does ridge and lasso
  method = "glmnet",
  # 5-fold cross validation
  trControl = trainControl("cv", number = 5),
  # The parameters of 'glmnet'
  tuneGrid = expand.grid(alpha = alphas, lambda = lambdas_net)
)



## Elasticnet - Fit final model --------------

# (the lambda.min model)
final_net =  glmnet(
  x = train_mat[,1:20], # matrix, omit id & is_train columns
  y = train_mat[,21], # wants factor outcome for classification... but trying with numeric for this
  standardize = T,
  alpha = net_cv[["bestTune"]][["alpha"]],
  lambda = net_cv[["bestTune"]][["lambda"]]
)




## Elasticnet - Predictions -----------------

# Make elasticnet predictions with final fitted model & SAVE as vectors

# still using training data
pred_net_tr = predict(
  final_net,
  type = "response",
  # Our chosen lambda
  # s is bc they want to use lambda name for something else (for under hood glmnet)
  s1 = net_cv[["bestTune"]][["lambda"]],
  s2 = net_cv[["bestTune"]][["alpha"]], ### lol have no idea what I'm doing w s1 & s2
  # Our data
  newx = train_mat[,1:20], # matrix, omit id & is_train columns
)

# onto test data
test_net = predict(
  final_net,
  type = "response",
  # Our chosen lambda
  # s is bc they want to use lambda name for something else (for under hood glmnet)
  s1 = net_cv[["bestTune"]][["lambda"]],
  s2 = net_cv[["bestTune"]][["alpha"]], ### lol have no idea what I'm doing w s1 & s2
  # Our data
  newx = test_mat[,1:20], # matrix, omit id & is_train columns
)


# Ridge - START HERE ----
## Ridge - Define range of Lambda for Ridge regression
## (glmnet wants decreasing range)
lambdas_ridge = 10^seq(from = 5, to = -2, length = 100)


## Ridge - Train w cross validation -----------------
ridge_cv = cv.glmnet(
  x = train_mat[,1:20],
  y = train_mat[,21],
  alpha = 0, # 0 for ridge (1 for lasso)
  standardize = T,
  lambda = lambdas_ridge,
  # New: How we make decisions and number of folds
  type.measure = "mse",
  nfolds = 5
)

## Ridge - Fit final model -----------------------
final_ridge =  glmnet(
  x = train_mat[,1:20],
  y = train_mat[,21],
  standardize = T,
  alpha = 0,  # 0 for ridge (1 for lasso)
  lambda = ridge_cv$lambda.min
)

## Ridge - Make ridge predictions & save as vectors ------------

# still using training data
pred_ridge = predict(
  final_ridge,
  type = "response",
  # Our chosen lambda
  s = ridge_cv$lambda.min,
  # Our data
  newx = train_mat[,1:20]
)

# onto test data
test_ridge = predict(
  final_ridge,
  type = "response",
  # Our chosen lambda
  s = ridge_cv$lambda.min,
  # Our data
  newx = test_mat[,1:20]
)

# Lasso - START HERE-----------
## Define our range of lambdas for lasso regression
## (glmnet wants decreasing range)
lambdas_lasso = 10^seq(from = 5, to = -2, length = 100)


## Lasso - Train w cross validation -----------------
lasso_cv = cv.glmnet(
  x = train_mat[,1:20],
  y = train_mat[,21],
  alpha = 1, # 1 for lasso (0 for ridge)
  standardize = T,
  lambda = lambdas_lasso,
  # New: How we make decisions and number of folds
  type.measure = "mse",
  nfolds = 5
)

## Lasso - Fit final model -----------------------
final_lasso =  glmnet(
  x = train_mat[,1:20],
  y = train_mat[,21],
  standardize = T,
  alpha = 0,  # 1 for lasso (0 for ridge)
  lambda = lasso_cv$lambda.min
)

## Lasso - make predictions & save as vectors ------------

# still using training data
pred_lasso = predict(
  final_lasso,
  type = "response",
  # Our chosen lambda
  s = lasso_cv$lambda.min,
  # Our data
  newx = train_mat[,1:20]
)


# onto test data
test_lasso = predict(
  final_lasso,
  type = "response",
  # Our chosen lambda
  s = lasso_cv$lambda.min,
  # Our data
  newx = test_mat[,1:20]
)

# Model Assessment - START HERE ------

# collect data to evaluate training and test error rates

# training error evaluation compilation
id = train_mat[,1]
truth_tr = train_mat[,21]

tr_eval = cbind.data.frame(id, truth_tr, pred_net_tr, pred_ridge, pred_lasso)

tr_eval$e_net_tr = tr_eval$truth_tr - tr_eval[,3]

tr_eval$e_ridge_tr = tr_eval$truth_tr - tr_eval[,4]

tr_eval$e_lasso_tr = tr_eval$truth_tr - tr_eval[,5]



# test error evaluation data compilation
id2 = test_mat[,1]
truth_te = test_mat[,21]

te_eval = cbind.data.frame(id2, truth_te, test_net, test_ridge, test_lasso)

te_eval$e_te_net = te_eval$truth_te - te_eval[,3]

te_eval$e_te_ridge = te_eval$truth_te - te_eval[,4]

te_eval$e_te_lasso = te_eval$truth_te - te_eval[,5]

## Model Assessment - Error Table -----

