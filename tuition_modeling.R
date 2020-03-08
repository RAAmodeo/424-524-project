# LEFT OFF at line 285...end of KNN predict -

# Packages & libraries ----
library(pacman)
p_load(dplyr, readr, janitor, ggplot2, caret)
p_load(glmnet, naniar, tidymodels, class)
p_load(skimr, rcompanion)


# Data - Load & Format ----
tui = read.csv("tuition_stnd_mat.csv")
reference = read.csv("tuition_reference_table.csv")



# Create SPLITS for training vs testing
set.seed(0726)
split = initial_split(tui, prop = 4/5)
tr = as.matrix(training(split))
te = as.matrix(testing(split))

# Save x and y separate for tr and te
x.tr = tr[,1:22]
y.tr = tr[,23]

x.te = te[,1:22]
y.te = te[,23]

# Elasticnet - START HERE ----------------------

## Elasticnet 1 is using caret, by RA
## Elasticnet 2, by NK

## Elasticnet 1 - Train with cross validation -------

## Our range of λ
lambdas_net = 10^seq(from = 5, to = -2, length = 100)
## Our range of α
alphas = seq(from = 0, to = 1, by = 0.1)

# Training
net_cv = train(
  # The formula
  y = y.tr, # wants factor outcome for classification... but trying with numeric for this
  x = x.tr[,3:22], # omit X, id, and outcome columns
  # The dataset
  data = tr, # matrix
  # The 'glmnet' package does ridge and lasso
  method = "glmnet",
  # 5-fold cross validation
  trControl = trainControl("cv", number = 5),
  # The parameters of 'glmnet'
  tuneGrid = expand.grid(alpha = alphas, lambda = lambdas_net)
)
plot(net_cv)

## Elasticnet 1 - Fit final model --------------

# (the lambda.min model)
final_net =  glmnet(
  y = y.tr, # wants factor outcome for classification... but trying with numeric for this
  x = x.tr[,3:22], # omit X, id, and outcome columns
  standardize = T,
  alpha = net_cv[["bestTune"]][["alpha"]],
  lambda = net_cv[["bestTune"]][["lambda"]]
)



## Elasticnet 1 - Predictions -----------------

# Make elasticnet predictions with final fitted model & SAVE as vectors

# still using training data
pred_net_tr = predict(
  final_net,
  type = "response",
  s1 = net_cv[["bestTune"]][["lambda"]],
  s2 = net_cv[["bestTune"]][["alpha"]],
  # Our data
  newx = x.tr[,3:22] # matrix, omit X, id, & outcome columns
)

pred_net_tr2 <- predict(final_net, newx = tr[,3:22])

pred_net_te2 <- predict(final_net, newx = te[,3:22])

# onto test data
test_net = predict(
  final_net,
  type = "response",
  s1 = net_cv[["bestTune"]][["lambda"]],
  s2 = net_cv[["bestTune"]][["alpha"]],
  # Our data
  newx = te[,3:22], # matrix, omit X, id & outcome columns
)



### Elasticnet 2 - Train with cross validation ----

##first model: logistic

model_glmnet = train(
   ft_ret_rate ~ .,
  data = tr[,3:23],
  method = "glmnet",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = expand.grid(
    alpha = seq(0, 1, 0.1),
    lambda = c(0.1, 0.01, 0.001, 0.0001, 0.00001))
)

plot(model_glmnet)






### Elasticnet 2 - Predictions ----
pred_net2_tr <- predict(model_glmnet, newdata = tr[,3:23])
pred_net2_te <- predict(model_glmnet, newdata = te[,3:23])

# Ridge - START HERE ----

## Ridge 1 by RA
## Ridge 2 by NK


## Define range of Lambda for Ridge regression (decreasing range for glmnet)
lambdas_ridge = 10^seq(from = 5, to = -2, length = 100)


## Ridge 1 - Train w cross validation -----------------
ridge_cv = cv.glmnet(
  x = x.tr,
  y = y.tr,
  alpha = 0, # 0 for ridge (1 for lasso)
  standardize = T,
  lambda = lambdas_ridge,
  # New: How we make decisions and number of folds
  type.measure = "mse",
  nfolds = 5
)

## Ridge 1 - Fit final model -----------------------
final_ridge =  glmnet(
  x = x.tr,
  y = y.tr,
  standardize = T,
  alpha = 0,  # 0 for ridge (1 for lasso)
  lambda = ridge_cv$lambda.min
)

## Ridge 1 - Make ridge predictions & save as vectors ------------

# still using training data
pred_ridge_te = predict(
  final_ridge,
  type = "response",
  # Our chosen lambda
  s = ridge_cv$lambda.min,
  # Our data
  newx = x.te
)

# onto test data
test_ridge = predict(
  final_ridge,
  type = "response",
  # Our chosen lambda
  s = ridge_cv$lambda.min,
  # Our data
  newx = x.te
)

# Ridge 2 - Train w cross validation -------

model_ridge = train(
  ft_ret_rate ~ .,
  data = tr[,3:23],
  method = "glmnet",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = expand.grid(
    alpha = 0,
    lambda = lambdas_ridge
  ))
plot(model_ridge)


## Ridge 2 - Predictions ------
pred_ridge2_tr <- predict(model_ridge, newdata = tr[,3:23])
pred_ridge2_te <- predict(model_ridge, newdata = te[,3:23])

# Lasso - START HERE-----------
## Define range of Lambda for Lasso regression (decreasing range for glmnet)

lambdas_lasso = 10^seq(from = 5, to = -2, length = 100)

## Lasso 1 - Train w cross validation -----------------
lasso_cv = cv.glmnet(
  x = x.tr[,3:22],
  y = y.tr,
  alpha = 1, # 1 for lasso (0 for ridge)
  standardize = T,
  lambda = lambdas_lasso,
  # New: How we make decisions and number of folds
  type.measure = "mse",
  nfolds = 5
)

## Lasso 1 - Fit final model -----------------------
final_lasso =  glmnet(
  x = x.tr[,3:22],
  y = y.tr,
  standardize = T,
  alpha = 0,  # 1 for lasso (0 for ridge)
  lambda = lasso_cv$lambda.min
)

## Lasso 1 - Predict & save as vectors ------------

# still using training data
pred_lasso = predict(
  final_lasso,
  type = "response",
  # Our chosen lambda
  s = lasso_cv$lambda.min,
  # Our data
  newx = x.tr[,3:22]
)


# onto test data
test_lasso = predict(
  final_lasso,
  type = "response",
  # Our chosen lambda
  s = lasso_cv$lambda.min,
  # Our data
  newx = x.te[,3:22]
)

# Lasso 2 - Train -----

model_lasso = train(
  ft_ret_rate ~ .,
  data = tr[,3:23],
  method = "glmnet",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = expand.grid(
    alpha = 1,
    lambda = lambdas_lasso
  ))
plot(model_lasso)

# Lasso 2 - Predict -----

pred_lasso2_tr <- predict(model_lasso, newdata = tr[,3:22])
pred_lasso2_te <- predict(model_lasso, newdata = te[,3:22])


# KNN - Train ----


model_knn <- train(
  ft_ret_rate ~., 
  data = tr[,3:23], 
  method = "knn",
  trControl = trainControl("cv", number = 5),
  preProcess = c("center","scale"),
  tuneLength = 5
)
model_knn
plot(model_knn)



# KNN - Predict ----

pred_knn_tr <- predict(model_knn, newdata = tr[,3:22])
pred_knn_te <- predict(model_knn, newdata = te[,3:22])


# Model Assessment - START HERE ------

# collect data to evaluate training and test error rates


# training error evaluation compilation
# in yardstick and wants factor outcome: accuracy_vec(y.te, pred_knn_te)

net.te <- glmnet::assess.glmnet(final_net,
                      newx = x.te[,3:22], newy = y.te)


lasso.te <- glmnet::assess.glmnet(final_lasso,
                                newx = x.te[,3:22], newy = y.te)


ridge.te <- glmnet::assess.glmnet(final_ridge,
                                  newx = x.te[,3:22], newy = y.te)





id.tr = tr[,1:3]
truth.tr = tr[,23]

tr_eval = cbind.data.frame(id.tr, truth.tr, pred_net_tr, pred_net2_tr,
                           pred_ridge2_tr,
                           pred_lasso, pred_lasso2_tr)

id.te = te[,1:3]
truth.te = te[,23]


te_eval = cbind.data.frame(id.te, truth.te, pred_net_te2, pred_net2_te,
                           pred_ridge, pred_ridge2_te,
                           test_lasso, pred_lasso2_te)

L1.net = MAE(truth.te, pred_net_te2)
L1.net2 = MAE(truth.te, pred_net2_te)
L1.rid = MAE(truth.te, pred_ridge)
L1.ri2 = MAE(truth.te, pred_ridge2_te)
L1.las = MAE(truth.te, test_lasso)
L1.las2 = MAE(truth.te, pred_lasso2_te)
L1.knn = MAE(truth.te, pred_knn_te)

## Test error metrics -----
a.net = postResample(pred = pred_net_te2, obs = truth.te)
a.net2 = postResample(pred = pred_net2_te, obs = truth.te)
a.rid = postResample(pred = pred_ridge, obs = truth.te)
a.rid2 = postResample(pred = pred_ridge2_te, obs = truth.te)
a.las = postResample(pred = test_lasso, obs = truth.te)
a.las2 = postResample(pred = pred_lasso2_te, obs = truth.te)
a.knn = postResample(pred = pred_knn_te, obs = truth.te)

tr.net = postResample(pred = pred_net_tr2, obs = truth.tr)
tr.net2 = postResample(pred = pred_net2_tr, obs = truth.tr)
tr.rid2 = postResample(pred = pred_ridge2_te, obs = truth.tr)
tr.las = postResample(pred = pred_lasso, obs = truth.tr)
tr.las2 = postResample(pred = pred_lasso2_tr, obs = truth.tr)
tr.knn = postResample(pred = pred_knn_tr, obs = truth.tr)

p_load(magrittr)
my.metric = rbind.data.frame(a.net, a.net2, a.rid,
                             a.rid2, a.las, a.las2,
                             a.knn, tr.net, tr.net2,
                             tr.rid2, tr.las, tr.las2,
                             tr.knn)

errors = c("RMSE", "R2", "MAE")
colnames(my.metric) <- errors

Model = c("elnet", "elnet2", "ridge", "ridge2", "lasso", "lasso2", "knn",
          "net.tr", "net2.tr", "rid2.tr", "las.tr", "las2.tr", "knn.tr")

my.metrics = cbind(Model, my.metric)

write.csv(my.metrics, file = "team_results_table.csv")



## plot ----

p_load(ggrepel)

p.1 <- ggplot(my.metrics,
              aes(x = MAE, y = RMSE)) +
              geom_point()+
              xlab("MAE")+
              ylab("RMSE")+
              ggtitle("Retention Prediction Modeling Evaluation")+ 
              geom_label_repel(aes(label = Model),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
                   theme_classic()
p.1

#geom_text(aes(label = Model)) +

p.2 <- ggplot(my.metrics,
              aes(x = Model, y = RMSE)) +
  geom_point()+
  xlab("Model")+
  ylab("RMSE")+
  ggtitle("Retention Prediction Modeling RMSE Evaluation")

p.2

### end ish -------------------



L1_tr.mae_net = mean(abs(tr_eval$truth_tr - tr_eval[,3]))

L1_tr.mae_ridge = mean(abs(tr_eval$truth_tr - tr_eval[,4])) 

L1_tr.mae_lasso = mean(abs(tr_eval$truth_tr - tr_eval[,5]))


L2_tr.mse_net = mean((abs(tr_eval$truth_tr - tr_eval[,3]))^2)

L2_tr.mse_ridge = mean((abs(tr_eval$truth_tr - tr_eval[,4]))^2) 

L2_tr.mse_lasso = mean((abs(tr_eval$truth_tr - tr_eval[,5]))^2)


# test error evaluation data compilation
id2 = test_mat[,1]
truth_te = test_mat[,21]

te_eval = cbind.data.frame(id2, truth_te, test_net, test_ridge, test_lasso)


L1_te.mae_net = mean(abs(te_eval$truth_te - te_eval[,3]))

L1_te.mae_ridge = mean(abs(te_eval$truth_te - te_eval[,4])) 

L1_te.mae_lasso = mean(abs(te_eval$truth_te - te_eval[,5]))


L2_te.mse_net = mean((abs(te_eval$truth_te - te_eval[,3]))^2)

L2_te.mse_ridge = mean((abs(te_eval$truth_te - te_eval[,4]))^2) 

L2_te.mse_lasso = mean((abs(te_eval$truth_te - te_eval[,5]))^2)


## Model Assessment - Error Table -----

our.model = c("Elasticnet", "Ridge", "Lasso")

L1.tr = c(L1_tr.mae_net, L1_tr.mae_ridge, L1_tr.mae_lasso)

L1.te = c(L1_te.mae_net, L1_te.mae_ridge, L1_te.mae_lasso)

L2.tr = c(L2_tr.mse_net, L2_tr.mse_ridge, L2_tr.mse_lasso)

L2.te = c(L2_te.mse_net, L2_te.mse_ridge, L2_te.mse_lasso)

assess = cbind.data.frame(our.model, L1.tr, L2.tr, L1.te, L2.te)

## Plots ------
library(ggplot2)

# Test MAE, L1 Plot
ggplot(data = assess, aes(x = our.model, y = L1.te)) +
  geom_point(size = 3) +
  xlab("Model") +
  ylab("Test MAE") +
  theme_minimal(base_size = 18)

# Test MSE, L2 Plot
ggplot(data = assess, aes(x = our.model, y = L2.te)) +
  geom_point(size = 3) +
  xlab("Model") +
  ylab("Test MSE") +
  theme_minimal(base_size = 18)

# Test errors plot
Plot_compare <- ggplot(data = assess, 
                       aes(x = L1.te, y = L2.te, label = our.model)) +
  geom_text()+
  geom_point(size = 4) +
  xlab("MAE") +
  xlim(6.495, 6.5) +
  ylab("MSE") +
  ylim(83.98, 84.15) +
  theme_minimal(base_size = 18)
Plot_compare