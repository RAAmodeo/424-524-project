# Packages & libraries ----
library(pacman)
p_load(dplyr, readr, janitor, ggplot2, caret)
p_load(glmnet, naniar, tidymodels, class)
p_load(skimr, rcompanion, magrittr, RANN)


# Data - Load & Format ----
tui = read.csv("tuition_stnd_mat2.csv")

drops = c("pct.incr_pell.represent.1yr", "pct.incr.pell.represent.2yr",
          "pct.incr.is.on.1yr", "pct.incr.is.on.2yr", "pct.incr.os.on.1yr", "pct.incr.os.on.2yr",
          "pct.incr.ret.1yr", "pct.incr.ret.2yr")
tui  <- tui[, !(names(tui) %in% drops)]

colSums(is.na(tui))
#tui %<>% na.omit()

# after confirming no NA are in outcome variable:
# using knnImpute to resolve remaining NA
tui = preProcess(
  x = tui[,4:38],
  method = c("knnImpute")
)                            %>% predict(tui)

reference = read.csv("tuition_referenc2_table.csv")

colSums(is.na(reference))
reference = cbind.data.frame(reference, tui$ft_ret_rate)



# Create SPLITS for training vs testing
set.seed(0726)
split = initial_split(tui, prop = 4/5)
tr = as.matrix(training(split))
te = as.matrix(testing(split))

# Save x and y separate for tr and te
x.tr = tr[,2:38]
y.tr = tr[,39]

x.te = te[,2:38]
y.te = te[,39]

# Elasticnet - START HERE ----------------------
## Our range of λ
lambdas_net = 10^seq(from = 5, to = -2, length = 100)
## Our range of α
alphas = seq(from = 0, to = 1, by = 0.1)

### Elasticnet 2 - Train with cross validation ----

##first model: logistic

model_glmnet = train(
  ft_ret_rate ~ .,
  data = tr[,2:39],
  method = "glmnet",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = expand.grid(
    alpha = seq(from = 0, to = 1, by = 0.1),
    lambda = 10^seq(from = 5, to = -2, length = 100)))


plot(model_glmnet)



### Elasticnet ch - Predictions ----
net.ch_tr <- predict(model_glmnet, newdata = tr[,2:38])
net.ch_te <- predict(model_glmnet, newdata = te[,2:38])

# Ridge - START HERE ----

# Ridge ch - Train w cross validation -------

model_ridge = train(
  ft_ret_rate ~ .,
  data = tr[,2:39],
  method = "glmnet",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = expand.grid(
    alpha = 0,
    lambda = 10^seq(from = 5, to = -2, length = 100)
  ))
plot(model_ridge)


## Ridge 2 - Predictions ------
ridge.ch_tr <- predict(model_ridge, newdata = tr[,2:38])
ridge.ch_te <- predict(model_ridge, newdata = te[,2:38])

# Lasso - START HERE-----------

# Lasso 2 - Train -----

model_lasso = train(
  ft_ret_rate ~ .,
  data = tr[,2:39],
  method = "glmnet",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = expand.grid(
    alpha = 1,
    lambda = 10^seq(from = 5, to = -2, length = 100)
  ))
plot(model_lasso)

# Lasso 2 - Predict -----

lasso.ch_tr <- predict(model_lasso, newdata = tr[,2:38])
lasso.ch_te <- predict(model_lasso, newdata = te[,2:38])


# KNN - Train ----


model_knn <- train(
  ft_ret_rate ~., 
  data = tr[,2:39], 
  method = "knn",
  trControl = trainControl("cv", number = 5),
  preProcess = c("center","scale"),
  tuneLength = 5
)
model_knn
plot(model_knn)



# KNN - Predict ----

knn.ch_tr <- predict(model_knn, newdata = tr[,2:38])
knn.ch_te <- predict(model_knn, newdata = te[,2:38])


# Model Assessment - START HERE ------
# collect data to evaluate training and test error rates
# training error evaluation compilation
# in yardstick and wants factor outcome: accuracy_vec(y.te, pred_knn_te)
library(magrittr)
y.te %<>% as.data.frame()

id.tr = tr[,1:3]
truth.tr = tr[,39]

id.te = te[,1:3]
truth.te = te[,39] 
 

tr_eval = cbind.data.frame(id.tr, truth.tr, net.ch_tr,
                           ridge.ch_tr,
                           lasso.ch_tr, knn.ch_tr)




te_eval = cbind.data.frame(id.te, truth.te, net.ch_te,
                           ridge.ch_te,
                           lasso.ch_te, knn.ch_te)

L1.net = MAE(truth.te, net.ch_te)

L1.rid = MAE(truth.te, ridge.ch_te)

L1.las = MAE(truth.te, lasso.ch_te)

L1.knn = MAE(truth.te, knn.ch_te)

## Test error metrics -----
a.net = postResample(pred = net.ch_te, obs = truth.te)

a.rid = postResample(pred = ridge.ch_te, obs = truth.te)

a.las = postResample(pred = lasso.ch_te, obs = truth.te)

a.knn = postResample(pred = knn.ch_te, obs = truth.te)

tr.net = postResample(pred = net.ch_tr, obs = truth.tr)

tr.rid2 = postResample(pred = ridge.ch_tr, obs = truth.tr)

tr.las2 = postResample(pred = lasso.ch_tr, obs = truth.tr)

tr.knn = postResample(pred = knn.ch_tr, obs = truth.tr)

p_load(magrittr)
my.metric = rbind.data.frame(a.net, a.rid,
                             a.las, a.knn)

errors = c("RMSE", "R2", "MAE")
colnames(my.metric) <- errors

Model = c("enet.ch", "ridge.ch", "lasso.ch", "knn.ch")

my.metrics = cbind(Model, my.metric)

write.csv(my.metrics, file = "team_results_table3.csv")

toplot = read.csv("team_results_table_all_te.csv")


#combine all eval metrics
team_results_all = rbind(team_results1[,2:5], my.metrics)
write.csv(team_results_all, file = "team_results_table_all.csv")


## plot ----

p_load(ggrepel)

p.1 <- ggplot(team_results_all,
              aes(x = MAE, y = RMSE)) +
  geom_point()+
  xlab("MAE")+
  ylab("RMSE")+
  ggtitle("Retention Prediction Modeling Evaluation - All")+ 
  geom_label_repel(aes(label = Model),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  theme_classic()
p.1


top.perform = team_results_all %>%
  select_if(RMSE >= median(RMSE))


p.2 <- ggplot(toplot,
              aes(x = MAE, y = RMSE )) +
  geom_point()+
  xlab("MAE")+
  ylab("RMSE")+
  ggtitle("Retention Prediction Modeling Evaluation - All Test")+ 
  geom_label_repel(aes(label = Model),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  theme_classic()
p.2
