library(pacman)
p_load(dplyr, readr, janitor, ggplot2, caret, naniar)
getwd()
# importing by-year tuition data
t11 = read_csv("tui_11.csv")
t12 = read.csv("tui_12.csv")
t13 = read.csv("tui_13.csv")
t14 = read.csv("tui_14.csv")
t15 = read.csv("tui_15.csv")
t16 = read.csv("tui_16.csv")


# combining to make overall tuition df
tui_clean <- as.data.frame(rbind(t11, t12, t13, t14, t15, t16))

write.csv(tui_clean, file =
            "C:/Users/Nina/Documents/College Classes/Winter 2020/Econ/424-524-project/tuition_clean1.csv")               

#checking for missing data ------------------------
#then replacing missing values with the median of each variable usinf preProcess function

gg_miss_var(tui_clean) # visual of the missing data 

tui_clean <- preProcess(
  x = tui_clean,
  method = c("medianImpute")
) %>% predict(tui_clean)

summary(tui_clean)
sum(is.na(tui_clean))

# split data into test and training data  20% 80%----------------------------------

set.seed(0726)
tui_clean_test <- sample_frac(tui_clean1, size = 0.2)
tui_clean_train <- setdiff(tui_clean, tui_clean_test)


#train data using four different models-----------------------
##first model: logistic----------------------

model_glmnet = train(
  ft_ret_rate ~ .,
  data = tui_clean_train %>% select(-c(a_unit_id, a_institution)),
  method = "glmnet",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = expand.grid(
    alpha = seq(0, 1, 0.1),
    lambda = c(0.1, 0.01, 0.001, 0.0001, 0.00001)
  )
)
model_glmnet


##Second model : ridge---------------

lambdas = 10^seq(from = 5, to = -2, length = 100) #create lambdas to pick from

tui_train_matrix <- as.matrix(tui_clean_train[,3:23])
#transform dataframe into matrix to use lasso/ridge and remove id and name of institution 

model_ridge = train(
  ft_ret_rate ~ .,
  data = tui_train_matrix ,
  method = "glmnet",
  tuneGrid = expand.grid(
    alpha = 0,
    lambda = lambdas
  ))
model_ridge


##Third model: lasso ---------------------
model_lasso = train(
  ft_ret_rate ~ .,
  data = tui_train_matrix ,
  method = "glmnet",
  tuneGrid = expand.grid(
    alpha = 1,
    lambda = lambdas
  ))
model_lasso
##Fouth model: random forest or ???-------------------



#CV error Estimate your error using cross validation.---------------


#Test Test  performance with the held out data.------------------
#need to find a way to estimate the accuracy of the prediction 
retention_predict <- predict(model_glmnet, newdata = tui_clean_test)
tui_clean_test$retention_predict <- retention_predict
