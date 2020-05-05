install.packages("h2o")
install.packages("caret")

library(h2o)
library(caret)

final_data <- read.csv('./data/final_dataset.csv')

h2o.init(nthreads = 15, max_mem_size = "10g")

trainIndex <- createDataPartition(
  y = final_data$price,
  p = 0.8,
  list = FALSE) 

df_train <- as.h2o(final_data[ trainIndex,])
df_test  <- as.h2o(final_data[-trainIndex,])

y <- "price"
x <- setdiff(names(final_data), y)
nfolds <- 5

# Linear Regression

glm <- h2o.glm(
  family= "gaussian", 
  x= x, 
  y= y, 
  remove_collinear_columns = TRUE,
  training_frame=df_train,
  lambda = 0, 
  compute_p_values = TRUE)

# lasso Regression

glm.lasso <- h2o.glm(
  family= "gaussian", 
  x= x, 
  y= y, 
  remove_collinear_columns = TRUE,
  training_frame=df_train,
  lambda = 0.25,
  alpha = 1)

# ridge regression

glm.ridge <- h2o.glm(
  family= "gaussian", 
  x= x, 
  y= y, 
  remove_collinear_columns = TRUE,
  training_frame=df_train,
  alpha = 0,
  lambda = 0.75)

# XGBoost Model

xgb_model <- h2o.xgboost(x = x,
                         y = y,
                         training_frame = df_train,
                         validation_frame = df_test,
                         ntrees = 50,
                         max_depth = 3,
                         min_rows = 2,
                         learn_rate = 0.1,
                         nfolds = 5,
                         fold_assignment = "Modulo",
                         keep_cross_validation_predictions = TRUE,
                         seed = 1)


# Hyper parameter tuning
hyper_params <- list(ntrees = seq(10, 1000),
                     learn_rate = seq(0.0001, 0.2),
                     max_depth = seq(1, 20),
                     sample_rate = seq(0.5, 1.0),
                     col_sample_rate = seq(0.2, 1.0))

search_criteria <- list(strategy = "RandomDiscrete",
                        max_models = 20, 
                        seed = 1)

xgb_grid <- h2o.grid(
                    algorithm = "xgboost",
                    grid_id = "dl_grid_random", 
                    x = x, y = y,
                     training_frame = df_train,
                     nfolds = 5,
                     seed = 1,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)

grid <- h2o.getGrid(grid_id = xgb_grid@grid_id, decreasing = TRUE)

grid 

#best model has 910 trees, nfolds=10, learn_rate = 0.2
xgb_model <- h2o.xgboost(x = x,
                         y = y,
                         training_frame = df_train,
                         validation_frame = df_test,
                         ntrees = 910,
                         max_depth = 3,
                         min_rows = 2,
                         learn_rate = 0.2,
                         nfolds = 10,
                         fold_assignment = "Modulo",
                         keep_cross_validation_predictions = TRUE,
                         seed = 1)


# finding variable importance of XGboost model
var_importance <- h2o.varimp(xgb_model)
View(var_importance$variable)


# find r2 and rmse on trained models
h2o.rmse(gbm, valid = TRUE)
h2o.rmse(glm.lasso, valid = TRUE)
h2o.rmse(glm.ridge, valid = TRUE)
h2o.rmse(xgb_model, valid = TRUE)

h2o.r2(gbm, valid = TRUE)
h2o.r2(glm.lasso, valid = TRUE)
h2o.r2(glm.ridge, valid = TRUE)
h2o.r2(xgb_model, valid = TRUE)

# predict on test data
h2o.predict(xgb_model, df_test[1])
h2o.predict(xgb_model, df_test[2])
h2o.predict(xgb_model, df_test[3])

# save model as a jar file to be used in spring boot application
h2o.save_mojo(xgb_model, path = './models/')