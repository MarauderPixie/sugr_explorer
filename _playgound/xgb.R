library(xgboost)

# classiy weekdays; start week on mondays and substract 1, so
# range is 0:6, as is needed for xgboost
xg_full <- ts_df %>% 
  mutate(wd = wday(ymdh, week_start = 1) - 1) %>% 
  select(-ymdh, -bg_sd) %>% 
  sample_frac(1)

# test/train split
xg_train <- xg_full[1:1058,]
xg_test <- xg_full[-1058:-1,]

# thist should'nt actually be necessary
xg_train_list <- list(data = as.matrix(xg_train[, 1:2]),
                      label = as.matrix(xg_train$wd))

xg_test_list <- list(data = as.matrix(xg_test[, 1:2]),
                     label = as.matrix(xg_test$wd))

# train model 50 times (should reduce error to ~0.005)
xg_model <- xgboost(data = xg_train_list$data, label = xg_train_list$label, 
                    max.depth = 1, eta = 1, nthread = 2, nrounds = 2, 
                    num_class = 7, objective = "multi:softmax")


# predict!
pred <- predict(xg_model, xg_test_list$data)

# give error
mean(pred != xg_test_list$label)

# look for yerself
tibble(
  lab = xg_test_list$label,
  pred = pred,
  bool = lab == pred
)




### with iris
iris_p <- iris %>% 
  mutate(
    Species = as.numeric(Species) - 1
  ) %>% 
  sample_frac(1)

iris_train <- list(data = as.matrix(iris_p[1:120, 1:4]),
                   labs = as.matrix(iris_p[1:120, 5]))
iris_test <- list(data = as.matrix(iris_p[121:150, 1:4]),
                  labs = as.matrix(iris_p[121:150, 5]))

iris_model <- xgboost(data = iris_train$data, label = iris_train$labs, 
                      max.depth = 1, eta = 1, nthread = 2, nrounds = 2, 
                      num_class = 3, objective = "multi:softmax")

iris_pred <- predict(iris_model, iris_test$data)

mean(iris_pred != iris_test$labs)
