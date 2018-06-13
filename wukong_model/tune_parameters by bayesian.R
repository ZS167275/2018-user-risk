library(caret)
library(xgboost)
library(Matrix)
library(rBayesianOptimization)


## ONLINE、LOCAL划分
train1 <- subset(feature,label> -1)
test1 <- subset(feature,label==(-1))
test2 <- subset(feature,label==(-2))


###线下训练集、测试集构建
ind <- createDataPartition(train1$label,p = 0.7,list = FALSE)
train1_a <- train1[ind, ]
train1_b <- train1[-ind, ]

### 线下测试集
output_vector <- train1_a$label
x1 <- data.matrix(train1_a[, c(-1,-2), with = FALSE])
x2 <- data.matrix(train1_b[, c(-1,-2), with = FALSE]) ##线下测试???


### ONLINE训练集、测试集
output_vector <- train1$label
x_train <- data.matrix(train1[, c(-1,-2), with = FALSE]) # 所有训练集

x_online_a <- data.matrix(test1[, c(-1,-2), with = FALSE]) ##线上测试a榜
x_online_b <- data.matrix(test2[, c(-1,-2), with = FALSE]) ##线上测试b榜



# ---------------------------贝叶斯优化调参------------------------------------

# 缺失值填充
train1[is.na(train1)] <- 0

# 设置随机种子
set.seed(1024)

# 数据准备
dtrain_xgb <- xgb.DMatrix(
  data.matrix(train1[, -1, with = FALSE]),
  label = train1$label
)


# 定义k折交叉检验的k值
cv_folds <- KFold(
  train1$label,
  nfolds = 5,
  stratified = TRUE,
  seed = 0
)



#定义xgb调参函数
xgb_cv_bayes <- function(eta,max_depth,subsample) {
    cv <- xgb.cv(
      params = list(
        booster = "gbtree",
        objective = "binary:logistic",
        eval_metric = "auc",
        eta = eta,
        max_depth = max_depth,
        min_child_weight = 3,
        subsample = subsample,
        colsample_bytree = 0.886,
        lambda = 1,
        alpha = 0
      ),
      data = dtrain_xgb,
      nround = 5000,
      folds = cv_folds,
      prediction = TRUE,
      showsd = TRUE,
      early_stopping_rounds = 100,
      maximize = TRUE,
      verbose = 0
    )
    list(
      Score = cv$evaluation_log[, max(test_auc_mean)],
      Pred = cv$pred
    )
}

# 设置参数列表
eta <- sample(seq(0.001, 0.005, 0.0005),5, replace = FALSE)
max_depth <- c(6L,7L,8L,9L,10L)
min_child_weight <- sample(seq(2L, 10L, 1), 5, replace = FALSE)
subsample <- sample(seq(.7, .95, .02),5, replace = FALSE)
colsample_bytree <- sample(seq(.7, .95, .02), 5, replace = FALSE)

# 实施贝叶斯优化调参
OPT_Res_Xgb <- BayesianOptimization(
  xgb_cv_bayes,
  bounds = list(
    eta = eta,
    max_depth = max_depth,
    min_child_weight = min_child_weight,
    subsample = subsample
  ),
  init_grid_dt = NULL,
  init_points = 10,
  n_iter = 20,
  acq = "ucb",
  kappa = 2.576,
  eps = 0.0,
  verbose = TRUE
)


