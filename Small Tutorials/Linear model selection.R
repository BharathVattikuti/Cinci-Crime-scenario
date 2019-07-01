# Packages
library(tidyverse)  # data manipulation and visualization
library(leaps)      # model selection functions

# Load data and remove rows with missing data
(
  hitters <- na.omit(ISLR::Hitters) %>%
    as_tibble
)
best_subset <- regsubsets(Salary ~ ., hitters, nbest = 2, nvmax = 19)
summary(best_subset)
plot(best_subset, scale="bic")

#### Forward Stepwise

forward <- regsubsets(Salary ~ ., hitters, nvmax = 19, method = "forward")
backward <- regsubsets(Salary ~ ., hitters, nvmax = 19, method = "backward")

########Model validation

# create training - testing data
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(hitters), replace = T, prob = c(0.6,0.4))
train <- hitters[sample, ]
test <- hitters[!sample, ]


# perform best subset selection
best_subset <- regsubsets(Salary ~ ., train, nvmax = 19)
results <- summary(best_subset)


# extract and plot results
tibble(predictors = 1:19,
       adj_R2 = results$adjr2,
       Cp = results$cp,
       BIC = results$bic) %>%
  gather(statistic, value, -predictors) %>%
  ggplot(aes(predictors, value, color = statistic)) +
  geom_line(show.legend = F) +
  geom_point(show.legend = F) +
  facet_wrap(~ statistic, scales = "free")



which.max(results$adjr2)
## [1] 10
which.min(results$bic)
## [1] 4
which.min(results$cp)
## [1] 8

# 10 variable model
coef(best_subset, 10)
# 4 variable model
coef(best_subset, 4)
# 8 variable model
coef(best_subset, 8)


forward <- regsubsets(Salary ~ ., train, nvmax = 19, method = "forward")
backward <- regsubsets(Salary ~ ., train, nvmax = 19, method = "backward")
# which models minimize Cp?
which.min(summary(forward)$cp)
## [1] 8
which.min(summary(backward)$cp)
## [1] 8

coef(best_subset, 8)
coef(forward, 8)
coef(backward, 8)


##########Directly Estimating Test Error
test_m <- model.matrix(Salary ~ ., data = test)

# create empty vector to fill with error values
validation_errors <- vector("double", length = 19)

for (i in 1:19) {
  coef_x <- coef(best_subset, id = i)                     # extract coefficients for model size i
  pred_x <- test_m[ , names(coef_x)] %*% coef_x           # predict salary using matrix algebra
  validation_errors[i] <- mean((test$Salary - pred_x)^2)  # compute test error btwn actual & predicted salary
}

# plot validation errors
plot(validation_errors, type = "b")


# create training - testing data
set.seed(5)
sample <- sample(c(TRUE, FALSE), nrow(hitters), replace = T, prob = c(0.6,0.4))
train <- hitters[sample, ]
test <- hitters[!sample, ]

# perform best subset selection
best_subset <- regsubsets(Salary ~ ., train, nvmax = 19)

# compute test validation errors
test_m <- model.matrix(Salary ~ ., data = test)
validation_errors <- vector("double", length = 19)

for (i in 1:19) {
  coef_x <- coef(best_subset, id = i)                     # extract coefficients for model size i
  pred_x <- test_m[ , names(coef_x)] %*% coef_x           # predict salary using matrix algebra
  validation_errors[i] <- mean((test$Salary - pred_x)^2)  # compute test error btwn actual & predicted salary
}

# plot validation errors
plot(validation_errors, type = "b")

predict.regsubsets <- function(object, newdata, id ,...) {
  form <- as.formula(object$call[[2]]) 
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

k <- 10
set.seed(1)
folds <- sample(1:k, nrow(hitters), replace = TRUE)
cv_errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))


for (j in 1:k) {
  
  # perform best subset on rows not equal to j
  best_subset <- regsubsets(Salary ~ ., hitters[folds != j, ], nvmax = 19)
  
  # perform cross-validation
  for (i in 1:19) {
    pred_x <- predict.regsubsets(best_subset, hitters[folds == j, ], id = i)
    cv_errors[j, i] <- mean((hitters$Salary[folds == j] - pred_x)^2)
  }
}

mean_cv_errors <- colMeans(cv_errors)
plot(mean_cv_errors, type = "b")
