# Packages
library(tidyverse)  # data manipulation and visualization
library(boot)       # resampling and bootstrapping

# Load data 
(auto <- as_tibble(ISLR::Auto))

ggplot(auto, aes(horsepower, mpg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, linetype = 2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, linetype = 3) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), se = FALSE, linetype = 4)


set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(auto), replace = T, prob = c(0.6,0.4))
train <- auto[sample, ]
test <- auto[!sample, ]

# loop for first ten polynomial
mse.df <- tibble(degree = 1:10, mse = NA)

for(i in 1:10) {
  lm.fit <- lm(mpg ~ poly(horsepower, i), data = train)
  mse.df[i, 2] <- mean((test$mpg - predict(lm.fit, test))^2)
}

ggplot(mse.df, aes(degree, mse)) +
  geom_line() +
  geom_point() +
  ylim(c(10, 30))


mse.df.2 <- tibble(sample = vector("integer", 100), 
                   degree = vector("integer", 100), 
                   mse = vector("double", 100))
counter <- 1


for(i in 1:10) {
  # random sample
  set.seed(i)
  sample <- sample(c(TRUE, FALSE), nrow(auto), replace = T, prob = c(0.6,0.4))
  train <- auto[sample, ]
  test <- auto[!sample, ]
  
  # modeling
  for(j in 1:10) {
    lm.fit <- lm(mpg ~ poly(horsepower, j), data = train)
    
    # add degree & mse values
    mse.df.2[counter, 2] <- j
    mse.df.2[counter, 3] <- mean((test$mpg - predict(lm.fit, test))^2)
    
    # add sample identifier
    mse.df.2[counter, 1] <- i
    counter <- counter + 1
  }
  next
}

ggplot(mse.df.2, aes(degree, mse, color = factor(sample))) +
  geom_line(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  ylim(c(10, 30))


##################Leave-One-Out Cross-Validation

glm.fit <- glm(mpg ~ horsepower, data = auto)
coef(glm.fit)

lm.fit <- lm(mpg ~ horsepower, data = auto)
coef(lm.fit)

# step 1: fit model
glm.fit <- glm(mpg ~ horsepower, data = auto)

# setp 2: perform LOOCV across entire data set
loocv.err <- cv.glm(auto, glm.fit)

str(loocv.err)

loocv.err$delta[1]

# create function that computes LOOCV MSE based on specified polynomial degree
loocv_error <- function(x) {
  glm.fit <- glm(mpg ~ poly(horsepower, x), data = auto)
  cv.glm(auto, glm.fit)$delta[1]
}

# compute LOOCV MSE for polynomial degrees 1-5
library(purrr)
1:5 %>% map_dbl(loocv_error)

# DO NOT RUN THIS CODE - YOU WILL BE WAITING A LONG TIME!!
system.time({
  diamonds.fit <- glm(price ~ carat + cut + color + clarity, data = diamonds)
  cv.glm(diamonds, diamonds.fit)
})


# create function that computes k-fold MSE based on specified polynomial degree
kfcv_error <- function(x) {
  glm.fit <- glm(mpg ~ poly(horsepower, x), data = auto)
  cv.glm(auto, glm.fit, K = 10)$delta[1]
}

# compute k-fold MSE for polynomial degrees 1-5
1:5 %>% map_dbl(kfcv_error)
## [1] 24.56850 19.22648 19.29535 19.46601 19.24090

# compare to LOOCV MSE values
1:5 %>% map_dbl(loocv_error)
## [1] 24.23151 19.24821 19.33498 19.42443 19.03321


system.time({
  diamonds.fit <- glm(price ~ carat + cut + color + clarity, data = diamonds)
  cv.glm(diamonds, diamonds.fit, K = 10)
})


stock <- ISLR::Smarket

# fit logistic regression model
glm.fit <- glm(Direction ~ Lag1 + Lag2, family = binomial, data = stock)

# The cost function here correlates to that in Eq. 3
cost <- function(r, pi = 0) mean(abs(r - pi) > 0.5)

# compute the k-fold estimated error with our cost function
cv.glm(stock, glm.fit, cost, K = 10)$delta[1]
## [1] 0.4984