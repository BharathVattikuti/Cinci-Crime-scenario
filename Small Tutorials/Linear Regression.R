# Packages
library(tidyverse)  # data manipulation and visualization
library(modelr)     # provides easy pipeline modeling functions
library(broom)      # helps to tidy up model outputs

# Load data (remove row numbers included as X1 variable)
advertising <- read_csv("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv") %>%
  select(-X1)

set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(advertising), replace = T, prob = c(0.6,0.4))
train <- advertising[sample, ]
test <- advertising[!sample, ]

model1 <- lm(sales ~ TV, data = train)

tidy(model1)
confint(model1)
summary(model1)
sigma(model1)/mean(train$sales)

# add model diagnostics to our training data
model1_results <- augment(model1, train)

ggplot(model1_results, aes(.fitted, .resid)) +
  geom_ref_line(h = 0) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Residuals vs Fitted")



p1 <- ggplot(model1_results, aes(.fitted, .std.resid)) +
  geom_ref_line(h = 0) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Standardized Residuals vs Fitted")

p2 <- ggplot(model1_results, aes(.fitted, sqrt(.std.resid))) +
  geom_ref_line(h = 0) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Scale-Location")

gridExtra::grid.arrange(p1, p2, nrow = 1)

qq_plot <- qqnorm(model1_results$.resid)
qq_plot <- qqline(model1_results$.resid)

par(mfrow=c(1, 2))

plot(model1, which = 4, id.n = 5)
plot(model1, which = 5, id.n = 5)

model1_results %>%
  top_n(5, wt = .cooksd)

(test <- test %>% 
    add_predictions(model1))

# test MSE
test %>% 
  add_predictions(model1) %>%
  summarise(MSE = mean((sales - pred)^2))

# training MSE
train %>% 
  add_predictions(model1) %>%
  summarise(MSE = mean((sales - pred)^2))

####### Multiple Regression ########

model2 <- lm(sales ~ TV + radio + newspaper, data = train)
summary(model2)
tidy(model2)
confint(model2)



# add model diagnostics to our training data
model1_results <- model1_results %>%
  mutate(Model = "Model 1")

model2_results <- augment(model2, train) %>%
  mutate(Model = "Model 2") %>%
  rbind(model1_results)

ggplot(model2_results, aes(.fitted, .resid)) +
  geom_ref_line(h = 0) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~ Model) +
  ggtitle("Residuals vs Fitted")


par(mfrow = c(1, 2))

# Left: model 1
qqnorm(model1_results$.resid); qqline(model1_results$.resid)

# Right: model 2
qqnorm(model2_results$.resid); qqline(model2_results$.resid)


test %>%
  gather_predictions(model1, model2) %>%
  group_by(model) %>%
  summarise(MSE = mean((sales - pred)^2))


# option A
model3 <- lm(sales ~ TV + radio + TV * radio, data = train)

# option B
model3 <- lm(sales ~ TV * radio, data = train)

summary(model3)

tidy(model3)

list(model1 = broom::glance(model1), 
     model2 = broom::glance(model2),
     model3 = broom::glance(model3))


# add model diagnostics to our training data
model3_results <- augment(model3, train) %>%
  mutate(Model = "Model 3") %>%
  rbind(model2_results)

ggplot(model3_results, aes(.fitted, .resid)) +
  geom_ref_line(h = 0) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~ Model) +
  ggtitle("Residuals vs Fitted")


ggplot(model3_results, aes(.resid)) +
  geom_histogram(binwidth = .25) +
  facet_wrap(~ Model, scales = "free_x") +
  ggtitle("Residual Histogram")


model3_results %>%
  filter(sales > 10) %>%
  ggplot(aes(.resid)) +
  geom_histogram(binwidth = .25) +
  facet_wrap(~ Model, scales = "free_x") +
  ggtitle("Residual Histogram")


par(mfrow = c(1, 2))

plot(model3, which = 4, id.n = 5)
plot(model3, which = 5, id.n = 5)

train[c(3, 5, 47, 65, 94),]

test %>%
  gather_predictions(model1, model2, model3) %>%
  group_by(model) %>%
  summarise(MSE = mean((sales - pred)^2))



############Correlation of Error Terms

df <- economics %>% 
  mutate(observation = 1:n())
df

model6 <- lm(pce ~ unemploy, data = df)

df %>%
  add_residuals(model6) %>%
  ggplot(aes(observation, resid)) +
  geom_line()
