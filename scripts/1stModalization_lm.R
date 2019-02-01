### EXPLORING THE DIAMONDS DATASET ###


# libraries ----

library(tidyverse)
library(modelr)
library(magrittr)


# 1st exploration ----

# subseting the data
diamonds_subset <- sample_n(tbl = diamonds, size = 5000)

# decision tree to identify the variables related
dt_all <- rpart::rpart(price ~., 
                       data = diamonds_subset)

# visualizing the dt
rattle::fancyRpartPlot(dt_all, cex = 0.7, tweak = 1)

# Observations: carat is the most relevant variable to use it to predict price
plot_priceVScarat <- diamonds %>% 
  ggplot() + 
    geom_point(aes(x = carat, y = price))
# they have a explonential relation. FOr the moment we are not going to modify that


# 1st modalization ----

# using a linear model
mod.lm <- lm(price ~ carat, 
             data = diamonds_subset)

# checking the results
par(mfrow = c(2,2))
plot(mod.lm)
par(mfrow = c(1,1))
# there seems that there are some patterns inside the residuals that our model
# is not explaining well. They are centered in specific places, so it could be 
# that a categorical value can explain them. 


# 2nd exploration ----

# looking for the categorical variable that is explaining more about the model 
dt_categories <- rpart::rpart(price ~ cut + color + clarity, 
                              data = diamonds_subset)

# ploting the decision tree
rattle::fancyRpartPlot(dt_categories)
# We are going to use color and clarity for our modalization process


# 2nd modalization ----

# creating a lm with new features
mod_caratANDcategories <- lm(price ~ carat + color + clarity, 
                             data = diamonds_subset)

# checking the errors 
par(mfrow = c(2,2))
plot(mod_caratANDcategories)
par(mfrow = c(1,1))
# the errors has a cuadratic shape, so we have to look for a cuadratic relation. 
# In order to use the data into a linear relation model, I have to transform the
# data. Also, there are a several amount of outliers that we should take out.


# 3rd exploration ----

# plting the relation between carat aand price
plot_priceVScarat

# How can I transform a exponantial relation between two variables
ggplot(diamonds_subset) +
  geom_point(aes(x = log(carat), y = log(price)))

# adding a new price and carat tranformation
diamonds_subset %<>% 
  mutate(log_price = log(price), log_carat = log(carat))


# 3rd modalization ----

# lm with transformed price and carat
mod_transformed <- lm(log_price ~ log_carat + color + clarity, 
                      data = diamonds_subset)

# checking the errors
par(mfrow = c(2,2))
plot(mod_transformed)
par(mfrow = c(1,1))

# transform predictions in log to real and error plot
diamonds_subset %<>% 
  add_predictions(model = mod_transformed, var = "log_pred") %>% 
  mutate(pred = exp(log_pred), errors = price - pred) %>% 
  ggplot() + 
    geom_hex(aes(y = errors, x = carat))
# With that graph we realize that the errors are being affected for outiers, 
# let's find them!


# 3rd  exploration ----

# ploting the outliers
boxplot(diamonds_subset$price)
