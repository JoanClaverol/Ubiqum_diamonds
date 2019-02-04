
# libraries -----

library(tidyverse)
library(magrittr)
library(modelr)

# data --------

diamonds


# pre-process ----

# dummify variables
diamonds_dummy <- fastDummies::dummy_cols(.data = diamonds,
                                           select_columns = c("cut", "color", "clarity"), 
                                           remove_first_dummy = F)

# logarithm transformation
diamonds_dummy %<>% 
  mutate(log_price = log(price), log_carat = log(carat))

# outliers
diamonds_dummy %<>%
  filter(carat <= 2.75)

# applying model ----

# loading the model 
load("scripts/model.rds")

# applying the model and storing the results 
results <- as.data.frame(predict(object = mod_final, newdata = diamonds_dummy))
colnames(results) <- "log_pred"

# adding actual prices
results$price <- diamonds_dummy$price
results$pred_price <- exp(results$log_pred)


# checking results ----

# regression metrics 
postResample(results$price, results$pred_price)

# ploting errors
plot(results$price ~ results$pred_price)
abline(a = 1, b = 1, col = "red")

# detecting the opportunities to buy
results %<>% 
  mutate(errors = price - pred_price) %>% 
  arrange(errors) %>% as_tibble()

