
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

# applying the model 
results <- as.data.frame(predict(object = mod_final, newdata = diamonds_dummy))
results$