### EXPLORING THE DIAMONDS DATASET ###


# libraries ----

library(tidyverse)


# feature selection ----

# decision tree to identify the variables related
mod_dt <- rpart::rpart(price ~., 
                       data = diamonds)

# visualizing the dt
rattle::fancyRpartPlot(mod_dt, cex = 0.7, tweak = 1)

# Observations: carat is the most relevant variable to use it to predict price
plot_priceVScarat <- diamonds %>% 
  ggplot() + 
    geom_point(aes(x = carat, y = price))
# they have a explonential relation. FOr the moment we are not going to modify that


# modalization ----

# using a linear model to 


