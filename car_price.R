library(tidyverse)
library(broom)
library(ggplot2)
library(ggmosaic)
library(GGally)
library(stringdist)
library(modeest)
library(mixtools)
library(glmnet)

ggplot2::theme_set(ggplot2::theme_bw())

car_price_orig <- read_csv("CarPrice.csv") %>%
  separate(CarName, c("brand", "model"), sep = " ", extra = "merge", 
           fill = "right") %>%
  mutate(enginetype = ifelse(.$enginetype %in% c("dohc", "ohc", "ohcv",
                                                 "rotor", "l", "ohcf"), 
                             enginetype, NA))

#checking for missing values
colSums(is.na(car_price_orig))
215*(1-mean(complete.cases(car_price_orig))) #num of missing obs

#checking frequency of engine locations
count(car_price_orig, enginelocation)

#checking frequency of engine types
count(car_price_orig, enginetype)

#checking NAs
view(filter(car_price_orig, is.na(model)))
view(filter(car_price_orig, is.na(enginelocation)))
view(filter(car_price_orig, is.na(horsepower)))
view(filter(car_price_orig, is.na(enginetype)))

#vector of properly-spelled brand names
brands <- setdiff(car_price_orig$brand, 
                  c("porscshce", "toyouta", "volkw", "vokswagen"))

car_price_trans <- car_price_orig %>%
          #matching misspelled brand names to correctly-spelled names by jaro-winkler
          #algorithm
  mutate(brand = brands[amatch(.$brand, brands, method = "jw", maxDist = 5)],
         #creating log-price variable
         log_price = log(price)) %>%
  #creating unordered factor variables
  mutate_at(c("symboling", "brand", "fueltype", "aspiration", "doornumber",
              "carbody", "drivewheel", "enginelocation", "enginetype", 
              "cylindernumber", "fuelsystem"), as_factor) %>%
  #dropping unused variables
  dplyr::select(-c("car_ID", "model"))

#capping price outliers at 5th and 95th %-tile
q <- 1.5*IQR(car_price_trans$log_price)
qnt <- quantile(car_price_trans$log_price, prob = c(0.25, 0.75))
caps <- quantile(car_price_trans$log_price, prob = c(0.5, 0.95))

car_price_cap <- car_price_trans %>% 
  mutate(log_price_cap = if_else(log_price < qnt[1] - q, caps[[1]],
                          if_else(log_price > qnt[2] + q, caps[2], log_price))) %>%
  mutate(price_cap = exp(log_price_cap)) %>%
  dplyr::select(-c(price, log_price))

 #######
## EDA ##
 #######

#boxplot of price
car_price_trans %>%
  ggplot(aes(x = "", y = price)) +
  geom_boxplot() +
  ylab("Price") +
  xlab("") +
  ggtitle("Boxplot of Car-Price")

#boxplot of log-price
car_price_trans %>%
  ggplot(aes(x = "", y = log_price)) +
  geom_boxplot() +
  ylab("Log-Price") +
  xlab("") +
  ggtitle("Boxplot of Log-Car-Price")

#boxplot of capped price
car_price_cap %>%
  ggplot(aes(x = "", y = price_cap)) +
  geom_boxplot() +
  ylab("Price") +
  xlab("") +
  ggtitle("Boxplot of Capped Car-Price")

#boxplot of capped log-price
car_price_cap %>%
  ggplot(aes(x = "", y = log_price_cap)) +
  geom_boxplot() +
  ylab("Log-Price") +
  xlab("") +
  ggtitle("Boxplot of Capped Log-Car-Price")

#density plot of capped price
car_price_cap %>%
  ggplot(aes(x = price_cap)) +
  geom_density() +
  xlab("Log-Price") +
  ggtitle("Density of Car-Price")

#density plot of capped log-price
car_price_cap %>%
  ggplot(aes(x = log_price_cap)) +
  geom_density() +
  xlab("Log-Price") +
  ggtitle("Density of Log-Car-Price")

#mixture distribution of log-price
mix_lp <- normalmixEM(car_price_cap$log_price_cap, k = 3)
mix_lp

mix_lp$mu[2]

#plot of mixture distribution
plot(mix_lp, which = 2, xlab2 ="Log Price" )

#probability density at mean log price for economy, mid-range, or luxury dists
print(
car_price_cap %>%
  group_by(brand) %>%
  summarize(mean_log_price = mean(log_price_cap)) %>%
  mutate(p_econ = dnorm(mean_log_price, mix_lp$mu[1], mix_lp$sigma[1]),
         p_mid = dnorm(mean_log_price, mix_lp$mu[3], mix_lp$sigma[3]),
         p_lux = dnorm(mean_log_price, mix_lp$mu[2], mix_lp$sigma[2])), n = Inf)
  

car_price_binned <- car_price_cap %>%
         #creating bins for economy, luxury, mid-range cars
  mutate(bin = as_factor(if_else(brand %in% c("alfa-romero", "audi", "bmw", 
                                    "chevrolet", "isuzu", "mazda", 
                                    "mercury", "peugeot", "saab", 
                                    "volvo"),
                       "mid",
                if_else(brand %in% c("jaguar", "buick", "porsche"),
                        "lux", "econ")))) 

View(count(car_price_binned, bin, brand))

#scatterplots of log-price against conts vars
car_price_binned %>%
  select_if(is.numeric) %>%
  pivot_longer(-log_price_cap, names_to = "var", values_to = "value") %>%
  ggplot(aes(x = value, y = log_price_cap)) +
  geom_point() +
  facet_wrap(~var, scales = "free") +
  ggtitle("Scatterplots of Continuous Predictors vs Capped Log-Price")

#boxplots of log-price against categorical vars
non_numeric <- !sapply(car_price_binned, is.numeric)
car_price_binned %>%
  dplyr::select("log_price_cap", names(non_numeric)[as.numeric(non_numeric) == 1]) %>%
  pivot_longer(-log_price_cap, names_to = "var", values_to = "value") %>%
  ggplot(aes(x = factor(value), y = log_price_cap)) +
  geom_boxplot() +
  facet_wrap(~var, scales = "free") +
  ggtitle("Boxplots of Categorical Predictors vs Capped Log-Price")

#scatterplots of continuous predictors against each other
car_price_binned %>%
  select(carlength, carwidth, citympg, curbweight, enginesize,
         highwaympg, horsepower) %>%
  ggpairs(title = "Plot of Continuous Predictor Variables")

 ##########
# Analysis #
 ##########

#splitting into training and testing
car_price_nona <- car_price_binned %>%
  drop_na

train <- car_price_nona %>%
  sample_frac(0.7)
test <- car_price_nona %>%
  setdiff(train)

x_train = model.matrix(log_price_cap~.+I(enginesize^2)-1-price_cap, train)[,-1]
x_test = model.matrix(log_price_cap~.+I(enginesize^2)-1-price_cap, test)[,-1]

y_train = train %>%
  select(log_price_cap) %>%
  unlist() %>%
  as.numeric()

y_test = test %>%
  select(log_price_cap) %>%
  unlist() %>%
  as.numeric()

#choosing best lambda by 10-fold cross-validation
set.seed(2020)
car_lasso <- cv.glmnet(x_train, y_train)
best_lamb <- car_lasso$lambda.1se
best_lamb

coef(car_lasso) #coefficients obtained from lasso
plot(car_lasso) #plot of MSE by lambda value

#calculating mean squared prediction error
preds <- tibble(actual = y_test, 
                fitted = predict(car_lasso, s = best_lamb, newx = x_test))
print(preds, n = Inf)
MSPE <- mean((preds$actual - preds$fitted)^2)
MSPE