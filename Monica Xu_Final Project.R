# UCLA Extension - Introduction to Data Science
#
# Final Project



# ---------------------------------------------------------------
# 1
# ---------------------------------------------------------------
# Read the housing data from the CSV file
housing <- read.csv("/Users/xumeng/Desktop/DS final project/housing.csv")

# Cast ocean_proximity to factor and display levels
housing$ocean_proximity <- as.factor(housing$ocean_proximity)

# Display the resulting levels of ocean_proximity
levels(housing$ocean_proximity)

#[1] "<1H OCEAN"  "INLAND"     "ISLAND"     "NEAR BAY"   "NEAR OCEAN"




# ---------------------------------------------------------------
# 2
# ---------------------------------------------------------------
#
install.packages("tidyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages('ggpubr')
library('ggpubr')
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)

# EDA and Data Visualization
#
#
# Step 2a: Run head() and tail() functions
head(housing)
tail(housing)

# （output) head(housing)
# longitude latitude housing_median_age total_rooms total_bedrooms population households
# 1   -122.23    37.88                 41         880            129        322        126
# 2   -122.22    37.86                 21        7099           1106       2401       1138
# 3   -122.24    37.85                 52        1467            190        496        177
# 4   -122.25    37.85                 52        1274            235        558        219
# 5   -122.25    37.85                 52        1627            280        565        259
# 6   -122.25    37.85                 52         919            213        413        193
# median_income median_house_value ocean_proximity
# 1        8.3252             452600        NEAR BAY
# 2        8.3014             358500        NEAR BAY
# 3        7.2574             352100        NEAR BAY
# 4        5.6431             341300        NEAR BAY
# 5        3.8462             342200        NEAR BAY
# 6        4.0368             269700        NEAR BAY
# tail(housing)
# longitude latitude housing_median_age total_rooms total_bedrooms population households
# 20635   -121.56    39.27                 28        2332            395       1041        344
# 20636   -121.09    39.48                 25        1665            374        845        330
# 20637   -121.21    39.49                 18         697            150        356        114
# 20638   -121.22    39.43                 17        2254            485       1007        433
# 20639   -121.32    39.43                 18        1860            409        741        349
# 20640   -121.24    39.37                 16        2785            616       1387        530
# median_income median_house_value ocean_proximity
# 20635        3.7125             116800          INLAND
# 20636        1.5603              78100          INLAND
# 20637        2.5568              77100          INLAND
# 20638        1.7000              92300          INLAND
# 20639        1.8672              84700          INLAND
# 20640        2.3886              89400          INLAND

#
#
# Step 2b: Run summary() function
summary(housing)

# （output) summary(housing)
# longitude         latitude     housing_median_age  total_rooms    total_bedrooms  
# Min.   :-124.3   Min.   :32.54   Min.   : 1.00      Min.   :    2   Min.   :   1.0  
# 1st Qu.:-121.8   1st Qu.:33.93   1st Qu.:18.00      1st Qu.: 1448   1st Qu.: 296.0  
# Median :-118.5   Median :34.26   Median :29.00      Median : 2127   Median : 435.0  
# Mean   :-119.6   Mean   :35.63   Mean   :28.64      Mean   : 2636   Mean   : 537.9  
# 3rd Qu.:-118.0   3rd Qu.:37.71   3rd Qu.:37.00      3rd Qu.: 3148   3rd Qu.: 647.0  
# Max.   :-114.3   Max.   :41.95   Max.   :52.00      Max.   :39320   Max.   :6445.0  
# NA's   :207     
#    population      households     median_income     median_house_value   ocean_proximity
#  Min.   :    3   Min.   :   1.0   Min.   : 0.4999   Min.   : 14999     <1H OCEAN :9136  
# 1st Qu.:  787   1st Qu.: 280.0   1st Qu.: 2.5634   1st Qu.:119600     INLAND    :6551  
# Median : 1166   Median : 409.0   Median : 3.5348   Median :179700     ISLAND    :   5  
# Mean   : 1425   Mean   : 499.5   Mean   : 3.8707   Mean   :206856     NEAR BAY  :2290  
# 3rd Qu.: 1725   3rd Qu.: 605.0   3rd Qu.: 4.7432   3rd Qu.:264725     NEAR OCEAN:2658  
# Max.   :35682   Max.   :6082.0   Max.   :15.0001   Max.   :500001                      

#
#
# Step 2c: Perform a correlation analysis
correlation_matrix <- cor(housing %>% select(-ocean_proximity))
print(correlation_matrix)

# （output) correlation_housing
#                      longitude    latitude housing_median_age total_rooms total_bedrooms
# longitude           1.00000000 -0.92466443        -0.10819681  0.04456798     0.06960802
# latitude           -0.92466443  1.00000000         0.01117267 -0.03609960    -0.06698283
# housing_median_age -0.10819681  0.01117267         1.00000000 -0.36126220    -0.32045104
# total_rooms         0.04456798 -0.03609960        -0.36126220  1.00000000     0.93037950
# total_bedrooms      0.06960802 -0.06698283        -0.32045104  0.93037950     1.00000000
# population          0.09977322 -0.10878475        -0.29624424  0.85712597     0.87774674
# households          0.05531009 -0.07103543        -0.30291601  0.91848449     0.97972827
# median_income      -0.01517587 -0.07980913        -0.11903399  0.19804965    -0.00772285
# median_house_value -0.04596662 -0.14416028         0.10562341  0.13415311     0.04968618
#                      population  households median_income median_house_value
# longitude           0.099773223  0.05531009  -0.015175865        -0.04596662
# latitude           -0.108784747 -0.07103543  -0.079809127        -0.14416028
# housing_median_age -0.296244240 -0.30291601  -0.119033990         0.10562341
# total_rooms         0.857125973  0.91848449   0.198049645         0.13415311
# total_bedrooms      0.877746743  0.97972827  -0.007722850         0.04968618
# population          1.000000000  0.90722227   0.004834346        -0.02464968
# households          0.907222266  1.00000000   0.013033052         0.06584265
# median_income       0.004834346  0.01303305   1.000000000         0.68807521
# median_house_value -0.024649679  0.06584265   0.688075208         1.00000000

#
#
# Step 2d: Create histograms for each numeric variable

# Create histogram for longitude
p1 <- ggplot(housing, aes(x = longitude)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(y = "", x = "Longitude") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(hjust = 0.5))

# Create histogram for latitude
p2 <- ggplot(housing, aes(x = latitude)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(y = "", x = "Latitude") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(hjust = 0.5))

# Create histogram for housing_median_age
p3 <- ggplot(housing, aes(x = housing_median_age)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(y = "", x = "Median Age") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(hjust = 0.5))

# Create histogram for total_rooms
p4 <- ggplot(housing, aes(x = total_rooms)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(y = "", x = "Total Rooms") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(hjust = 0.5))

# Create histogram for total_bedrooms
p5 <- ggplot(housing, aes(x = total_bedrooms)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(y = "", x = "Total Bedrooms") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(hjust = 0.5))

# Create histogram for population
p6 <- ggplot(housing, aes(x = population)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(y = "", x = "Population") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(hjust = 0.5))

# Create histogram for households
p7 <- ggplot(housing, aes(x = households)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(y = "", x = "Households") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(hjust = 0.5))

# Create histogram for median_income
p8 <- ggplot(housing, aes(x = median_income)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(y = "", x = "Median Income") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(hjust = 0.5))

# Create histogram for median_house_value
p9 <- ggplot(housing, aes(x = median_house_value)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(y = "", x = "Median House Value") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(hjust = 0.5))

# Create histogram for ocean_proximity
p10 <- ggplot(housing, aes(x = ocean_proximity)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(y = "", x = "Ocean Proximity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(hjust = 0.5))

# Combine all the histograms
ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, ncol = 2)
#
#
# Step 2e: Produce boxplots for each numeric variable

# Create boxplot for longitude
e1 <- ggplot(housing, aes(y = longitude)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(y = "Longitude", x = "") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

# Create boxplot for latitude
e2 <- ggplot(housing, aes(y = latitude)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(y = "Latitude", x = "") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

# Create boxplot for housing_median_age
e3 <- ggplot(housing, aes(y = housing_median_age)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(y = "Median Age", x = "") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

# Create boxplot for total_rooms
e4 <- ggplot(housing, aes(y = total_rooms)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(y = "Total Rooms", x = "") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

# Create boxplot for total_bedrooms
e5 <- ggplot(housing, aes(y = total_bedrooms)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(y = "Total Bedrooms", x = "") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

# Create boxplot for population
e6 <- ggplot(housing, aes(y = population)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(y = "Population", x = "") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

# Create boxplot for households
e7 <- ggplot(housing, aes(y = households)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(y = "Households", x = "") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

# Create boxplot for median_income
e8 <- ggplot(housing, aes(y = median_income)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(y = "Median Income", x = "") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

# Create boxplot for median_house_value
e9 <- ggplot(housing, aes(y = median_house_value)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(y = "Median House Value", x = "") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

# Create a bar plot for ocean_proximity
e10 <- ggplot(housing, aes(x = ocean_proximity)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(y = "Count", x = "Ocean Proximity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

# Combine all the boxplots
ggarrange(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, ncol = 2)
#
#
# Step 2f: Produce boxplots for the variables: housing_median_age, median_income, and median_house_value "with respect" to ocean_proximity

# Load the grid package if not already loaded
if (!requireNamespace("grid", quietly = TRUE)) {
  install.packages("grid")
}
install.packages("grid")
library(grid)

# Create a boxplot for housing_median_age with respect to ocean_proximity
f1 <- ggplot(housing, aes(x = ocean_proximity, y = housing_median_age)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(y = "Housing Median Age", x = "Ocean Proximity") +
  ggtitle("Boxplot of Housing Median Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

# Create a boxplot for median_income with respect to ocean_proximity
f2 <- ggplot(housing, aes(x = ocean_proximity, y = median_income)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(y = "Median Income", x = "Ocean Proximity") +
  ggtitle("Boxplot of Median Income") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

# Create a boxplot for median_house_value with respect to ocean_proximity
f3 <- ggplot(housing, aes(x = ocean_proximity, y = median_house_value)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(y = "Median House Value", x = "Ocean Proximity") +
  ggtitle("Boxplot of Median House Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"))

# Combine all the plots
ggarrange(f1, f2, f3, ncol = 2)




# ---------------------------------------------------------------
# 3
# ---------------------------------------------------------------
#
#
# Step 3a: 
# Check summary before imputation
summary(housing$total_bedrooms)

# Compute the median of total_bedrooms (excluding NA values)
median_bedrooms <- median(housing$total_bedrooms, na.rm = TRUE)

# Replace missing values with the computed median
housing$total_bedrooms[is.na(housing$total_bedrooms)] <- median_bedrooms

# Check summary after imputation
summary(housing$total_bedrooms)

# (output)> summary(housing$total_bedrooms)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#    1.0   296.0   435.0   537.9   647.0  6445.0     207 

#
#
# Step 3b: 
# Split the ocean_proximity variable into binary categorical variables
split_housing <- housing

# Define the categories to split into binary variables
categories_to_split <- levels(housing$ocean_proximity)

# Create binary variables for each category
for (category in categories_to_split) {
  split_housing[paste(category, "_binary", sep = "_")] <- as.integer(housing$ocean_proximity == category)
}

# Remove the original ocean_proximity variable
split_housing <- subset(split_housing, select = -ocean_proximity)
#
#
# Step 3c: 
# Create the mean_bedrooms and mean_rooms variables
housing$mean_bedrooms <- housing$total_bedrooms / housing$households
housing$mean_rooms <- housing$total_rooms / housing$households

# Remove the total_bedrooms and total_rooms variables
housing <- housing %>%
  select(-total_bedrooms, -total_rooms)
#
#
# Step 3d: 
# Perform feature scaling on selected numerical variables
selected_numerical_cols <- c(
  "longitude", "latitude", "housing_median_age",
  "population", "households", "median_income",
  "mean_bedrooms", "mean_rooms"
)

split_housing[, selected_numerical_cols] <- scale(split_housing[, selected_numerical_cols])
#
#
# Step 3e:
# Create the cleaned_housing data frame
cleaned_housing <- split_housing




# ---------------------------------------------------------------
# 4
# ---------------------------------------------------------------
#
#
# Step 4a: Create Random Sample Index
set.seed(120) # For reproducibility
sample_index <- sample(nrow(cleaned_housing), nrow(cleaned_housing))

# Step 4b: Create Training Set
train_size <- floor(0.7 * nrow(cleaned_housing))
train <- cleaned_housing[sample_index[1:train_size], ]

# Step 4c: Create Test Set
test <- cleaned_housing[sample_index[(train_size + 1):nrow(cleaned_housing)], ]




# ---------------------------------------------------------------
# 5
# ---------------------------------------------------------------
#
#
# Supervised Machine Learning - Regression
library(randomForest)

# Separate training set into predictors (train_x) and response variable (train_y)
train_x <- train[, -which(names(train) == "median_house_value")]
train_y <- train$median_house_value

# Build the random forest model
rf <- randomForest(x = train_x, y = train_y,
                   ntree = 500, importance = TRUE)

# Display the metrics computed by the algorithm
print(names(rf))

# (output)> print(names(rf))
# [1] "call"            "type"            "predicted"       "mse"             "rsq"            
# [6] "oob.times"       "importance"      "importanceSD"    "localImportance" "proximity"      
# [11] "ntree"           "mtry"            "forest"          "coefs"           "y"              
# [16] "test"            "inbag"          




# ---------------------------------------------------------------
# 6
# ---------------------------------------------------------------
#
#
# Step 6a: Calculate RMSE for the trained model
last_mse <- rf$mse[length(rf$mse)]
rmse <- sqrt(last_mse)
print(paste("Root Mean Squared Error (RMSE) for trained model:", rmse))

# [1] "Root Mean Squared Error (RMSE) for trained model: 48879.5587263745"


# Step 6b: Predict using the test set
test_x <- test[, -which(names(test) == "median_house_value")]
test_y <- test$median_house_value
predicted_values <- predict(rf, newdata = test_x)
head(predicted_values)

# head(predicted_values)
#        1        2        3        4        5        6 
# 178644.5 261115.0 119192.1 276503.7 142053.0 165740.3 


# Step 6c: Calculate RMSE for the test set
test_rmse <- sqrt(mean((predicted_values - test_y)^2))
print(paste("Root Mean Squared Error (RMSE) for test set:", test_rmse))

# [1] "Root Mean Squared Error (RMSE) for test set: 50654.4675627263"


# Step 6d: Compare training and test set RMSE
print(paste("Training set RMSE:", rmse))
print(paste("Test set RMSE:", test_rmse))
if (test_rmse >= rmse) {
  print("The model is not overfit and makes good predictions.")
} else {
  print("The model may be overfitting the training data.")
}

# [1] "Training set RMSE: 48879.5587263745"
# [1] "Test set RMSE: 50654.4675627263"
# [1] "The model is not overfit and makes good predictions."


# Step 6e: Variable Importance Plot
var_importance <- varImpPlot(rf, main = "Variable Importance Plot")
print(var_importance)

# print(var_importance)
#                     %IncMSE IncNodePurity
# longitude          66.3384431  2.213261e+13
# latitude           63.8868064  1.955344e+13
# housing_median_age 76.5141652  8.573690e+12
# population         38.8715398  6.443404e+12
# households         39.6454945  6.733184e+12
# median_income      96.5734458  6.491421e+13
# NEAR BAY           14.7900071  1.197363e+12
# <1H OCEAN          20.5182836  3.804873e+12
# INLAND             46.6236402  2.675998e+13
# NEAR OCEAN         16.3309867  1.965423e+12
# ISLAND              0.3769583  2.917351e+10
# mean_bedrooms      44.0503920  6.533129e+12
# mean_rooms         45.9029863  1.789014e+13







# Complementary：I want to try adjusting the data to 1000 and observe if it leads to a more precise outcome. 
# Build the random forest model
rf <- randomForest(x = train_x, y = train_y,
                   ntree = 1000, importance = TRUE, max_features='auto')
last_mse <- rf$mse[length(rf$mse)]
rmse <- sqrt(last_mse)
print(paste("Root Mean Squared Error (RMSE) for trained model:", rmse))
# [1] "Root Mean Squared Error (RMSE) for trained model: 48774.8061844676"
test_x <- test[, -which(names(test) == "median_house_value")]
test_y <- test$median_house_value
predicted_values <- predict(rf, newdata = test_x)
test_rmse <- sqrt(mean((predicted_values - test_y)^2))
print(paste("Root Mean Squared Error (RMSE) for test set:", test_rmse))
# [1] "Root Mean Squared Error (RMSE) for test set: 50715.1198587607"


# Choose fewer varibles to observe differences in results
sf <- c("median_income",  "housing_median_age",
        "longitude" )
train_xnew <- train[, (names(train) %in% sf)]
test_xnew <- train[, (names(train) %in% sf)]
rf1 = randomForest(x=train_xnew, y=train_y ,
                   ntree=500, importance=TRUE)
rf1_last_mse <- rf1$mse[length(rf$mse)]
rf1_rmse <- sqrt(last_mse)
predicted_values <- predict(rf1, newdata = test_xnew)
test_y <- test$median_house_value
test_rmse <- sqrt(mean((predicted_values - test_y)^2))
print(paste("Root Mean Squared Error (RMSE) for test set:", test_rmse))