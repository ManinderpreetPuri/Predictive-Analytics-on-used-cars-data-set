# Name: Maninderpreet Singh Puri
# Student ID: 20494381
# BUS5PA Assignment 1- Part 2

###################################################################################################
#install.packages("psych", dependencies = TRUE)
#install.packages("SmartEDA")
#install.packages("corrplot", dependencies = TRUE)
#install.packages("ISLR")
library(psych)
library(corrplot)
library("ISLR")
library("SmartEDA")
library(GGally)
library(caret)
library(qwraps2)
library(plotly)
library(dplyr)
library(ggplot2)
library(stats)

#Configuring working directory
setwd("C:/Users/Sazee/Desktop/BUS5PA/Assignment 1")

# Loading the dataset.
df <- read.csv(file="used_car.csv", header=TRUE, sep=",")

# Initial look at the dataset
head(df)
tail(df)

# Examining the summary statistics of the dataset
summary(df)

##################################################################################################
# Q1
# c. Carry out and demonstrate data transformation where necessary.

# Transforming categorical (ordinal) variables in to numerical:

# Condition- (Levels: Fair -> 1, Good -> 2, Excellent-> 3)
df$condition <- factor(df$condition, levels=c("Fair","Good", "Excellent"), labels=c(1,2,3))

#engine_cylinders- (Levels: 3 Cylinders -> 3, 4 Cylinders -> 4, 
#5 Cylinders -> 5, 6 Cylinders -> 6, 8 Cylinders -> 8, 12 Cylinders -> 12)
df$engine_cylinders <- factor(df$engine_cylinders, levels=c("3 Cylinders","4 Cylinders","5 Cylinders","6 Cylinders","8 Cylinders","12 Cylinders"), labels=c(3,4,5,6,8,12))

#maximum_seating- (Levels: 4 seats -> 4, 5 seats -> 5, 6 seats -> 6, 7 seats -> 7, 8 seats -> 8)
df$maximum_seating <- factor(df$maximum_seating, levels=c("4 seats","5 seats","6 seats","7 seats","8 seats"), labels=c(4,5,6,7,8))

# owner_count- (Levels: 1 owner -> 1, 2 owners -> 2, 3 owners -> 3, 4 owners -> 4, 5 owners -> 5, 6 owners -> 6, 7 owners -> 7, 8 owners -> 8, 9 owners -> 9)
df$owner_count <- factor(df$owner_count, levels=c("1","2", "3", "4", "5", "6", "7", "8", "9"), labels=c(1,2,3,4,5,6,7,8,9))

# year- (Levels: "1990" is 31, "1997" is 24, "1998" is 23, "1999" is 22, "2000" is 21, "2001" is 20, "2002" is 19, "2003" is 18, 
#"2004" is 17, "2005" is 16, "2006" is 15, "2007" is 14, "2008" is 13, "2009" is 12, "2010" is 11, "2011" is 10, "2012" is 9, 
#"2013" is 8, "2014" is 7, "2015" is 6, "2016" is 5, "2017" is 4, "2018" is 3, "2019" is 2, "2020" is 1, "2021" is 0).
df$age_car <- factor(df$year, levels=c("1990","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021"), labels=c(31,24,23,22,21,20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0))

#Transforming age into a numeric parameter
df$condition = as.numeric(as.character(df$condition))
df$engine_cylinders = as.numeric(as.character(df$engine_cylinders))
df$maximum_seating = as.numeric(as.character(df$maximum_seating))
df$owner_count = as.numeric(as.character(df$owner_count))
df$age_car = as.numeric(as.character(df$age_car))

# Transform categorical (nominal) variables in to numerical. We will use One-hot encoding technique:

# body_type- (Categories: Hatchback, Sedan, SUV / Crossover)
df$body_type_Hatchback <- as.numeric(df$body_type == "Hatchback")
df$body_type_Sedan <- as.numeric(df$body_type == "Sedan")
df$body_type_SUV_Crossover <- as.numeric(df$body_type == "SUV / Crossover")

# fuel_type- (Categories: Diesel/Gasoline/Hybrid)
df$fuel_type_Diesel <- as.numeric(df$fuel_type == "Diesel")
df$fuel_type_Gasoline <- as.numeric(df$fuel_type == "Gasoline")
df$fuel_type_Hybrid <- as.numeric(df$fuel_type == "Hybrid")

# make_name- (Categories: Audi, BMW, Ford, Mercedes-Benz, Toyota, Volkswagen)
df$make_name_Audi <- as.numeric(df$make_name == "Audi")
df$make_name_BMW <- as.numeric(df$make_name == "BMW")
df$make_name_Ford <- as.numeric(df$make_name == "Ford")
df$make_name_Merc_Benz <- as.numeric(df$make_name == "Mercedes-Benz")
df$make_name_Toyota <- as.numeric(df$make_name == "Toyota")
df$make_name_Volkswagen <- as.numeric(df$make_name == "Volkswagen")

# salvage: Categories- (Categories: TRUE, FALSE)
df$salvage_True <- as.numeric(df$salvage == "TRUE")
df$salvage_False <- as.numeric(df$salvage == "FALSE")

# transmission- (Categories: Automatic, Continuously Variable Transmission, Dual clutch, Manual)
df$transmission_Automatic <- as.numeric(df$transmission == "Automatic")  
df$transmission_Continuously_Variable_Transmission <- as.numeric(df$transmission == "Continuously Variable Transmission")
df$transmission_Dual_Clutch <- as.numeric(df$transmission == "Dual Clutch")
df$transmission_Manual <- as.numeric(df$transmission == "Manual")

# wheel_system- (Categories: All- Wheel Drive, Four- Wheel Drive, Front- Wheel Drive, Rear- Wheel Drive)
df$wheel_system_All_Wheel_Drive <- as.numeric(df$wheel_system == "All-Wheel Drive")
df$wheel_system_Four_Wheel_Drive <- as.numeric(df$wheel_system == "Four-Wheel Drive")
df$wheel_system_Front_Wheel_Drive <- as.numeric(df$wheel_system == "Front-Wheel Drive")
df$wheel_system_Rear_Wheel_Drive <- as.numeric(df$wheel_system == "Rear-Wheel Drive")

#Review the data set and statistics
View(df)
summary(df)

############################################################################################################

#Q2
#a.
#Calculating , median, max, and standard deviation for each of the continuous variables

#Mean
mean(df$price)
mean(df$back_legroom)
mean(df$city_fuel_economy, na.rm = TRUE)
mean(df$daysonmarket)
mean(df$engine_displacement)
mean(df$front_legroom, na.rm = TRUE)
mean(df$fuel_tank_volume)
mean(df$height)
mean(df$highway_fuel_economy)
mean(df$horsepower)
mean(df$length)
mean(df$mileage, na.rm = TRUE)
mean(df$wheelbase)
mean(df$width)
mean(df$age_car)
mean(df$condition)
mean(df$owner_count)
mean(df$maximum_seating)
mean(df$engine_cylinders)


#Median
median(df$price)
median(df$back_legroom)
median(df$city_fuel_economy, na.rm = TRUE)
median(df$daysonmarket)
median(df$engine_displacement)
median(df$front_legroom, na.rm = TRUE)
median(df$fuel_tank_volume)
median(df$height)
median(df$highway_fuel_economy)
median(df$horsepower)
median(df$length)
median(df$mileage, na.rm = TRUE)
median(df$wheelbase)
median(df$width)
median(df$age_car)
median(df$condition)
median(df$owner_count)
median(df$maximum_seating)
median(df$engine_cylinders)


#Max
max(df$price)
max(df$back_legroom)
max(df$city_fuel_economy, na.rm = TRUE)
max(df$daysonmarket)
max(df$engine_displacement)
max(df$front_legroom, na.rm = TRUE)
max(df$fuel_tank_volume)
max(df$height)
max(df$highway_fuel_economy)
max(df$horsepower)
max(df$length)
max(df$mileage, na.rm = TRUE)
max(df$wheelbase)
max(df$width)
max(df$age_car)
max(df$condition)
max(df$owner_count)
max(df$maximum_seating)
max(df$engine_cylinders)

#Standard Deviation
sd(df$price)
sd(df$back_legroom)
sd(df$city_fuel_economy, na.rm = TRUE)
sd(df$daysonmarket)
sd(df$engine_displacement)
sd(df$front_legroom, na.rm = TRUE)
sd(df$fuel_tank_volume)
sd(df$height)
sd(df$highway_fuel_economy)
sd(df$horsepower)
sd(df$length)
sd(df$mileage, na.rm = TRUE)
sd(df$wheelbase)
sd(df$width)
sd(df$age_car)
sd(df$condition)
sd(df$owner_count)
sd(df$maximum_seating)
sd(df$engine_cylinders)

#Calculating the count for each categorical variable.

# Count for "condition" variable.
sum(df$condition == "1")
sum(df$condition == "2")
sum(df$condition == "3")
total_condition = sum(df$condition == "1") + sum(df$condition == "2") + sum(df$condition == "3")
cat("Total of number of values in condition parameter is:", total_condition)

# We can also check the number of values in each category using summary command.
summary(df$condition)

# Count for "engine_cylinders" variable.
sum(df$engine_cylinders == "3")
sum(df$engine_cylinders == "4")
sum(df$engine_cylinders == "5")
sum(df$engine_cylinders == "6")
sum(df$engine_cylinders == "8")
sum(df$engine_cylinders == "12")
total_eng_cyl = sum(df$engine_cylinders == "3") + sum(df$engine_cylinders == "4") + sum(df$engine_cylinders == "5") + sum(df$engine_cylinders == "6") + sum(df$engine_cylinders == "8") + sum(df$engine_cylinders == "12")
cat("Total of number of values in engine_cylinders parameter is:", total_eng_cyl)

# We can also check the number of values in each category using summary command.
summary(df$engine_cylinders)

# Count for "maximum_seating" variable.
sum(df$maximum_seating == "4")
sum(df$maximum_seating == "5")
sum(df$maximum_seating == "6")
sum(df$maximum_seating == "7")
sum(df$maximum_seating == "8")
total_max_seating = sum(df$maximum_seating == "4") + sum(df$maximum_seating == "5") + sum(df$maximum_seating == "6") + sum(df$maximum_seating == "7") + sum(df$maximum_seating == "8")
cat("Total of number of values in maximum_seating parameter is:", total_max_seating)

# We can also check the number of values in each category using summary command.
summary(df$maximum_seating)

# Count for "owner_count" variable.
sum(df$owner_count == "1")
sum(df$owner_count == "2")
sum(df$owner_count == "3")
sum(df$owner_count == "4")
sum(df$owner_count == "5")
sum(df$owner_count == "6")
sum(df$owner_count == "7")
sum(df$owner_count == "8")
sum(df$owner_count == "9")
total_owner_count = sum(df$owner_count == "1") + sum(df$owner_count == "2") + sum(df$owner_count == "3") + sum(df$owner_count == "4") + sum(df$owner_count == "5") + sum(df$owner_count == "6") + sum(df$owner_count == "7") + sum(df$owner_count == "8") + sum(df$owner_count == "9")
cat("Total of number of values in owner_count parameter is:", total_owner_count)

# We can also check the number of values in each category using summary command.
summary(df$owner_count)

# Count for "body_type" variable.
sum(df$body_type_Hatchback)
sum(df$body_type_Sedan)
sum(df$body_type_SUV_Crossover)
total_body_type = sum(df$body_type_Hatchback) + sum(df$body_type_Sedan) + sum(df$body_type_SUV_Crossover)
cat("Total of number of values in body_type parameter is:", total_body_type)

# Count for "fuel_type" variable.
sum(df$fuel_type_Diesel)
sum(df$fuel_type_Gasoline)
sum(df$fuel_type_Hybrid)
total_fuel_type = sum(df$fuel_type_Diesel) + sum(df$fuel_type_Gasoline) + sum(df$fuel_type_Hybrid)
cat("Total of number of values in fuel_type parameter is:", total_fuel_type)

# Count for "make_name" variable.
sum(df$make_name_Audi)
sum(df$make_name_BMW)
sum(df$make_name_Ford)
sum(df$make_name_Merc_Benz)
sum(df$make_name_Toyota)
sum(df$make_name_Volkswagen)
total_make_name = sum(df$make_name_Audi) + sum(df$make_name_BMW) + sum(df$make_name_Ford) + sum(df$make_name_Merc_Benz) + sum(df$make_name_Toyota) + sum(df$make_name_Volkswagen)
cat("Total of number of values in make_name parameter is:", total_make_name)

# Count for "wheel_system" variable.
sum(df$wheel_system_All_Wheel_Drive)
sum(df$wheel_system_Four_Wheel_Drive)
sum(df$wheel_system_Front_Wheel_Drive)
sum(df$wheel_system_Rear_Wheel_Drive)
total_wheel_system = sum(df$wheel_system_All_Wheel_Drive) + sum(df$wheel_system_Four_Wheel_Drive) + sum(df$wheel_system_Front_Wheel_Drive) + sum(df$wheel_system_Rear_Wheel_Drive)
cat("Total of number of values in wheel_system parameter is:", total_wheel_system)

###########################################################################################
#Q3

# Exploring the variables with histogram (omit missing values NA).
hist(df$price, breaks= 154, col = 'Orange')
hist(df$back_legroom, breaks = 154, col = 'Orange')
hist(df$city_fuel_economy, breaks= 154, col = 'Orange', na.rm = TRUE)
hist(df$daysonmarket, breaks= 154, col = 'Orange')
hist(df$engine_displacement, breaks= 154, col = 'Orange')
hist(df$front_legroom, breaks= 154, col = 'Orange', na.rm = TRUE)
hist(df$fuel_tank_volume, breaks= 154, col = 'Orange')
hist(df$height, breaks= 154, col = 'Orange')
hist(df$highway_fuel_economy, breaks= 154, col = 'Orange')
hist(df$horsepower, breaks= 154, col = 'Orange')
hist(df$length, breaks= 154, col = 'Orange')
hist(df$mileage, breaks= 154, col = 'Orange', na.rm = TRUE)
hist(df$wheelbase, breaks= 154, col = 'Orange')
hist(df$width, breaks= 154, col = 'Orange')
hist(df$age_car, breaks= 154, col = 'Orange')
hist(df$condition, breaks= 154, col = 'Orange')
hist(df$maximum_seating, breaks= 154, col = 'Orange')
hist(df$owner_count, breaks= 154, col = 'Orange')
hist(df$engine_cylinders, breaks= 154, col = 'Orange')


# Summary statistics

options(qwraps2_markup = "markdown")
summary_table(df)

# a.

# Checking the measures of variability

#Variance
var(df$mileage, na.rm = TRUE)
var(df$price)
var(df$engine_displacement)
var(df$horsepower)
var(df$daysonmarket)
var(df$length)
var(df$city_fuel_economy, na.rm = TRUE)
var(df$highway_fuel_economy)
var(df$width)
var(df$height)
var(df$wheelbase)
var(df$fuel_tank_volume)
var(df$back_legroom)
var(df$age_car)
var(df$front_legroom, na.rm = TRUE)
var(df$condition)
var(df$maximum_seating)
var(df$owner_count)
var(df$engine_cylinders)

#Range
range_mileage = max(df$mileage, na.rm = TRUE) - min(df$mileage, na.rm = TRUE)
range_mileage
range_price = max(df$price) - min(df$price)
range_price
range_eng_displacement = max(df$engine_displacement) - min(df$engine_displacement)
range_eng_displacement
range_horsepower = max(df$horsepower) - min(df$horsepower)
range_horsepower
range_daysonmarket = max(df$daysonmarket) - min(df$daysonmarket)
range_daysonmarket
range_length = max(df$length) - min(df$length)
range_length
range_city_fuel_economy = max(df$city_fuel_economy, na.rm = TRUE) - min(df$city_fuel_economy, na.rm = TRUE)
range_city_fuel_economy
range_highway_fuel_economy = max(df$highway_fuel_economy) - min(df$highway_fuel_economy)
range_highway_fuel_economy
range_width = max(df$width) - min(df$width)
range_width
range_height = max(df$height) - min(df$height)
range_height
range_wheelbase = max(df$wheelbase) - min(df$wheelbase)
range_wheelbase
range_fuel_tank_volume = max(df$fuel_tank_volume) - min(df$fuel_tank_volume)
range_fuel_tank_volume
range_age_car = max(df$age_car) - min(df$age_car)
range_age_car
range_back_legroom = max(df$back_legroom) - min(df$back_legroom)
range_back_legroom
range_front_legroom = max(df$front_legroom, na.rm = TRUE) - min(df$front_legroom, na.rm = TRUE)
range_front_legroom
range_condition = max(df$condition) - min(df$condition)
range_condition
range_maximum_seating = max(df$maximum_seating) - min(df$maximum_seating)
range_maximum_seating
range_engine_cylinders = max(df$engine_cylinders) - min(df$engine_cylinders)
range_engine_cylinders
range_owner_count = max(df$owner_count) - min(df$owner_count)
range_owner_count


# Calculating min, 1st quartile, median, 3rd quartile, max
fivenum(df$mileage, na.rm = TRUE)
fivenum(df$price)
fivenum(df$engine_displacement)
fivenum(df$horsepower)
fivenum(df$daysonmarket)
fivenum(df$length)
fivenum(df$city_fuel_economy, na.rm = TRUE)
fivenum(df$highway_fuel_economy)
fivenum(df$width)
fivenum(df$height)
fivenum(df$wheelbase)
fivenum(df$fuel_tank_volume)
fivenum(df$back_legroom)
fivenum(df$front_legroom, na.rm = TRUE)
fivenum(df$age_car)
fivenum(df$condition)
fivenum(df$maximum_seating)
fivenum(df$owner_count)
fivenum(df$engine_cylinders)

#Interquartile Range
IQR(df$mileage, na.rm = TRUE)
IQR(df$price)
IQR(df$engine_displacement)
IQR(df$horsepower)
IQR(df$daysonmarket)
IQR(df$length)
IQR(df$city_fuel_economy, na.rm = TRUE)
IQR(df$highway_fuel_economy)
IQR(df$width)
IQR(df$height)
IQR(df$wheelbase)
IQR(df$fuel_tank_volume)
IQR(df$back_legroom)
IQR(df$front_legroom, na.rm = TRUE)
IQR(df$age_car)
IQR(df$maximum_seating)
IQR(df$engine_cylinders)
IQR(df$owner_count)
IQR(df$condition)

#Standard Deviation
sd(df$mileage, na.rm = TRUE)
sd(df$price)
sd(df$engine_displacement)
sd(df$horsepower)
sd(df$daysonmarket)
sd(df$length)
sd(df$city_fuel_economy, na.rm = TRUE)
sd(df$highway_fuel_economy)
sd(df$width)
sd(df$height)
sd(df$wheelbase)
sd(df$fuel_tank_volume)
sd(df$back_legroom)
sd(df$age_car)
sd(df$front_legroom, na.rm = TRUE)
sd(df$engine_cylinders)
sd(df$maximum_seating)
sd(df$owner_count)
sd(df$condition)


#c. Are there any values that seem extreme? 

# We can check extreme values using boxplot
boxplot(df$mileage, na.rm = TRUE)
boxplot(df$price)
boxplot(df$engine_displacement)
boxplot(df$horsepower)
boxplot(df$daysonmarket)
boxplot(df$length)
boxplot(df$city_fuel_economy, na.rm = TRUE)
boxplot(df$highway_fuel_economy)
boxplot(df$width)
boxplot(df$height)
boxplot(df$wheelbase)
boxplot(df$fuel_tank_volume)
boxplot(df$back_legroom)
boxplot(df$age_car)
boxplot(df$condition)
boxplot(df$owner_count)
boxplot(df$maximum_seating)
boxplot(df$engine_cylinders)
boxplot(df$front_legroom, na.rm = TRUE)

# #Treating outliers
# IQR_mileage = IQR(df$mileage, na.rm = TRUE)
# IQR_price = IQR(df$price)
# IQR_engine_displacement = IQR(df$engine_displacement)
# IQR_horsepower = IQR(df$horsepower)
# IQR_daysonmarket = IQR(df$daysonmarket)
# IQR_length = IQR(df$length)
# IQR_city_fuel_economy = IQR(df$city_fuel_economy, na.rm = TRUE)
# IQR_highway_fuel_economy = IQR(df$highway_fuel_economy)
# IQR_width = IQR(df$width)
# IQR_height = IQR(df$height)
# IQR_wheelbase = IQR(df$wheelbase)
# IQR_fuel_tank_volume = IQR(df$fuel_tank_volume)
# IQR_back_legroom = IQR(df$back_legroom)
# IQR_age_car = IQR(df$age_car)
# IQR_condition = IQR(df$condition)
# IQR_owner_count = IQR(df$owner_count)
# IQR_maximum_seating = IQR(df$maximum_seating)
# IQR_engine_cylinders = IQR(df$engine_cylinders)
# IQR_front_legroom = IQR(df$front_legroom, na.rm = TRUE)
# 
# first_quantile_price = quantile(df$price, 0.25)
# up_outliers_price = first_quantile_price + 1.5 * IQR_price
# up_outliers_price
# third_quantile_price = quantile(df$price, 0.75)
# low_outliers_price = third_quantile_price - 1.5 * IQR_price
# low_outliers_price
# 
# first_quantile_mileage = quantile(df$mileage, 0.2, na.rm = TRUE)
# up_outliers_mileage = first_quantile_mileage + 1.5 * IQR_mileage
# up_outliers_mileage
# third_quantile_mileage = quantile(df$mileage, 0.75, na.rm = TRUE)
# low_outliers_mileage = third_quantile_mileage - 1.5 * IQR_mileage
# low_outliers_mileage
# 
# first_quantile_engine_displacement = quantile(df$engine_displacement, 0.25)
# up_outliers_engine_displacement = first_quantile_engine_displacement + 1.5 * IQR_engine_displacement
# up_outliers_engine_displacement
# third_quantile_engine_displacement = quantile(df$engine_displacement, 0.75)
# low_outliers_engine_displacement = third_quantile_engine_displacement - 1.5 * IQR_engine_displacement
# low_outliers_engine_displacement
# 
# first_quantile_horsepower = quantile(df$horsepower, 0.25)
# up_outliers_horsepower = first_quantile_horsepower + 1.5 * IQR_horsepower
# up_outliers_horsepower
# third_quantile_horsepower = quantile(df$horsepower, 0.75)
# low_outliers_horsepower = third_quantile_horsepower - 1.5 * IQR_horsepower
# low_outliers_horsepower
# 
# first_quantile_daysonmarket = quantile(df$daysonmarket, 0.25)
# up_outliers_daysonmarket = first_quantile_daysonmarket + 1.5 * IQR_daysonmarket
# up_outliers_daysonmarket
# third_quantile_daysonmarket = quantile(df$daysonmarket, 0.75)
# low_outliers_daysonmarket = third_quantile_daysonmarket - 1.5 * IQR_daysonmarket
# low_outliers_daysonmarket
# 
# first_quantile_length = quantile(df$length, 0.25)
# up_outliers_length = first_quantile_length + 1.5 * IQR_length
# up_outliers_length
# third_quantile_length = quantile(df$length, 0.75)
# low_outliers_length = third_quantile_length - 1.5 * IQR_length
# low_outliers_length
# 
# first_quantile_city_fuel_economy = quantile(df$city_fuel_economy, 0.25, na.rm = TRUE)
# up_outliers_city_fuel_economy = first_quantile_city_fuel_economy + 1.5 * IQR_city_fuel_economy
# up_outliers_city_fuel_economy
# third_quantile_city_fuel_economy = quantile(df$city_fuel_economy, 0.75, na.rm = TRUE)
# low_outliers_city_fuel_economy = third_quantile_city_fuel_economy - 1.5 * IQR_city_fuel_economy
# low_outliers_city_fuel_economy
# 
# first_quantile_highway_fuel_economy = quantile(df$highway_fuel_economy, 0.25)
# up_outliers_highway_fuel_economy = first_quantile_highway_fuel_economy + 1.5 * IQR_highway_fuel_economy
# up_outliers_highway_fuel_economy
# third_quantile_highway_fuel_economy = quantile(df$highway_fuel_economy, 0.75)
# low_outliers_highway_fuel_economy = third_quantile_highway_fuel_economy - 1.5 * IQR_highway_fuel_economy
# low_outliers_highway_fuel_economy
# 
# first_quantile_height = quantile(df$height, 0.25)
# up_outliers_height = first_quantile_height + 1.5 * IQR_height
# up_outliers_height
# third_quantile_height = quantile(df$height, 0.75)
# low_outliers_height = third_quantile_height - 1.5 * IQR_height
# low_outliers_height
# 
# first_quantile_wheelbase = quantile(df$wheelbase, 0.25)
# up_outliers_wheelbase = first_quantile_wheelbase + 1.5 * IQR_wheelbase
# up_outliers_wheelbase
# third_quantile_wheelbase = quantile(df$wheelbase, 0.75)
# low_outliers_wheelbase = third_quantile_wheelbase - 1.5 * IQR_wheelbase
# low_outliers_wheelbase
# 
# first_quantile_fuel_tank_volume = quantile(df$fuel_tank_volume, 0.25)
# up_outliers_fuel_tank_volume = first_quantile_fuel_tank_volume + 1.5 * IQR_fuel_tank_volume
# up_outliers_fuel_tank_volume
# third_quantile_fuel_tank_volume = quantile(df$fuel_tank_volume, 0.75)
# low_outliers_fuel_tank_volume = third_quantile_fuel_tank_volume - 1.5 * IQR_fuel_tank_volume
# low_outliers_fuel_tank_volume
# 
# first_quantile_back_legroom = quantile(df$back_legroom, 0.25)
# up_outliers_back_legroom = first_quantile_back_legroom + 1.5 * IQR_back_legroom
# up_outliers_back_legroom
# third_quantile_back_legroom = quantile(df$back_legroom, 0.75)
# low_outliers_back_legroom = third_quantile_back_legroom - 1.5 * IQR_back_legroom
# low_outliers_back_legroom
#
# first_quantile_age_car = quantile(df$age_car, 0.25)
# up_outliers_age_car = first_quantile_age_car + 1.5 * IQR_age_car
# up_outliers_age_car
# third_quantile_age_car = quantile(df$age_car, 0.75)
# low_outliers_age_car = third_quantile_age_car - 1.5 * IQR_age_car
# low_outliers_age_car
# 
# first_quantile_front_legroom = quantile(df$front_legroom, 0.25, na.rm = TRUE)
# up_outliers_front_legroom = first_quantile_front_legroom + 1.5 * IQR_front_legroom
# up_outliers_front_legroom
# third_quantile_front_legroom = quantile(df$front_legroom, 0.75, na.rm = TRUE)
# low_outliers_front_legroom = third_quantile_front_legroom - 1.5 * IQR_front_legroom
# low_outliers_front_legroom
#
# first_quantile_owner_count = quantile(df$owner_count, 0.25, na.rm = TRUE)
# up_outliers_owner_count = first_quantile_owner_count + 1.5 * IQR_owner_count
# up_outliers_owner_count
# third_quantile_owner_count = quantile(df$owner_count, 0.75, na.rm = TRUE)
# low_outliers_owner_count = third_quantile_owner_count - 1.5 * IQR_owner_count
# low_outliers_owner_count
#
# first_quantile_engine_cylinders = quantile(df$engine_cylinders, 0.25, na.rm = TRUE)
# up_outliers_engine_cylinders = first_quantile_engine_cylinders + 1.5 * IQR_engine_cylinders
# up_outliers_engine_cylinders
# third_quantile_engine_cylinders = quantile(df$engine_cylinders, 0.75, na.rm = TRUE)
# low_outliers_engine_cylinders = third_quantile_engine_cylinders - 1.5 * IQR_engine_cylinders
# low_outliers_engine_cylinders
#
# first_quantile_maximum_seating = quantile(df$maximum_seating, 0.25, na.rm = TRUE)
# up_outliers_maximum_seating = first_quantile_maximum_seating + 1.5 * IQR_maximum_seating
# up_outliers_maximum_seating
# third_quantile_maximum_seating = quantile(df$maximum_seating, 0.75, na.rm = TRUE)
# low_outliers_maximum_seating = third_quantile_maximum_seating - 1.5 * IQR_maximum_seating
# low_outliers_maximum_seating
#
# first_quantile_condition = quantile(df$condition, 0.25, na.rm = TRUE)
# up_outliers_condition = first_quantile_condition + 1.5 * IQR_condition
# up_outliers_condition
# third_quantile_condition = quantile(df$condition, 0.75, na.rm = TRUE)
# low_outliers_condition = third_quantile_condition - 1.5 * IQR_condition
# low_outliers_condition
# 
# df = subset(df, df$price >= low_outliers_price & df$price <= up_outliers_price & df$mileage >= low_outliers_mileage & df$mileage <= up_outliers_mileage & df$mileage >= low_outliers_mileage & df$mileage <= up_outliers_mileage & df$engine_displacement >= low_outliers_engine_displacement & df$engine_displacement <= up_outliers_engine_displacement & df$horsepower >= low_outliers_horsepower & df$horsepower <= up_outliers_horsepower & df$daysonmarket >= low_outliers_daysonmarket & df$daysonmarket <= up_outliers_daysonmarket & df$length >= low_outliers_length & df$length <= up_outliers_length & df$city_fuel_economy >= low_outliers_city_fuel_economy & df$city_fuel_economy <= up_outliers_city_fuel_economy & df$highway_fuel_economy >= low_outliers_highway_fuel_economy & df$highway_fuel_economy <= up_outliers_highway_fuel_economy & df$height >= low_outliers_height & df$height <= up_outliers_height & df$wheelbase >= low_outliers_wheelbase & df$wheelbase <= up_outliers_wheelbase & df$fuel_tank_volume >= low_outliers_fuel_tank_volume & df$fuel_tank_volume <= up_outliers_fuel_tank_volume & df$back_legroom >= low_outliers_back_legroom & df$back_legroom <= up_outliers_back_legroom & df$front_legroom >= low_outliers_front_legroom & df$front_legroom <= up_outliers_front_legroom & df$age_car >= low_outliers_age_car & df$age_car <= up_outliers_age_car & df$condition >= low_outliers_condition & df$condition <= up_outliers_condition & df$owner_count >= low_outliers_owner_count & df$owner_count <= up_outliers_owner_count & df$maximum_seating >= low_outliers_maximum_seating & df$maximum_seating <= up_outliers_maximum_seating & df$engine_cylinders >= low_outliers_engine_cylinders & df$engine_cylinders <= up_outliers_engine_cylinders)
# 
# hist(df$price, breaks= 154, col = 'Orange')
# hist(df$back_legroom, breaks = 154, col = 'Orange')
# hist(df$city_fuel_economy, breaks= 154, col = 'Orange', na.rm = TRUE)
# hist(df$daysonmarket, breaks= 154, col = 'Orange')
# hist(df$engine_displacement, breaks= 154, col = 'Orange')
# hist(df$front_legroom, breaks= 154, col = 'Orange', na.rm = TRUE)
# hist(df$fuel_tank_volume, breaks= 154, col = 'Orange')
# hist(df$height, breaks= 154, col = 'Orange')
# hist(df$highway_fuel_economy, breaks= 154, col = 'Orange')
# hist(df$horsepower, breaks= 154, col = 'Orange')
# hist(df$length, breaks= 154, col = 'Orange')
# hist(df$mileage, breaks= 154, col = 'Orange', na.rm = TRUE)
# hist(df$wheelbase, breaks= 154, col = 'Orange')
# hist(df$width, breaks= 154, col = 'Orange')
# hist(df$age_car, breaks= 154, col = 'Orange')
# hist(df$condition, breaks= 154, col = 'Orange')
# hist(df$maximum_seating, breaks= 154, col = 'Orange')
# hist(df$owner_count, breaks= 154, col = 'Orange')
# hist(df$engine_cylinders, breaks= 154, col = 'Orange')

# 
# boxplot(df$mileage, na.rm = TRUE) + title( main="mileage")
# boxplot(df$price) + title( main="new_price")
# boxplot(df$engine_displacement) + title( main="engine_displacement")
# boxplot(df$horsepower) + title( main="horsepower")
# boxplot(df$daysonmarket) + title( main="daysonmarket")
# boxplot(df$length) + title( main="length")
# boxplot(df$fuel_economy, na.rm = TRUE) + title( main="city_fuel_economy")
# boxplot(df$highway_fuel_economy) + title( main="highway_fuel_economy")
# boxplot(df$width) + title( main="width")
# boxplot(df$height) + title( main="height")
# boxplot(df$wheelbase) + title( main="wheelbase")
# boxplot(df$fuel_tank_volume) + title( main="fuel_tank_volume")
# boxplot(df$back_legroom) + title( main="back_legroom"
# boxplot(df$age_car) + title( main="age_car"))
# boxplot(df$condition) + title( main="condition"))
# boxplot(df$maximum_seating) + title( main="maximum_seating"))
# boxplot(df$owner_count) + title( main="owner_count"))
# boxplot(df$engine_cylinders) + title( main="engine_cylinders"))
# boxplot(df$front_legroom, na.rm = TRUE) + title( main="front_legroom")


#Apply log transformation

df$mileage <- log(df$mileage)
df$price <- log(df$price)
df$engine_displacement <- log(df$engine_displacement)
df$horsepower <- log(df$horsepower)
df$daysonmarket <- log(df$daysonmarket)
df$length <- log(df$length)
df$city_fuel_economy <- log(df$city_fuel_economy)
df$highway_fuel_economy <- log(df$highway_fuel_economy)
df$width <- log(df$width)
df$height <- log(df$height)
df$wheelbase <- log(df$wheelbase)
df$fuel_tank_volume <- log(df$fuel_tank_volume)
df$back_legroom <- log(df$back_legroom)
df$age_car <- log(df$age_car)
df$condition <- log(df$condition)
df$maximum_seating <- log(df$maximum_seating)
df$owner_count <- log(df$owner_count)
df$engine_cylinders <- log(df$engine_cylinders)
df$front_legroom <- log(df$front_legroom)

summary(df)

hist(df$price, breaks= 154, col = 'Orange')
hist(df$back_legroom, breaks = 154, col = 'Orange')
hist(df$city_fuel_economy, breaks= 154, col = 'Orange', na.rm = TRUE)
hist(df$daysonmarket, breaks= 154, col = 'Orange')
hist(df$engine_displacement, breaks= 154, col = 'Orange')
hist(df$front_legroom, breaks= 154, col = 'Orange', na.rm = TRUE)
hist(df$fuel_tank_volume, breaks= 154, col = 'Orange')
hist(df$height, breaks= 154, col = 'Orange')
hist(df$highway_fuel_economy, breaks= 154, col = 'Orange')
hist(df$horsepower, breaks= 154, col = 'Orange')
hist(df$length, breaks= 154, col = 'Orange')
hist(df$mileage, breaks= 154, col = 'Orange', na.rm = TRUE)
hist(df$wheelbase, breaks= 154, col = 'Orange')
hist(df$width, breaks= 154, col = 'Orange')
hist(df$condition, breaks= 154, col = 'Orange')
hist(df$maximum_seating, breaks= 154, col = 'Orange')
hist(df$owner_count, breaks= 154, col = 'Orange')
hist(df$engine_cylinders, breaks= 154, col = 'Orange')
hist(df$age_car, breaks= 154, col = 'Orange')


boxplot(df$mileage, na.rm = TRUE) + title( main="new_mileage")
boxplot(df$price) + title( main="new_price")
boxplot(df$engine_displacement) + title( main="new_engine_displacement")
boxplot(df$horsepower) + title( main="new_horsepower")
boxplot(df$daysonmarket) + title( main="new_daysonmarket")
boxplot(df$length) + title( main="new_length")
boxplot(df$city_fuel_economy, na.rm = TRUE) + title( main="new_city_fuel_economy")
boxplot(df$highway_fuel_economy) + title( main="new_highway_fuel_economy")
boxplot(df$width) + title( main="new_width")
boxplot(df$height) + title( main="new_height")
boxplot(df$wheelbase) + title( main="new_wheelbase")
boxplot(df$fuel_tank_volume) + title( main="new_fuel_tank_volume")
boxplot(df$back_legroom) + title( main="new_back_legroom")
boxplot(df$age_car) + title( main="new_age_car")
boxplot(df$condition) + title( main="new_condition")
boxplot(df$maximum_seating) + title( main="new_maximum_seating")
boxplot(df$owner_count) + title( main="new_owner_count")
boxplot(df$engine_cylinders) + title( main="new_engine_cylinders")
boxplot(df$front_legroom, na.rm = TRUE) + title( main="new_front_legroom")

####################################################################################

# Q4
# a. Which, if any, of the variables have missing values? 

#Checking extreme values
a = sum(is.na(df))
a
summary(df)

#Restricting the value of daysofmarket and age_car to equal to or above zero. 
df = subset(df, df$daysonmarket >= 0)
df = subset(df, df$age_car >= 0)

# c. Apply the 3 methods of missing value and demonstrate the output (summary statistics and transformation plot) for each 
# method in (4-b). (hint: the objective is to identify the impact of using each of the methods you mentioned 
# in the 4-b on the summary statistics output above). Which method of handling missing values is most suitable for
# this data set? Discuss briefly referring to the data set.

# Replace NAs with a specific value, such as 0.
df_NA_zero <- df
is.na(df_NA_zero)
df_NA_zero[is.na(df_NA_zero)] <- 0
df_NA_zero[is.na(df_NA_zero)]
summary(df_NA_zero)

# Delete records with NAs
df_NA_deleted <- df[complete.cases(df),]
summary(df_NA_deleted)

# Replace with mean of the column (can bias sample and misinterpretation)
#Check how many NAs
summary(df$mileage)
summary(df$front_legroom)
summary(df$city_fuel_economy)

# Check column mean
mean(df$mileage, na.rm = TRUE) 
mean(df$city_fuel_economy, na.rm = TRUE) 
mean(df$front_legroom, na.rm = TRUE) 

# Check column locations have NAs
is.na(df$mileage)
is.na(df$city_fuel_economy) 
is.na(df$front_legroom) 

df$mileage[is.na(df$mileage)]
df$city_fuel_economy[is.na(df$city_fuel_economy)]
df$front_legroom[is.na(df$front_legroom)]

# Replacing missing values with the mean
df$mileage[is.na(df$mileage)] <- mean(df$mileage, na.rm = TRUE)
df$city_fuel_economy[is.na(df$city_fuel_economy)] <- mean(df$city_fuel_economy, na.rm = TRUE) 
df$front_legroom[is.na(df$front_legroom)] <- mean(df$front_legroom, na.rm = TRUE)
summary(df)

##############################################################################################################

#Q 5.
# a. Evaluate the correlations between the variables. 

# First we isolate the target variable which is price from the dataset. For this:
# Create a new variable for the targ price
target <- df$price
# Remove the target variable and create a new dataframe with only the 
# attributes.
used_car <- subset(df, select = -c(price, body_type, fuel_type, make_name, salvage, transmission, wheel_system, year))

# Analyze the correlation of attributes
# We explore the correlation between attributes in the dataset using ggcorr from GGally
ggcorr(used_car, label=TRUE)

# Other ways of visualizing correlation plots
cor.plot(used_car, numbers = TRUE, cex = .5)
# corclean <- cor(used_car)
# round(corclean, digits = 3)
# corrplot(corclean, method = "circle")

# Without manually identifying the correlated variables from the plots, 
# we can use the caret R package to automatically detect the highly 
# correlated variables based on a threshold value.

# Get the correlation matrix using caret package

# For this step we need to convert the dataset to the matrix format 
# and then get the cross-correlation among the variable.
M <- data.matrix(used_car)
corrM <- cor(M)
corrplot(corrM)

# Find the variables with higher cross-correlation

highlyCorrM <- findCorrelation(corrM, cutoff=0.6)
# get the column names that are highly correlated
names(used_car)[highlyCorrM]

# b. Which variables should be used for dimension reduction and why? Carry out dimensionality reduction
#The following variables are highly correlated. We can remove one variable so the inter-correlation between variables 
# will be minimum.
# [1] "city_fuel_economy"              "highway_fuel_economy"           "horsepower"                     "fuel_tank_volume"              
# [5] "width"                          "wheel_system_Front_Wheel_Drive" "engine_displacement"            "wheelbase"                     
# [9] "height"                         "transmission_Automatic"         "body_type_SUV_Crossover"        "fuel_type_Gasoline"            
# [13] "age_car"                        "salvage_False" 

#used_car_reduced <- subset(used_car, select = -c(height, city_fuel_economy, fuel_tank_volume, horsepower, length))

used_car_reduced <- used_car

# c. Explore the distribution of selected variables (from step 5-a) against the target variable. Explain.
ggcorr(used_car_reduced , label=TRUE)
cor.plot(used_car_reduced, numbers = TRUE, cex = .5)


# Merge the target variable back to the dataset
used_car_reduced$price <- target
summary(used_car_reduced)

ggcorr(used_car_reduced, label=TRUE)
cor.plot(used_car_reduced, numbers = TRUE, cex = .5)

ExpData(data=used_car_reduced,type=2, fun = c("mean", "median", "var"))

#Data Partition
smp_size <- floor(2/3 * nrow(used_car_reduced)) 
set.seed(2)

#Sample dataset
used_car_reduced <-
used_car_reduced[sample(nrow(used_car_reduced)), ]

# Create train and test datasets
used_car.train <- used_car_reduced[1:smp_size, ] 
used_car.test <- used_car_reduced[(smp_size+1):nrow(used_car_reduced), ] 
nrow(used_car.train)
nrow(used_car.test)

#Specifying target and input variables
# In this step we specify the target variable and input variables to the regression model. It is 
# specified in the format formula <- A ~ B + C + D in which A is the target variable name and B,C,D 
# are the names of inputs. 
# To specify all variables as input we can use '.' Instead of mentioning all attribute names. 
# Therefore, for this dataset we specify the formula as follows:
formula = price ~.

#fit the linear regression algorithm
model <- lm(formula = formula, data = used_car.train)

# Display the coefficients of the linear regression model
summary(model)$coefficients

#Regression equation
as.formula(
  paste0("y ~ ", round(coefficients(model)[1],2), " + ", 
         paste(sprintf("%.2f * %s",coefficients(model)[-1],
                       names(coefficients(model)[-1])), 
               collapse=" + ")
  )
)

# Make Predictions for test and train dataset
used_car.train$predicted.price <- predict(model, used_car.train)
used_car.test$predicted.price <- predict(model, used_car.test)

print("Actual Values")
head(used_car.test$price[1:5])
print("Predicted Values")
head(used_car.test$predicted.price[1:5])

# Plot Predicted values vs Actual values of the target variable
pl1 <-used_car.test %>%
ggplot(aes(price,predicted.price)) +
  geom_point(alpha=0.5) +
  stat_smooth(aes(colour='red')) +
  xlab('Actual value of price') +
  ylab('Predicted value of price')+
  theme_bw()
ggplotly(pl1)

#  Model assessment with R-Squared and Root Mean Squared Error (RMSE)

# Calculate the R-squared value

r_squared <- summary(model)$r.squared
print(paste("R Squared: ", r_squared))

# Calculate the Root Mean Squared Error (RMSE)

error <- used_car.test$price-used_car.test$predicted.price
rmse <- sqrt(mean(error^2))
print(paste("Root Mean Square Error: ", rmse))

# [1] "R Squared:  0.865726083321077"
# > 
#   > # Calculate the Root Mean Squared Error (RMSE)
#   > 
#   > error <- used_car.test$price-used_car.test$predicted.price
# > rmse <- sqrt(mean(error^2))
# > print(paste("Root Mean Square Error: ", rmse))
# [1] "Root Mean Square Error:  0.16724699370683"