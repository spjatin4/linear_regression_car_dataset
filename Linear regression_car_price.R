#### Learn how we apply linear regression to predict price of the car####


# Download the data set from https://archive.ics.uci.edu/ml/datasets/automobile 

#Load necessary libraries
library(MASS)
library(ggplot2)
library(car)
library(dplyr)
library(tidyr)
library(stringr)
library(corrplot)
library(PerformanceAnalytics)


########## Business objective ################
# 1. Build car price prediction model for Geely Auto 
# which can assist them to strategize their entry in American market and learn 
# the factors influencing car price #################

#Set default directory
setwd('~/Documents/iiitB - Analytics/Assignments/26th Nov/')

#Load the dataset
car <- read.csv('CarPrice_Assignment.csv',header = T)

# How the dataset looks
head(car)

# Understand the structure of the dataset
str(car)


#Check for missing values
missing_values <- car %>%
  summarise_all(funs(sum(is.na(.))/n()))
missing_values
#Conclusion: No missing values is found in the dataset


#Extract Car Make from CarName variable
car$carmake <- tolower(str_split_fixed(car$CarName," ",2)) # Converted to lower case for consistency
car$carmake <- as.factor(car$carmake[,1]) #Converted the variable into factor



# Summarise the dataset
summary(car)
str(car)

#Summarise the CarMake Variable
table(car$carmake)


#Noticed: CarMake names are mis-spelled and requires rectification
car$carmake <- with(car, ifelse(carmake=="maxda","mazda",
                                ifelse(carmake %in% c('vw','vokswagen'),"volkswagen",
                                       ifelse(carmake=="porcshce","porsche",
                                              ifelse(carmake=="toyouta","toyota",as.character(carmake))))))
# Again converted to factor type
car$carmake <- as.factor(car$carmake) 


####### Levels updation and converting categorical to numerical variable ##########--------------------------

levels(car$fueltype) <- c(0,1) #Changing the levels for fuel type to 0,1 (0 for diesel and 1 for gas)
car$fueltype <- as.numeric(levels(car$fueltype))[car$fueltype]

#Chaning the levels for aspiration to 0,1 (0 for turbo and 1 for std)
levels(car$aspiration) <- c(1,0) 
car$aspiration <- as.numeric(levels(car$aspiration))[car$aspiration]


#Chaning the levels for doornumber to 0,1 (0 for two and 1 for four)
levels(car$doornumber) <- c(1,0)
car$doornumber <- as.numeric(levels(car$doornumber))[car$doornumber]


#Chaning the levels for engine location to 0,1 (0 for rear and 1 for front)
levels(car$enginelocation) <- c(1,0)
car$enginelocation <- as.numeric(levels(car$enginelocation))[car$enginelocation]


# Since we would like to consider symboling as categorical variable, converting to factor type
car$symboling <- as.factor(car$symboling)


# Lets check the summary of car dataset.
summary(car)

########  Dummary variable creation for the variable whose levels are more than two----------

#Dummary variable for carbody variable ------------------------------------------------------
carbodydummy <- data.frame(model.matrix(~carbody, data = car))

# Remove X intercept column
carbodydummy <- carbodydummy[,-1]
View(carbodydummy)

#Merge carbody dummy with main dataset
car <- cbind(car,carbodydummy)

#Dummary variable for drivewheel variable ---------------------------------------------------
drivewheeldummy <- data.frame(model.matrix(~drivewheel, data = car))

# Remove X intercept column
drivewheeldummy <- drivewheeldummy[,-1]
View(drivewheeldummy)

#Merge drivewheel dummy with main dataset
car <- cbind(car,drivewheeldummy)

#Dummary variable for Enginetype variable ----------------------------------------------------
enginetypedummy <- data.frame(model.matrix(~enginetype, data = car))

# Remove X intercept column
enginetypedummy <- enginetypedummy[,-1]
View(enginetypedummy)

#Merge Enginetype dummy with main dataset
car <- cbind(car,enginetypedummy)


#Dummary variable for cylindernumber variable ------------------------------------------------
cylindernumberdummy <- data.frame(model.matrix(~cylindernumber, data = car))

# Remove X intercept column
cylindernumberdummy <- cylindernumberdummy[,-1]
View(cylindernumberdummy)

#Merge cylindernumber dummy with main dataset
car <- cbind(car,cylindernumberdummy)

#Dummary variable for fuelsystem variable ------------------------------------------------
fuelsystemdummy <- data.frame(model.matrix(~fuelsystem, data = car))

# Remove X intercept column
fuelsystemdummy <- fuelsystemdummy[,-1]
View(fuelsystemdummy)

#Merge fuelsystem dummy with main dataset
car <- cbind(car,fuelsystemdummy)

#Dummary variable for carmake variable ------------------------------------------------
carmakedummy <- data.frame(model.matrix(~carmake, data = car))

# Remove X intercept column
carmakedummy <- carmakedummy[,-1]
View(carmakedummy)

#Merge carmake dummy with main dataset
car <- cbind(car,carmakedummy)

#Dummary variable for symboling variable ------------------------------------------------
symbolingdummy <- data.frame(model.matrix(~symboling, data = car))

# Remove X intercept column
symbolingdummy <- symbolingdummy[,-1]
View(symbolingdummy)

#Merge symboling dummy with main dataset
car <- cbind(car,symbolingdummy)


#### Remove columns names which are not relevant for modeling  and also the original categorical variable---------------------
rmcol <- c('car_ID','symboling','CarName','carbody','drivewheel','enginetype','cylindernumber','fuelsystem','carmake')

car_final <- car[,!colnames(car) %in% rmcol]

#Check the structure of the dataset
str(car_final)



# Select the numerical variable for understanding correlation-------------------------------------------
corcol <- c('wheelbase','carlength','carwidth','carheight','curbweight','enginesize','boreratio','stroke',
            'compressionratio','horsepower','peakrpm','citympg','highwaympg','price')
corr_car <- car_final[,colnames(car_final) %in% corcol]

#Plot correlation matrix using Performance analytics library
chart.Correlation(corr_car) 

#Build correlation matrix table for numeric relation
corrplot(cor(corr_car),method = 'number')

#Conclusion of the correlation study
# 1. Its clear from the analysis that many variables are highly correlated
# 2. Curb weight highly correlated with carwidth, carlength, enginesize, horsepower, wheelbase
# 3. Curb weight, horsepower, carwidth, enginesize, carlength are higly correlated with Price (dependent variable)
# 4. This could also lead to multicollinearlity problem


################### Derived Metrics ################---------------------------------------------------

#Lets see the impact of curbweight to cars length
#cw2cl: curbweight / carlength
car_final$cw2cl <- car_final$curbweight/car_final$carlength

# Engine size to horsepower
#es2hp: enginsize / horsepower
car_final$es2hp <- car_final$enginesize/car_final$horsepower

# curbweight to Engine size
#cw2es: curbweight / enginesize
car_final$cw2es <- car_final$curbweight/car_final$enginesize



############ Model development stage ###### -----------------------------------------------------------

# We need to sample the data and split the sample into train and test dataset.

# Sampling process-------------------------------------------------------------------------------------
set.seed(100)

# Training set to have 70% of the main data set and balance to test set
indices <- sample(1:nrow(car_final),0.7*nrow(car_final)) 
train <- car_final[indices,]
test <- car_final[-indices,]

# Model 1 using linear regression----------------------------------------------------------------------

model_1 <- lm(price~. ,data = train, singular.ok = T)
summary(model_1)

#### Conclusion of Model 1:
# 1.there are lot of variables like fuel_type, aspiration etc whose p-value > 0.05
# 2.Tried runing VIF (variable inflation factor), however, got an error "aliased co-efficients in the model"
# 3.This error due to perfect collinear variable within the model.
# 4.R-square: ~97% and Adjusted R-square: ~94%


# Unable to check multicollinearity:"aliased co-efficients in the model"
# We used the function alias to find out the variable
alias(model_1)

# Identify the list of variables to removed from the formula.
attributes(alias(model_1)$Complete)$dimnames[[1]]

# Create a new object to store the formula and remove the perfect collinear variable.
m1_formula<-as.formula(paste(paste(deparse(alias(model_1)$Model),collapse = ""),
                             paste(attributes(alias(model_1)$Complete)$dimnames[[1]],collapse = "-"),sep="-"))

# Re-run the model to understand the relationship
model_1a <- lm(m1_formula,data=train)
summary(model_1a)
# Conclusion for model 1a:
# 1.We find many variables to be insignificant basis p-value, however, 
#             lets run VIF on the model and understand the multicollinearity
# 2. R-squre at ~98% and Adjusted R-square ~97%

# VIF on model 1a
vif(model_1a)
# Conclusion: VIF for compression_ratio and fueltype pretty high.

# Since there are so many variables in the model, 
# We can run stepAIC for getting best fitted model based on AIC----------------------



############ STEP AIC process ##################################

# We will run stepAIC on model_1a and then select the variables which are impacting AIC,
# We will also closing monitor the impact on R-square after running the stepAIC.

step_aic1 <- stepAIC(model_1a)

# We can check the formula for the best model by analysing the step_aic1 object
step_aic1$anova

# We can see the difference between final model and initial model (model_1a)

#Lets extract the formula from the final_model and build model_2 for further finetuning.
m2_formula<- as.formula(step_aic1$call)

# Build model_2 based on the final model extracted from StepAIC--------------------
model_2 <- lm(m2_formula, data=train)

# Check the model output.
summary(model_2)

#Conclusion model_2: 
# 1. Most of the variables are significant, however,
# carmakesaab, es2hp (engine size to horsepower ratio), enginetypeotor, fuelsystem2bbl are insignificant
# 2. Marginal change in R-square and Adj. R-square which means after
#    performing stepAIC, we have not seen any impact on R-square which is a good sign.

# Lets analyse the VIF on model_2

vif(model_2)
#Conclusion VIF on model_2: curbweight, cw2cl (curbweight ratio to carlength),carlength, enginesize, horsepower,
# have got higher value. For model 3 we will remove curbweight and then analyse the output.

# Lets build model_3 after removing curbweight-------------------------------------

# Lets modify the formula for model 3
m3_formula <- as.formula(paste(paste(deparse(m2_formula),collapse = ""),"curbweight",sep = "-"))

# build model 3

model_3 <- lm(m3_formula,data = train)

summary(model_3)

# Conclusion model_3: 
# 1. R-square and Adj.Rsquare are unchanged from model_2
# 2. Still there are few variables whose p_value is insignificant.

# Lets run vif function on model_3 to understand multicollinearity
vif(model_3)

#Conclusion VIF on model 3:
# 1. Engine size, curbweight ratio to car length (cw2cl) ,curb weigth ratio to engine size(cw2es), fuelsystemmpfi, 
# have multicollinearity.

# Build model 4 after removing enginesize---------------------------------------------

# Create formula for model_4
m4_formula<- as.formula(paste(paste(deparse(m3_formula),collapse = ""),"enginesize",sep = "-"))

# build model 4
model_4 <- lm(m4_formula,data = train)
summary(model_4)

# Conclusion model 4
# 1. Slight change in R-square and Adj.R-square. Dropped to 96% and 94% respectively
# 2. Still variables like carmakesaab,wheelbase, carwidth, enginetypel etc. are insignificant

# lets understand multicollinearity for model 4
vif(model_4)

# Conclusion VIF model 4:
# 1. carbodysedan, car length, and fuelsystemmpfi have multicollinearity.


# Build model 5 after removing carbodysedan------------------------------------

# Create formula for model 5:
m5_formula<- as.formula(paste(paste(deparse(m4_formula),collapse = ""),"carbodysedan",sep = "-"))

# build model 5 using formula:
model_5 <- lm(m5_formula, data = train)

summary(model_5)

# Conclusion model 5:
# 1. Marginal change in R-square and Adj.R-square. Dropped to 95% and 94% respectively
# 2. Still variables like wheelbase, carwidth, carmakesaab,carmakehonda etc. are insignificant

# Check multicollinearity in model 5:
vif(model_5)

# Conclusion:
# 1. Carlength,wheelbase, carwidth have multicollinearity
# 2. Carlength could be removed from the next model version

# Build model 6 after removing carlength------------------------------------

# Create formula for model 6
m6_formula<- as.formula(paste(paste(deparse(m5_formula),collapse = ""),"carlength",sep = "-"))

# build model 6 using formula:
model_6 <- lm(m6_formula,data = train)
summary(model_6)

# Conclusion model 6:
# 1. Marginal change in R-square and Adj.R-square. Dropped to 95% and 94% respectively
# 2. Still variables like carbodyhardtop, carbodyhatchback,engintypel etc. are insignificant

# Check multicollinearity in model 6:
vif(model_6)

# Conclusion VIF model 6:
# 1. Carwidth and fuelsystemmpfi needs attention
# 2. Build next model after removing fuelsystemmpfi since its not significant (p_value).

# Build model 7 after removing fuelsystemmpfi--------------------------------

# Create formula for model 7:
m7_formula<- as.formula(paste(paste(deparse(m6_formula),collapse = ""),"fuelsystemmpfi",sep = "-"))

# Build model using formula:

model_7 <- lm(m7_formula, data=train)
summary(model_7)

# Conclusion model 7:
# 1.Marginal change in R-square and Adj.R-square. Dropped to 95% and 93% respectively
# 2. Need to check multicollinearity

# Check multicollinearity in model 6:
vif(model_7)


# Conclusion vif on model 7:
# 1. Carwidth, curbweigth ratio to car length, drivewheelrwd, peakrpm etc needs attention.
# 2. For next model, remove carwidth and analyse the results.

# Build model 8 after removing carwidth--------------------------

# Create formula for model 8:
m8_formula<- as.formula(paste(paste(deparse(m7_formula),collapse = ""),"carwidth",sep = "-"))

# Build model using formula:
model_8 <- lm(m8_formula,data=train)
summary(model_8)

# Conclusion model 8:
# 1.Marginal change in R-square and Adj.R-square. Dropped to 95% and 93% respectively
# 2.Few variables are still insignificant.

# Check multicollinearity
vif(model_8)

# Conclusion:
# 1. Still we got few variables who have high collinerity.
# 2. for model 9, lets run stepAIC on model 8 and see the best fit.


# Build Model 9 basis step AIC on model 8
# Step AIC process ---------------------------------------------------

step_aic8 <- stepAIC(model_8)

# We can check the formula for the best model by analysing the step_aic1 object
step_aic8$anova

# For model 9 we will store the formula from step_aic8
m9_formula <- as.formula(step_aic8$call)

# Build model 9 from the formulation obtained from step aic
model_9 <- lm(m9_formula,data=train)

summary(model_9)

# Conclusion model 9:
# 1. Marginal change in R-square and Adj.R-square. Dropped to 92% and 90% respectively
# 2. carbodywagon & cylindernumbersix are insignificant

# Check multipcollinearity for model 9
vif(model_9)

#Conclusion:
# 1. The list looks good compared to previous version.
# 2. curbweight ratio to engine size and curbweigth ratio to car length still are concern.
# 3. For model 10, we will remove horsepower and see the impact


# Build model 10 after removing cylindernumbersix ---------------------------

# Create formula for model 10:
m10_formula <- as.formula(paste(paste(deparse(m9_formula),collapse = ""),"cylindernumbersix",sep = "-"))

# build model using the formula 10:
model_10 <- lm(m10_formula,data=train)

summary(model_10)

#Conclusion model 10:
# 1. Slight change in R-square and Adj.R-square. Dropped to 94% and 93% respectively
# 2. After removing cylindernumbersix, carbodywagon looks like insignificant.

# Check multicollinearity model 10:
vif(model_10)

#Conclusion:
# 1. Looks good compared to the previous model 9.
# 2. We will remove carbodywagon and see the impact on the next model.

# Build model 11 using carbodywagon -------------------------------------

#Create formula for model 11:
m11_formula <- as.formula(paste(paste(deparse(m10_formula),collapse = ""),"carbodywagon",sep = "-"))

# Build model 11 using the formula:
model_11 <- lm(m11_formula,data = train)

summary(model_11)

#Conclusion model 11:
# 1. No change in R-square and Adj.R-square
# 2. All the variables are significant and are in the acceptable range.

# Check multicollinearity for model 11:
vif(model_11)

# Conclusion: No concerns (considering a threshold of 0-4)



# Final Conclusion before predicting
# 1. After all these iterations, model 11 has all the variables which are significant.
# 2. Model 20 has a r-square of 94.2% and adj.r-squre of 93.2% which is extremely good.
# 3. Lets predict the test dataset using model 11.

#----------------------------------------------------------------------

########## Predict test dataset using model 11 ##############################

# While performing the prediction, we have ignored price variable.
pred <- predict(model_11,test[,!colnames(test) %in% c('price')])

# Merge the predicted price into the test dataset----
test$predicted_price <- pred


# Lets find the out the rsquare for the predicted price.
predicted_rsquare<- cor(test$price,test$predicted_price)^2
round(predicted_rsquare,2)



# Lets plot the comparison between predicted and actual car price
plot(test$price)
lines(test$predicted_price, col='red',type='l',lty=2)



# Conclusion:
# 1. Compared to training set, we got 85% rsquare which is not far-off.
# 2. We can accept the model and predict the car price.

