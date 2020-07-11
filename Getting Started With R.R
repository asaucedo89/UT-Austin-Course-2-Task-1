##Install package 
install.packages("readr") 

##Call on a package <- 
library(readr)

#Upload file 
cars <- read_csv("C:/Users/admin/Desktop/Lets See/cars.csv")
View(cars)

##Getting to Know Your Data##
attributes(cars)#List your attributes within your data set.
summary(cars) #Prints the min, max, mean, median, and quartiles of each attribute.
str(cars) #Displays the structure of your data set.
names(cars) #Names your attributes within your data set.
cars$`name of car` #Will print out the instances within that particular column in your data set.

#Histogram Plot <- column data type must be numeric
hist(cars$`speed of car`)

#Scatter (Box) Plot
plot(cars$`speed of car`,cars$`distance of car`)

#Normal Quantile Plot- is a way to see if your data is normally distributed.
qqnorm(cars$`distance of car`)

##Preprocessing your Data##
#Also known as data cleaning  is a vital step in your analysis process. Some reasons to prepare your 
#data is so it can be analyzed, it might be noisy data (missing values/outliers), it could have 
#attributes that aren’t helpful, etc. 

#A vector is a sequence of the same data type.

#Converting data types
str(cars) ##name of car is character, speed of car is numeric, and distance of car is numeric. 
cars$`name of car`<-as.numeric(cars$`name of car`)
str(cars) ##<- now name of car is numeric, but NA by coersion! 

#Reverted to original data types
cars <- read_csv("C:/Users/admin/Desktop/Lets See/cars.csv")
View(cars)

#Renaming attributes/columns in your dataset:
names(cars)<-c("cars","speed","distance") 
names(cars) #<- column names were changed! 

#Missing values: 
summary(cars) #<- TRUE if it’s missing, FALSE if it’s not
sum(is.na(cars))# <- How many values are NA in dataset

#address missing values:
#There are multiple ways to confront missing values in your dataset – all depend on how much they will 
#affect your dataset. Here are a few options:

#Remove any observations containing missing data. (If the missing data is less than 10% of the total data 
#and only after comparing the min/max of all the features both with and without the missing data.)
na.omit(cars$cars)#Drops any rows with missing values and omits them forever
na.exclude(cars$cars)#Drops any rows with missing values, but keeps track of where they were.
#Replace the missing values with the mean
cars$cars[is.na(cars$cars)]<-mean(cars$cars,na.rm = TRUE)


####Creating Testing and Training Sets###
#Once preprocessed dataset, it’s now time to create training and testing sets for my model. First need to 
#create set seed. 
set.seed(123) #Helpful for others who want to recreate same results
  
#We want to split our data into two sets for modeling. One is the training set and the other one being the 
#test set. A common split is 70/30, which means that 70% of the data will be the training set’s size and 30% 
#of the data will be the test set’s size.

#Training and Testing Size (do not create the sets)
trainSize<-round(nrow(cars)*0.7) 
testSize<-nrow(cars)-trainSize
trainSize #35 Values (70% of the dataset [.7*50])
testSize #15 values (30% or the remaining values)

#create the training and test sets
training_indices<-sample(seq_len(nrow(cars)),size =trainSize)
trainSet<-cars[training_indices,]
testSet<-cars[-training_indices,] 

#Now ready to run your data through your modeling algorithm
#The model that I will be using is the Linear Regression Model, which is helpful when trying to discover the 
#relationship between two variables. These two variables represent the X and Y within the linear equation. 
#The X variable is the predictor variable, also known as the independent variable because it doesn’t depend 
#on other attributes while making predictions. Y is the response variable, also known as the dependent variable 
#because its value depends on the other variables. In our case, these two variables will be Speed and Distance. 
#We are trying to predict Distance, so it is our dependent/response/Y variable. Speed is our independent/predictor/X variable.

LR_Model<-lm(distance~ speed, trainSet)
summary(LR_Model) #<- See results of optimized model
                  #Multiple R-squared:  0.9529,	Adjusted R-squared:  0.9515
                  #p-value: < 2.2e-16

#Multiple R-squared- How well the regression line fits the data (1 means it’s a perfect fit).
#p-value - Tells how much the Independent Variable/Predictor affects the Dependent Variable/Response/. 
  #A p-value of more than 0.05 means the Independent Variable has no effect on the Dependent Variable; 
  #less than 0.05 means the relationship is statistically significant.

#Predictions
#Predict the cars distances through the speed of the cars
PredictionsName <- predict(LR_Model,testSet)
PredictionsName

##adding predictions to the test set for comparison
testSet$Predictions <- PredictionsName

#Export File 
write.table(testSet, file='PredVsActCars.csv', sep = ",")

#Upload Iris file 
library(readr)
iris <- read_csv("C:/Users/admin/Desktop/Lets See/iris.csv")
View(iris)

##Getting to Know Your Data##
attributes(iris)#List your attributes within your data set.
summary(iris) #Prints the min, max, mean, median, and quartiles of each attribute.
str(iris) #Displays the structure of your data set.
names(iris) #Names your attributes within your data set.

#Missing values: 
summary(iris) #<- ALL FALSE
sum(is.na(iris))# <- 0

####Creating Testing and Training Sets###
set.seed(123) 

#Training and Testing Size 
trainSize<-round(nrow(iris)*0.7) 
testSize<-nrow(iris)-trainSize
trainSize 
testSize 

#create the training and test sets
training_indices<-sample(seq_len(nrow(iris)),size =trainSize)
trainSet<-iris[training_indices,]
testSet<-iris[-training_indices,] 

iris_model<-lm(Petal.Length~ Petal.Width, trainSet)
print(iris_model)
summary(iris_model) 

#Predictions
#Predict the cars distances through the speed of the cars
PredictIris <- predict(iris_model,testSet)
PredictIris

##adding predictions to the test set for comparison
testSet$Predictions <- PredictIris

#Export File 
write.table(testSet, file='PredVsActIris.csv', sep = ",")
