

#importing all the libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(MASS)
library(Information)
library(gridExtra)
library(stringr)
library(caret)
library(car)
library(CombMSC)

#Import the data
seeds <- read.csv("~/Docs/Business Analytics/Classification/seeds.csv")

#view data
View(seeds)

#scatter plots
g <- ggplot(data=train.data, aes(x = Area, y = Perimeter))
print(g) 
g <-g + 
  geom_point(aes(color=Type, shape=Type)) +
  xlab("Area") +
  ylab("Perimeter") +
  ggtitle("Area and Perimeter")+
  geom_smooth(method="lm")
print(g)

#histogram
histogram <- ggplot(data=seeds, aes(x=Area)) +
  geom_histogram(binwidth=0.2, color="Black", aes(fill=Type)) + 
  xlab("Area") +  
  ylab("Frequency") + 
  ggtitle("Histogram of Sepal Width")
print(histogram)


#correlation of data
correlation_data <- cor(seeds[,1:7])
ggcorrplot(correlation_data, method = "circle")


# build linear regression model on full data
linearMod <- lm(Type ~ Area, data=seeds)  
print(linearMod)
summary(linearMod)

# Split the data into training and test set
training.samples <- seeds$Type %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- seeds[training.samples, ]
test.data <- seeds[-training.samples, ]

#Build model on training data
model <- lm(Type ~ Area, data=train.data)

#make predictions on test data
TypePred <- predict(model, test.data)

summary(model)

# make predictions and compute the values of R2, RMSE, MAE
predictions <- model %>% predict(test.data)
data.frame( R2 = R2(predictions, test.data$Type),
            RMSE = RMSE(predictions, test.data$Type),
            MAE = MAE(predictions, test.data$Type))


RMSE(predictions, test.data$Type)/mean(test.data$Type)


#Calculate prediction accuracy
actuals_preds <- data.frame(cbind(actuals=test.data$Type, predicteds=TypePred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
head(actuals_preds)

#check the accuracy of model
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max)) 
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  

#k-fold cross validation
set.seed(9)
# Define training control
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(Type ~ Area + Perimeter + Kernel.Length + Kernel.Width + Kernel.Groove , data = seeds, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

model <- lm(Type ~ Area + Perimeter + Kernel.Length + Kernel.Width + Kernel.Groove, data = seeds)
AIC(model)


#CP, AIC and BIC Criterion values
s2 <- sigma(lm(Type ~ Area + Perimeter + Kernel.Length + Kernel.Width + Kernel.Groove,data=train.data)) #extracts residual standard error from lm
n = nrow(train.data)
c(Cp(model,S2=(s2^2)), AIC(model,k=2),AIC(model,k=log(n)))

#k-fold cross validation
set.seed(9)
# Define training control
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(Type ~ Area + Perimeter + Kernel.Length + Kernel.Width , data = seeds, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
summary(model)

model <- lm(Type ~ Area + Perimeter + Kernel.Length + Kernel.Width , data = seeds)
AIC(model)
BIC(model)


#k-fold cross validation
set.seed(9)
# Define training control
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(Type ~ Area + Perimeter + Kernel.Length, data = seeds, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
summary(model)

model <- lm(Type ~ Area + Perimeter + Kernel.Length, data = seeds)
AIC(model)
BIC(model)

#k-fold cross validation
set.seed(9)
# Define training control
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(Type ~ Area + Perimeter, data = seeds, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
summary(model)

model <- lm(Type ~ Area + Perimeter, data = seeds)
AIC(model)
BIC(model)

#k-fold cross validation
set.seed(9)
# Define training control
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(Type ~ Area, data = seeds, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
summary(model)

model <- lm(Type ~ Area, data = seeds)
AIC(model)
BIC(model)

#CP, AIC and BIC Criterion values
s2 <- sigma(lm(Type ~ Area + Perimeter + Kernel.Length + Kernel.Width,data=train.data)) #extracts residual standard error from lm
n = nrow(train.data)
c(Cp(model,S2=(s2^2)), AIC(model,k=2),AIC(model,k=log(n)))

#CP, AIC and BIC Criterion values
s2 <- sigma(lm(Type ~ Area ,data=train.data)) #extracts residual standard error from lm
n = nrow(train.data)
c(Cp(model,S2=(s2^2)), AIC(model,k=2),AIC(model,k=log(n)))


#Leave one out cross validation - LOOCV

# Define training control
train.control <- trainControl(method = "LOOCV")
# Train the model
model <- train(Type~ Area + Perimeter + Kernel.Length + Kernel.Width + Kernel.Groove, data = seeds, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)            
summary(model)


#Repeated k-fold

# Define training control
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, repeats = 3)
# Train the model
model <- train(Type~ Area + Perimeter + Kernel.Length + Kernel.Width + Kernel.Groove, data = seeds, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
summary(model)


#k-fold cross validation
set.seed(9)
# Define training control
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(Type ~ Area + Perimeter + Kernel.Length + Kernel.Width + Kernel.Groove , data = seeds, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
summary(model)


#Build the Linear Dicriminant Analysis


# Split the data into training (80%) and test set (20%)
set.seed(123)
training.samples <- seeds$Type %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- seeds[training.samples, ]
test.data <- seeds[-training.samples, ]


# Fit the model
model <- lda(Type~ Area + Perimeter + Kernel.Length + Kernel.Width + Kernel.Groove, data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model accuracy
mean(predictions$class==test.data$Type)

#Plot LDA
plot(model)

lda.data <- cbind(train.data, predict(model)$x)
ggplot(lda.data, aes(LD1, LD2)) +
  geom_point(aes(color = Type))


#k-fold for linear Discriminant Analysis (predictor=1)
set.seed(9)
# Define training control
control1 <- trainControl(method="cv", number=10)
seeds$Type <- as.character(seeds$Type)
#Train the model
ldamodel1 <- train(Type ~Area, data=seeds, method="lda", trControl=control1)
#Summarize
print(ldamodel1)

#k-fold for linear Discriminant Analysis (predictor=2)
set.seed(9)
# Define training control
control1 <- trainControl(method="cv", number=10)
seeds$Type <- as.character(seeds$Type)
#Train the model
ldamodel2 <- train(Type ~Area+Perimeter, data=seeds, method="lda", trControl=control1)
#Summarize
print(ldamodel2)

#k-fold for linear Discriminant Analysis (predictor=3)
set.seed(9)
# Define training control
control1 <- trainControl(method="cv", number=10)
seeds$Type <- as.character(seeds$Type)
#Train the model
ldamodel3 <- train(Type ~Area+Perimeter+Kernel.Length, data=seeds, method="lda", trControl=control1)
#Summarize
print(ldamodel3)

#k-fold for linear Discriminant Analysis(predictor=4)
set.seed(9)
# Define training control
control1 <- trainControl(method="cv", number=10)
seeds$Type <- as.character(seeds$Type)
#Train the model
ldamodel4 <- train(Type ~Area+Perimeter +Kernel.Length+Kernel.Width, data=seeds, method="lda", trControl=control1)
#Summarize
print(ldamodel4)


#k-fold for linear Discriminant Analysis (predictor=5)
set.seed(9)
# Define training control
control1 <- trainControl(method="cv", number=10)
seeds$Type <- as.character(seeds$Type)
#Train the model
ldamodel5 <- train(Type ~Area+Perimeter +Kernel.Length+Kernel.Width +Kernel.Groove, data=seeds, method="lda", trControl=control1)
#Summarize
print(ldamodel5)

#Repeated CV FOR LDA
set.seed(9)
# Define training control
control1 <- trainControl(method="repeatedcv", number=10, repeats=3)
#Train the model
ldamodel5 <- train(Type ~Area+Perimeter +Kernel.Length+Kernel.Width +Kernel.Groove, data=seeds, method="lda", trControl=control1)
#Summarize
print(ldamodel5)


set.seed(190)
model1 <- glm(Type~ Area + Perimeter + Kernel.Length + Kernel.Width +Kernel.Groove,data=train.data)
summary(model1)

library(boot)
# Run LOOCV
loocv.model = cv.glm(data=train.data, glmfit=model1, K=nrow(train.data))
# Extract the MSE
cat('\nLeave-one-out-cross-validation MSE:', loocv.model$delta[1])

# Run 10-fold cross-validation of the model1 on the data set
cv.model = cv.glm(data=train.data, glmfit=model1, K=10)
# Extract the MSE
cat('10-fold cross-validation MSE:', cv.model$delta[1])