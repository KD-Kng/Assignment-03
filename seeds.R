library(readxl)
seeds <- read_excel("Docs/Business Analytics/Classification/seeds.xlsx")
View(seeds)
install.packages("caret")
library(caret)
# Slpit the data in 80% training, 20% testing
data_split <- createDataPartition(seeds$Type, p = 0.8, list = FALSE)
# split testing data
testset <- seeds[-data_split,]
split Training data
trainset <- seeds[data_split,]
# check the attributes and dimension of data
dim(trainset)
# Understand the structure of data
str(trainset)
#summary of data
summary(trainset)
 
levels(trainset$Type)
#histogram
hist(trainset$Kernel.Width)
# Plot the boxplot
par(mfrow=c(1,5))
  for(i in 1:5) {
  boxplot(trainset[,i], main=names(trainset)[i])
}
# Scatter the plots
g <- ggplot(data=trainset, aes(x = Area, y = Perimeter))
print(g)
g <-g + 
    geom_point(aes(color=Type, shape=Type)) +
    xlab("Area") +
    ylab("Perimeter") +
    ggtitle("Area and Perimeter")+
    geom_smooth(method="lm")
print(g)
#PLot Boxplot
box <- ggplot(data=trainset, aes(x=Kernel.Length, y=Kernel.Width)) +
    geom_boxplot(aes(fill=Type)) + 
    ylab("Perimeter") +
    ggtitle("Seed Boxplot") +
    stat_summary(fun=mean, geom="point", shape=5, size=4) 
print(box)
install.packages("ggthemes")
library(ggthemes)
install.packages("ggplot2")
library(ggplot2)
#Histogram
histogram <- ggplot(data=seeds, aes(x=Area)) +
    geom_histogram(binwidth=0.2, color="black", aes(fill=Type)) + 
    xlab("Area") +  
    ylab("Frequency") + 
    ggtitle("Histogram of Area")
print
#check the correlation of data
correlation_data <- cor(seeds[,1:5])
ggcorrplot(correlation_data, method = "circle")
install.packages("car")
library("car")
#Q-Q Plot
qqPlot(seeds$Area)
install.packages("tidyverse")
library("tidyverse")
# Loading the Caret package which is used for data partition
library(caret)
# Create a partition on dataset (80% training 20% testing) 
data_split <- createDataPartition(seeds$Type, p = 0.8, list = FALSE)
# select 20% data for training
testset <- seeds[-data_split,]
# select 80% data to build or train the models
trainset <- seeds[data_split,]

Normalizing the data
# Estimating preprocessing parameters
preproc.param <- trainset %>% 
+     preProcess(method = c("center", "scale"))
# Transforming the data
traindata <- preproc.param %>% predict(trainset)
testdata <- preproc.param %>% predict(testset)
# Fit LDA model
model <- lda(Type~., data = traindata)
predictions <- model %>% predict(testdata)
mean(predictions$class==testdata$Type)

lda.data <- cbind(traindata, predict(model)$x)
# Plot LDA
ggplot(lda.data, aes(LD1, LD2)) +
+     geom_point(aes(color = Type))

#Logistic Regression
glm(Type ~ Area, data= seeds) 
glm(Type ~ Area+ Perimeter, data= seeds) 
lm(Type ~ Area+ Perimeter+ Compactness+ Kernel.Length+ Kernel.Width+ Asymmetry.Coeff+ Kernel.Groove, data= seeds) 
#plot Logistic Regression
plot(seeds$Type, seeds$Area, pch = 16, xlab = "Type", ylab = "Area")




