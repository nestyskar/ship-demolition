#R code for the classification decision tree
#Required libraries

library(readxl)
library(dplyr)
library(psych)
library(MVN)
library(ggplot2)
library(rpart)
library(caret)

#Importing data from Excel

data1 <- read_excel("C:/Users/nesty/Desktop/ShipsDemolition.xlsx", 1)
names(data1) <- make.names(names(data1))
attach(data1)

# Convertation of 0 to N and 1 to Y

data1$Demolished<-ifelse(data1$Demolished==1, 'Y', 'N')

# Cheking the percentage of demolished and ships in use

table(data1$Demolished)/sum(table(data1$Demolished))

# Stratified sampling for the data set separation on train and test sets

set.seed(101)
tr.index = createDataPartition(y = data1$Demolished, p = 0.5, list = FALSE)
trSet = data1[tr.index, ]
testSet = data1[-tr.index, ]

# Cheking the percentage of demolished and not demolished in train and test sets

table(trSet$Demolished)/sum(table(trSet$Demolished))
table(testSet$Demolished)/sum(table(testSet$Demolished))

# Building a tree

tree <- rpart(Demolished ~ Type + Dwt + Age ,data = trSet)
tree

# Checking the tree on the test set

prediction <- predict(tree, newdata = testSet, type = 'class')
prediction
mean(prediction == testSet$Demolished)

# Recording a tree into .pdf

pdf('tree1.pdf')
plot(tree, branch=1, uniform=TRUE)
text(tree)
dev.off()

# For the example's reproducibility

set.seed(505)

# Reducing the threshold penalty for the difficulty with the step of 0.005 (cp - complexity parameter that is used to control and to select the size of the decision tree) 

rtp.a1 <- rpart(Demolished ~ Type + Dwt + Age ,data = trSet, control = rpart.control(cp = .005)) 

# Graph of the dependence of the conditional errors on the nodes' quantity

plotcp(rtp.a1) 
with(rtp.a1, {lines(cptable[, 2] + 1, cptable[, 3], type = "b", col = "red")
  legend("topright", c("Learning sample error",
                       "CV error", "min(CV error)+SE"),
         lty = c(1, 1, 2), col = c("red", "black", "black"), bty = "n") })

# After finding the min of CV error on the graph for tree pruning

rtp.a1 <- prune(rtp.a1, cp = 0.014)

# Checking the pruned tree

prediction <- predict(rtp.a1, newdata = testSet, type = 'class')
prediction
mean(prediction == testSet$Demolished)

# Recording a pruned tree into .pdf

pdf('treePruned.pdf')
plot(rtp.a1, branch=1, uniform=TRUE)
text(rtp.a1)
dev.off()

R code for Random forest
#Required libraries
library(readxl)
library(dplyr)
library(psych)
library(MVN)
library(ggplot2)
library(rpart)
library(caret)
#Importing data from Excel
data1 <- read_excel("C:/Users/nesty/Desktop/ShipsDemolition.xlsx", 1)
names(data1) <- make.names(names(data1))
attach(data1)
# Type to be a factor variable
data1$Type <- as.factor(as.character(data1$Type))
# Convertation of 0 to N and 1 to Y
data1$Demolished<-ifelse(data1$Demolished==1, 'Y', 'N')
# Demolished to be a factor variable
data1$Demolished <- as.factor(as.character(data1$Demolished))
# Stratified sampling for the data set separation on train and test sets
set.seed(101)
tr.index = createDataPartition(y = data1$Demolished, p = 0.5, list = FALSE)
trSet = data1[tr.index, ]
testSet = data1[-tr.index, ]
# Random Forest building
library(randomForest)
rf1 <- randomForest(Demolished ~ ., data = trSet, importance = TRUE)
predictions <- predict(rf1, testSet)
print(sqrt(sum((as.vector(predictions - testSet$Demolished))^2))/length(predictions))
plot(rf1)
rf1
importance(rf1)
