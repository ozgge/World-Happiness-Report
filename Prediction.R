Happiness2015 <- read.csv("C:/Users/Özge/Desktop/2015.csv")

#Changing the name of columns
colnames (Happiness2015) <- c("Country", "Region", "Happiness.Rank", "Happiness.Score",
                              "Standard.Error", "Economy", "Family",
                              "Life.Expectancy", "Freedom", "Trust", "Generosity",
                              "Dystopia.Residual")

#remove the columns that are not necessary for prediction 
Happiness2015 <- Happiness2015[, -c(5)]
# Seperate The Happiness 2015 dataset into the train and the test

# Next, we need to split the data into a training set and a testing set. As their names imply, the training set is used to train and build the model, and then this model is tested on the testing set. Let’s say we want to have about 75% of the data in the training set and 25% of the data in the testing set. It can be done as follows:
library(caTools)
library(ggplot2)


set.seed(123)

dataset <- Happiness2015[4:11]


split = sample.split(dataset$Happiness.Score, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)



#Multiple Linear Regression
predictionModel = lm(Happiness.Score ~ ., data = training_set)

summary(predictionModel)


prediction1 <- predict(predictionModel, newdata = test_set)
head(prediction1)
head(test_set$Happiness.Score)


SSE <- sum((test_set$Happiness.Score - prediction1) ^ 2)
SST <- sum((test_set$Happiness.Score - mean(test_set$Happiness.Score)) ^ 2)
1 - SSE/SST


ypredLM = predict(predictionModel, newdata = test_set)

PredActualLM <- as.data.frame(cbind(Prediction = ypredLM, Actual = test_set$Happiness.Score))

gglinearmodel <- ggplot(PredActualLM, aes(Actual, Prediction )) + geom_point() + theme_bw() + geom_abline() +
  labs(title = "Multiple Linear Regression", x = "Actual Happiness Score",
       y = "Predicted Happiness Score") +
  theme(plot.title = element_text(family = "Calibri", face = "italic", size = (17)), 
        axis.title = element_text(family = "Calibri", size = (12)))
gglinearmodel


# Fitting SVR to the dataset
library(e1071)
regressor_svr = svm(formula = Happiness.Score ~ .,
                    data = dataset,
                    type = 'eps-regression',
                    kernel = 'radial')

# Predicting a new result
y_pred_svr = predict(regressor_svr,  newdata = test_set)
head(y_pred_svr)
head(test_set$Happiness.Score)



Pred_Actual_svr <- as.data.frame(cbind(Prediction = y_pred_svr, Actual = test_set$Happiness.Score))


Pred_Actual_lm.versus.svr <- cbind(Prediction.lm = y_pred_lm, Prediction.svr = y_pred_svr, Actual = test_set$Happiness.Score)


gg.svr <- ggplot(Pred_Actual_svr, aes(Actual, Prediction )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "SVR", x = "Actual happiness score",
       y = "Predicted happiness score") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
        axis.title = element_text(family = "Helvetica", size = (10)))
gg.svr


# Fitting Decision Tree Regression to the dataset
library(rpart)
predictionModel2 = rpart(formula = Happiness.Score ~ ., data = dataset,
                         control = rpart.control(minsplit = 10))

# Predicting a new result with Decision Tree Regression
yPredDT = predict(predictionModel2, newdata = test_set)

head(yPredDT)
head(test_set$Happiness.Score)


Pred_Actual_dt <- as.data.frame(cbind(Prediction = yPredDT, Actual = test_set$Happiness.Score))
library(ggplot2)

ggdecisiontree <- ggplot(Pred_Actual_dt, aes(Actual, Prediction )) + geom_point() + theme_bw() + geom_abline() +
  labs(title = "Decision Tree Regression", x = "Actual happiness score",
       y = "Predicted happiness score") +
  theme(plot.title = element_text(family = "Arial", face = "italic", size = (17)), 
        axis.title = element_text(family = "Arial", size = (12)))
ggdecisiontree




# Fitting Random Forest Regression to the dataset
library(randomForest)

set.seed(1234)
regressor_rf = randomForest(x = dataset[-1],
                            y = dataset$Happiness.Score,
                            ntree = 500)


# Predicting a new result with Random Forest Regression
y_pred_rf = predict(regressor_rf, newdata = test_set)

head(y_pred_rf)
head(test_set$Happiness.Score)


Pred_Actual_rf <- as.data.frame(cbind(Prediction = y_pred_rf, Actual = test_set$Happiness.Score))


gg.rf <- ggplot(Pred_Actual_rf, aes(Actual, Prediction )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Random Forest Regression", x = "Actual happiness score",
       y = "Predicted happiness score") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
        axis.title = element_text(family = "Helvetica", size = (10)))
gg.rf


# Fitting Neural Net to the training set
library(neuralnet)

nn <- neuralnet(Happiness.Score ~ Economy + Family + Life.Expectancy + Freedom + Generosity + Trust + Dystopia.Residual,
                data=training_set,hidden=10,linear.output=TRUE)


predicted.nn.values <- compute(nn,test_set[,2:8])

Pred_Actual_nn <- as.data.frame(cbind(Prediction = predicted.nn.values$net.result, Actual = test_set$Happiness.Score))

gg.nn <- ggplot(Pred_Actual_nn, aes(Actual, V1 )) +
  geom_point() + theme_bw() + geom_abline() +
  labs(title = "Neural Net", x = "Actual happiness score",
       y = "Predicted happiness score") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
        axis.title = element_text(family = "Helvetica", size = (10)))
gg.nn



MSE.lm <- sum((test_set$Happiness.Score - ypredLM)^2)/nrow(test_set)
MSE.nn <- sum((test_set$Happiness.Score - predicted.nn.values$net.result)^2)/nrow(test_set)

print(paste("Mean Squared Error (Multiple Linear Regression):", MSE.lm))
print(paste("Mean Squared Error (Neural Net):", MSE.nn))


ggarrange(gglinearmodel, gg.svr, ggdecisiontree, gg.rf, gg.nn, ncol = 2, nrow = 3)


#clustering
library(NbClust)
library(pamm)
library(cluster)
library(factoextra)

number <- NbClust(Happiness2015[, 4:11], distance="euclidean",
                  min.nc=2, max.nc=15, method='kmeans', index='all', alphaBeale = 0.1)
number

set.seed(2017)
pam <- pam(Happiness2015[, 4:11], diss=FALSE, 3, keep.data=TRUE)
fviz_silhouette(pam)
Happiness2015$country[pam$id.med]
fviz_cluster(pam, stand = FALSE, geom = "point",ellipse.type = "norm")



