library(AER)

data("CreditCard")
dim(CreditCard)

CreditCard$class <- CreditCard$card
CreditCard <- subset(CreditCard,select=-c(card,expenditure,share))

set.seed(123)
library(caret)

rows <- sample(nrow(CreditCard))
CreditCard <- CreditCard[rows,]
split <- round(nrow(CreditCard)*.60)
train <- CreditCard[1:split,]
test <- CreditCard[(split+1):nrow(CreditCard),]

dim(train)
dim(test)


####GLM method
glmModel <- glm(class~ . , data=train, family=binomial)

pred.glmModel <- predict(glmModel, newdata=test, type="response")

library(caTools)

roc.glmModel <-colAUC(pred.glmModel,test$class,plotROC = TRUE)

####glm boost
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)

set.seed(2014)

glmBoostModel <- train(class ~ ., data=train, method = "glmboost", metric="ROC", trControl = fitControl,
                       tuneLength=5, center=TRUE, family=Binomial(link = c("logit")))

pred.glmBoostModel <- as.vector(predict(glmBoostModel, newdata=test, type="prob")[,"yes"])


roc.glmBoostModel <-colAUC(pred.glmBoostModel,test$class,plotROC = TRUE)


##CART
set.seed(2014)

cartModel <- train(class ~ ., data=train, method = "rpart", metric="ROC",
                   trControl = fitControl, tuneLength=5)

pred.cartModel <- as.vector(predict(cartModel, 
                                    newdata=test, type="prob")[,"yes"])

roc.cartModel <-colAUC(pred.cartModel,test$class,plotROC = TRUE)


##condition inference model 
set.seed(2014)

partyModel <- train(class ~ ., data=train, method = "ctree", metric="ROC", 
                    trControl = fitControl, tuneLength=5)

pred.partyModel <- as.vector(predict(partyModel, newdata=test, type="prob")[,"yes"])

roc.partyModel <-colAUC(pred.partyModel,test$class,plotROC = TRUE)

###Elastic Net

set.seed(2014)

eNetModel <- train(class ~ ., data=train, method = "glmnet", metric="ROC", 
                   trControl = fitControl, family="binomial", tuneLength=5)

pred.eNetModel <- as.vector(predict(eNetModel, newdata=test, type="prob")[,"yes"])

roc.eNetModel <-colAUC(pred.eNetModel,test$class,plotROC = TRUE)


#Earth

set.seed(2014)



pred.earthModel <- as.vector(predict(earthModel, newdata=test, type="prob")[,"yes"])

roc.earthModel <-colAUC(pred.earthModel,test$class,plotROC = TRUE)

##Boosted trees

set.seed(2014)

gbmModel <- train(class ~ ., data=train, method = "gbm", metric="ROC",
                  trControl = fitControl, verbose=FALSE, tuneLength=5)

pred.gbmModel <- as.vector(predict(gbmModel, newdata=test, type="prob")[,"yes"])

roc.gbmModel <-colAUC(pred.gbmModel,test$class,plotROC = TRUE)


#Random forest

set.seed(2014)

rfModel <- train(class ~ ., data=train, method = "rf", metric="ROC", 
                 trControl = fitControl, verbose=FALSE, tuneLength=5)


pred.rfModel <- as.vector(predict(rfModel, newdata=test, type="prob")[,"yes"])

roc.rfModel <-colAUC(pred.rfModel,test$class,plotROC = TRUE)




test.auc <- data.frame(model=c("glm","glmboost","gbm","glmnet","earth","cart","ctree","rForest"),
                       summary=c(roc.glmModel, roc.glmBoostModel, roc.gbmModel, 
                             roc.eNetModel, roc.earthModel, roc.cartModel,
                             roc.partyModel, roc.rfModel))

test.auc <- test.auc[order(test.auc$summary, decreasing=TRUE),]

test.auc$model <- factor(test.auc$model, levels=test.auc$model)

test.auc

library(ggplot2)
theme_set(theme_gray(base_size = 18))
ggplot(test.auc,aes(x=model, y=summary))+ 
         geom_bar(fill = "light blue", stat="identity")

plot(cartModel)
plot(glmModel)
plot(glmBoostModel)
plot(cartModel)
plot(partyModel)
plot(eNetModel)
plot(earthModel)
plot(gbmModel)
plot(rfModel)

summary(gbmModel)


