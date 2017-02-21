df <- read.csv("BreastCancer.csv")
df <- na.omit(df)
head(df)

mylogit <- glm(Class~., data=df, family = binomial()) #generalized linear model


#Predicts fit.logit with the df. The output of the predict function is a probability.
prob <- predict(mylogit, df, type="response") 
head(prob)

#Sets a threshold of 0.5. If >0.5 is TRUE, then malignant, else benign
logit.pred <- factor(prob > .5, levels = c(FALSE, TRUE), labels = c("benign","malignant"))
head(logit.pred)

logit.perf <- table(df$Class, logit.pred, dnn = c("Actual", "Predicted"))
logit.perf


