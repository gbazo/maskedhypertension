data <- read.csv("/home/gabriel/Documents/Prof_Edu/treino2.csv", header = TRUE)

data[is.na.data.frame(data) == TRUE] <- 99

data_na <- data[complete.cases(data), ]

# remove a chave
data_na <- data_na[,-1]

data <- scale(data_na[,1:24], center=TRUE, scale=TRUE)

data <- data.frame(data, data_na[,25])

names(data)[grep('data_na...25.', names(data))] <- 'hipercat'

data[,'hipercat'] <- factor(data[,'hipercat'])

data <- data[,-6]

library(earth)

# feature selection analysis
v2 <- earth(hipercat ~ ., data=data)

ev <- evimp(v2)
ev

##################################################

library(pROC)

roc_obj <- roc(test$hipercat, x$posterior.1, ci = TRUE)

auc(roc_obj)

plot(roc_obj, print.auc=TRUE)

ggroc(roc_obj, linetype=2) #ggplot2

coords(roc_obj, "best")

####################################################

library(MASS)
library(plyr)

set.seed(123)

folds <- split(data, cut(sample(1:nrow(data)),5))

errs.lda <- rep(NA, length(folds))

for (i in 1:length(folds)) {
  test <- ldply(folds[i], data.frame)
  train <- ldply(folds[-i], data.frame)
  tmp.model <- lda(hipercat ~ mediapadiastolica+epworth+result_trigli+result_colesterol+gfrbaselinecr, 
                   train[,-1], method = 'mle')
  tmp.predict <- predict(tmp.model, newdata=test)
  conf.mat <- table(test$hipercat, tmp.predict$class)
  errs.lda[i] <- 1 - sum(diag(conf.mat))/sum(conf.mat)
}

print(sprintf("average error: %.3f percent", 100*mean(errs.lda)))

conf.mat

table(test$hipercat)

library(caret)

confusionMatrix(conf.mat, positive = '1')

IB <- predict(tmp.model, newdata=test, type='prob')

x <- data.frame(test$hipercat, IB)

View(x)

write.csv(data, file = "lda.csv")

