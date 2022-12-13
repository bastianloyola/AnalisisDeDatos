library("C50")
library("caret")
library("ggpubr")
library(dplyr)
bank <- read.csv('https://raw.githubusercontent.com/bastianloyola/AnalisisDeDatos/main/bank-additional/bank-additional/bank-additional-full.csv', sep=';')


bank$y <- factor(bank$y);
bank$job <- factor(bank$job);
bank$marital <- factor(bank$marital);
bank$education <- factor(bank$education);
bank$default <- factor(bank$default);
bank$housing <- factor(bank$housing);
bank$loan <- factor(bank$loan);
bank$contact <- factor(bank$contact);
bank$month <- factor(bank$month);
bank$poutcome <- factor(bank$poutcome);
bank$day_of_week <-factor(bank$day_of_week)



set.seed(1234)

#para grafico
training.index = createDataPartition(bank$y, p=0.00999)$Resample1
training.set = bank[training.index, ]
tree = C5.0(y ~ ., training.set)
tree.rules = C5.0(x = training.set[, -21], y = training.set$y, rules = T)
summary(tree.rules)
plot(tree)


# para matriz
set.seed(1234)
training.index = createDataPartition(bank$y, p=0.01)$Resample1
training.set = bank[training.index, ]
test.set = bank[-training.index, ]


tree = C5.0(y ~ ., training.set)

tree.rules = C5.0(x = training.set[, -21], y = training.set$y, rules = T)
tree.pred.y = predict(tree, test.set[,-21], type = "class")
tree.pred.prob = predict(tree, test.set[,-21], type = "prob")

conf.matrix.tree = confusionMatrix(table(test.set$y, tree.pred.y))
print(conf.matrix.tree)
tree.pred.y


head(tree.pred.prob)


plot(tree)
summary(tree)


summary(tree.rules)
#Probar seeds 
#1
set.seed(1)
training.index = createDataPartition(bank$y, p=0.7)$Resample1
training.set = bank[training.index, ]
test.set = bank[-training.index, ]
tree = C5.0(y ~ ., training.set)
tree.rules = C5.0(x = training.set[, -21], y = training.set$y, rules = T)
summary(tree.rules)
plot(tree)
tree.pred.y = predict(tree, test.set[,-21], type = "class")
tree.pred.prob = predict(tree, test.set[,-21], type = "prob")
conf.matrix.tree = confusionMatrix(table(test.set$y, tree.pred.y))
print(conf.matrix.tree)
