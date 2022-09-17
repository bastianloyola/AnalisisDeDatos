

bank <- read.csv('https://raw.githubusercontent.com/bastianloyola/AnalisisDeDatos/main/bank/bank.csv', sep=';')

#
bank$y <- factor(bank$y)


#Resumen base de datos
summary(bank)


cor(bank$duration,bank$pdays)


cor(bank$duration,bank$age)

#Regresion logistica entre duracion e Y
logistica <- glm(y ~ duration, family = "binomial", data = bank)

print(summary(logistica))

#Exactitud
sum(round(predict(logistica, type = "response")) == as.numeric(bank$y)) / length(bank$y)

#Sensibilidad
sum(round(predict(logistica, type = "response")) == as.numeric(bank$y) & as.numeric(bank$y) == 1) / sum(as.numeric(bank$y))

#Presicion
sum(round(predict(logistica, type = "response")) == as.numeric(bank$y) & as.numeric(bank$y) == 1) / sum(round(predict(logistica, type = "response") == 1))
