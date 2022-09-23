

bank <- read.csv('https://raw.githubusercontent.com/bastianloyola/AnalisisDeDatos/main/bank-additional/bank-additional/bank-additional-full.csv', sep=';')

#
bank$y <- factor(bank$y)


#Resumen base de datos
summary(bank)


cor(bank$duration,bank$pdays)


cor(bank$duration,bank$age)

library(RColorBrewer)
coul <- brewer.pal(5, "Set3") 
plot(bank$y, col = coul)

source("http://www.sthda.com/upload/rquery_cormat.r")
mydata <- mtcars[, c(1,3,4,5,6,7)]
mydata <- bank[,c("age","duration","campaign","pdays","previous","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed")]
require("corrplot")
rquery.cormat(mydata)


bankno <- subset(bank, bank$y == 'no')
bankyes <- subset(bank, bank$y == 'yes')


library(ggplot2)

#Edad
ggplot(bankno, aes(x=age)) +
  geom_histogram(binwidth=.5, colour="black", fill="orange") + ggtitle("Age con Y = No")

ggsave("ageno.png")

ggplot(bankyes, aes(x=age)) +
  geom_histogram(binwidth=.5, colour="black", fill="blue") + ggtitle("Age con Y = Yes")

ggsave("ageyes.png")

#Duracion

ggplot(bankno, aes(x=duration)) +
  geom_histogram(binwidth=.5, colour="orange", fill="orange") + ggtitle("duration con Y = No")

ggsave("durationno.png")

ggplot(bankyes, aes(x=duration)) +
  geom_histogram(binwidth=.5, colour="blue", fill="blue") + ggtitle("duration con Y = Yes")

ggsave("durationyes.png")

#campaign
ggplot(bankno, aes(x=campaign)) +
  geom_histogram(binwidth=.5, colour="black", fill="orange") + ggtitle("campaign con Y = No")

ggsave("campaignno.png")

ggplot(bankyes, aes(x=campaign)) +
  geom_histogram(binwidth=.5, colour="black", fill="blue") + ggtitle("campaign con Y = Yes")

ggsave("campaignyes.png")

#pdays
ggplot(bankno, aes(x=pdays)) +
  geom_histogram(binwidth=.5, colour="black", fill="orange") + ggtitle("pdays con Y = No")

ggsave("pdaysno.png")

ggplot(bankyes, aes(x=pdays)) +
  geom_histogram(binwidth=.5, colour="black", fill="blue") + ggtitle("pdays con Y = Yes")

ggsave("pdaysyes.png")

#previous

ggplot(bankno, aes(x=previous)) +
  geom_histogram(binwidth=.5, colour="black", fill="orange") + ggtitle("previous con Y = No")

ggsave("previousno.png")

ggplot(bankyes, aes(x=previous)) +
  geom_histogram(binwidth=.5, colour="black", fill="blue") + ggtitle("previous con Y = Yes")

ggsave("previousyes.png")

#emp.var.rate

ggplot(bankno, aes(x=emp.var.rate)) +
  geom_histogram(binwidth=.5, colour="black", fill="orange") + ggtitle("emp.var.rate con Y = No")

ggsave("empno.png")

ggplot(bankyes, aes(x=emp.var.rate)) +
  geom_histogram(binwidth=.5, colour="black", fill="blue") + ggtitle("emp.var.rate con Y = Yes")

ggsave("empyes.png")

#cons.price.idx

ggplot(bankno, aes(x=cons.price.idx)) +
  geom_histogram(binwidth=.5, colour="black", fill="orange") + ggtitle("cons.price.idx con Y = No")

ggsave("priceno.png")

ggplot(bankyes, aes(x=cons.price.idx)) +
  geom_histogram(binwidth=.5, colour="black", fill="blue") + ggtitle("cons.price.idx con Y = Yes")

ggsave("priceyes.png")

#cons.conf.idx

ggplot(bankno, aes(x=cons.conf.idx)) +
  geom_histogram(binwidth=.5, colour="black", fill="orange") + ggtitle("cons.conf.idx con Y = No")

ggsave("confno.png")

ggplot(bankyes, aes(x=cons.conf.idx)) +
  geom_histogram(binwidth=.5, colour="black", fill="blue") + ggtitle("cons.conf.idx con Y = Yes")

ggsave("confyes.png")

#euribor3m

ggplot(bankno, aes(x=euribor3m)) +
  geom_histogram(binwidth=.5, colour="black", fill="orange") + ggtitle("euribor3m con Y = No")

ggsave("eurino.png")

ggplot(bankyes, aes(x=euribor3m)) +
  geom_histogram(binwidth=.5, colour="black", fill="blue") + ggtitle("euribor3m con Y = Yes")

ggsave("euriyes.png")

#nr.employed

ggplot(bankno, aes(x=nr.employed)) +
  geom_histogram(binwidth=.5, colour="black", fill="orange") + ggtitle("nr.employed con Y = No")

ggsave("employno.png")

ggplot(bankyes, aes(x=nr.employed)) +
  geom_histogram(binwidth=.5, colour="black", fill="blue") + ggtitle("nr.employed con Y = Yes")

ggsave("employyes.png")

ggplot(bank,aes(x = y,y = age))+geom_boxplot(aes(fill= y))+xlab("Suscrito")
ggsave("boxage.png")
ggplot(bank,aes(x = y,y = duration))+geom_boxplot(aes(fill= y))+xlab("Suscrito")
ggsave("boxduration.png")
ggplot(bank,aes(x = y,y = campaign))+geom_boxplot(aes(fill= y))+xlab("Suscrito")
ggsave("boxcampaign.png")
ggplot(bank,aes(x = y,y = pdays))+geom_boxplot(aes(fill= y))+xlab("Suscrito")
ggsave("boxpdays.png")
ggplot(bank,aes(x = y,y = previous))+geom_boxplot(aes(fill= y))+xlab("Suscrito")
ggsave("boxprevious.png")
ggplot(bank,aes(x = y,y = emp.var.rate))+geom_boxplot(aes(fill= y))+xlab("Suscrito")
ggsave("boxemp.png")
ggplot(bank,aes(x = y,y = cons.price.idx))+geom_boxplot(aes(fill= y))+xlab("Suscrito")
ggsave("boxprice.png")
ggplot(bank,aes(x = y,y = cons.conf.idx))+geom_boxplot(aes(fill= y))+xlab("Suscrito")
ggsave("boxconf.png")
ggplot(bank,aes(x = y,y = euribor3m))+geom_boxplot(aes(fill= y))+xlab("Suscrito")
ggsave("boxeuri.png")
ggplot(bank,aes(x = y,y = nr.employed))+geom_boxplot(aes(fill= y))+xlab("Suscrito")
ggsave("boxemploy.png")

# #Regresion logistica entre duracion e Y
# logistica <- glm(y ~ duration, family = "binomial", data = bank)
# 
# print(summary(logistica))
# 
# #Exactitud
# sum(round(predict(logistica, type = "response")) == as.numeric(bank$y)) / length(bank$y)
# 
# #Sensibilidad
# sum(round(predict(logistica, type = "response")) == as.numeric(bank$y) & as.numeric(bank$y) == 1) / sum(as.numeric(bank$y))
# 
# #Presicion
# sum(round(predict(logistica, type = "response")) == as.numeric(bank$y) & as.numeric(bank$y) == 1) / sum(round(predict(logistica, type = "response") == 1))
