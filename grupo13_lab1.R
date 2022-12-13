library(ggplot2)
library(RColorBrewer)

bank <- read.csv('https://raw.githubusercontent.com/bastianloyola/AnalisisDeDatos/main/bank-additional/bank-additional/bank-additional-full.csv', sep=';')

#Factorizacion de variables categoricas
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

#Valor de y en la base de datos
bank$y <- factor(bank$y)

#Duplicados y removerlos
sum(duplicated(bank))
#Existen duplicados
bank_unique <- unique(bank)
#Resumen base de datos
summary(bank_unique)




coul <- brewer.pal(5, "Set3") 
plot(bank$y, col = coul)

source("http://www.sthda.com/upload/rquery_cormat.r")
#variables numericas
bank_nume <- bank_unique[,c("age","duration","campaign","pdays","previous","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m","nr.employed")]
require("corrplot")
#Correlacion de variables numericas
rquery.cormat(bank_nume)
#Resumen variables numericas
summary(bank_nume)

#Separacion de la base de datos acorde del valor de y
bankno <- subset(bank_unique, bank_unique$y == 'no')
bankyes <- subset(bank_unique, bank_unique$y == 'yes')


#Analisis para pdays
bank999 <- subset(bank_unique, bank_unique$pdays == 999)



#Variables categoricas
bank_unique_cate <- bank_unique[,c("job","marital","education","default","housing","loan","contact","month","poutcome")]



bank_unique <- bank_unique[,c("job","marital","education","default","housing","loan","contact","month", "day_of_week", "poutcome","y")]

samplee <- sample(1:nrow(bank_unique), 100, replace = FALSE)
samplee <- bank_unique[samplee,]
# Resumen base de datos 
summary(bank)

summary(bank_unique)

# Chi test de homogeneidad por cada variable categorica contra Y considerando un alpha = 0.05
# Job
chisq.test(bank_unique$job, bank_unique$y)
count(bank_unique, bank_unique$job == 'unknown') # 0,8%
ggplot(bank_unique,aes(job))+geom_bar(aes(fill= y), position = position_fill())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Marital
chisq.test(bank_unique$marital, bank_unique$y)
count(bank_unique, bank_unique$marital == 'unknown') # 0.19%
ggplot(bank_unique,aes(marital))+geom_bar(aes(fill= y), position = position_fill())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Education
chisq.test(bank_unique$education, bank_unique$y)
count(bank_unique, bank_unique$education == 'unknown') # 4.2%
ggplot(bank_unique,aes(education))+geom_bar(aes(fill= y), position = position_fill())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Default
chisq.test(bank_unique$default, bank_unique$y)
count(bank_unique, bank_unique$default == 'unknown') # 20.88%
ggplot(bank_unique,aes(default))+geom_bar(aes(fill= y), position = position_fill())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Housing
chisq.test(bank_unique$housing, bank_unique$y) # p-valor = 0.05664 > alpha 
count(bank_unique, bank_unique$housing == 'unknown') # 2.4%
# Se entiende que tienen las mismas proporciones tanto para si y para no con housing
ggplot(bank_unique,aes(housing))+geom_bar(aes(fill= y), position = position_fill())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Loan
chisq.test(bank_unique$loan, bank_unique$y) # p-valor = 0.05772 > alpha 
count(bank_unique, bank_unique$loan == 'unknown') # 2.4%
# Se entiende que tienen las mismas proporciones tanto para si y para no con loan
ggplot(bank_unique,aes(loan))+geom_bar(aes(fill= y), position = position_fill())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Contact
chisq.test(bank_unique$contact, bank_unique$y)
count(bank_unique, bank_unique$contact == 'unknown') #0%
ggplot(bank_unique,aes(contact))+geom_bar(aes(fill= y), position = position_fill())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Month
chisq.test(bank_unique$month, bank_unique$y)
count(bank_unique, bank_unique$month == 'unknown') #0%
ggplot(bank_unique,aes(month))+geom_bar(aes(fill= y), position = position_fill())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Day_of_week
chisq.test(bank_unique$day_of_week, bank_unique$y)
count(bank_unique, bank_unique$day_of_week == 'unknown') #0%
ggplot(bank_unique,aes(day_of_week))+geom_bar(aes(fill= y), position = position_fill())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Poutcome
chisq.test(bank_unique$poutcome, bank_unique$y)
count(bank_unique, bank_unique$poutcome == 'unknown') #0%
ggplot(bank_unique,aes(poutcome))+geom_bar(aes(fill= y), position = position_fill())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Por tanto se verifica que solo Housing y Loan son variables que tienen las mismas proporciones para Y
# Es por ello que no son significativas a la hora de hacer un modelo.

# Se puede revisar adem?s que la variable 'unknown' es menos del 5% en job, marital, education, housing, loan y poutcome
# lo cual se pueden borrar aquellas observaciones para que no interfieran con el modelo a crear.

bank_clean <- filter(bank_unique,  bank_unique$job != 'unknown', bank_unique$marital !='unknown', bank_unique$education != 'unknown')

ggplot(bank_clean,aes(job))+geom_bar(aes(fill= y), position = position_fill())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=13))

ggplot(bank_clean,aes(marital))+geom_bar(aes(fill= y), position = position_fill())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=13))
ggplot(bank_clean,aes(education))+geom_bar(aes(fill= y), position = position_fill())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=13))
ggplot(bank_clean,aes(default))+geom_bar(aes(fill= y), position = position_fill())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=13))
ggplot(bank_clean,aes(housing))+geom_bar(aes(fill= y), position = position_fill())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=13))
ggplot(bank_clean,aes(loan))+geom_bar(aes(fill= y), position = position_fill())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=13))
ggplot(bank_clean,aes(contact))+geom_bar(aes(fill= y), position = position_fill())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=13))
ggplot(bank_clean,aes(month))+geom_bar(aes(fill= y), position = position_fill())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=13))
ggplot(bank_clean,aes(day_of_week))+geom_bar(aes(fill= y), position = position_fill())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=13))
ggplot(bank_clean,aes(poutcome))+geom_bar(aes(fill= y), position = position_fill())+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=13))

# a <- cramer(bank_clean$job, bank_clean$y) # xd no funciona




#Variables numericas


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

