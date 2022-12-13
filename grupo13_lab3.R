install.packages('arulesViz')
library('arulesViz')

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
bank$day_of_week <-factor(bank$day_of_week)

#Valor de y en la base de datos
bank$y <- factor(bank$y)
summary(bank)

#Existen duplicados
bank_unique <- unique(bank)

bins <- 16


max_age <- max(bank_unique$age)
min_age <- min(bank_unique$age)

width <- (max_age - min_age)/bins

bank_unique$age <- cut(bank_unique$age, breaks = seq(min_age, max_age, width))


max_duration <- max(bank_unique$duration)
min_duration <- min(bank_unique$duration)
width <- (max_duration - min_duration)/bins

bank_unique$duration <- cut(bank_unique$duration, breaks = seq(min_duration, max_duration, width))


min_campaign <- min(bank_unique$campaign)
max_campaign <- max(bank_unique$campaign)
width <- (max_campaign - min_campaign)/bins

bank_unique$campaign <- cut(bank_unique$campaign, breaks = seq(min_campaign, max_campaign, width))

min_pdays <- min(bank_unique$pdays)
max_pdays <- max(bank_unique$pdays)
width <- (max_pdays - min_pdays)/bins

bank_unique$pdays <- cut(bank_unique$pdays, breaks = seq(min_pdays, max_pdays, width))

min_previous <- min(bank_unique$previous)
max_previous <- max(bank_unique$previous)
width <- (max_previous - min_previous)/bins

bank_unique$previous <- cut(bank_unique$previous, breaks = seq(min_previous, max_previous, width))

#1
min_emp.var.rate <- min(bank_unique$emp.var.rate)
max_emp.var.rate <- max(bank_unique$emp.var.rate)
width <- (max_emp.var.rate - min_emp.var.rate)/bins

bank_unique$emp.var.rate <- cut(bank_unique$emp.var.rate, breaks = seq(min_emp.var.rate, max_emp.var.rate, width))

#2
min_cons.price.idx <- min(bank_unique$cons.price.idx)
max_cons.price.idx <- max(bank_unique$cons.price.idx)
width <- (max_cons.price.idx - min_cons.price.idx)/bins

bank_unique$cons.price.idx <- cut(bank_unique$cons.price.idx, breaks = seq(min_cons.price.idx, max_cons.price.idx, width))

#3
min_cons.conf.idx <- min(bank_unique$cons.conf.idx)
max_cons.conf.idx <- max(bank_unique$cons.conf.idx)
width <- (max_cons.conf.idx - min_cons.conf.idx)/bins

bank_unique$cons.conf.idx <- cut(bank_unique$cons.conf.idx, breaks = seq(min_cons.conf.idx, max_cons.conf.idx, width))

#4
min_euribor3m <- min(bank_unique$euribor3m)
max_euribor3m <- max(bank_unique$euribor3m)
width <- (max_euribor3m - min_euribor3m)/bins

bank_unique$euribor3m <- cut(bank_unique$euribor3m, breaks = seq(min_euribor3m, max_euribor3m, width))

#5
min_nr.employed <- min(bank_unique$nr.employed)
max_nr.employed <- max(bank_unique$nr.employed)
width <- (max_nr.employed - min_nr.employed)/bins

bank_unique$nr.employed <- cut(bank_unique$nr.employed, breaks = seq(min_nr.employed, max_nr.employed, width))

str(bank_unique)

rules <- apriori(bank_unique,parameter = list(supp=0.1, conf=0.8, minlen=2))

rulesyes <- apriori(bank_unique,parameter = list(supp=0.001, conf=0.8, minlen=2),
                 appearance =list(default='lhs', rhs='y=yes'),
                 control=list(verbose=FALSE))

rulesno <- apriori(bank_unique,parameter = list(supp=0.1, conf=0.8, minlen=2),
                    appearance =list(default='lhs', rhs='y=no'),
                    control=list(verbose=FALSE))

inspect(head(sort(rulesyes, by='lift'),3))
inspect(head(sort(rulesyes, by='supp'),3))
inspect(head(sort(rulesyes, by='conf'),3))
inspect(head(sort(rulesno, by='lift'),3))
inspect(head(sort(rulesno, by='supp'),3))
inspect(head(sort(rulesno, by='conf'),3))

inspect(head(sort(rules, by='supp'),3))
inspect(head(sort(rules, by='conf'),3))
inspect(head(sort(rules, by='lift'),3))


