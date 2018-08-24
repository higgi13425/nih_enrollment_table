library(arsenal)
library(knitr)
library(survival)
library(tidyverse)
data(mockstudy)
dim(mockstudy)
glimpse(mockstudy)

#summary by groups
tab1 <- tableby(arm ~ sex + age, data = mockstudy)
summary(tab1, text=TRUE)

#summary without groups
tab.noby <- tableby(~ bmi + sex +age, data = mockstudy)
summary(tab.noby)

myvars <- names(mockstudy)
rhs <- paste(myvars[8:10], collapse = '+')
rhs 

as.formula(paste('arm ~', rhs))

summary(tableby(as.formula(paste('arm ~', rhs)), data=mockstudy))

summary(tableby(arm ~ sex + fu.time, data=mockstudy), digits=4, digits.p =2, digits.pct =1)

tab1 <- tableby(arm ~ sex + age, data = mockstudy)
write2word(tab1, file = 'table1.docx')

#assign arms
train_df$arm <- c(rep(1,250), rep(2, 250))
train_df$race<- as.factor(train_df$race)

rhs <- paste(names(train_df[c(8,12,14)]), collapse = '+')
tab2 <- summary(tableby(as.formula(paste('arm ~', rhs)), data=train_df), pfootnote=TRUE)
tab2
write2word(tab2, file = 'table3.docx')
write2html(tab2, "~/table3.html")
