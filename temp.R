library(readr)
library(tidyverse)
library(plyr)
library(esquisse)
library(ggfortify)
library(seasonal)
library(forecast)
library(lubridate)


transactions <- read_csv("transactions.csv")



transact_date_clean <- transactions

transact_date_clean$date <- as.Date(transact_date_clean$date, "%d/%m/%y")


#transact_date_clean$ny = year(transact_date_clean$date)

#class(transact_date_clean$ny)


transact_arranged <- arrange(transact_date_clean,industry,location)


transact_aggregate <- aggregate(monthly_amount~industry+location+date, data=transact_date_clean,mean)


transact_aggregate$YEAR = year(transact_aggregate$date)

transact_aggregate$MONTH = month(transact_aggregate$date)

transact_aggregate$MONTH = as.factor(transact_aggregate$MONTH)


test <- transact_aggregate %>% 
  filter(industry==9 & location==10)



ggplot(test) +
  aes(x = date, y = monthly_amount) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  theme_linedraw()


test1 <- test %>% select(date,monthly_amount)

time_ser=ts(test1,frequency=12,start=c(2013,1),end=c(2016,11))


autoplot(time_ser[,"monthly_amount"]) +
  ggtitle("Mean Monthly amount for industry : 1 ,Location : 1") +
  xlab("Year") +
  ylab("Mean Monthly amount")



time_ser[,"monthly_amount"] %>% seas() %>%
  autoplot() + xlab("Year") +
  ggtitle("SEATS decomposition of Mean Monthly amount for industry : 1 ,Location : 1")


time_ser1=ts(test1$monthly_amount,frequency=12,start=c(2013,1),end=c(2016,11))

ggseasonplot(time_ser1) +
  ylab("Mean Monthly amount ") +
  ggtitle("Seasonal plot: Mean Monthly amount for industry : 1 ,Location : 1")


ggseasonplot(time_ser1, polar=TRUE) +
  ylab("Mean Monthly amount ") +
  ggtitle("Polar seasonal plot: Mean Monthly amount for industry : 1 ,Location : 1")







ind = ceiling(nrow(test) * 0.75)

train_ind <- seq_len(ind)

trainset <- test[train_ind,]

testset <- test[-train_ind,]



data.lm = lm(formula = monthly_amount ~ YEAR + MONTH, data = trainset)

summary(data.lm)



a <- predict(data.lm, testset)


MONTH <- as.factor(12)
YEAR <- 2016


dec_2016 <- data.frame(YEAR , MONTH)

str(dec_2016)


b <- predict(data.lm, dec_2016)





