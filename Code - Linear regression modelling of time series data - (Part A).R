library(readr)
library(tidyverse)
library(plyr)
library(esquisse)
library(ggfortify)
library(seasonal)
library(forecast)
library(lubridate)
library(sqldf)
library(hydroGOF)

transactions <- read_csv("transactions.csv")







transact_date_clean <- transactions

transact_date_clean$date <- as.Date(transact_date_clean$date, "%d/%m/%y")



transact_arranged <- arrange(transact_date_clean,industry,location)



transact_arranged$industry = as.factor(transact_arranged$industry)


transact_arranged$location = as.factor(transact_arranged$location)



transact_arranged$YEAR = year(transact_arranged$date)

transact_arranged$MONTH = month(transact_arranged$date)

transact_arranged$MONTH = as.factor(transact_arranged$MONTH)




ind_agg <- aggregate(monthly_amount~industry+date, data=transact_arranged,mean)



ggplot(ind_agg) +
  aes(x = date, y = monthly_amount) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  labs(x = "Date", y = "Mean Monthly amount", title = "Mean Monthly amount across industries") +
  theme_gray() +
  facet_wrap(vars(industry), scales = "free") +
  geom_smooth(method='lm', color = "black", size = 0.5)



  


loc_agg <- aggregate(monthly_amount~location+date, data=transact_arranged,mean)



ggplot(loc_agg) +
  aes(x = date, y = monthly_amount) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  labs(x = "Date", y = "Mean Monthly amount", title = "Mean Monthly amount across Locations") +
  theme_gray() +
  facet_wrap(vars(location), scales = "free") +
  geom_smooth(method='lm', color = "black", size = 0.5)



transact_arranged$customer_count <- 1

ind_agg_cus <- aggregate(customer_count~industry+date, data=transact_arranged,sum)

ggplot(ind_agg_cus) +
  aes(x = date, y = customer_count) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  labs(x = "Date", y = "Number of customers", title = "Number of customers across each Industry") +
  theme_gray() +
  facet_wrap(vars(industry), scales = "free") +
  geom_smooth(method='lm', color = "black", size = 0.5)


loc_agg_cus <- aggregate(customer_count~location+date, data=transact_arranged,sum)

ggplot(loc_agg_cus) +
  aes(x = date, y = customer_count) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  labs(x = "Date", y = "Number of customers", title = "Number of customers across each Location") +
  theme_gray() +
  facet_wrap(vars(location), scales = "free") +
  geom_smooth(method='lm', color = "black", size = 0.5)




ind_agg_total_month <- aggregate(monthly_amount~industry+date, data=transact_arranged,sum)



ggplot(ind_agg_total_month) +
  aes(x = date, y = monthly_amount) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  labs(x = "Date", y = "Total Monthly amount", title = "Total Monthly amount across industries") +
  theme_gray() +
  facet_wrap(vars(industry), scales = "free") +
  geom_smooth(method='lm', color = "black", size = 0.5)


loc_agg_total_month <- aggregate(monthly_amount~location+date, data=transact_arranged,sum)



ggplot(loc_agg_total_month) +
  aes(x = date, y = monthly_amount) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  labs(x = "Date", y = "Total Monthly amount", title = "Total Monthly amount across locations") +
  theme_gray() +
  facet_wrap(vars(location), scales = "free") +
  geom_smooth(method='lm', color = "black", size = 0.5)






ggplot(transact_arranged) +
  aes(x = industry, weight = monthly_amount) +
  geom_bar(fill = "#0c4c8a") +
  labs(x = "Industry", y = "Amount", title = "Total Amount collected across Industries") +
  theme_gray()


ggplot(transact_arranged) +
  aes(x = location, weight = monthly_amount) +
  geom_bar(fill = "#0c4c8a") +
  labs(x = "Location", y = "Amount", title = "Total Amount collected across Locations") +
  theme_gray()



ggplot(transact_arranged) +
  aes(x = MONTH, weight = monthly_amount) +
  geom_bar(fill = "#0c4c8a") +
  labs(x = "Month", y = "Amount", title = "Total Amount collected across Months") +
  theme_gray()







ggplot(transact_arranged) +
  aes(x = location, fill = industry, weight = monthly_amount) +
  geom_bar(position = "dodge") +
  scale_fill_hue() +
  labs(y = "Monthly Amount", title = "Total Monthly Amount across various industry location combinations") +
  theme_gray()




ggplot(transact_arranged) +
  aes(x = MONTH, fill = industry, weight = monthly_amount) +
  geom_bar(position = "dodge") +
  scale_fill_hue() +
  labs(y = "Amount", title = "Total amount across months and industry") +
  theme_minimal()




ggplot(transact_arranged) +
  aes(x = MONTH, fill = location, weight = monthly_amount) +
  geom_bar(position = "dodge") +
  scale_fill_hue() +
  labs(y = "Amount", title = "Total amount across months and location") +
  theme_minimal()







transact_aggregate <- aggregate(monthly_amount~industry+location+date, data=transact_date_clean,mean)


transact_aggregate$YEAR = year(transact_aggregate$date)

transact_aggregate$MONTH = month(transact_aggregate$date)

transact_aggregate$MONTH = as.factor(transact_aggregate$MONTH)




i1l1 <- transact_aggregate %>% 
  filter(industry==1 & location==1)


ggplot(i1l1) +
  aes(x = date, y = monthly_amount) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  theme_linedraw() +
  geom_smooth(method='lm', color = "black", size = 0.5) +
  labs(y = "Average Monthly Amount", title = "Avergae monthly amount of industry 1 location 1") 



time_ser=ts(i1l1,frequency=12,start=c(2013,1),end=c(2016,11))

time_ser[,"monthly_amount"] %>% seas() %>%
  autoplot() + xlab("Year") +
  ggtitle("SEATS decomposition of Mean Monthly amount for industry : 1 ,Location : 1")




time_ser1=ts(i1l1$monthly_amount,frequency=12,start=c(2013,1),end=c(2016,11))

ggseasonplot(time_ser1) +
  ylab("Mean Monthly amount ") +
  ggtitle("Seasonal plot: Mean Monthly amount for industry : 1 ,Location : 1")


ggseasonplot(time_ser1, polar=TRUE) +
  ylab("Mean Monthly amount ") +
  ggtitle("Polar seasonal plot: Mean Monthly amount for industry : 1 ,Location : 1")



ind = 36

train_ind <- seq_len(ind)

trainset <- i1l1[train_ind,]

testset <- i1l1[-train_ind,]



data.lm = lm(formula = monthly_amount ~ YEAR + MONTH, data = trainset)

summary(data.lm)



test_pred <- predict(data.lm, testset)


test_pred_add <- list(test_pred)


test_prediction <- as.data.frame(test_pred_add)




names(test_prediction)[1] <- "Test_Prediction"


test_actual_pred <- cbind(testset , test_prediction)


rmse <- hydroGOF::rmse(test_actual_pred$Test_Prediction, test_actual_pred$monthly_amount )








MONTH <- as.factor(12)
YEAR <- 2016


dec_2016 <- data.frame(YEAR , MONTH)


pred_dec_2016 <- predict(data.lm, dec_2016)




pred_plot <- i1l1 %>% select(YEAR, MONTH)



Act_pred_plot <- predict(data.lm, pred_plot)


Act_pred_plot1 <- list(Act_pred_plot)


Act_pred_plot_df <- as.data.frame(Act_pred_plot1)



names(Act_pred_plot_df)[1] <- "Predicted"



new_i1l1 <- i1l1



new_i1l1 <- cbind(new_i1l1 , Act_pred_plot_df)





pred_vs_actual <- new_i1l1 %>% select(monthly_amount, Predicted)


names(pred_vs_actual)[1] <- "Actual"


time_ser_2=ts(pred_vs_actual,frequency=12,start=c(2013,1),end=c(2016,11))


autoplot(time_ser_2[,]) +
  ggtitle("Actual and Predicted values of Mean Monthly amount for industry : 1 ,Location : 1") +
  xlab("Year") +
  ylab("Mean Monthly amount")

  

  Industry_new <- list()

location_new <- list()

prediction_123 <- list()

actual_123 <- list()

date_123 <- list()
  
  
  industry <- list()

location <- list()

R_Square <- list()

Adjusted_R_Square <- list()

RMSE_Error <- list()

pred_dec_2016 <- list()


for(i in 1:10)
{
  for(j in 1:10)
  {
    filt <- transact_aggregate %>% 
      filter(industry==i & location==j)
    
    if(nrow(filt) > 15)
    {
      
      ind = ceiling(nrow(filt) * 0.75)
      
      train_ind <- seq_len(ind)
      
      trainset <- filt[train_ind,]
      
      testset <- filt[-train_ind,]
      
      
      data.lm = lm(formula = monthly_amount ~ YEAR + MONTH, data = trainset)
      
      summary(data.lm)
      
      a <- predict(data.lm, testset)
      
      b <- predict(data.lm, dec_2016)
      
      
      

        
        
        test_pred_add1 <- list(a)
      
      
      test_prediction1 <- as.data.frame(test_pred_add1)
      
      
      
      
      names(test_prediction1)[1] <- "Test_Prediction"
      
      
      test_actual_pred1 <- cbind(testset , test_prediction1)
      
      
      rmse <- hydroGOF::rmse(test_actual_pred1$Test_Prediction, test_actual_pred1$monthly_amount )
      
        
        
      
      Industry_new <- append(Industry_new , test_actual_pred1$industry)
      
      location_new <- append(location_new , test_actual_pred1$location)
      
      prediction_123 <- append(prediction_123 , test_actual_pred1$Test_Prediction)
      
      actual_123 <- append(actual_123 , test_actual_pred1$monthly_amount)
      
      
      date_123 <- append(date_123, test_actual_pred1$date)
      
        
        

      
      
      
      
      
      industry <- append(industry , i)
      
      location <- append(location , j)
      
      R_Square <- append(R_Square , summary(data.lm)$r.squared)
      
      Adjusted_R_Square <- append(Adjusted_R_Square , summary(data.lm)$adj.r.squared)
      
      RMSE_Error <- append(RMSE_Error , rmse)
      
      pred_dec_2016 <- append(pred_dec_2016 , b)
    }
    
  }
}



eval_metrics <- as.data.frame(do.call(rbind, Map(data.frame, A=industry, B=location , C=R_Square, D=Adjusted_R_Square , E = RMSE_Error, F = pred_dec_2016)))        


names(eval_metrics)[1] <- "Industry"
names(eval_metrics)[2] <- "Location"
names(eval_metrics)[3] <- "R_Square"
names(eval_metrics)[4] <- "Adjusted_R_Square"
names(eval_metrics)[5] <- "RMSE_Error"
names(eval_metrics)[6] <- "pred_dec_2016"







pred_actual_diff <- as.data.frame(do.call(rbind, Map(data.frame, A=date_123, B=Industry_new , C=location_new, D=prediction_123 , E = actual_123)))        


names(pred_actual_diff)[1] <- "Date"
names(pred_actual_diff)[2] <- "Industry"
names(pred_actual_diff)[3] <- "Location"
names(pred_actual_diff)[4] <- "Prediction"
names(pred_actual_diff)[5] <- "Actual_Value"

pred_actual_diff$difference = pred_actual_diff$Prediction - pred_actual_diff$Actual_Value



