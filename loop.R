MONTH <- as.factor(12)
YEAR <- 2016


dec_2016 <- data.frame(YEAR , MONTH)



industry <- list()

location <- list()

R_Square <- list()

Adjusted_R_Square <- list()

pred_dec_2016 <- list()


for(i in 1:10)
{
  for(j in 1:10)
  {
    test <- transact_aggregate %>% 
      filter(industry==i & location==j)
    
    if(nrow(test) > 15)
    {
    
    ind = ceiling(nrow(test) * 0.75)
    
    train_ind <- seq_len(ind)
    
    trainset <- test[train_ind,]
    
    testset <- test[-train_ind,]
    
    
    data.lm = lm(formula = monthly_amount ~ YEAR + MONTH, data = trainset)
    
    summary(data.lm)
    
    a <- predict(data.lm, testset)
    
    b <- predict(data.lm, dec_2016)
    
  
    
    
    
    industry <- append(industry , i)
    
    location <- append(location , j)
    
    R_Square <- append(R_Square , summary(data.lm)$r.squared)
    
    Adjusted_R_Square <- append(Adjusted_R_Square , summary(data.lm)$adj.r.squared)
    
    pred_dec_2016 <- append(pred_dec_2016 , b)
    }
    
  }
}

yan <- as.data.frame(do.call(rbind, Map(data.frame, A=industry, B=location , C=R_Square, D=Adjusted_R_Square , E = pred_dec_2016)))        
