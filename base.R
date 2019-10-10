library("stochvol")
library("ggplot2")

data <- read.csv("kous.csv", na.strings=".")
names(data) <- c("date", "rate")
data <- na.omit(data)

# Use data after implementing flexible exchange rate system in Korea
# This is since Dec, 1997. 
data$date <- as.Date(data$date)
data <- subset(data, data$date > "1997-11-30")

return <- logret(data$rate, demean = TRUE)

par(mfrow=c(2,1))
plot(data, type="l")
plot(return, type="l")

mod1 <- svsample(return)

summary(mod1, showlatent = FALSE)
volplot(mod1, forecast = 30, dates = data$date[-1])


par(mfrow = c(3, 1))
paratraceplot(mod1)

par(mfrow = c(1, 3))
paradensplot(res, showobs = FALSE)

plot(mod1, showobs = TRUE)
plot(resid(mod1), return)


# One-day ahead prediction and its 98% posterior interval based on stochvol. 
predict_stochvol <- function(train_size=1000, predict_length){
  
  return_predict <- array(NA, dim=c(predict_length,3,2))
  dimnames(return_predict)[[3]] <- c("y", "h")
  
  for (i in 1:predict_length){
    return_train <- return[1:train_size]
    
    mod <- svsample(return_train, draws=5000, burn=1000)
    pred <- predict(mod, 1)
    
    return_predict[i, , "y"] <- as.vector(quantile(pred$y, c(0.01, 0.5, 0.99)))
    return_predict[i, , "h"] <- as.vector(quantile(pred$h, c(0.01, 0.5, 0.99)))
    
    train_size = train_size + 1
  }
  
  return(return_predict)
}


pred <- predict_stochvol(predict_length=120)

# This is the plot1.pdf.
pred_date <- data$date[1002:1121]

ggplot() + 
  geom_line(aes(x=pred_date, y=return[1001:1120], col="return")) + 
  geom_line(aes(x=pred_date, y=pred[, 1, "y"], col="interval")) + 
  geom_line(aes(x=pred_date, y=pred[, 3, "y"], col="interval")) +  
  scale_colour_manual("", 
                      values=c("interval"="red", "return"="darkgreen"),
                      labels=c("98% posterior interval of one-day ahead forecast", 
                               "actual return")) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position=c(0.8, 0.9), legend.title=element_blank()) + 
  labs(title="98% posterior interval of forecast within the timeframe 
       (2001-11-26 ~ 2002-05-16) after training on 1000 days", 
       x="Date", y="Return")

