library("stochvol")
library("ggplot2")

data <- read.csv("usdjpy.csv")

time = matrix(NA, ncol=2, nrow=nrow(data))

for (i in 1:nrow(time)){
  time[i, ] <- strsplit(as.character(data$Local.time[i]), split=" ")[[1]][c(1,2)]
  time[i, 2] <- strsplit(time[i, 2], split="\\.")[[1]][1]
}

data <- data.frame(time=as.POSIXct(strptime(paste(time[, 1], time[, 2]), 
                                            format="%d.%m.%Y %H:%M:%S")), 
                   rate=data$Close) 

data <- data[-which(data$time <= "2019-09-09 05:00:00" | 
                    (data$time >= "2019-09-14 05:00:00" & data$time <= "2019-09-16 05:00:00") | 
                    (data$time >= "2019-09-21 05:00:00" & data$time <= "2019-09-23 05:00:00")), ]

plot(data, type="l")

return <- logret(data$rate, demean = TRUE)


predict_stochvol <- function(train_size=150, predict_length){
  
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

pred <- predict_stochvol(predict_length=100)
pred_time <- data$time[152:251]
pred_time <- c(pred_time[1:87], as.POSIXct("2019-09-21 05:00:00"), pred_time[88:100])

# This results in plot2.pdf. 
prediction_df <- data.frame(time=pred_time, 
                            q1=c(pred[1:87, 1, "y"], NA, pred[88:100, 1, "y"]), 
                            q2=c(pred[1:87, 2, "y"], NA, pred[88:100, 2, "y"]),
                            q3=c(pred[1:87, 3, "y"], NA, pred[88:100, 3, "y"]),
                            return=c(return[151:237], NA, return[238:250]))

  
ggplot(data=prediction_df) + 
  geom_line(aes(x=time, y=return, col="return")) + 
  geom_line(aes(x=time, y=q1, col="interval")) + 
  geom_line(aes(x=time, y=q3, col="interval")) +  
  scale_colour_manual("", 
                      values=c("interval"="red", "return"="darkgreen"),
                      labels=c("98% posterior interval of one-hour ahead forecast", 
                               "actual return")) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position=c(0.7, 0.95), legend.title=element_blank()) + 
  labs(title="98% posterior interval of forecast within the timeframe 
       (2019-09-17 ~ 2019-09-23) after training on 150 hours", 
       x="Date", y="Return")

