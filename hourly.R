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
