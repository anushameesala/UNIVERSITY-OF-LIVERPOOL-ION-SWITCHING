setwd("C:\\Users\\anush\\OneDrive\\Desktop\\fs\\dar\\finalproject")
library(data.table)

train <- fread("traink.csv")
test <- fread("testk.csv")

gc()
time <- test$time
exTime1 <- seq(from = -9.9998, to = 0, by = .0001)
exTime2 <- seq(from = 500.0001, to = 510.9999, by = .0001)
with(train,{
  extend1 <<- train[time >=40.0001 & time < 50,]
  extend2 <<- train[time >=50.0001 & time < 61,] 
})
extend1 <- extend1[order(extend1$time),]
extend2 <- extend2[order(extend2$time),]
extend1$time <- exTime1
extend2$time <- exTime2
train <- rbind(extend1, train, extend2)
exTime1 <- seq(from = 480.0001, to = 500.0000, by = .0001)
exTime2 <- seq(from = 700.0001, to = 720.0000, by = .0001)
with(test,{
  extend1 <<- rbind(test[time > 500 & time <= 510,],test[time > 500 & time <= 510,])
  extend2 <<- test[time > 680 & time <= 700,]
})
extend1 <- extend1
extend2 <- extend2[order(extend2$time),]
extend1$time <- exTime1
extend2$time <- exTime2
test <- rbind(extend1, test, extend2)

rm(extend1, extend2, exTime1, exTime2)
library (RcppRoll)
lags = c(11, 21, 31, 51, 101, 1001)
CenteredRoll <- function(DF, lags){
  
  Features = NULL
  
  for (l in lags) {
    Start = Sys.time()
    mea <-RcppRoll::roll_mean(DF$signal,n = l,fill = NA,align = "center")
    End = Sys.time()
    print(End - Start)
    
    Start = Sys.time()
    sdev <-RcppRoll::roll_sd(DF$signal,n = l,fill = NA,align = "center")
    End = Sys.time()
    print(End - Start)
    
    Start = Sys.time()
    maxi <-RcppRoll::roll_max(DF$signal,n = l,fill = NA,align = "center")
    
    End = Sys.time()
    print(End - Start)
    
    Start = Sys.time()
    mini <-RcppRoll::roll_min(DF$signal,n = l,fill = NA,align = "center")
    End = Sys.time()
    print(End - Start)
    
    Start = Sys.time()
    Wmea <- RcppRoll::roll_mean(DF$signal,n = l,weights = c((1:((l-1)/2)) / ((l-1)/2 * ((l-1)/2 + 1) / 1)-max((1:((l-1)/2)) / ((l-1)/2 * ((l-1)/2 + 1) / 1))/((l-1)/2),max((1:((l-1)/2)) / ((l-1)/2 * ((l-1)/2 + 1) / 1))*2,(((l-1)/2):1) / ((l-1)/2 * ((l-1)/2 + 1) / 1)-max((1:((l-1)/2)) / ((l-1)/2 * ((l-1)/2 + 1) / 1))/((l-1)/2)), fill = NA,align = "center")
    
    End = Sys.time()
    print(End - Start)
    
    #medi,
    Features <- cbind(Features, mea, sdev, maxi, mini, Wmea)
    
    
    colnames(Features)[c(
      (ncol(Features) - 4),
      (ncol(Features) - 3),
      (ncol(Features) - 2),
      (ncol(Features) - 1),
      ncol(Features))] <-
      c(
        paste("mea", l, sep = "_"),
        paste("sdev", l, sep = "_"),
        paste("max", l, sep = "_"),
        paste("min", l, sep = "_"),
        paste("Wmea", l, sep = "_")
      )
    
    gc()
    print(l)
  }
  
  return(Features)
}
LRRoll <- function(DF){
  
  LR = NULL
  
  l=100
  right_W100 <- 
    RcppRoll::roll_mean(
      DF$signal,
      n = l,
      weights = (1:l) / (l * (l + 1) / 2),
      fill = NA,
      align = "right"
    )
  left_W100 <- 
    RcppRoll::roll_mean(
      DF$signal,
      n = l,
      weights = (l:1) / (l * (l + 1) / 2),
      fill = NA,
      align = "left"
    )
  
  l=1000
  right_W1000 <- 
    RcppRoll::roll_mean(
      DF$signal,
      n = l,
      weights = (1:l) / (l * (l + 1) / 2),
      fill = NA,
      align = "right"
    )
  left_W1000 <- 
    RcppRoll::roll_mean(
      DF$signal,
      n = l,
      weights = (l:1) / (l * (l + 1) / 2),
      fill = NA,
      align = "left"
    )
  
  LR <- cbind(LR, right_W100, right_W1000, left_W100, left_W1000)
  
  return(LR)
}
specialFeatures <- function(DF){
  
  SFeatures = NULL
  #Lags----------------------------------
  L1 <- c(NA, DF$signal[1:(nrow(DF)-1)])
  L2 <- c(c(NA,NA), DF$signal[1:(nrow(DF)-2)])
  L3 <- c(c(NA, NA, NA), DF$signal[1:(nrow(DF)-3)])
  
  F1 <- c(DF$signal[2:nrow(DF)], NA)
  F2 <- c(DF$signal[3:nrow(DF)], c(NA, NA))
  F3 <- c(DF$signal[4:nrow(DF)], c(NA, NA, NA))
  #specials----------------------------------------
  dL1 <- c(NA, diff(DF$signal))
  dF1 <- c(diff(DF$signal), NA)
  signalAbs <- abs(DF$signal)
  SigSquarePN <- sign(DF$signal)*abs(DF$signal)^2
  SigSquare <- DF$signal^2
  signaleSqRo <- sign(DF$signal)*abs(DF$signal)^(1/2)
  
  SFeatures <- cbind(SFeatures, L1, L2, L3, F1, F2, F3, dL1, dF1, signalAbs, SigSquarePN, SigSquare, signaleSqRo)
  
  return(SFeatures)
}
RollFeat <- CenteredRoll(train, lags)
LRFeat <- LRRoll(train)
specials <- specialFeatures(train)
DF_tr <- cbind(train, RollFeat, LRFeat, specials)
rm(RollFeat, LRFeat, specials)
DF_tr$signal_M1001 <- DF_tr$signal - DF_tr$mea_1001
DF_tr$signal_M101 <- DF_tr$signal - DF_tr$mea_101
DF_tr$batch <- DF_tr$time %/%10
#DF_tr$batch <- round(DF_tr$time/10, digits = 0) 
batch75 <- aggregate(signal~batch, data = DF_tr, FUN = quantile, probs = .75)
colnames(batch75)[2] <- "signal75"
DF_tr <- merge(x = DF_tr, y = batch75, by = "batch", all.x = T)
batch25 <- aggregate(signal~batch, data = DF_tr, FUN = quantile, probs = .25)
colnames(batch25)[2] <- "signal25"
DF_tr <- merge(x = DF_tr, y = batch25, by = "batch", all.x = T)
batchMax <- aggregate(signal~batch, data = DF_tr, FUN = max)
colnames(batchMax)[2] <- "signalMax"
DF_tr <- merge(x = DF_tr, y = batchMax, by = "batch", all.x = T)
batchMin <- aggregate(signal~batch, data = DF_tr, FUN = min)
colnames(batchMin)[2] <- "signalMin"
DF_tr <- merge(x = DF_tr, y = batchMin, by = "batch", all.x = T)
DF_tr <- DF_tr[order(DF_tr$time),]
DF_tr$UL <- DF_tr$max_1001 - DF_tr$min_1001
DF_tr$DD <- 0
DF_tr$DD <- ifelse(DF_tr$time <= 100, 1, DF_tr$DD)
DF_tr$DD <- ifelse(DF_tr$time > 100 & DF_tr$time <= 150, 1, DF_tr$DD)
DF_tr$DD <- ifelse(DF_tr$time > 150 & DF_tr$time <= 200, 3, DF_tr$DD)
DF_tr$DD <- ifelse(DF_tr$time > 200 & DF_tr$time <= 250, 10, DF_tr$DD)
DF_tr$DD <- ifelse(DF_tr$time > 250 & DF_tr$time <= 300, 5, DF_tr$DD)
DF_tr$DD <- ifelse(DF_tr$time > 300 & DF_tr$time <= 350, 1, DF_tr$DD)
DF_tr$DD <- ifelse(DF_tr$time > 350 & DF_tr$time <= 400, 3, DF_tr$DD)
DF_tr$DD <- ifelse(DF_tr$time > 400 & DF_tr$time <= 450, 5, DF_tr$DD)
DF_tr$DD <- ifelse(DF_tr$time > 450 & DF_tr$time <= 500, 10, DF_tr$DD)
DF_tr$DD <- ifelse(DF_tr$time > 500, 1, DF_tr$DD)
DF_tr <- DF_tr[complete.cases(DF_tr),]
rm(batch75, batch25, batchMax, batchMin, train)
gc()
RollFeat <- CenteredRoll(test, lags)
LRFeat <- LRRoll(test)
specials <- specialFeatures(test)
DF_te <- cbind(test, RollFeat, LRFeat, specials)
rm(RollFeat, LRFeat, specials)
DF_te$signal_M1001 <- DF_te$signal - DF_te$mea_1001#centered mean
DF_te$signal_M101 <- DF_te$signal - DF_te$mea_101
DF_te$batch <- DF_te$time %/%10
batch75 <- aggregate(signal~batch, data = DF_te, FUN = quantile, probs = .75)
colnames(batch75)[2] <- "signal75"
DF_te <- merge(x = DF_te, y = batch75, by = "batch", all.x = T)
batch25 <- aggregate(signal~batch, data = DF_te, FUN = quantile, probs = .25)
colnames(batch25)[2] <- "signal25"
DF_te <- merge(x = DF_te, y = batch25, by = "batch", all.x = T)
batchMax <- aggregate(signal~batch, data = DF_te, FUN = max)
colnames(batchMax)[2] <- "signalMax"
DF_te <- merge(x = DF_te, y = batchMax, by = "batch", all.x = T)
batchMin <- aggregate(signal~batch, data = DF_te, FUN = min)
colnames(batchMin)[2] <- "signalMin"
DF_te <- merge(x = DF_te, y = batchMin, by = "batch", all.x = T)
DF_te <- DF_te[order(DF_te$time),]
DF_te$UL <- DF_te$max_1001 - DF_te$min_1001
DF_te$DD <- 0
DF_te$DD <- ifelse(DF_te$time <= 500, 1, DF_te$DD)
DF_te$DD <- ifelse(DF_te$time > 500 & DF_te$time <= 510, 1, DF_te$DD)
DF_te$DD <- ifelse(DF_te$time > 510 & DF_te$time <= 520, 3, DF_te$DD)
DF_te$DD <- ifelse(DF_te$time > 520 & DF_te$time <= 530, 5, DF_te$DD)
DF_te$DD <- ifelse(DF_te$time > 530 & DF_te$time <= 540, 1, DF_te$DD)
DF_te$DD <- ifelse(DF_te$time > 540 & DF_te$time <= 550, 1, DF_te$DD)
DF_te$DD <- ifelse(DF_te$time > 550 & DF_te$time <= 560, 10, DF_te$DD)
DF_te$DD <- ifelse(DF_te$time > 560 & DF_te$time <= 570, 5, DF_te$DD)
DF_te$DD <- ifelse(DF_te$time > 570 & DF_te$time <= 580, 10, DF_te$DD)
DF_te$DD <- ifelse(DF_te$time > 580 & DF_te$time <= 590, 1, DF_te$DD)
DF_te$DD <- ifelse(DF_te$time > 590 & DF_te$time <= 600, 3, DF_te$DD)
DF_te$DD <- ifelse(DF_te$time > 600, 1, DF_te$DD)
DF_te <- DF_te[complete.cases(DF_te),]
rm(batch75, batch25, batchMax, batchMin, test)
gc()
DF_tr <- as.data.frame(DF_tr)
DF_te <- as.data.frame(DF_te)
DF_tr <- DF_tr[DF_tr$time > 0 & DF_tr$time <= 500,-which(colnames(DF_tr) %in% c("time", "batch"))]
DF_te <- DF_te[DF_te$time > 500 & DF_te$time <= 700,-which(colnames(DF_te) %in% c("time", "batch"))]
gc()
head(DF_tr)
head(DF_te)
dim(DF_tr)
dim(DF_te)
library(keras)
y_tr <- to_categorical(DF_tr$open_channels)
DF_tr <-
  DF_tr[,-c(which(
    colnames(DF_tr) %in% c(
      "open_channels"
    )
  ))]
DF_tr <- as.matrix(DF_tr)
DF_te <- as.matrix(DF_te)
for(c in 1:ncol(DF_tr)){
  trm <- (DF_tr[,c] - mean(c(DF_tr[,c], DF_te[,c]))) / sd(c(DF_tr[,c], DF_te[,c]))
  tem <- (DF_te[,c] - mean(c(DF_tr[,c], DF_te[,c]))) / sd(c(DF_tr[,c], DF_te[,c]))
  
  DF_tr[,c] <- trm
  DF_te[,c] <- tem
}
rm(tem, trm)
gc()
DF_tr <- array_reshape(DF_tr, c(dim(DF_tr), 1))
gc()
DF_te <- array_reshape(DF_te, c(dim(DF_te), 1))
gc()
dim(DF_tr); dim(DF_te)
lr_scheduler = function(epoch, lr) {
  if (epoch < 25) {
    return(.001-.00002*epoch)
  } else if(epoch >= 25 & epoch < 35){
    return(.0012-.00002*epoch)
  } else if(epoch >= 35 & epoch < 40){
    return(.0013-.00002*epoch)
  } else if(epoch >= 40 & epoch < 50){
    return(.0014-.00002*epoch)
  } else {
    return(.0015-.00002*epoch)
  }
}
model <- keras_model_sequential() 
model %>%layer_conv_1d(filters = 10,kernel_size = 8,strides = 1,activation='relu',padding = "same", input_shape = c(dim(DF_tr)[2],1)) %>%
  layer_conv_1d(filters = 15,kernel_size = 6,dilation_rate = 8,activation='relu',padding = "same") %>%
  layer_conv_1d(filters = 20,kernel_size = 3,dilation_rate = 6,activation='relu',padding = "same") %>%
  layer_flatten() %>% 
  
  layer_dense(units = 96,activation = 'relu'
    #,regularizer_l1_l2(l1 = .01, l2 = .0001)
  ) %>%
  layer_dropout(rate = 0.05) %>%
  layer_dense(units = 32,
              activation = 'relu'
  ) %>%
  layer_dropout(rate = 0.025) %>%
  layer_dense(units = 11, activation = 'softmax')
summary(model)
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adam(beta_1 = .9, beta_2 = .99),
  metrics = c('accuracy')
)
gc()
history <- model %>% fit(
  DF_tr, y_tr, 
  epochs = 50, 
  batch_size = 50000,
  callbacks = callback_learning_rate_scheduler(lr_scheduler)
)
gc()
plot(history)
predKeras <- model %>% predict_classes(DF_te)
predKeras <- predKeras
sample_sub <- fread("sample_submission.csv", colClasses = "character")
sample_sub$open_channels <- predKeras
fwrite(sample_sub, "submission.csv")
gc()
test <- fread("testk.csv")
library(ggplot2)
sample_sub$open_channels <- as.factor(sample_sub$open_channels)
sample_sub$time <- test$time[test$time > 500 & test$time <= 700]
sample_sub$time <- test$time[test$time > 500 & test$time <= 700]
options(repr.plot.width = 15, repr.plot.height = 10)
ggplot(sample_sub, aes(x=time,y=signal))+geom_point(shape=16, aes(color = open_channels), alpha = 0.4)+theme_grey(base_size = 22)+guides(colour=guide_legend(override.aes = list(size=10)))
gc()