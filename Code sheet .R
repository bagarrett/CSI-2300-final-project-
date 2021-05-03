data <- read.csv("Mines_Park_pH_Fault.csv", stringsAsFactors = FALSE)
attr(data$dateTime, "tzone") <- "America/Denver"

library(lubridate)


#finding where the fault occurred and what row the observation is at
colnames(data)
dim(data)
head(data$dateTime)
tail(data$dateTime)

month.val <- month(data$dateTime)
day.val <- day(data$dateTime)
hour.val <- hour(data$dateTime)
fault.time <- which(month.val==4 & day.val==24 & hour.val==10)[1]
data$dateTime[fault.time]

#narrowing the data to just before the fault occurred
data_before_fault <- data[0:fault.time , ]

#plotting the data to see the pH when the fault occured
ts.plot(data$ras_ph, xlab="Index", ylab="pH")
abline(v = 20338, col = "blue", lwd = 2)
abline(h=6.5,col=4,lwd=2)

complete_model <- lm(ras_ph ~ . , data = data)
summary(complete_model)

BIC_model <- step(complete_model, direction="backward", k = log(nrow(data)),
                  trace = 0)
summary(BIC_model)

#narrowing the data to just the prefered columns for the control variables
data_edited <- data[, c("bio_1_blow_flow", 
                        "bio_2_blow_flow", 
                        "mbr_1_tmp", 
                        "mbr_2_tmp",
                        "ambient_temp", 
                        "bio_1_phase_1", 
                        "bio_1_phase_2", 
                        "bio_2_phase_1",
                        "bio_2_phase_2", 
                        "mbr_1_mode_1", 
                        "mbr_1_mode_2", 
                        "mbr_1_mode_4",
                        "mbr_2_mode_1", 
                        "mbr_2_mode_2", 
                        "mbr_2_mode_4", 
                        "ras_ph")]

data_firstquarter <- data_before_fault[, c("bio_1_blow_flow", 
                           "bio_2_blow_flow", 
                           "mbr_1_tmp", 
                           "mbr_2_tmp",
                           "ras_ph")]
                           
data_secondquarter <- data_before_fault[,c ("ambient_temp", 
                              "bio_1_phase_1", 
                              "bio_1_phase_2", 
                              "ras_ph")]

data_thirdquarter <- data_before_fault[, c("bio_2_phase_1",
                           "bio_2_phase_2", 
                           "mbr_1_mode_1", 
                           "mbr_1_mode_2",
                           "ras_ph")]
data_fourthquarter <- data_before_fault[, c("mbr_1_mode_4",
                               "mbr_2_mode_1", 
                               "mbr_2_mode_2", 
                               "mbr_2_mode_4", 
                               "ras_ph")]

#seeing which variables are correlated with the other variables
pairs(ras_ph ~ ., data = data_firstquarter)
pairs(ras_ph ~ ., data = data_secondquarter)
pairs(ras_ph ~ ., data = data_thirdquarter)
pairs(ras_ph ~ ., data = data_fourthquarter)


#changing the data groups to be from the whole data set
data_firstquarter <- data[, c("bio_1_blow_flow", 
                                           "bio_2_blow_flow", 
                                           "mbr_1_tmp", 
                                           "mbr_2_tmp",
                                           "ras_ph")]

data_secondquarter <- data[,c ("ambient_temp", 
                                            "bio_1_phase_1", 
                                            "bio_1_phase_2", 
                                            "ras_ph")]

data_thirdquarter <- data[, c("bio_2_phase_1",
                                           "bio_2_phase_2", 
                                           "mbr_1_mode_1", 
                                           "mbr_1_mode_2",
                                           "ras_ph")]
data_fourthquarter <- data[, c("mbr_1_mode_4",
                                            "mbr_2_mode_1", 
                                            "mbr_2_mode_2", 
                                            "mbr_2_mode_4", 
                                            "ras_ph")]

#seeing if there is a correlation with plots against the time
par(mfrow = c(2,2))
for(i in 1:4){
  ts.plot(data_firstquarter[,i], xlab = "index",
          ylab = paste(colnames(data_firstquarter[i])))
  abline(v = 20338, col = "blue", lwd = 2)
}


par(mfrow = c(1,3))
for(i in 1:3){
  ts.plot(data_secondquarter[,i], xlab = "index",
          ylab = paste(colnames(data_secondquarter[i])))
  abline(v = 20338, col = "blue", lwd = 2)
}


par(mfrow = c(2,2))
for(i in 1:4){
  ts.plot(data_thirdquarter[,i], xlab = "index",
          ylab = paste(colnames(data_thirdquarter[i])))
  abline(v = 20338, col = "blue", lwd = 2)
}


par(mfrow = c(2,2))
for(i in 1:4){
  ts.plot(data_fourthquarter[,i], xlab = "index",
          ylab = paste(colnames(data_fourthquarter[i])))
  abline(v = 20338, col = "blue", lwd = 2)
}

plot(ras_ph ~ ambient_temp, data = data)


#seeing which varaibles make the best model

full_model <- lm(ras_ph ~ ., data = data_edited)

summary(full_model)

#doing a backwards regressi9on to clear out the variables there don't need to be
BIC_model <- step(full_model, direction="backward", k = log(nrow(data_edited)),
                  trace = 0)
summary(BIC_model)

#checking the correlation on the response variables

response_vars <- c("mbr_1_perm_flow", 
                   "mbr_2_perm_flow", 
                   "ras_temp",
                   "bio_1_do", 
                   "bio_2_do", 
                   "mbr_1_level", 
                   "mbr_2_level", 
                   "perm_turb", 
                   "sewage_flow", 
                   "bio_1_level", 
                   "bio_2_level",
                   "bio_1_temp", 
                   "bio_2_temp", 
                   "bio_1_tss", 
                   "bio_2_tss", 
                   "perm_tank_level", 
                   "ras_do", 
                   "ras_ph", 
                   "perm_cond", 
                   "ras_tss")


#seeing a correlation between pH and response variables

response_pairs1 <- data[, response_vars[c(1,2,3,4,5,18)]]
response_pairs2 <- data[, response_vars[c(6, 7, 8, 9, 10, 18)]]
response_pairs3 <- data[, response_vars[c(11, 12, 13, 14, 15, 18)]]
response_pairs4 <- data[, response_vars[c(16, 17, 19, 20, 18)]]


pairs(ras_ph ~ ., data = response_pairs1)
pairs(ras_ph ~ ., data = response_pairs2)
pairs(ras_ph ~ ., data = response_pairs3)
pairs(ras_ph ~ ., data = response_pairs4)

response_data <- data[, c("mbr_1_perm_flow", 
                          "mbr_2_perm_flow", 
                          "ras_temp",
                          "bio_1_do", 
                          "bio_2_do", 
                          "mbr_1_level", 
                          "mbr_2_level", 
                          "perm_turb", 
                          "sewage_flow", 
                          "bio_1_level", 
                          "bio_2_level",
                          "bio_1_temp", 
                          "bio_2_temp", 
                          "bio_1_tss", 
                          "bio_2_tss", 
                          "perm_tank_level", 
                          "ras_do", 
                          "ras_ph", 
                          "perm_cond", 
                          "ras_tss")]

#splitting the response varaible data
data_responsefirstquarter <- data[, c("mbr_1_perm_flow", 
                                      "mbr_2_perm_flow", 
                                      "ras_temp",
                                      "bio_1_do",
                                      "bio_2_do" )]
data_responsesecondquarter <- data[, c("mbr_1_level", 
                                       "mbr_2_level", 
                                       "perm_turb", 
                                       "sewage_flow", 
                                       "bio_1_level")]
data_responsethirdquarter<- data[, c("bio_2_level",
                                     "bio_1_temp", 
                                     "bio_2_temp", 
                                     "bio_1_tss", 
                                     "bio_2_tss")]
data_responsefourthquarter <- data[, c("perm_tank_level", 
                                       "ras_do", 
                                       "ras_ph", 
                                       "perm_cond", 
                                       "ras_tss")]

#graphing to visually see correlation between response variables and time

par(mfrow = c(2,3))
for(i in 1:5){
  ts.plot(data_responsefirstquarter[,i], xlab = "index",
          ylab = paste(colnames(data_responsefirstquarter[i])))
  abline(v = 20338, col = "blue", lwd = 2)
}

par(mfrow = c(2,3))
for(i in 1:5){
  ts.plot(data_responsesecondquarter[,i], xlab = "index",
          ylab = paste(colnames(data_responsesecondquarter[i])))
  abline(v = 20338, col = "blue", lwd = 2)
}

par(mfrow = c(2,3))
for(i in 1:5){
  ts.plot(data_responsethirdquarter[,i], xlab = "index",
          ylab = paste(colnames(data_responsethirdquarter[i])))
  abline(v = 20338, col = "blue", lwd = 2)
}

par(mfrow = c(2,3))
for(i in 1:5){
  ts.plot(data_responsefourthquarter[,i], xlab = "index",
          ylab = paste(colnames(data_responsefourthquarter[i])))
  abline(v = 20338, col = "blue", lwd = 2)
}

#making a linear mode with the response varaibles
response_model <- lm(ras_ph ~ . , data = response_data)
summary(response_model)

BIC_model <- step(response_model, direction="backward", k = log(nrow(response_data)),
                  trace = 0)
summary(BIC_model)

#setting out the best variables
best_variables <- c("mbr_1_perm_flow",
                    "ras_temp",
                    "bio_1_do" ,
                      "bio_2_do" ,
                    "mbr_2_level",
                     "perm_turb" ,
                    "sewage_flow" ,
                    "bio_1_level" ,
                      "bio_2_level" ,
                    "bio_1_temp" ,
                    "bio_2_temp" ,
                    "bio_1_tss" ,
                    "bio_2_tss" ,
                    "ras_do" ,
                    "perm_cond" ,
                    "ras_tss",
                    "ras_ph")


best_data <- response_data[, best_variables]

#testing various r-squared values
test_model <- lm(ras_ph ~ bio_1_temp, data = response_data)
summary(test_model)



test_model <- lm(ras_ph ~ bio_2_temp, data = response_data)
summary(test_model)

test_model <- lm(ras_ph ~ perm_cond, data = response_data)
summary(test_model)

test_model <- lm(ras_ph ~ ras_temp, data = response_data)
summary(test_model)

test_model <- lm(ras_ph ~ bio_1_temp + bio_2_temp + perm_cond + ras_temp, data = response_data)
summary(test_model)


best_model <- lm(formula = ras_ph ~ mbr_1_perm_flow + ras_temp + bio_1_do + 
                   bio_2_do + mbr_2_level + perm_turb + sewage_flow + bio_1_level + 
                   bio_2_level + bio_1_temp + bio_2_temp + bio_1_tss + bio_2_tss + 
                   ras_do + perm_cond + ras_tss, data = response_data)
summary(best_model)

plot(best_model)

#plotting the chosen best variables

data_best_1 <- data[, c("mbr_1_perm_flow",
                        "ras_temp",
                        "bio_1_do" ,
                        "bio_2_do")]
data_best_2 <- data[, c("mbr_2_level",
                        "perm_turb" ,
                        "sewage_flow" ,
                        "bio_1_level")]
data_best_3 <- data[, c("bio_2_level" ,
                        "bio_1_temp" ,
                        "bio_2_temp" ,
                        "bio_1_tss")]
data_best_4 <- data[, c("bio_2_tss" ,
                    "ras_do" ,
                    "perm_cond" ,
                    "ras_tss")]


par(mfrow = c(2,2))
for(i in 1:4){
  ts.plot(data_best_1[,i], xlab = "index",
          ylab = paste(colnames(data_best_1[i])))
  abline(v = 20338, col = "blue", lwd = 2)
}
par(mfrow = c(2,2))
for(i in 1:4){
  ts.plot(data_best_2[,i], xlab = "index",
          ylab = paste(colnames(data_best_2[i])))
  abline(v = 20338, col = "blue", lwd = 2)
}
par(mfrow = c(2,2))
for(i in 1:4){
  ts.plot(data_best_3[,i], xlab = "index",
          ylab = paste(colnames(data_best_3[i])))
  abline(v = 20338, col = "blue", lwd = 2)
}
par(mfrow = c(2,2))
for(i in 1:4){
  ts.plot(data_best_4[,i], xlab = "index",
          ylab = paste(colnames(data_best_4[i])))
  abline(v = 20338, col = "blue", lwd = 2)
}

plot(best_model)

round(coef(best_model), 3)


suppressMessages(library(glmnet))
filtered_data <- which((colnames(response_data) == "ras_ph"))
y <- data$ras_ph
x <- as.matrix(response_data[, -filtered_data])
lasso_fit <- cv.glmnet(x, y)
round(coef(lasso_fit), 3)

lasso_predictions <- predict(lasso_fit, newx=x)
lasso_residuals <- lasso_predictions - y
SSR <- sum(lasso_residuals ^ 2)
mean_ph_price <- mean(data$ras_ph)
SST <- sum((data$ras_ph - mean_ph_price) ^ 2)
R2 <- 1 - SSR / SST
R2
