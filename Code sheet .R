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

#narrowing the data to just the prefered columns
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

#seeing if there is a correlation
par(mfrow = c(2,2))
for(i in 1:4){
  ts.plot(data_firstquarter[,i], xlab = "index")
  abline(v = 20338, col = "blue", lwd = 2)
}



ts.plot(data$ras_ph, xlab="Index", ylab="pH")
abline(v = 20338, col = "blue", lwd = 2)
abline(h=6.5,col=4,lwd=2)











