data <- read.csv("Mines_Park_pH_Fault.csv", stringsAsFactors = FALSE)
attr(data$dateTime, "tzone") <- "America/Denver"


data_1 <- data[, 2:9]
data_2 <- data[, 10:19]
data_3 <- data[, 20:29]
data_4 <- data[, 30:39]
data_4 <- data[, 40:43]

pairs(data_1)

