data <- read.csv("Mines_Park_pH_Fault.csv", stringsAsFactors = FALSE)
attr(data$dateTime, "tzone") <- "America/Denver"


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
colnames(data)

