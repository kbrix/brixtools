# Data for testing

claim_data_raw <- read.csv("data-raw/bulkcargoclaim.csv")
policy_data_raw <- read.csv("data-raw/bulkcargopolicy.csv")

claim_data <- claim_data_raw
policy_data <- policy_data_raw

names(claim_data) <- c("claim", "deductible", "age", "brt", "dwt", "value", "hp", "year", "code")
names(policy_data) <- c("time", "deductible", "age", "brt", "dwt", "value", "hp", "year", "code", "claim")

claim_data[, c(1, 2, 4, 5, 6, 7)] <- sapply(claim_data[, c(1, 2, 4, 5, 6, 7)], log)
policy_data[, c(2, 4, 5, 6, 7)] <- sapply(policy_data[, c(2, 4, 5, 6, 7)], log)

devtools::use_data(claim_data, overwrite = TRUE)
devtools::use_data(policy_data, overwrite = TRUE) #saves data to correct folder
