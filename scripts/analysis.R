library(tidyverse)

# A2.2

crime_data <- read_csv("data/question_a2_1.csv")
crime_data <- crime_data[,-1]

table(crime_data$crime)

# shooting, gunfire

crime_data$crime <- tolower(crime_data$crime)
crime_data$crime <- ifelse(crime_data$crime == "shoting" | crime_data$crime == "shootin" | crime_data$crime == "shotting", "shooting",crime_data$crime)

length(unique(crime_data$crime))

crime_count <- tibble(crime = names(table(crime_data$crime)),count = table(crime_data$crime))

crime_count <- head(arrange(crime_count, -count),5)

write.csv(crime_count,"data/question_a2_1.csv")


# A3.1

crime_hour <- tibble(hour = names(table(crime_data$hour)),count = table(crime_data$hour))

plot(as.numeric(crime_hour$hour),as.numeric(crime_hour$count),xlab = "hour", ylab = "crimes", main = "Crimes by Hour")

# A3.2


