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

write.csv(crime_count,"data/question_a2_2.csv")


# A3.1

crime_hour <- tibble(hour = names(table(crime_data$hour)),count = table(crime_data$hour))

plot(as.numeric(crime_hour$hour),as.numeric(crime_hour$count),xlab = "hour", ylab = "crimes", main = "Crimes by Hour",type = "l")

# A3.2

crimes_hour <- crime_data %>% select(-neighborhood) %>% filter(crime == "shooting" | crime == "murder" | crime == "gunfire" | crime == "illegal gun possession" | crime == "stabbing") %>% group_by(crime,hour) %>% summarize(counts = n())

plot(crimes_hour$hour[crimes_hour$crime == "gunfire"],crimes_hour$counts[crimes_hour$crime == "gunfire"],xlab = "hour", ylab = "crimes", main = "Crimes by Hour",type = "l",ylim = c(0,30))
points(crimes_hour$hour[crimes_hour$crime == "shooting"],crimes_hour$counts[crimes_hour$crime == "shooting"],type = "l",col = 2)
points(crimes_hour$hour[crimes_hour$crime == "murder"],crimes_hour$counts[crimes_hour$crime == "murder"],type = "l",col = 3)
points(crimes_hour$hour[crimes_hour$crime == "stabbing"],crimes_hour$counts[crimes_hour$crime == "stabbing"],type = "l",col = 4)
points(crimes_hour$hour[crimes_hour$crime == "illegal gun possession"],crimes_hour$counts[crimes_hour$crime == "illegal gun possession"],type = "l",col = 5)
legend("topleft",legend = c("shooting","murder","gunfire","illegal gun possession","stabbing"),lty = 1,col = c(1,2,3,4,5))

# A3.3

crimes2 <- crime_data %>% filter(neighborhood == "Dorchester" | neighborhood == "Downtown") %>% group_by(neighborhood,hour) %>% summarise(counts = n())




