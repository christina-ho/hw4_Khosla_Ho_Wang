require(tidyverse)
require(lubridate)
require(rvest)

url <- "https://www.universalhub.com/crime/home.html"
response <- read_html(url)

neighbourhood <- html_nodes(x = response, xpath = '//option[contains(@value, "crime")]')[1:20]
neighbourhood <- html_text(neighbourhood,trim = T)
neighbourhood <- gsub(" ","-",neighbourhood)

Nlinks <- paste0('https://www.universalhub.com/crime/',tolower(neighbourhood),'.html')
Nlinks <- c(Nlinks,'https://www.universalhub.com/crime/dorchester.html?page=1','https://www.universalhub.com/crime/mission-hill','https://www.universalhub.com/newcrime/9967')
Nlinks <- Nlinks[-c(6,14)]



type_extractor <- function(links){
  content <- read_html(links)
  type <- html_nodes(x=content, xpath='//td[contains(@class,"name")]')
  type <- html_text(type,trim = T)
  type
}

type_extractor(Nlinks[1])

time_extractor <- function(links){
  content <- read_html(links)
  time <- html_nodes(x=content, xpath='//td[contains(@class,"date")]')
  time <- html_text(time,trim = T)
  time <- lubridate::parse_date_time(time, c("%m%d%y - %I:%M %p"))
  lubridate::hour(time)
}

time_extractor(Nlinks[1])

lengths <- NA

for(i in 1:length(Nlinks)){
  lengths[i] <- length(time_extractor(Nlinks[i]))
}
 
nbhood <- c(neighbourhood[-c(6,14)],"Dorchester","Mission-Hill","Chinatown")

neighborhoods <- vector()
types1 <- vector()
types <- vector()
time1 <- vector()
time <- vector()

for(i in 1:21){
  neighbourhood1 <- rep(nbhood[i],lengths[i])
  neighborhoods <- c(neighborhoods,neighbourhood1)
  types1 <- type_extractor(Nlinks[i])
  types <- c(types,types1)
  time1 <- time_extractor(Nlinks[i])
  time <- c(time,time1)
}

crime_data <- tibble('crime' = types, 'hour' = time, 'neighborhood' = neighborhoods)

dir.create("data/")
write.csv(crime_data,"data/question_a2_1.csv")

