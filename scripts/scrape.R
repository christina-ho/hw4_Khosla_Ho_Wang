require(tidyverse)
require(lubridate)
require(rvest)

url <- "https://www.universalhub.com/crime/home.html"
response <- read_html(url)

neighbourhood <- html_nodes(x = response, xpath = '//option[contains(@value, "crime")]')[1:20]
neighbourhood <- html_text(neighbourhood,trim = T)
neighbourhood <- gsub(" ","-",neighbourhood)

Nlinks <- paste0('https://www.universalhub.com/crime/',tolower(neighbourhood),'.html')

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

Ns <- tibble('neighborhood' = neighbourhood, 'link' = Nlinks)

crime_data <- Ns[1,] %>% rowwise() %>% do(crime = type_extractor(.$link))
hou







