require(tidyverse)
require(lubridate)
require(rvest)

url <- "https://www.universalhub.com/crime/home.html"
response <- read_html(url)

neighbourhood <- html_nodes(x = response, xpath = '//option[contains(@value, "crime")]')[1:20]
neighbourhood <- html_text(neighbourhood,trim = T)
neighbourhood <- gsub(" ","-",neighbourhood)

Nlinks <- paste0('https://www.universalhub.com/crime/',tolower(neighbourhood),'.html')




