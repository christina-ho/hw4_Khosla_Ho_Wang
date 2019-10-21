require(tidyverse)


# B1

set.seed(2048)

sqf <- read_csv(file = '../data_hw4/sqf_08_16.csv')

sqf.data <- sqf %>% 
  filter(year >= 2013 & year <= 2015, suspected.crime == "cpw") %>% 
  mutate(precinct = as.factor(precinct),
         time.period = as.factor(time.period)) %>%
  select(id,year,found.weapon,precinct,location.housing,suspected.crime,starts_with("additional."),
         starts_with("stopped.bc"),suspect.age,suspect.build,suspect.sex,suspect.height,suspect.weight,
         inside,radio.run,day,month,time.period,officer.uniform) %>% 
  filter(complete.cases(.)) %>% sample_n(size = n())

p20 <- nrow(sqf.data)/5

train_sqf <- sqf.data[1:(p20*3),]
validation_sqf <- sqf.data[((p20*3)+1):(p20*4),]
test_sqf <- sqf.data[((p20*4)+1):nrow(sqf.data),]








