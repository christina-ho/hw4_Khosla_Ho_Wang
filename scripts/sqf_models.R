require(tidyverse)
require(gtools)
require(ROCR)

# B1

set.seed(2048)

sqf <- read_csv(file = '../data_hw4/sqf_08_16.csv')

sqf.data <- sqf %>% 
  filter(year >= 2013 & year <= 2015, suspected.crime == "cpw") %>% 
  mutate(precinct = as.factor(precinct),
         time.period = as.factor(time.period)) %>%
  select(id,year,found.weapon,precinct,location.housing,starts_with("additional."),
         starts_with("stopped.bc"),suspect.age,suspect.build,suspect.sex,suspect.height,suspect.weight,
         inside,radio.run,officer.uniform,day,month,time.period,observation.period) %>% 
  filter(complete.cases(.)) %>% sample_n(size = n())

p20 <- nrow(sqf.data)/5

train_sqf <- sqf.data[1:(p20*3),]
validation_sqf <- sqf.data[((p20*3)+1):(p20*4),]
test_sqf <- sqf.data[((p20*4)+1):nrow(sqf.data),]

# B2

model_performance <- tibble(feature_one = rep(NA,528),feature_two = rep(NA,528),validation_auc = rep(NA,528))

names <- colnames(sqf.data)
names <- names[-c(1,2,3,4)]

test <- combinations(33,2,names)

model_performance$feature_one <- test[,1]
model_performance$feature_two <- test[,2]

for(i in 1:nrow(model_performance)){
  traindata <- train_sqf %>% 
    select(found.weapon,as.character(model_performance[i,1]),as.character(model_performance[i,2]),precinct)
  valdata <- validation_sqf %>% 
    select(found.weapon,as.character(model_performance[i,1]),as.character(model_performance[i,2]),precinct)
  trainfit <- glm(found.weapon ~ .,family = binomial,data = traindata)
  predictions <- predict(trainfit,valdata,type = 'response')
  rocr.pred <- prediction(predictions, valdata$found.weapon)
  model_performance[i,3] <- performance(rocr.pred, "auc")@y.values[[1]]
  print(i)
}







