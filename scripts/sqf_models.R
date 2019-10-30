require(tidyverse)
require(gtools)
require(ROCR)

dir.create("data/")
dir.create("figures/")

# B1

sqf <- read_csv(file = 'data_hw4/sqf_08_16.csv')

set.seed(2048)

sqf.data <- sqf %>% 
  filter(year >= 2013 & year <= 2015, suspected.crime == "cpw") %>% 
  mutate(precinct = as.factor(precinct),
         time.period = as.factor(time.period)) %>%
  select(id,year,found.weapon,precinct,location.housing,starts_with("additional."),
         starts_with("stopped.bc"),suspect.age,suspect.build,suspect.sex,suspect.height,suspect.weight,
         inside,radio.run,officer.uniform,day,month,time.period,observation.period) %>% 
  na.omit() %>% 
  sample_n(size = n())

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

which.max(model_performance$validation_auc)
# 415
model_performance[415,3]

tvdata <- rbind(train_sqf,validation_sqf)

fitb23 <- glm(found.weapon ~ location.housing + stopped.bc.object + precinct,data = tvdata,family = binomial(link = "logit"))
predictions <- predict(fitb23,test_sqf,type = 'response')
rocr.pred <- prediction(predictions, test_sqf$found.weapon)
performance(rocr.pred, "auc")@y.values[[1]]
# 0.7447619

png("figures/question_b2.png")
hist(model_performance$validation_auc)
abline(v = performance(rocr.pred, "auc")@y.values[[1]],col = 2,)
abline(v = model_performance[415,3],col = 2,lty = 2)
dev.off()

# B3

sqf_pre_2015 <- sqf.data %>% filter(year < 2015) %>% select(-id,-year) %>% sample_n(size = n())
sqf_2015 <- sqf.data %>% filter(year == 2015) %>% select(-id,-year) %>% sample_n(size = n())

sqf_pre_train <- sqf_pre_2015[1:(nrow(sqf_pre_2015)/2),]
sqf_pre_test <- sqf_pre_2015[((nrow(sqf_pre_2015)/2)+1):nrow(sqf_pre_2015),]

fitb33 <- glm(found.weapon ~ . , data = sqf_pre_train,family = binomial(link = "logit"))
predictions3 <- predict(fitb33,sqf_pre_test,type = 'response')
rocr.pred3 <- prediction(predictions3, sqf_pre_test$found.weapon)
performance(rocr.pred3, "auc")@y.values[[1]]
# 0.8229727

# B4

sqf <- read_csv(file = 'data_hw4/sqf_08_16.csv')

sqf.data <- sqf %>% 
  filter(suspected.crime == "cpw") %>% 
  mutate(precinct = as.factor(precinct),
         time.period = as.factor(time.period)) %>%
  select(id,year,found.weapon,precinct,location.housing,starts_with("additional."),
         starts_with("stopped.bc"),suspect.age,suspect.build,suspect.sex,suspect.height,suspect.weight,
         inside,radio.run,officer.uniform,day,month,time.period,observation.period) %>% 
  filter(complete.cases(.)) %>% sample_n(size = n())

sqf.data08 <- sqf.data %>% filter(year == 2008) %>% select(-id,-year)

fitb41 <- glm(found.weapon ~ .,data = sqf.data08,family = binomial(link = "logit"))

sqf.data09_16 <- sqf.data %>% filter(year != 2008) %>% select(-id,-year) %>% filter(precinct != 121)
predictions4 <- predict(fitb41,sqf.data09_16,type = 'response')
rocr.pred4 <- prediction(predictions4, sqf.data09_16$found.weapon)
performance(rocr.pred4, "auc")@y.values[[1]]
# 0.8114827

years <- c(2009:2016)
auc1 <- NA

for(i in 1:length(years)){
  sqf.data09_16 <- sqf.data %>% filter(year == years[i]) %>% select(-id,-year) %>% filter(precinct != 121)
  predictions42 <- predict(fitb41,sqf.data09_16,type = 'response')
  rocr.pred42 <- prediction(predictions42, sqf.data09_16$found.weapon)
  auc1[i] <- performance(rocr.pred42, "auc")@y.values[[1]]
}

png("figures/question_b4.png")
plot(years,auc1)
dev.off()

