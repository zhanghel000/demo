library(tidyverse)
library(ggplot2)
library(ggfortify)
library(car)


source("http://brianlukoff.com/sta235-22fa/finalex.R")
#helen zhang hz6943

# q2  SAT score of 1400
1.1359010 + 0.0004083*(1400) 
# 1.707521


# q7 
#music major
1.1822148 + (-0.0000806 * x)+ -0.1320347 +   (0.0011588*x) 
1.1822148 - 0.1320347 #1.05018
-0.0000806 + 0.0011588 #0.0010782 slope
# 1.05018 + 0.0010782*sat

# journalism major:
1.1822148 + (-0.0000806 * x) -0.0618722 +  (0.0021391*x)
1.1822148 -0.0618722 #1.120343
-0.0000806 + 0.0021391 #0.0020585 slope
# 1.120343 + 0.0020585*sat

# business major
# 1.1822148 - 0.0000806*sat


# Q9
# If a new student were added to the data set with an SAT score of 400 
# (yikes!), a GPA of 4.0, and a major of Journalism, that student 
# would have


# q 10)
9.8+(0.13*10) # 11.1
exp(11.1) # (66171.16)

# q11) 
9.8+(0.13*11) #11.23 wrong
exp(11.23) #75357.6 wrong
# .13 indicates a 13% increase
80000*1.13 #90400 salary for next year


# q12 log odds of man with BMI of 22 and has diabetes
-3.61923  + 0.29915*(1) + (0.06023 *22) #-1.99502

# q13 probability of having diabetes for a woman with BMI of 27?
# if  probability is 13.45%, please enter 0.1345
-3.61923 + 0.06023*27
exp(-1.99302)
1- 0.1362832 #0.8637168


# q14
exp(0.29915) # 1.348712 inc(34.87%)

# q15
-3.61923 + 0.06023*20
exp(-2.41463)
1- 0.08940041 # 0.9105996 probability 

-3.61923 + 0.06023*10
exp(-3.01693)
1- 0.04895127 #0.9510487 prob

#q16
train_index <- 1:500
train_data <- babywt[train_index,]
test_data <- babywt[-train_index,]
fullmodel = lm(LowBwt~gestation + parity + age + height+ weight+smoke, data = train_data)

test_data_actual <- test_data$LowBwt == 1
test_predicted <- predict(fullmodel, test_data, type="response") > 0.5
sum(test_predicted == test_data_actual)/nrow(test_data) # answer 0.8486647

actual.normal <- test_data$LowBwt == 0
predicted.normal <- predict(fullmodel, test_data, type="response") <= 0.5
sum(predicted.normal == actual.normal)/nrow(actual.normal) # answer 0.8486647


# ---- IDK how to use this
test_data$prediction <- ifelse(predict(fullmodel, test_data, type="response") > 0.5, "low", "normal")
test_data$actual <- ifelse(test_data$LowBwt == 0, "normal", "low")

table(test_data$prediction, test_data$actual, dnn=c("predicted", "actual"))
# ------


# q18
aic_model = lm(LowBwt ~ gestation + parity + age + height+ weight+smoke, data = train_data)
step(aic_model,direction = 'backward')
# -1247.03
help(step)



# 20
model0 = lm(sales~time + month, data = souvenirs)
model1 = lm(sales~lag(time) + time + month, data = souvenirs)
predict(model0, data.frame(month = 'Dec', time = 96), interval = 'prediction')
# 82453.61


help(lag)
     #21
summary(model0) #0.46550 march
# april 0.20214
# jan 0.66438

summary(model1)


# 27
0.585+0.05*-.05???0.02*0???0.01*0*-.05 #0.5825
0.585+0.05*.05???0.02*1???0.01*.05 #0.567
0.5825- 0.567 #0.0155
