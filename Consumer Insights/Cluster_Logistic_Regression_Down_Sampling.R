rm(list=ls(all=TRUE))

setwd("~/Desktop/Morris Hitte Customer Insights")

library(data.table)
library(dplyr)
library(QuantPsyc)

result0 <- fread('loyal_cluster0_log_reg.csv')
result1 <- fread('loyal_cluster1_log_reg.csv')
result2 <- fread('loyal_cluster2_log_reg.csv')
result3 <- fread('loyal_cluster3_log_reg.csv')

### Creating Interaction Terms
result0$mail_int1 <- result0$Mail_Order_Buyer_M*result0$Mail_Ord_Clothes
result0$mail_int4 <- result0$Mail_Order_Buyer_M*result0$Mail_Ord_Big_Clothes


result1$mail_int1 <- result1$Mail_Order_Buyer_M*result1$Mail_Ord_Clothes
result1$mail_int4 <- result1$Mail_Order_Buyer_M*result1$Mail_Ord_Big_Clothes


result2$mail_int1 <- result2$Mail_Order_Buyer_M*result2$Mail_Ord_Clothes
result2$mail_int4 <- result2$Mail_Order_Buyer_M*result2$Mail_Ord_Big_Clothes


result3$mail_int1 <- result3$Mail_Order_Buyer_M*result3$Mail_Ord_Clothes
result3$mail_int4 <- result3$Mail_Order_Buyer_M*result3$Mail_Ord_Big_Clothes


### Setting Factors

result0 <- data.frame(lapply(result0, factor))
result1 <- data.frame(lapply(result1, factor))
result2 <- data.frame(lapply(result2, factor))
result3 <- data.frame(lapply(result3, factor))

str(result0)
str(result1)
str(result2)
str(result3)

### Setting base levels

### Number of times client shopped in 30 days: A
### Mail order buyer: Y
### Mail order response: N

drop <- c("client_times_shopped_30_days_A", "Mail_Order_Buyer_Y", "Mail_Order_Response_N")
result0 <- result0[,!(names(result0) %in% drop)]
result1 <- result1[,!(names(result1) %in% drop)]
result2 <- result2[,!(names(result2) %in% drop)]
result3 <- result3[,!(names(result3) %in% drop)]

logitmod0 <- glm(Bought_C ~., family = "binomial", data=result0)
summary(logitmod0)
lm.beta(logitmod0)
exp(logitmod0$coefficients)
logitmod1 <- glm(Bought_C ~., family = "binomial", data=result1)
summary(logitmod1)
lm.beta(logitmod1)
exp(logitmod1$coefficients)
logitmod2 <- glm(Bought_C ~., family = "binomial", data=result2)
summary(logitmod2)
lm.beta(logitmod2)
exp(logitmod2$coefficients)
logitmod3 <- glm(Bought_C ~., family = "binomial", data=result3)
summary(logitmod3)
lm.beta(logitmod3)
exp(logitmod3$coefficients)

######################################### NON-LOYAL #############################################################

result4 <- fread('non_loyal_cluster0_log_reg.csv')
result5 <- fread('non_loyal_cluster1_log_reg.csv')

### Creating Interaction Terms
result4$mail_int1 <- result4$Mail_Order_Buyer_M*result4$Mail_Ord_Clothes
result4$mail_int4 <- result4$Mail_Order_Buyer_M*result4$Mail_Ord_Big_Clothes

result5$mail_int1 <- result5$Mail_Order_Buyer_M*result5$Mail_Ord_Clothes
result5$mail_int4 <- result5$Mail_Order_Buyer_M*result5$Mail_Ord_Big_Clothes

### Setting Factors

result4 <- data.frame(lapply(result4, factor))
result5 <- data.frame(lapply(result5, factor))

str(result4)
str(result5)

### Setting base levels

### Number of times client shopped in 30 days: A
### Mail order buyer: Y
### Mail order response: Y

drop <- c("client_times_shopped_30_days_A", "Mail_Order_Buyer_Y", "Mail_Order_Response_Y")
result4 <- result4[,!(names(result4) %in% drop)]
result5 <- result5[,!(names(result5) %in% drop)]

logitmod4 <- glm(Bought_C ~., family = "binomial", data=result4)
summary(logitmod4)
lm.beta(logitmod4)
exp(logitmod4$coefficients)
logitmod5 <- glm(Bought_C ~., family = "binomial", data=result5)
summary(logitmod5)
lm.beta(logitmod5)
exp(logitmod5$coefficients)

######################################### NOT ENGAGED ##########################################################

result6 <- fread('not_engaged_cluster1_log_reg.csv')
result7 <- fread('not_engaged_cluster3_log_reg.csv')
result8 <- fread('not_engaged_cluster4_log_reg.csv')

### Setting Factors

result6 <- data.frame(lapply(result6, factor))
result7 <- data.frame(lapply(result7, factor))
result8 <- data.frame(lapply(result8, factor))

### Down Sampling
library(caret)
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notations.
set.seed(100)
trainDataIndex <- createDataPartition(result6$Shopped_in_30_days, p=0.7, list = F)
trainData6 <- result6[trainDataIndex, ]
testData6 <- result6[-trainDataIndex, ]

set.seed(777)
down_train6 <- downSample(x = trainData6[, colnames(trainData6) %ni% "Shopped_in_30_days"],
                         y = trainData6$Shopped_in_30_days)

trainDataIndex <- createDataPartition(result7$Shopped_in_30_days, p=0.7, list = F)
trainData7 <- result7[trainDataIndex, ]
testData7 <- result7[-trainDataIndex, ]

set.seed(777)
down_train7 <- downSample(x = trainData7[, colnames(trainData7) %ni% "Shopped_in_30_days"],
                          y = trainData7$Shopped_in_30_days)

trainDataIndex <- createDataPartition(result8$Shopped_in_30_days, p=0.7, list = F)
trainData8 <- result8[trainDataIndex, ]
testData8 <- result8[-trainDataIndex, ]

set.seed(777)
down_train8 <- downSample(x = trainData8[, colnames(trainData8) %ni% "Shopped_in_30_days"],
                          y = trainData8$Shopped_in_30_days)

### Setting base levels

str(down_train6)
str(down_train7)
str(down_train8)

### Mail order buyer: Y
### Mail order response: Y

drop <- c("Mail_Order_Buyer_Y", "Mail_Order_Response_Y")
down_train6 <- down_train6[,!(names(down_train6) %in% drop)]
down_train7 <- down_train7[,!(names(down_train7) %in% drop)]
down_train8 <- down_train8[,!(names(down_train8) %in% drop)]

down_train7 <- na.omit(down_train7)
fctr <- which(sapply(down_train7, is.factor))
lev <- lapply(down_train7[fctr], levels)
nl <- lengths(lev)
print(nl)

logitmod6 <- glm(Class ~., family = "binomial", data=down_train6)
summary(logitmod6)
lm.beta(logitmod6)
exp(logitmod6$coefficients)
logitmod7 <- glm(Class ~., family = "binomial", data=down_train7)
summary(logitmod7)
lm.beta(logitmod7)
exp(logitmod7$coefficients)
logitmod8 <- glm(Class ~., family = "binomial", data=down_train8)
summary(logitmod8)
lm.beta(logitmod8)
exp(logitmod8$coefficients)