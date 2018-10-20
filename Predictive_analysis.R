install.packages('tree')
library(tree)
install.packages('rpart')
library(rpart)
install.packages('rpart.plot')
library(rpart.plot)
install.packages('caret')
library(caret)
install.packages('e1071')
library(e1071)
install.packages('ROCR')
library(ROCR)
install.packages('pROC')
library(pROC)

data<-read.csv('E:\\Programming DS with R and python -MSIS 5223\\Project\\MSIS_Project_data\\final_data_set.csv',sep=',')

#Removing id variable and subsetting test data set

data_test<-subset(data, Loan_Status=='current', select=c(-1))
unique(data_test$Loan_Status)
write.csv(data_test,'E:\\Programming DS with R and python -MSIS 5223\\Project\\MSIS_Project_data\\data_test.csv',sep=",",row.names=F)
data_test<-read.csv('E:\\Programming DS with R and python -MSIS 5223\\Project\\MSIS_Project_data\\data_test.csv',sep=',',header=T,encoding='UTF-8')

data_analysis<-subset(data, Loan_Status!='current', select=c(-1))
write.csv(data_analysis,'E:\\Programming DS with R and python -MSIS 5223\\Project\\MSIS_Project_data\\data_analysis.csv',sep=",",row.names=F)
data_analysis<-read.csv('E:\\Programming DS with R and python -MSIS 5223\\Project\\MSIS_Project_data\\data_analysis.csv',sep=',',header=T,encoding='UTF-8')


inTrain <- createDataPartition(data_analysis$Loan_Status, p=0.7, list=FALSE) 
train_data<-data_analysis[inTrain,]
valid_data<-data_analysis[-inTrain,]

write.csv(train_data,'E:\\Programming DS with R and python -MSIS 5223\\Project\\MSIS_Project_data\\train_data.csv',sep=",",row.names=F)
train_data<-read.csv('E:\\Programming DS with R and python -MSIS 5223\\Project\\MSIS_Project_data\\train_data.csv',sep=',',header=T,encoding='UTF-8')


write.csv(valid_data,'E:\\Programming DS with R and python -MSIS 5223\\Project\\MSIS_Project_data\\valid_data.csv',sep=",",row.names=F)
valid_data<-read.csv('E:\\Programming DS with R and python -MSIS 5223\\Project\\MSIS_Project_data\\valid_data.csv',sep=',',header=T,encoding='UTF-8')

summary(train_data$funded_amnt)
summary(valid_data$funded_amnt)
# train rpart tree on training data 
rpart_tree <- rpart(Loan_Status ~ ., train_data, method = 'class')
plot(rpart_tree)
text(rpart_tree,pretty=0)
library(rattle)
fancyRpartPlot(rpart_tree)
rpart.plot(rpart_tree)
summary(rpart_tree)

# predict labels for Valid data using rpart tree
rpart_pred1 <- predict(rpart_tree, train_data, type = 'class')
rpart_pred1_prob <- predict(rpart_tree, train_data, type = 'prob')
valid_data$last_fico_range_low=valid_data$X0
rpart_pred2 <- predict(rpart_tree, valid_data, type = 'class')
rpart_pred2_prob <- predict(rpart_tree, valid_data, type = 'prob')

# compare labels predicted by rpart to real ones
confusionMatrix(rpart_pred1, train_data$Loan_Status)
confusionMatrix(rpart_pred2, valid_data$Loan_Status)

install.packages('AUC')
library(AUC)
#cutoff=0.5
pred_cut_off <- ifelse(rpart_pred2_prob > 0.5, 1,0) #Setting cut-off to be at 0.5
table(valid_data$Loan_Status,as.data.frame(pred_cut_off)$Fully_Paid )
pred <- prediction(as.data.frame(pred_cut_off)$Fully_Paid,valid_data$Loan_Status)
perf <- performance(pred, "tpr", "fpr")
#Printing AUC Value
perf1 <- performance(pred, "auc")
print(perf1@y.values[[1]])
#Plotting the ROC-curve
install.packages('pROC')
library(pROC)

auc<-auc(valid_data$Loan_Status,rpart_pred2_prob[,2])
plot(roc(valid_data$Loan_Status,rpart_pred2_prob[,2]))
pred <- prediction(as.data.frame(pred_cut_off)$Fully_Paid,valid_data$Loan_Status)
roc = performance(pred, "tpr", "fpr")
plot(roc, col="orange", lwd=2)

#cutoff=0.8
pred_cut_off1 <- ifelse(rpart_pred2_prob > 0.8, 1,0) #Setting cut-off to be at 0.8
table(valid_data$Loan_Status,as.data.frame(pred_cut_off1)$Fully_Paid )
pred1 <- prediction(as.data.frame(pred_cut_off1)$Fully_Paid,valid_data$Loan_Status)
perf2 <- performance(pred1, "tpr", "fpr")
#Printing AUC Value
perf2 <- performance(pred1, "auc")
print(perf2@y.values[[1]])
#Plotting the ROC-curve


auc1<-auc(valid_data$Loan_Status,rpart_pred2_prob[,2])
plot(roc(valid_data$Loan_Status,rpart_pred2_prob[,2]))
pred1 <- prediction(as.data.frame(pred_cut_off1)$Fully_Paid,valid_data$Loan_Status)
roc = performance(pred1, "tpr", "fpr")
plot(roc, col="orange", lwd=2)

rpart_pred_test<-predict(rpart_tree, data_test, type = 'class')
typeof(rpart_pred_test)
data_test$Loan_status_predict<-rpart_pred_test
unique(data_test$home_ownership)
unique(train_data$home_ownership)

#Linear Regression

c = read.csv("E:\\Programming DS with R and python -MSIS 5223\\Project\\MSIS_Project_data\\final_data_set.csv", header=T, sep=",")
colnames(c)

train.size = 0.5
valid.size = 0.25
test.size = 0.25

#### Calculate the sample sizes
samp.train = floor(train.size * nrow(c))
samp.valid = floor(valid.size * nrow(c))
samp.test = floor(test.size * nrow(c))

indices.train = sort(sample(seq_len(nrow(c)), size=samp.train))
indices.valid_test = setdiff(seq_len(nrow(c)), indices.train)
indices.valid = sort(sample(indices.valid_test, size=samp.valid))
indices.test = setdiff(indices.valid_test, indices.valid)

ctr = c[indices.train,]
cv = c[indices.valid,]
ct = c[indices.test,]

summary(ctr$dti)

summary(cv$dti)
summary(ct$dti)
ctr_reg = lm(ctr$funded_amnt~ctr$int_rate+ctr$installment+ctr$annual_inc+ctr$dti_categ+ctr$avg_cur_bal+ctr$last_fico_range_high
             +ctr$home_ownership+ctr$verification_status+ctr$pymnt_plan+ctr$application_type+ctr$verification_status)

summary(ctr_reg)
vif(ctr_reg)
plot(ctr_reg)

cv_reg = lm(cv$funded_amnt~cv$int_rate+cv$installment+cv$annual_inc+cv$dti_categ+cv$avg_cur_bal+cv$last_fico_range_high
            +cv$home_ownership+cv$verification_status+cv$pymnt_plan+cv$application_type+cv$verification_status)

summary(cv_reg)
vif(cv_reg)
plot(cv_reg)


ct_reg = lm(ct$funded_amnt~ct$int_rate+ct$installment+ct$annual_inc+ct$dti_categ+ct$avg_cur_bal+ct$last_fico_range_high
            +ct$home_ownership+ct$verification_status+ct$pymnt_plan+ct$application_type+ct$verification_status)

summary(ct_reg)
vif(ct_reg)
plot(ct_reg)







