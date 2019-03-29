library(ggplot2)


path <- "F:/Imarticus learntron Final Projects/R Project Material/Assignment 1- Plan of Action/bank.csv"
bank <- read.csv(path,header = T)
View(bank)

ncol(bank)
nrow(bank)

str(bank)


#Check Null,blanks, zeros,NA's,?

col_name <- colnames(bank)[apply(bank,2, function(n) any(is.na(n)))]
if(length(col_name)>0)
{
  print("NA is present in columns : ")
  print(col_name)
}else
  print("No NA")

col_name <- colnames(bank) [apply(bank, 2, function(n) any(n == ""))]
if(length(col_name) >0)
{
  print("Blank is present in column : ")
  print(col_name)
}else
  print("No Blanks")

col_name <- colnames(bank) [apply(bank,2,function(n) any(n == 0))]
if(length(col_name) >0)
{
  print("0 present in column : ")
  print(col_name)
}else
  print("No zero's")

col_name <- colnames(bank) [apply(bank,2,function(n) any(n == "?"))]
if(length(col_name) >0)
{
  print("? present in column : ")
  print(col_name)
}else
  print("No ?")

###################### EDA #############################3

#checking levels of columns
levels(bank$job)
levels(bank$marital)
levels(bank$education)
levels(bank$default)
levels(bank$housing)
levels(bank$loan)
levels(bank$contact)

#Reducing no.of levels in columns(i.e. job and education)
#-------------------------

# businessman = entrepreneur and self-employed
#  unemployed = retired

bank$job = as.character(bank$job)
bank$job[bank$job == "entrepreneur" | bank$job == "self-employed"] = "businessman"
bank$job[bank$job == "retired"] = "unemployed"
bank$job = as.factor(bank$job)
levels(bank$job)

#basic.4y , basic.6y & basic.9y = basic

levels(bank$education)
bank$education = as.character(bank$education)
bank$education[bank$education == "basic.4y" | bank$education == "basic.6y" | bank$education == "basic.9y"] = "basic"
bank$education = as.factor(bank$education)


#-------------------------------------
# Check no. of blanks in each columns

table(bank$job)
100*prop.table(table(bank$job))

100*prop.table(table(bank$marital))
100*prop.table(table(bank$education))
100*prop.table(table(bank$default))
100*prop.table(table(bank$housing))
100*prop.table(table(bank$loan))

100*prop.table(table(bank$contact))
sort(100*prop.table(table(bank$month)))
sort(100*prop.table(table(bank$day_of_week)))




# Graphs for data Analysis Before Building Model
#-------------------
plot(bank$previous)
hist(bank$previous)


plot(bank$job)
plot(bank$marital)
plot(bank$education)
plot(bank$default)
plot(bank$housing)
plot(bank$loan)


#--------------------------
#Replece blanks with no in default and loan column
bank$default <- as.character(bank$default)
bank$default <- ifelse(nchar(bank$default)==0 ,"no",bank$default)
bank$default <-as.factor(bank$default)
levels(bank$default)

bank$loan = as.character(bank$loan)
bank$loan <- ifelse(nchar(bank$loan)==0,"no",bank$loan)
bank$loan = as.factor(bank$loan)
table(bank$loan)

#-----------------------
# checking relation between factor columns using condition
mstatus <- bank[bank$marital == "married",]
100*prop.table(table(mstatus$job))
100*prop.table(table(mstatus$education))

status <- bank[bank$marital == "single",]
100*prop.table(table(status$job))
100*prop.table(table(status$education))


job1 <- bank[bank$job == "technician",]
100*prop.table(table(job1$marital))

# Replacing blanks with married(54) and single(26) levels
table(bank$marital)
bank$marital = as.character(bank$marital)
bank$marital <- ifelse(bank$job == "admin." & nchar(bank$marital)==0 ,"single",bank$marital)
bank$marital <- ifelse(bank$job == "technician" & nchar(bank$marital)==0 ,"single",bank$marital)
bank$marital <- ifelse(nchar(bank$marital)==0,"married",bank$marital)
bank$marital = as.factor(bank$marital)
table(bank$marital)

#-----------------------------------
#replaceing blanks according to priority in housing column
mhouse <- bank[bank$marital == "married",]
nrow(mhouse)
100*prop.table(table(mhouse$housing))

bank$housing = as.character(bank$housing)
bank$housing <- ifelse(nchar(bank$housing)==0 & bank$marital == "married", "yes",bank$housing)
bank$housing <- ifelse(nchar(bank$housing)==0, "no" , bank$housing)
bank$housing =as.factor(bank$housing)
table(bank$housing)
str(bank)

#--------------------------------------
#replaceing blanks according to priority in job column
100*prop.table(table(bank$job))
m1 <- bank[bank$education == "basic",]
100*prop.table(table(m1$job))
m2 <- bank[bank$education == "professional.course",]
100*prop.table(table(m2$job))
m3 <- bank[bank$education =="university.degree",]
100*prop.table(table(m3$job))
m4 <- bank[bank$education == "high.school",]
100*prop.table(table(m4$job))

# Replace with admin. (168),blue-collar(105) & technician(57)
bank$job = as.character(bank$job)
bank$job <- ifelse(nchar(bank$job)==0 & bank$education == "basic","blue-collar",bank$job) 
bank$job <- ifelse(nchar(bank$job)==0 & bank$education == "professional.course", "technician",bank$job)
bank$job <- ifelse(nchar(bank$job)==0 & bank$education == "university.degree", "technician",bank$job)
bank$job <- ifelse(nchar(bank$job)==0 , "admin.",bank$job)
bank$job = as.factor(bank$job)
table(bank$job)


#----------------------
# check relation between columns
mjob <- bank[bank$job =="management",]
100*prop.table(table(mjob$education))
bjob <- bank[bank$job =="blue-collar",]
100*prop.table(table(bjob$education))
ajob <- bank[bank$job =="admin.",]
100*prop.table(table(ajob$education))
hjob <- bank[bank$job =="housemaid",]
100*prop.table(table(hjob$education))
tjob <- bank[bank$job =="technician",]
100*prop.table(table(tjob$education))
ujob <- bank[bank$job =="unemployed",]
100*prop.table(table(ujob$education))
sjob <- bank[bank$job =="services",]
100*prop.table(table(sjob$education))
job <- bank[bank$job =="student",]
100*prop.table(table(job$education))

# replace blanks with basic(613), university.degree(589) , high.school(317) and proffessional.course(212)

bank$education = as.character(bank$education)
bank$education <- ifelse(nchar(bank$education)==0 & bank$job == "services","high.school",bank$education)
bank$education <- ifelse(nchar(bank$education)==0 & bank$job =="student","high.school",bank$education)
bank$education <- ifelse(nchar(bank$education)==0 & bank$job == "housemaid","basic",bank$education)
bank$education <- ifelse(nchar(bank$education)==0 & bank$job =="unemployed","basic",bank$education)
bank$education <- ifelse(nchar(bank$education)==0 & bank$job == "blue-collar","basic",bank$education)
bank$education <- ifelse(nchar(bank$education)==0 & bank$job =="technician", "professional.course",bank$education)
bank$education <- ifelse(nchar(bank$education)==0,"university.degree",bank$education)
bank$education = as.factor(bank$education)
table(bank$education)



##################### Model Building #########################

# # split the dataset into Training and Testing
# sample = sample(2, nrow(diab), replace = T, prob = c(0.7,0.3))

ind = sample(seq_len(nrow(bank)), floor(nrow(bank)*0.7))
train = bank[ind,]
test = bank[-ind,]

nrow(train)
nrow(test)
ncol(bank)
str(bank)  

# to check the count of each value of a factor variable against the Y-variable
# ----------------------------------------------------------------------------
100*prop.table(table(train$y))

100*prop.table(table(test$y))

100*prop.table(table(bank$y)) #actual y

colnames(train)
colnames(test)

#Model Building --- > RANDOM FOREST
#---------------------------------------

library(caret) # for confusion matrix  
library(randomForest)

# randomly shuffle the dataset as there is a bias
grp = runif(nrow(bank))
bank = bank[order(grp),]

#Model - 1 -----> Accuracy (91.72%) & Sensitivity (51.68%)




#  call the randomforest() for Classification
# ---------------------------------------------------------------------

rf1 = randomForest(train[,1:20], factor(train[,21])) #Build model 
rf1

summary(rf1)
# ntree=500 is the default. More the better.

# predict the Classification for the Testing data
# ------------------------------------------------
pdct_rf1 = predict(rf1, test)

# Confusion Matrix
#table(predicted = pdct_rf1 , actual = test$y)
confusionMatrix(pdct_rf1 ,test$y, positive = "yes")

# test the accuracy of the prediction
# ----------------------------------
mean(test[,"y"]==pdct_rf1)

#------------------------------------
### Feature selection
# --------------------
# variables used by the randomforest()
varUsed(rf1, by.tree = F, count=F)

# importance of features/attributes.
# higher the value, more important it is
# used for feature selection to optimise other algorithm
# uses the MeanDecreaseGini
# -------------------------------------------------------------
importance(rf1)

# variable importance - for feature selection
# ----------------------------------------------
varImpPlot(rf1)

# how an individual Decision Tree performs in a Random Forest
# to find any particular tree performance
# -----------------------------------------------------------
getTree(rf1, 1, labelVar = T)

#------- model 2 (using 17 imp variables)
bank1 <- bank[,c("age","job","marital","education","housing","month","day_of_week",
                 "duration","campaign","pdays","previous","poutcome","emp.var.rate",
                 "cons.price.idx","cons.conf.idx","euribor3m","nr.employed","y")]
View(bank1)

# split the dataset into Training and Testing
# sample = sample(2, nrow(diab), replace = T, prob = c(0.7,0.3))

ind = sample(seq_len(nrow(bank1)), floor(nrow(bank1)*0.7))
train1 = bank1[ind,]
test1 = bank1[-ind,]

ncol(bank1)
str(bank1)  

rf2 = randomForest(train1[,1:17], factor(train1[,18])) #Build model 
rf2

summary(rf2)

# predict the Classification for the Testing data
# ------------------------------------------------
pdct_rf2 = predict(rf2, test1)

#Confusion Matrix
confusionMatrix(pdct_rf2 ,test1$y, positive = "yes")


#------------------ model 3
bank2 <- bank[,c("age","job","marital","education","month","day_of_week",
                 "duration","campaign","pdays","poutcome","emp.var.rate",
                 "cons.price.idx","cons.conf.idx","euribor3m","nr.employed","y")]

# split the dataset into Training and Testing
# sample = sample(2, nrow(diab), replace = T, prob = c(0.7,0.3))

ind = sample(seq_len(nrow(bank2)), floor(nrow(bank2)*0.7))
train2 = bank2[ind,]
test2 = bank2[-ind,]

ncol(bank2)
str(bank2)  

rf3 = randomForest(train2[,1:15], factor(train2[,16])) #Build model 
rf3

summary(rf3)
# ntree=500 is the default. More the better.

# predict the Classification for the Testing data
# ------------------------------------------------
pdct_rf3 = predict(rf3, test2)

confusionMatrix(pdct_rf3 ,test2$y, positive = "yes")

# Tune Mtry
t1 <- tuneRF(train1[,1:17],train1[,18],stepFactor = 1,
             plot = TRUE,ntreeTry = 200,
             improve = 0.05,trace = TRUE)



#-------------------- Model 4

bank3 <- bank1[,c("age","job","marital","education","housing","month","day_of_week",
                  "duration","campaign","previous","poutcome","emp.var.rate",
                  "cons.price.idx","cons.conf.idx","euribor3m","nr.employed","y")]
ncol(bank1)
ncol(bank3)

# split the dataset into Training and Testing
# sample = sample(2, nrow(diab), replace = T, prob = c(0.7,0.3))

ind = sample(seq_len(nrow(bank3)), floor(nrow(bank3)*0.7))
train4 = bank3[ind,]
test4 = bank3[-ind,]

ncol(train1)
colnames(train1)

rf4 = randomForest(train4[,1:16], factor(train4[,17])) #Build model 
rf4

summary(rf4)
# ntree=500 is the default. More the better.

# predict the Classification for the Testing data
# ------------------------------------------------
pdct_rf4 = predict(rf4, test4)
View(test4)

confusionMatrix(pdct_rf4 ,test4$y, positive = "yes")

######----------------------------
# plot the ROC curve. Only for bivariate classifications
# ------------------------------------------------------
library(ROCR)
oob.votes = predict(rf4, test4, type="prob")
oob.votes
summary(oob.votes)
View(test4)
# positive only
# --------------
oob.pred = oob.votes[,2]
pred.obj = prediction(oob.pred,test4[,17])
summary(pred.obj)

# rp.perf = performance(pred.obj,"rec","prec")
rp.perf = performance(pred.obj, measure="tpr",x.measure="fpr")
plot(rp.perf)


####################### Logistic Regression ###########################

# build the logistic regression model
# GLM - generalised linear model
glm1 = glm(y ~ ., family = "binomial", data=train)
ncol(train)
# summarise model
summary(glm1)

###################################################
# cross validation technique
library(caret)
library(e1071)

train_Control = trainControl(method="cv", number=10)
cvmodel1 = train(y~., data=train, trControl=train_Control,
                 method="glm", family=binomial())
summary(cvmodel1)

cvmodel1$results
cvmodel1$finalModel
pdct_cv = predict(glm1,train,type="response")
pdct_cv = ifelse(pdct_cv <= 0.45, 0, 1)

# build the confusion matrix
table(predicted = pdct_cv, actual = train$y)
#######################################################

# predict the Y-values
predict_y = predict(glm1,test,type="response") #response means prob.
head(predict_y)

predictions = ifelse(predict_y <=0.5, 0,1) 
predictions = as.factor(predictions)
levels(predictions) <- c("no","yes")

# build the confusion matrix
table(predicted = predictions,actual = test$y)
confusionMatrix(predictions,test$y, positive = "yes")

# feature selection technique
# -----------------------------
step(glm1)


# confusion matrix statistics
library(caret)
predictions = ifelse(predict_y <=0.5, 0,1)
predictions = as.factor(predictions)
levels(predictions) <- c("no","yes")
# param1 -> actual Y
# param2 -> predicted Y
# param3 -> positive class (0/1, Yes/No etc..)
confusionMatrix(test$y, as.factor(predictions) ,positive = "yes" )


library(ROCR)
# ROC curve is generated by plotting TPR against the FPR
# 0.5 < GOOD_MODEL <= 1
pr = prediction(predict_y, test$y) #it hepls to perform ROC curve
summary(pr)


#evaluation
evals = performance(pr,"acc") #it returns values in slots
evals
plot(evals)
abline(h=0.83, v=0.46) #this gives horizontal and vertical lines to detect peak point near the upper line
#h and v stands for horizontal and vertical

#identifying the optimal values for best accuracy
#performance values are stored in slots
#display "evals" to understand the output
#y.values = accuracy
#x.values = cutoff

evals
max_yval = which.max(slot(evals, "y.values")[[1]])

max_acc = slot(evals,"y.values")[[1]][max_yval]
max_cutoff = slot(evals,"x.values")[[1]][max_yval]
print(paste("Best accuracy = ",round(max_acc,4),
            "Best cutoff = ",round(max_cutoff,4)))


perf = performance(pr, measure = "tpr", x.measure = "fpr")
plot(perf)

abline(a=0, b=1)

#area under this curve(AUC)
auc = performance(pr,"auc")
round(unlist(slot(auc,"y.values")),3) #3 gives value upto 3 decimal points

#colored graph
plot(perf,colorize=T,main="ROC Curve",ylab = "sensitivity",
     xlab="1-specificity")
abline(a=0, b=1)


#less AIC, good model


###################################################################

# Apply best model from above models

path <- "F:/Imarticus learntron Final Projects/R Project Material/Assignment 1- Plan of Action/bank-additional.csv"
testing <- read.csv(path,header = TRUE)
View(testing)

#Checking blanks in testing dataset

col_name <- colnames(testing) [apply(testing, 2, function(n) any(n == ""))]
if(length(col_name) >0)
{
  print("Blank is present in column : ")
  print(col_name)
}else
  print("No Blanks")

colnames(bank)
colnames(testing)
ncol(testing)

#Reducing no.of levels in columns(i.e. job and education)
#-------------------------

# bussinessman = entrepreneur and self-employed
#  unemployed = retired

testing$job = as.character(testing$job)
testing$job[testing$job == "entrepreneur" | testing$job == "self-employed"] = "businessman"
testing$job[testing$job == "retired"] = "unemployed"
testing$job = as.factor(testing$job)
levels(testing$job)

#basic.4y , basic.6y & basic.9y = basic

levels(testing$education)
testing$education = as.character(testing$education)
testing$education[testing$education == "basic.4y" | testing$education == "basic.6y" | testing$education == "basic.9y"] = "basic"
testing$education = as.factor(testing$education)

#-------------------- 
testing1 <- testing[,c("age","job","marital","education","housing","month","day_of_week",
                       "duration","campaign","previous","poutcome","emp.var.rate",
                       "cons.price.idx","cons.conf.idx","euribor3m","nr.employed")]
View(testing1)
ncol(testing1)

str(testing1)
# predict the Classification for the Testing data
# ------------------------------------------------
pdct_rf = predict(rf5, testing1)
testing$y <- pdct_rf
View(testing)

# Exporting final dataset in predicted_bank_additional.csv
write.csv(testing,"F:/Imarticus learntron Final Projects/R Project Material/predicted_bank_additional.csv")
