###Load the training data
###Library needed in this project
library(Hmisc)
library(VIM)
library(mice)
library(kknn)
library(dplyr)
library(randomForest)
###Import Data
setwd('F:/Miaozhi/Academic/Data_Science/Bootcamp/Project_Machine_Learning')
training = read.csv('training.csv',header = T)
test = read.csv('test.csv',header = T)
attach(training)
attach(test)
head(training)
names(training)
dim(training)   #250000 rows     33 columns
dim(test)   #550000     31
#weight and lable culumns are not in test
summary(training)
summary(test)
# Read data and mark 999.0 as NAs

training[training==-999.0] <- NA
#test[test==-999.0] <- NA
testId = test$EventId

str(training)

weight <- training$Weight
labels <- training$Label

train_with_label <- training[, -c(1,32)]
train <- training[, -c(1,32,33)]
test <- test[,-1]

sum(complete.cases(train))

miscol = colnames(train_with_label)[colSums(is.na(train_with_label)) > 0]


# df0 = filter(train_with_label,PRI_jet_num == 0)
# df1 = filter(train_with_label,PRI_jet_num == 1)
# df2 = filter(train_with_label,PRI_jet_num == 2)
# df3 = filter(train_with_label,PRI_jet_num == 3)
###Deal with missing data

aggr(training) #Graphically show the missing values of traning
               # no missing value
md.pattern(training)  #show the missing value from the data perspective
                      # no missing value
# ###KNN imputation
# sqrt(nrow(training))    #k=500
# training.imputed500NN = kNN(training, k = 500) #Using 500 nearest neighbors.
# training.imputed500NN

###kkNN imputation
complete3 = df3[complete.cases(df3),]
missing3 = df3[!complete.cases(df3),]

df3.euclidean = kknn(DER_mass_MMC ~ ., complete3, missing3, k = 148, distance = 2)
summary(df3.euclidean)

fit = fitted(df3.euclidean)

# sqrt(nrow(df3))   k=148

 try1 = train_with_label
#impute with mean
 for(item in miscol){
   try1[,item]= impute(try1[,item], mean)
 }
write.csv(try1,'impute_mean.csv')
 
 try2 = train_with_label
 #impute with minimum
 for(item in miscol){
   try2[,item]= impute(try2[,item], min)
 }
 
write.csv(try2,'impute_by_min.csv')

 try3 = train
 complete_try3 = try3[complete.cases(try3),]
 missing_try3 = try3[!complete.cases(try3),]
 k = sqrt(nrow(train_with_label))   #k=500
 #impute with kknn
 for(item in miscol){
   it = as.formula(paste0(item,'~.'))
   fit = kknn(it, complete_try3, missing_try3, k = k, distance = 2)
   values = fit$fitted.values
   print(item)
   i=1
   j=1
   for (i in 1:length(try3[,item])){
     if(i%%5000==0){
       print(i)
     }
     if(is.na(try3[i,item])){
        try3[i,item]=values[j]
        print(try3[i,item])
        j=j+1
     }
   }
 }
 write.csv(try3,'impute_by_kknn.csv')
 
 try4 = train_with_label
 #impute with minimum
 for(item in miscol){
   try4[,item]= impute(try4[,item], 'random')
 }
 write.csv(try4, 'impute_by_random.csv')
 
 try5 = train_with_label
 #impute with minimum
 for(item in miscol){
   try5[,item]= impute(try5[,item], median)
 }
 write.csv(try5, 'impute_by_median.csv')
 
set.seed(0)
train_sample = train[sample(nrow(train),nrow(train)/10),]
attemp.train = train[sample(nrow(train),nrow(train)/10),]
attemp.test = test[sample(nrow(test),nrow(test)/10),]
###########################
#####Random Forest#########
###########################

rf.attemp = randomForest(medv ~ ., data = attemp, subset = train, importance = TRUE,na.action=na.omit)
importance(rf.attemp)
 