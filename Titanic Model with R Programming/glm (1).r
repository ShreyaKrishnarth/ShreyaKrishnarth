#add file into train 

titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE)

#add file into test

titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE)

#checking first 6 value of the data

head(titanic.train)
head(titanic.test)

str(titanic.test)   #displaying structure of the data

#median of the age where na is removed

median(titanic.train$Age, na.rm = T)
median(titanic.test$Age, na.rm = T)

#adding a new column in both dataframes

titanic.train$IsTrainSet <- T
titanic.test$IsTrainSet <- F

#calling names of cloumns
names(titanic.train)
names(titanic.test)

#there is no column such as survived in test so we have to add one

titanic.test$Survived <- NA

#combine two dataset into one

titanic.all <- rbind(titanic.train,titanic.test)

table(titanic.all$IsTrainSet)  #tells categorical data with its frequency
table(titanic.all$Embarked)

#checking missing value with library naniar

library(naniar)

miss_var = miss_var_summary(titanic.all)
View(miss_var)

#cleaning missing values from Embarked

titanic.all[titanic.all$Embarked=='',"Embarked"] <- 'Q'
table(titanic.all$Embarked)

table(is.na(titanic.all$Age))

#Cleaning missing values of Age

median.age <- median(titanic.all$Age, na.rm = TRUE)
titanic.all[is.na(titanic.all$Age),"Age"] <- median.age
table(is.na(titanic.all$Age))

table(is.na(titanic.all$Fare))

#cleaning missing value of Fare

median.Fare <- median(titanic.all$Fare, na.rm = TRUE)
titanic.all[is.na(titanic.all$Fare),"Fare"] <- median.Fare
table(is.na(titanic.all$Fare))

summary(titanic.all)

#catergorical casting of data

str(titanic.all)

titanic.all$Sex <-as.factor(titanic.all$Sex)
titanic.all$Embarked <- as.factor(titanic.all$Embarked)

#removing un necessry columns

titanic.all$Name <- NULL
titanic.all$Ticket <- NULL
titanic.all$Cabin <- NULL

#creating dummies of categorical variable using fastdummies
#it automatically detect all the categorical variable from data

library(fastDummies)

final <- dummy_cols(titanic.all, remove_selected_columns = T, remove_most_frequent_dummy = T)

#split train and test datasets

titanic.train <- final[final$IsTrainSet==T,]
titanic.test <- final[final$IsTrainSet==F,]


titanic.train$IsTrainSet <- NULL
titanic.test$IsTrainSet <- NULL

#for logistic regression model we make survive(dependent) in factor i.e. 0 & 1
#for simple regression we will take survived also a numeric

titanic.train$Survived <- as.factor(titanic.train$Survived)
str(titanic.train)

titanic.model <- glm(formula = Survived~.,
                     family = "binomial",
                     data = titanic.train)

summary(titanic.model)
step(titanic.model)
#from step we get a new model

model2 <- glm(formula = Survived ~ Pclass + Age + SibSp + Sex_female + 
                Embarked_C, family = "binomial", data = titanic.train)

summary(model2)
library(car)
vif(model2)  # must be less than 5

#type = response will give a probabilistic output used for glm models
survived_test <- predict(model2,titanic.test, type = "response")
#for confusion matrix we need a predicted output and a real output so we predict on train as well
survived_train <- predict(model2,titanic.train, type = "response")

#now setting higher than 0.6 probability to 1 and less to 0

titanic.test$Survived <- ifelse(survived_test>0.6,1,0)

titanic.train$predicted <- ifelse(survived_train>0.6,1,0)

#checking accuracy

library(caret)

caret::confusionMatrix(as.factor(titanic.train$predicted),
                       as.factor(titanic.train$Survived))

#titanic model prediction output
PassengerId <- titanic.test$PassengerId
output <- as.data.frame(PassengerId)
output$Survived <- titanic.test$Survived

#for exporting data

write.csv(output,file = "prediction.csv", row.names = F)