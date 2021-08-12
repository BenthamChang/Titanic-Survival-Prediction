
# Import Library ----------------------------------------------------------

library(ggplot2) # for ggplot
library(gridExtra) # for making subplots in html
library(magrittr) # for %>%
library(dplyr) # for filter, glimpse, mutate function
library(distr) # for generating distribution
library(DMwR) # for imputation of NA by Knn, only used in fare in testing data
library(randomForest) # for random forest algorithm
library(kableExtra) # for tidy table in html form
library(caret) # for one hot encoding

# Read Data ---------------------------------------------------------------

# set working directory and read training and testing data
setwd("C:/Users/Chang Wen Teng/desktop/Submitted Resume/2020/MSD_Final Round_Offer Get/MSD Interview")

# na.strings = "" is for identifying the type of na in csv, otherwise, "" will be considered as characters
Train.df <- read.csv("train.csv", na.strings = "") 
Test.df <- read.csv("test.csv",na.strings = "")

# Exploratory Data Analysis -----------------------------------------------

# have a look on data
glimpse(Train.df) # 891 * 12
head(Train.df)
tail(Train.df)

# ggplot

# Sex x Survival
g <- ggplot(data = Train.df)
g + geom_bar(aes(x=Sex,fill=factor(Survived)),stat='count',position='dodge')+
  labs(y="Survival",title="Sex x Survival")+
  theme_light()

# Pclass x Survival
g + geom_bar(aes(x=Pclass,fill=factor(Survived)),stat='count',position='dodge')+
  labs(y="Survival",title="Pclass x Survival")+
  theme_light()+
  coord_flip()

# Age x Survival
g+geom_histogram(aes(x=Age,color=factor(Survived)),binwidth = 12,fill="white")+
  labs(title="Age x Survival (binwidth = 12)")
  theme_light()
  
# Fare x Survival
g+geom_histogram(aes(x=Fare,color=factor(Survived)),binwidth = 100,fill="white")+
  labs(title="Fare x Survival (binwidth = 100)")+
  theme_light()

# Fare x Pclass
g+geom_histogram(aes(x=Fare,color=factor(Pclass)),binwidth = 100,fill="white")+
  labs(title="Fare x Survival (binwidth = 100)")+
  theme_light()

# Sibsp / Parch x Survived
g+geom_histogram(aes(x=SibSp,color=factor(Survived)),binwidth = 1,fill="white",show.legend = F)+
  labs(title="SibSp x Survived (binwidth = 1)")+
  theme_light()

g+geom_histogram(aes(x=Parch,color=factor(Survived)),binwidth = 1,fill="white",show.legend = F)+
  labs(title="Parch x Survived (binwidth = 1)")+
  theme_light()

g+geom_histogram(aes(x=SibSp+Parch,color=factor(Survived)),binwidth = 1,fill="white")+
  labs(title="Family (Sibsp+Parch) x Survived (binwidth = 1)")+
  theme_light()+
  theme(legend.position=c(0.95,0.95),legend.justification = c(0.95,0.95))

# Missing Values and Feature Engineering ----------------------------------

# To see the number of NAs in each column
# There are NAs in 3 columns, Age, Cabin, and Embarked
colSums(is.na(Train.df))
colSums(is.na(Test.df))


# Treat missing value: Embarked -----------------------------------------

# To find the rows that include Embarked = NA
embarked_na <- Train.df[is.na(Train.df[,12]),1]
embarked_na

Train.df[embarked_na,]

# Compare the feature patterns of these 2 passengers to the rest 

ggplot(data=Train.df[-embarked_na,])+
  geom_boxplot(aes(y=Fare,x=factor(Pclass),fill=factor(Embarked)))+
  geom_hline(yintercept=80,linetype="dashed",lwd=0.5,color="navy")+
  theme_light()+
  theme(legend.position = c(0.95,0.95),legend.justification = c(0.95,0.95))+
  coord_flip()

ggplot(data=Train.df[-embarked_na,])+
  geom_bar(aes(x=Pclass,fill=Embarked))+
  labs(title="Histogram of Pclass (partitioned by Embarked)")+
  theme_light()+
  theme(legend.position = c(0.05,0.95),legend.justification = c(0.05,0.95))

ggplot(data=Train.df[-embarked_na,])+
  geom_bar(aes(x=Parch+SibSp,fill=Embarked))+
  labs(title="Histogram of Family (partitioned by Embarked)")+
  theme_light()+
  theme(legend.position = c(0.95,0.95),legend.justification = c(0.95,0.95))
  
Train.df[embarked_na,"Embarked"] <- "S"

# Treat missing value: Cabin --------------------------------------------

# Create a new feature: Cabin_Binary which Cabin = NA (0) and Cabin != NA (1)
# I set a generating function and apply on both training and testing set
Cabin_Binary_na <- c()
Binary_Generator <- function(X,Variable){
  for (i in 1:nrow(X)) {
    if(is.na(Variable[i])==T){
      Cabin_Binary_na[i] = 0
    }
    else{Cabin_Binary_na[i] = 1}
  }
  return(factor(Cabin_Binary_na))
}

Train.df <- Train.df[,-11] %>% mutate(Cabin_Binary = Binary_Generator(Train.df,Train.df$Cabin))
Test.df <- Test.df[,-10] %>% mutate(Cabin_Binary = Binary_Generator(Test.df,Test.df$Cabin))
dim(Train.df); dim(Test.df)

ggplot(data = Train.df)+
  geom_bar(aes(x=Cabin_Binary,fill=factor(Survived)),stat='count')+
  labs(title="Binary_Cabin x Survived")+
  coord_flip()

# If Cabin is not "NA", there is roughly 67% to survive
sum(Train.df[Train.df[,"Cabin_Binary"]==1,]$Survived)/sum(as.numeric(Train.df[,"Cabin_Binary"])-1)
# If Cabin is "NA", there is roughly 30% to survive
sum(Train.df[Train.df[,"Cabin_Binary"]==0,]$Survived)/sum(2-as.numeric(Train.df[,"Cabin_Binary"]))

# Add a "Title" column being extracted from Name --------------------------

# determine if column, Name, includes "Mr.","Mrs.","Miss.", and "Master." 
# (grep: return row numbers that contains particular value)
# NA indicates that column, Name, doesn't contain all of these 4 strings
# Assign Mr. = 1, Mrs. = 2, Miss. = 3, Master. = 4 to new column Title 
Names <- c("Mr.","Mrs.","Miss.","Master.")
Title_generator <- function(X){
  Title <- c()
  Index_grep <- list()
  for (i in 1:length(Names)){
    Index_grep[[i]] = grep(Names[i], X$Name,fixed = T)
    Title[Index_grep[[i]]] <- i 
    print(paste(Names[i],":",sum(na.omit(Title)==i)))
    if (i==4) {
      print(paste("Total : ",sum(is.na(Title)==F)))
      print(paste("NA :", sum(is.na(Title)==T)))
    }
  }
  return(Title)
}
# Add column, Title, into each data set
Train.df <- Train.df %>% mutate(Title = Title_generator(Train.df))
Test.df <- Test.df %>% mutate(Title = Title_generator(Test.df))
dim(Train.df);dim(Test.df)

# Populate Title based on Age and Sex
# find the rows with na values in "Title"
# The reason that I don't use loop is becuase I am checking data at the
# same time, so that I can customized "Title".

# Training Data
na_title <- c()
na_title <- is.na(Train.df$Title)
Train.df[na_title,c("Name","Age","Sex")]
na_Mr_title <- subset(Train.df[na_title,],Sex == "male"& Age>12)[,1]
na_Mr_title <- c(na_Mr_title,767)
Train.df[na_Mr_title,"Title"] <- 1
na_title <- is.na(Train.df$Title)
Train.df[na_title,c("Name","Age","Sex")]
na_Mrs_title <- subset(Train.df[na_title,],Sex == "female"& Age>25)[,1]
na_Mrs_title <- c(na_Mrs_title,711)
Train.df[na_Mrs_title,"Title"] <- 2
na_title <- is.na(Train.df$Title)
Train.df[na_title,c("Name","Age","Sex")]
na_Miss_title <- subset(Train.df[na_title,],Sex == "female"& Age<=25)[,1]
Train.df[na_Miss_title,"Title"] <- 3

# Testing Data
na_title <- is.na(Test.df$Title)
Test.df[na_title,c("Name","Age","Sex")]
na_Mr_title <- subset(Test.df[na_title,],Sex == "male"& Age>12)[,1]
Test.df[na_Mr_title-nrow(Train.df),"Title"] <- 1
na_title <- is.na(Test.df$Title)
Test.df[na_title,c("Name","Age","Sex")]
na_Mrs_title <- subset(Test.df[na_title,],Sex == "female"& Age>25)[,1]
Test.df[na_Mrs_title-nrow(Train.df),"Title"] <- 2
na_title <- is.na(Test.df$Title)
Test.df[na_title,c("Name","Age","Sex")]
na_Miss_title <- 89
Test.df[na_Miss_title,"Title"] <- 3

ggplot(data=Train.df)+
  geom_bar(aes(x=Title,fill=factor(Survived)))+
  labs(title = "Title x Survival",subtitle = "1: Mr. | 2: Mrs. | 3: Miss | 4: Master.")+
  theme_light()+
  coord_flip()

# Treat missing value: Age --------------------------------------------

# Generate random number from each distribution 
Random_number <- list()
set.seed(100)
Age_Dist_Generator <- function(X){
  for (i in 1:length(Names)){
    h <- c()
    h <- hist(subset(X[,c("Age","Title")],Title==i)$Age,plot=FALSE)
    probability = h$counts/sum(h$counts)
    support = h$mids
    dist <- DiscreteDistribution(supp = support,prob = probability)
    Random_number[[i]] <- dist@r(10000)
    # check if the random number generates correctly from the distribution
    # hist(Random_number[[i]])
  }
  return(Random_number)
}

# Populate Age based on Age Distribution partitioned by Title 
# find the rows with na values in column, Age
Age_Generator <- function(X,Rand){
  na_age <- is.na(X$Age)
  for (i in 1:length(Names)) {
    na_partition_row <- c()
    na_partition_row <- subset(X[na_age,],Title==i)[,1]
    if(X[1,1]==892){
      X[na_partition_row-891,"Age"] <- Rand[[i]][1:length(na_partition_row)]
      }
    else X[na_partition_row,"Age"] <- Rand[[i]][1:length(na_partition_row)]
      }
  return(X)
}

Train_Rand <- Age_Dist_Generator(Train.df)
Train.df.final <- Age_Generator(Train.df,Train_Rand)


# Training Data
# Have a look on each distribution before and after "Age" imputation
# Should be similar since I fill in the data based on each distribution
  
par(mfrow=c(3,4))
for (i in 1:length(Names)) {
  hist(subset(Train.df[,c("Age","Title")],Title==i)$Age,xlab="Age",main=paste("(With NA) Age x",Names[i]))
}
for (i in 1:length(Names)) {
  hist(Train_Rand[[i]],xlab="Random Variable (Age)",main=paste("(n = 10000) Age x",Names[i]))
}
for (i in 1:length(Names)) {
  hist(subset(Train.df.final[,c("Age","Title")],Title==i)$Age,xlab="Age",main=paste("(Imputation) Age x",Names[i]))
}

# Testing Age -  Data imputation
Test_Rand <- Age_Dist_Generator(Test.df)
Test.df <- Age_Generator(Test.df,Test_Rand)

# Treat missing value: Fare (Testing Set) ---------------------------------

## knn imputation 
Test.df.final <- knnImputation(Test.df)

# Testing data
# Have a look on each distribution before and after "Age" imputation
# Should be similar since I fill in the data based on each distribution

par(mfrow=c(2,4))

for (i in 1:length(Names)) {
  hist(subset(Test.df[,c("Age","Title")],Title==i)$Age,main=paste("Age x",Names[i]))
}
for (i in 1:length(Names)) {
  hist(subset(Test.df.final[,c("Age","Title")],Title==i)$Age,main=paste("Age x",Names[i]))
}

# Make sure the data are cleaned 
colSums(is.na(Train.df.final))
colSums(is.na(Test.df.final))

#€Add new features which are proved to be good in EDA by Tableau Plot
Train.df.final <- Train.df.final%>%mutate(Family = SibSp+Parch)
Test.df.final <- Test.df.final%>%mutate(Family = SibSp+Parch)

# remove some useless columns
Train.df.final2 <- Train.df.final[,-c(1,4,7,8,9)]

# Transform Pclass and Title from numerics to characters and transform them to one-hot encodings
Train.df.final2[,"Pclass"] <- as.character(Train.df.final2[,"Pclass"])
Train.df.final2[,"Title"] <- as.character(Train.df.final2[,"Title"])
dmy <- dummyVars(" ~ Title+Pclass", data = Train.df.final2)
trsf <- data.frame(predict(dmy, newdata = Train.df.final2))
Train.df.final3 <- cbind(Train.df.final2[,-c(2,8)],trsf)
glimpse(Train.df.final3)

# Testing dataset turns
Test.df.final2 <- Test.df.final[,-c(3,6,7,8)]
glimpse(Test.df.final2)
Test.df.final2[,"Pclass"] <- as.character(Test.df.final2[,"Pclass"])
Test.df.final2[,"Title"] <- as.character(Test.df.final2[,"Title"])
dmy <- dummyVars(" ~ Title+Pclass", data = Test.df.final2)
trsf <- data.frame(predict(dmy, newdata = Test.df.final2))
Test.df.final3 <- cbind(Test.df.final2[,-c(2,8)],trsf)
glimpse(Test.df.final3)

# Bin Fare -

##€Fare: find which rows are lower than 50, and which are higher
Fare_Train <- Train.df.final3[,4]
Fare_Test <- Test.df.final3[,4]
Fare_Boolean_train <- c(Train.df.final3[,4]<50)
Fare_Boolean_test <- c(Test.df.final3[,4]<50)

##€if Fare<50 then "0", o.w. "1"
Train.df.final3[Fare_Boolean_train,4] <- "0"
Train.df.final3[!Fare_Boolean_train,4] <- "1"
Test.df.final3[Fare_Boolean_test,4] <- "0"
Test.df.final3[!Fare_Boolean_test,4] <- "1"

# Model Building (Logistic Regression) ------------------------------------

Train_df <- Train.df.final3[,c(1,3,4,6,7,8,9,10,12,13)]
glimpse(Train_df)
Test_df <- Test.df.final3[,c(1,3,4,6,7,8,9,10,12,13)]
glimpse(Test_df)

# leave-one-out cross validation to tune the hyperparameter probability
Leave_one_out_Log <- function(prob){
  Acc =  0
  for (i in 1:dim(Train_df)[1]) {
    glm.fit <- glm(Survived~.,data=Train_df[-i,],family=binomial)
    log.pred <- predict(glm.fit,newdata=Train_df[i,],type="response")
    log.pred.binary <- ifelse(log.pred>prob,1,0)
    if(log.pred.binary==Train_df[i,1]){
      Acc = 1 + Acc
    }
  }
  return(Acc/length(Train_df$Survived))
}

# run over the threshold from 0.1 to 1 with step 0.1 and record the corresponding accuracy
# This will take around 40 - 60 secs 
log_Acc <- sapply(seq(0.1,1,0.1), Leave_one_out_Log)
log_Acc # threshold = 0.6 

# ggplot, It's necessary to transform data into dataframe to perform ggplot
A <- data.frame(x=seq(0.1,1,0.1),y=log_Acc)

ggplot(A,aes(x=x,y=y,label=round(y,3)))+
  geom_point(col="blue3",size=2)+
  geom_point(aes(x=0.5,y=log_Acc[5]),col="orange2",size=2)+
  geom_point(aes(x=0.6,y=log_Acc[6]),col="orange2",size=2)+
  geom_text(nudge_x = 0.1,vjust=0.25,hjust=1.3)+
  labs(x = 'Probability Threshold',
       y = 'Accuracy',
       title = 'Accuracy x Threshold (Logistic Regression)')+
  theme_light()

# Prediction on testing set 
glm.fit <- glm(Survived~.,data=Train_df,family=binomial)
summary(glm.fit)
log.pred <- predict(glm.fit,newdata=Test_df,type="response")
log.pred.binary <- ifelse(log.pred>0.6,1,0)

# write.csv(data.frame(PassengerId=Test_df[,"PassengerId"],Survived=log.pred.binary),"file1.csv",row.names = F)

# xgboost
Train_df$Fare <- as.numeric(Train_df$Fare)
Train_df$Cabin_Binary <- as.numeric(Train_df$Cabin_Binary)
Test_df$Fare <- as.numeric(Test_df$Fare)
Test_df$Cabin_Binary <- as.numeric(Test_df$Cabin_Binary)
str(Train_df)
library(xgboost)
xg_best <- xgboost(data = as.matrix(Train_df[,-1]), label = Train_df$Survived, max.depth = 2, eta = 1, nthread = 4, nrounds = 100, objective = "binary:logistic")
mccr(val.df[,open_flag],ifelse(predict(xg_best,as.matrix(val.df.feature))>=prob,1,0))
pred <- ifelse(predict(xg_best,as.matrix(Test_df[,-1]))>=0.6,1,0)
write.csv(pred,"xgboost.csv")
