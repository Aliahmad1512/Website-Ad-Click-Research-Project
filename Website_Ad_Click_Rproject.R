####################### Website Ad-Click ############################
########################### R-PROJECT ###############################

# In case of a classification problem: the target variable is a categorical in nature.
# We will do Logistic Regression.


# 1) Identifying the Problem Statement:
#Predict who is likely going to click on the Advertisement so it can contribute
#to the more revenue generation to the organisation.

# 2) Import the dataset
## Here, the target variable is "Clicked" which is categorical in nature.

web <- read.csv(choose.files(), stringsAsFactors = TRUE)
web

# Basic EDA

head(web)
View(web)
class(web)
names(web)

dim(web)
# dim() function returns Total dimension i.e. 
# both the number of rows and column in a dataframe.
nrow(bank)
ncol(bank)
# We can also use ncol() function to find the number of columns
# and nrow() to find the number of rows separately.

summary(web)

library(psych)  ### stats package - "psych" ####

describe(web)
# This function provides more deep dive statistics including
# standard deviation, mean absolute deviation, skew, etc

# Here "stringr" removes the underscore "_" from the column names
install.packages("stringr")
library(stringr)

colnames(web) <- str_replace_all(colnames(web),"[_]","")

# Now again looking at the column names
colnames(web)

# Identifying the type of variables:-

# Continous- "VistID","TimeSpent","Age","AvgIncome","InternetUsage","CountryName"
# Categorical- "Citycode","AdTopic","Male","TimePeriod","Weekday","Month","Year","Clicked"      

# Structure check of the variables
str(web)

# Converting the categorical variables into factors
web$Clicked <- as.factor(web$Clicked)

length(unique(web$VistID))
# 6657 too many unique values and can't help in prediction.

# Remove useless columns
web <- web[,-c(1,6,7)]

str(web)

# 3) Data pre-processing:
# find the missing value by using visualization
install.packages("Amelia")
library(Amelia)

missmap(web, main="Website-Ad - Finding Missing Data",
        col=c("red", "black"), legend = F)

colSums(is.na(web))
colSums(web=="") 
# This dataset do not have any missing values

# 4) Data visualizations : Univariate analysis & Bivariate analysis

################## Univariate Analysis ##################

# Multiple Continuous Variables

ColsForHist <- c("TimeSpent","Age","AvgIncome","InternetUsage")
par(mfrow=c(2,2))

library(RColorBrewer)
# library for colors

# Using loops to create the histograms for each column
for (ColumnName in ColsForHist){
  hist(web[,c(ColumnName)], main=paste('Histogram of:', ColumnName), 
       col=brewer.pal(8,"Spectral"))
}

# Multiple Categorical Variables
ColsForBar <- c("Male","TimePeriod","Clicked","Weekday","Month","Year","Citycode")
par(mfrow=c(2,2)) 

for (ColumnName in ColsForBar){
  barplot(table(web[,c(ColumnName)]), main=paste('Barplot of:', ColumnName), 
          col=brewer.pal(8,"Paired"))
}

################ Bivariate Analysis ###################

# Relationship between target variable and predictors variables
# Categorical vs Continuous --- Box Plot
# Categorical vs Categorical -- Grouped Bar chart


############################################################
# Categorical vs Continuous analysis-- Boxplot

# Here "TimeSpent","Age","AvgIncome","InternetUsage" - continous variables

# Categorical vs Continuous analysis-- Boxplot
par(mfrow=c(2,2))

boxplot(TimeSpent~Clicked, data = web, col=brewer.pal(8,"Accent"))

boxplot(Age~Clicked, data = web, col=brewer.pal(8,"Accent"))

boxplot(AvgIncome~Clicked, data = web, col=brewer.pal(8,"Accent"))

boxplot(InternetUsage~Clicked, data = web, col=brewer.pal(8,"Accent"))

# Categorical vs Categorical analysis-- Grouped Bar chart
install.packages("ggplot2")
library(ggplot2) #### for creating graphics####

#Male vs Clicked
ggplot(web, aes(fill=Clicked, y=Clicked, x=Male)) + 
  geom_bar(position="stack", stat="identity")

#TimePeriod vs Clicked
ggplot(web, aes(fill=Clicked, y=Clicked, x=TimePeriod)) + 
  geom_bar(position="stack", stat="identity")

#Citycode vs Clicked
ggplot(web, aes(fill=Clicked, y=Clicked, x=Citycode)) + 
  geom_bar(position="stack", stat="identity")

#Weekday vs Clicked
ggplot(web, aes(fill=Clicked, y=Clicked, x=Weekday)) + 
  geom_bar(position="stack", stat="identity")


# Relationship between target variable and predictors variable
# Categorical vs Continuous --- ANOVA
# Categorical vs Categorical -- Chi-square test

################ ANOVA TEST ##############################
# Continuous vs Categorical relationship strength: ANOVA
# Analysis of Variance(ANOVA)

# H0 Null hypothesis : Variables are not correlated
# Small P-Value < 5% - Variables are correlated 
#### (Null hypothesis H0 is rejected) #########
# Large P-Value > 5% - Variables are not correlated 
#### (Null hypothesis H0 is accepted) ##########

anova <- aov(TimeSpent~Clicked, data = web)
anova
summary(anova)

anova <- aov(Age~Clicked, data = web)
anova
summary(anova)

anova <- aov(AvgIncome~Clicked, data = web)
anova
summary(anova)

anova <- aov(InternetUsage~Clicked, data = web)
anova
summary(anova)

## Good predictor variables are - "TimeSpent","Age","AvgIncome","InternetUsage"

################ CHI-SQUARE TEST ##############################
# Categorical vs Categorical
# H0 Null hypothesis : Variables are not correlated

# Small P-Value - Variables are correlated 
#### (Null hypothesis H0 is rejected) #########
# Large P-Value - Variables are not correlated 
#### (Null hypothesis H0 is accepted) ##########

# We do cross tabulation as the input and gives you the result

Chisqcols <- c("Male","TimePeriod","Citycode","Weekday","Month","Year")

for(chi_cols in Chisqcols){
  CrossTabResult=table(web[,c('Clicked',chi_cols)])
  ChiResult=chisq.test(CrossTabResult)
  print(chi_cols)
  print(ChiResult)
}

#We reject the columns -- "Weekday", "Month" as they have large p-value.
#Since we get a p-Value less than the significance level of 0.05,we reject 
#the null hypothesis and conclude that the rest variables are correlated.

#5) Feature selection
##potential predictors are- "TimeSpent","Age","AvgIncome","InternetUsage","Male","TimePeriod","Citycode","Year"

#converting target variable to numeric
web$Clicked <- as.numeric(web$Clicked)
web$Clicked <- ifelse(web$Clicked == 2,1,0)

#6) Splitting the data into train & test

library(caTools)

set.seed(123)
split <- sample.split(web$Clicked, SplitRatio = 0.75)
split

table(split)
# will show how many are true and false value

training <- subset(web, split==TRUE)
nrow(training)
test <- subset(web, split==FALSE)
nrow(test)

###################################################################
# 8) Building logistic regression model glm 

classifier <- glm(Clicked~., data = training, family = 'binomial')
classifier
summary(classifier)

# there are some variable which is not statically significant, 
# hence, we have to remove this.

classifier1 <- glm(Clicked~.-Male-Weekday-Month-Year, data = training, family = 'binomial')
classifier1
summary(classifier1)

# Significant variables are-- "TimeSpent","Age","AvgIncome","InternetUsage","TimePeriod","Citycode"

#Null deviance: 6882.3 -- system generated error without taking independent variable 
#Residual deviance: 2095.9 -- error with independent variable
#AIC: 2131.9 -- adjested r-square in logistic regression, akike information criterian

#Lower the AIC value, better the model

#9) Predictions : predict model by using test dataset.

pred <- predict(classifier1, newdata = test, type = "response")
pred

pred_thre_50 <- ifelse(pred>0.5,1,0)
pred_thre_50

cm <- table(test$Clicked, pred_thre_50)
cm
#########################################################
# Sensitivity / Recall = TP / (TP+FN)
# Accuracy = (TP+TN)/(TP+FP+FN+TN)
# Specificity = TN / (TN+FP)
# Precision/PPV = TP / (TP+FP)
# False Negative value = FN/(FN+TN)
# False Positive value = FP / (FP+TP)
# F1-Measures = (2*Recall*Precision)/(Recall+Precision)
#########################################################

(861+677)/(861+44+83+677)
#accuracy = 92%

library(caret)
library(e1071)
confusionMatrix(cm)

####################################
# Accuracy : 0.9237
# Sensitivity/Recall : 0.9121         
# Specificity : 0.9390          
# Pos Pred Value/Precision : 0.9514         
# Neg Pred Value : 0.8908          
# Balanced Accuracy : 0.9255
####################################

# F1-Measures/F1-score = (2*Recall*Precision)/(Recall+Precision)

F1_measures <- (2*0.9121*0.9514)/(0.9121+0.9514)
# F1-Measures/F1-score : 0.9313356


# Multicollinearity check: 

library(lmtest)
library(faraway)
library(car)

vif(classifier1)


# Now ROC and AUC for the Sigmoid curve
# ROC curve plots the true positive rate against false positive rate

library(ROCR)

rocprediction <- prediction(test$Clicked, pred_thre_50)
rocprediction

rocperformance <- performance(rocprediction,'tpr','fpr')
rocperformance

plot(rocperformance, col='red', print.cutoffs.at=seq(0.1, by=.1))
abline(a=0,b=1)

################################## Business Recommendation ###################################################
#We can see that more people aged between 30 to 40 are spending more time on site daily. 
#We can give more advertisement related to discounts to the people between 50 to 60 in order to generate more revenue for the organisation.
#User tends to click on a Ad later in a day or probably early in the morning.
#It is expected based on the age feature that most people are working so it seems appropriate as they either find time early or late in the day.
#Also Sunday seems to be effective for clicking on a ad from the bar chart. 
#The area income of users ranges between $13,996.50 - $79,484.80. Quite a large distribution of incomes. This tells us that site visitors hail from various social classes.


####################################################################################################################################################################
######################################################################## LOGISTIC REGRESSION MODEL #################################################################
####################################################################################################################################################################
