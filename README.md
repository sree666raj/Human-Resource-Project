# Human-Resourse-Project
Projects worked on in PGDDA

To understand what factors they need to focus on, in order to curb attrition.
Need to coin most crucial factor responsible for attrition and possible solution for same.
Make necessary changes in order to reduce attrition percentage, which is 15% per year for now.

### HR Analytics Case Study ####

library(dplyr)
library(ggplot2)
library(Amelia)
library(mlbench)
library(caret)
library(caTools)
library(mice)
library(Information)
library(InformationValue)
library(car)
library(ROCR)


# Loading data 

# General Data
df_gen <- read.csv("general_data.csv", stringsAsFactors = FALSE)

# Empployee Survey Data
df_emp.survey <- read.csv("employee_survey_data.csv", stringsAsFactors = FALSE)

# Manager Survey Data
df_mgr.survey <- read.csv("manager_survey_data.csv", stringsAsFactors = FALSE)

# In Time data
df_in.time <- read.csv("in_time.csv", stringsAsFactors = FALSE)

# Out Time data
df_out.time <- read.csv("out_time.csv", stringsAsFactors = FALSE)

# Calculate daily working hours

time_func <- function(x)as.POSIXct(strptime(x, "%Y-%m-%d %H:%M:%S"))
df_in.time <- cbind(df_in.time[,1], data.frame(lapply(df_in.time[,-1], time_func), stringsAsFactors = FALSE))

colnames(df_in.time)[1] <- "EmployeeID"

df_out.time <- cbind(df_out.time[,1], data.frame(lapply(df_out.time[,-1], time_func), stringsAsFactors = FALSE))
colnames(df_out.time)[1] <- "EmployeeID"

setdiff(df_in.time$EmployeeID, df_out.time$EmployeeID) # Identical EmployeeID in both data sets

df_w.hrs <- cbind(df_in.time[,1], (df_out.time[,-1] - df_in.time[,-1]))
colnames(df_w.hrs)[1] <- "EmployeeID"

# Check number of missing values in each column of working hours

# Count NA function
na.fun <- function(x)sum(is.na(x))
df_na.count <- data.frame(lapply(df_w.hrs, na.fun))

# Columns which have all NA values

col.na <- which((apply(df_na.count, 2, function(x)x>4000)) == TRUE)

# Remove columns with all NA values

df_w.hrs <- df_w.hrs[,-col.na]

df_w.hrs <- cbind(df_w.hrs[,1], data.frame(lapply(df_w.hrs[,-1], function(x)round(x,0))))

# Let's visualize missing values in working hours using missmap() function
# from Amelia package

missmap(df_w.hrs, col = c("blue", "red"), legend = TRUE)

# 5% values are missing in the data, spread acrros the observations
# Lets's impute the missing values

df_w.hrs <- data.frame(lapply(df_w.hrs, as.numeric))
set.seed(123)

imputed <- complete(mice(df_w.hrs[,-1], m=1, maxit = 3,method = 'mean'))

df_w.hrs[,-1]<- imputed

df_w.hrs <- cbind(df_w.hrs[,1], data.frame(lapply(df_w.hrs[,-1], function(x)round(x,0))))           
colnames(df_w.hrs)[1] <- "EmployeeID"

# Creating new variable of avg working hrd

df_w.hrs$avg_w.hrs <- round(apply(df_w.hrs[,-1], 1, mean),0)

# Creating new variable of median working hrs

df_w.hrs$med_w.hrs <- round(apply(df_w.hrs[,-1], 1, median),0)
sum(df_w.hrs$avg_w.hrs - df_w.hrs$med_w.hrs)

# As we can see only for 20 records mean is different than median so we will
# consider avg working hrs 

# Add output variable (attrition) in the hrs datat frame

df_w.hrs$Attrition <- df_gen[match(df_w.hrs$EmployeeID, df_gen$EmployeeID), 2]

# Let's visulaize the attrition data basis working hrs

ggplot(df_w.hrs, aes(x=Attrition, y=avg_w.hrs)) + geom_boxplot() +
  labs(title = "Working hrs vs Attrition",x="Attrition", y = "Working Hrs")
       

# Clearly it shows avg working hrs of Attrition group is higher than non-attrition grp

# Let's do clustering to understand which group of working hrs is 
# leading to higher attrition
hrs.dist <- dist(df_w.hrs[,-c(1,251,252,253)])
hrs.clust1 <- hclust(hrs.dist, method = 'complete') 

plot(hrs.clust1)

rect.hclust(hrs.clust1, k=2, border = 'red')

clusterCut <- cutree(hrs.clust1, k=2)

df_w.hrs$cluster <- clusterCut

table(df_w.hrs$Attrition, factor(df_w.hrs$cluster))

# Avg working hrs <= is clustered as cluster 1 while avg working hrs > 8 is
# clustered as cluster 2. cluster 1 has lower chances of attrition, cluster 2
# has higher chances of attrition


# Add working hours data in general data

df_gen$avg_w.hrs <- df_w.hrs[match(df_w.hrs$EmployeeID, df_gen$EmployeeID),251]
df_gen$avg_w.hrs.bucket <- ifelse(df_gen$avg_w.hrs<=8, "6-8", "9-11")

# Let's understand Employee Survey data

summary(df_emp.survey)

# Since the missing rows are very less, let's display the rows with missing values

df_emp.survey[rowSums(is.na(df_emp.survey)) >0,]

# Let's check if any rows with more than 1 missing value

df_emp.survey[rowSums(is.na(df_emp.survey)) >1,] # none of rows have two or more missing values

# Impute missing values in employee survey data

imputed1 <- complete(mice(df_emp.survey[,-1], m=5, maxit = 5,method = 'pmm'))

df_emp.survey[,-1]<- imputed1

summary(df_emp.survey) # Now missing values are imputed

# Let's understand Manager Survey data

summary(df_mgr.survey) # No missing values in this data

# Add Employee Survey data in general data

setdiff(df_gen$EmployeeID, df_emp.survey$EmployeeID) # Identical EmployeeID in both data sets

df_full.data1 <- merge(df_gen, df_emp.survey, by = "EmployeeID")

# Add Manager Survey data in general data

setdiff(df_full.data1$EmployeeID, df_mgr.survey$EmployeeID) # Identical EmployeeID in both data sets

df_full.data2 <- merge(df_full.data1, df_mgr.survey, by = "EmployeeID")

# Let's take a glimpse of megred data

summary(df_full.data2) # 28 missing values in data

# Since this is a very less number (0.7%), let's remove these rows

df_full.data2 <- df_full.data2[complete.cases(df_full.data2$TotalWorkingYears),]
df_full.data2 <- df_full.data2[complete.cases(df_full.data2$NumCompaniesWorked),]

# Let's see if there is any missing value

sum(is.na(df_full.data2)) # no missing values

# Employee count is "1" for all rows. Let's remove this column

summary(df_full.data1$EmployeeCount) 
df_full.data2$EmployeeCount <- NULL

# Standard Hours is "8" for all employees, so let's remove this variable

summary(df_full.data2$StandardHours)
df_full.data2$StandardHours <- NULL

#### Create dummy variables

# When Attirtion is 0 it represents "No Attrition" , when 1 it represents Attrition"

df_full.data2$Attrition <- as.factor(df_full.data2$Attrition)
levels(df_full.data2$Attrition)

levels(df_full.data2$Attrition) <- c(0,1)
df_full.data2$Attrition <- as.numeric(levels(df_full.data2$Attrition))[df_full.data2$Attrition]

# Create dummy for BusinessTravel

dummy_1 <- data.frame(model.matrix( ~BusinessTravel, data = df_full.data2))

#check the dummy_1 data frame.
View(dummy_1)

dummy_1 <- dummy_1[,-1]

# When dummy of BusinessTravel is 0 it represents "Non-Travel"
df_full.data2 <- cbind(df_full.data2[,-4], dummy_1)
View(df_full.data2)

# Create dummy for Department

dummy_2 <- data.frame(model.matrix( ~Department, data = df_full.data2))

#check the dummy_2 data frame.
View(dummy_2)

dummy_2 <- dummy_2[,-1]

# When dummy of Department is 0 it represents "Human Resources"
df_full.data2 <- cbind(df_full.data2[,-4], dummy_2)
View(df_full.data2)

# Create dummy for Education

# Let's rename the categories as per data dictionary
df_full.data2$Education <- ifelse(df_full.data2$Education == 1, "Below College",
                                  df_full.data2$Education)
df_full.data2$Education <- ifelse(df_full.data2$Education == 2, "College",
                                  df_full.data2$Education)
df_full.data2$Education <- ifelse(df_full.data2$Education == 3, "Bachelor",
                                  df_full.data2$Education)
df_full.data2$Education <- ifelse(df_full.data2$Education == 4, "Master",
                                  df_full.data2$Education)
df_full.data2$Education <- ifelse(df_full.data2$Education == 5, "Doctor",
                                  df_full.data2$Education)


# Create dummy

dummy_3 <- data.frame(model.matrix( ~Education, data = df_full.data2))

#check the dummy_3 data frame.
View(dummy_3)

dummy_3 <- dummy_3[,-1]

# When dummy of Education is 0 it represents "Bachelor"
df_full.data2 <- cbind(df_full.data2[,-5], dummy_3)
View(df_full.data2)


# Create dummy for EducationField

dummy_4 <- data.frame(model.matrix( ~EducationField, data = df_full.data2))

#check the dummy_4 data frame.
View(dummy_4)

dummy_4 <- dummy_4[,-1]

# When dummy of EducationField is 0 it represents "Human Resources"
df_full.data2 <- cbind(df_full.data2[,-5], dummy_4)
View(df_full.data2)


# When Gender is 0 it represents "Female" , when 1 it represents "Male"

df_full.data2$Gender <- as.factor(df_full.data2$Gender)
levels(df_full.data2$Gender)

levels(df_full.data2$Gender) <- c(0,1)
df_full.data2$Gender <- as.numeric(levels(df_full.data2$Gender))[df_full.data2$Gender]

# Create dummy for JobRole

dummy_5 <- data.frame(model.matrix( ~JobRole, data = df_full.data2))

#check the dummy_5 data frame.
View(dummy_5)

dummy_5 <- dummy_5[,-1]

# When dummies of JobRole are 0 it represents "Healthcare Representative"
df_full.data2 <- cbind(df_full.data2[,-7], dummy_5)
View(df_full.data2)


# Create dummy for MaritalStatus

dummy_6 <- data.frame(model.matrix( ~MaritalStatus, data = df_full.data2))

#check the dummy_6 data frame.
View(dummy_6)

dummy_6 <- dummy_6[,-1]

# When dummies of JobRole are 0 it represents "Healthcare Representative"
df_full.data2 <- cbind(df_full.data2[,-7], dummy_6)
View(df_full.data2)


# When avg_w.hrs.bucket is 0 it represents 6-8 average working hrs , 
# when 1 it represents 9-11 working hrs

df_full.data2$avg_w.hrs.bucket <- as.factor(df_full.data2$avg_w.hrs.bucket)
levels(df_full.data2$avg_w.hrs.bucket)

levels(df_full.data2$avg_w.hrs.bucket) <- c(0,1)
df_full.data2$avg_w.hrs.bucket <- as.numeric(levels(df_full.data2$avg_w.hrs.bucket))[df_full.data2$avg_w.hrs.bucket]

## All values in column OVer18 are"Y". So, let's delete this column
df_full.data2$Over18 <- NULL

## Let's explore the data

# Monthly Income vs Attrition

ggplot(df_full.data2, aes(x = factor(Attrition), y = MonthlyIncome)) + 
  geom_boxplot()+labs(title = "Monthly Income vs Attrition",x="Attrition", y = "Monthly Income")


# There appears to be no remarkable difference in median monthly salary between
# attrition and no attrition. However, the spread of monthly salary 
# no-attrition is large which shows that a very high salary is associated less
# attrition

# YearsAtCompany vs Attrition
ggplot(df_full.data2, aes(x = factor(Attrition), y = YearsAtCompany)) + 
  geom_boxplot()+labs(title = "YearsAtCompany vs Attrition",x="Attrition", y = "YearsAtCompany")

# Attrition is high with people who have worked less number of years at the
# present company

# Scale data for modelling

df_full.data2[,-c(1,3)] <- data.frame(apply(df_full.data2[,-c(1,3)], 2, scale))

## Let's run Logistic Regression Model

mod1 <- glm(Attrition ~ .-EmployeeID, data = df_full.data2, family = "binomial")
summary(mod1)


### Check outliers

cooksd <- cooks.distance(mod1)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")

abline(h = 10*mean(cooksd, na.rm=T), col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>10*mean(cooksd, na.rm=T),names(cooksd),""), col="red",adj = -0.5)

# In the plot all observation above the red line are classified as outliers.
# The redline represents the 10 times of the mean distance

outliers <- which(cooksd > 10*mean(cooksd, na.rm = TRUE))

# Let's create a new data set excluding outlier values and run glm

df_full.data3 <- df_full.data2[-outliers,]

mod2 <- glm(Attrition ~ .-EmployeeID, data = df_full.data3, family = "binomial")
summary(mod2)

# Since the outliers comprise of 48 rows. so by removing these 48 rows a total
# of 48(outliers)+28(missing values) are being removed from original data set.
# This is only 1.9% of the data


## checking Information Values

IV <- create_infotables(data = df_full.data3, y = "Attrition", bins = 10, parallel = FALSE)
IV_value <- data.frame(IV$Summary)
print(IV_value)


# Dividing data into training and test set


set.seed(123)

x1 <- sample.split(df_full.data3$Attrition, SplitRatio = 0.75)
train1 <- subset(df_full.data3, x1 == TRUE)
test1 <- subset(df_full.data3, x1 == FALSE)

# Run model on training set

mod1 <- glm(Attrition ~ ., data = train1[,-1], family = "binomial")
summary(mod3) # AIC of model is 2032

vif(mod1)

# EducationFieldLifeSciences has highest VIF and is insignificant

pred1 <- predict(mod1, newdata = test1[,-1], type = "response")

# plot ROCR curve
ROCRpred1 <- prediction(pred1, test1$Attrition)

ROCRperf1 <- performance(ROCRpred1, "tpr", "fpr")

plot(ROCRperf1, colorize = TRUE, print.cutoffs.ar = seq(0,1, by= 0.05), text.adj = c(-0.2, 1.7))

as.numeric(performance(ROCRpred1, "auc")@y.values)

# Optimum cutoff value

optcutoff1 <- optimalCutoff(test1$Attrition, pred1)[1]

sensitivity(test1$Attrition, pred1, threshold = optcutoff1)
# Sensitivity is 0.4390244

# Let's choose a cutoff value of optcutoff   (0.4486) for this model

table(test1$Attrition, pred1 > optcutoff1)

# Drop EducationFieldLifeSciences and run model2

train2 <- train1[, -31]
test2 <- test1[, -31]

mod2 <- glm(Attrition ~ ., data = train2[,-1], family = "binomial")
summary(mod2) # AIC of mod4 is 2033 which is almost same as mod1

vif(mod2)


pred2 <- predict(mod2, newdata = test2[,-1], type = "response")

plotROC(test2$Attrition, pred2) # the model has AUC 84.76% which is pretty good

Concordance(test2$Attrition, pred2)$Concordance 
# concordance is 85%. Concordance measures how often model calculated 
# probability scores of all actual 1's is greater than the model calculated 
# probability scores of 0's

optcutoff2 <- optimalCutoff(test2$Attrition, pred2)[1]

sensitivity(test2$Attrition, pred2, threshold = optcutoff2)

# Senisitivity is 0.445122

confusionMatrix(test2$Attrition, pred2, threshold = optcutoff2)

IV2 <- create_infotables(data = train2, y = "Attrition", bins = 10, parallel = FALSE)
IV_value2 <- data.frame(IV2$Summary)

# Drop EducationFieldMedical and run model3

train3 <- train2[, -32]
test3 <- test2[, -32]

mod3 <- glm(Attrition ~ ., data = train3[,-1], family = "binomial")
summary(mod3) # AIC of mod3 is 2032 which is almost same as mod2

vif(mod3)

IV3 <- create_infotables(data = train3, y = "Attrition", bins = 10, parallel = FALSE)
IV_value3 <- data.frame(IV3$Summary)

pred3 <- predict(mod3, newdata = test3[,-1], type = "response")

plotROC(test3$Attrition, pred3) # the model has AUC 84.78% which pretty good

Concordance(test3$Attrition, pred3)$Concordance 

# Concordance is 85%

# Drop EducationBelow.College and run model4

train4 <- train3[, -27]
test4 <- test3[,-27]

mod4 <- glm(Attrition ~ ., data = train4[,-1], family = "binomial")
summary(mod4) # AIC of mod4 is 2030 which is better than mod3

vif(mod4)

IV4 <- create_infotables(data = train4, y = "Attrition", bins = 10, parallel = FALSE)
IV_value4 <- data.frame(IV4$Summary)

pred4 <- predict(mod4, newdata = test4[,-1], type = "response")

plotROC(test4$Attrition, pred4) # the model has AUC 84.93% which pretty good

Concordance(test4$Attrition, pred4)$Concordance 

optcutoff4 <- optimalCutoff(test4$Attrition, pred4)[1]

sensitivity(test4$Attrition, pred4, threshold = optcutoff4)

# Senisitivity is 0.4512195

confusionMatrix(test4$Attrition, pred4, threshold = optcutoff4)

# Drop EducationFieldMarketing  and run model5

train5 <- train4[, -30]
test5 <- test4[,-30]

mod5 <- glm(Attrition ~ ., data = train5[,-1], family = "binomial")
summary(mod5) # AIC of mod5 is 2026

vif(mod5)

IV5 <- create_infotables(data = train5, y = "Attrition", bins = 10, parallel = FALSE)
IV_value5 <- data.frame(IV5$Summary)

pred5 <- predict(mod5, newdata = test5[,-1], type = "response")

plotROC(test5$Attrition, pred5) # the model has AUC 84.71% which pretty good

Concordance(test5$Attrition, pred5)$Concordance 
# concordance is 0.8501

optcutoff5 <- optimalCutoff(test5$Attrition, pred5)[1]

sensitivity(test5$Attrition, pred5, threshold = optcutoff5)

# Senisitivity is 0.4573171

confusionMatrix(test5$Attrition, pred5, threshold = optcutoff5)

# Drop variables JobRoleSales.Executive, DepartmentResearch...Development ,
# EducationMaster, StockOptionLevel, JobRoleSales.Representative,
# JobRoleHuman.Resources which have low IV and run model6

train6 <- train5[, -c(38,32, 25, 39, 29, 10)]
test6 <- test5[, -c(38,32, 25, 39, 29, 10)]

mod6 <- glm(Attrition ~ ., data = train6[,-1], family = "binomial")
summary(mod6) # AIC of mod6 is 2040

vif(mod6)

IV6 <- create_infotables(data = train6, y = "Attrition", bins = 10, parallel = FALSE)
IV_value6 <- data.frame(IV6$Summary)

pred6 <- predict(mod6, newdata = test6[,-1], type = "response")

plotROC(test6$Attrition, pred6) # the model has AUC 84.41% which pretty good

Concordance(test6$Attrition, pred6)$Concordance 
# concordance is 0.8466

optcutoff6 <- optimalCutoff(test6$Attrition, pred6)[1]

sensitivity(test6$Attrition, pred6, threshold = optcutoff6)

# Senisitivity is 0.4146341

confusionMatrix(test6$Attrition, pred6, threshold = optcutoff6)

# Drop variables BusinessTravelTravel_Rarely and run model7

train7 <- train6[, -23]
test7 <- test6[, -23]

mod7 <- glm(Attrition ~ ., data = train7[,-1], family = "binomial")
summary(mod7) # AIC of mod7 is 2043

vif(mod7)

IV7 <- create_infotables(data = train7, y = "Attrition", bins = 10, parallel = FALSE)
IV_value7 <- data.frame(IV7$Summary)

pred7 <- predict(mod7, newdata = test7[,-1], type = "response")

plotROC(test7$Attrition, pred7) # the model has AUC 84.41% which pretty good

Concordance(test7$Attrition, pred7)$Concordance 
# concordance is 0.8446

optcutoff7 <- optimalCutoff(test7$Attrition, pred7)[1]

sensitivity(test7$Attrition, pred7, threshold = optcutoff7)

# Senisitivity is 0.4268293

confusionMatrix(test7$Attrition, pred7, threshold = optcutoff7)

# Drop variables  YearsAtCompany and run model8

train8 <- train7[, -12]
test8 <- test7[, -12]

mod8 <- glm(Attrition ~ ., data = train8[,-1], family = "binomial")
summary(mod8) # AIC of mod8 is 2042

vif(mod8)

IV8 <- create_infotables(data = train8, y = "Attrition", bins = 10, parallel = FALSE)
IV_value8 <- data.frame(IV8$Summary)

pred8 <- predict(mod8, newdata = test8[,-1], type = "response")

plotROC(test8$Attrition, pred8) # the model has AUC 84.21% which pretty good

Concordance(test8$Attrition, pred8)$Concordance 
# concordance is 0.8448

optcutoff8 <- optimalCutoff(test8$Attrition, pred8)[1]

sensitivity(test8$Attrition, pred8, threshold = optcutoff8)

# Senisitivity is 0.4268293

confusionMatrix(test8$Attrition, pred8, threshold = optcutoff8)

# Drop variables  JobRoleManager, PerformanceRating, JobRoleLaboratory.Technician,
# EducationDoctor, DepartmentSales, JobInvolvement,
# EducationFieldOther and run model 9


train9 <- train8[, -c(28,20,27,24,22,19,25)]
test9 <- test8[, -c(28,20,27,24,22,19,25)]

mod9 <- glm(Attrition ~ ., data = train9[,-1], family = "binomial")
summary(mod9) # AIC of mod9 is 2057

vif(mod9)

IV9 <- create_infotables(data = train9, y = "Attrition", bins = 10, parallel = FALSE)
IV_value9 <- data.frame(IV9$Summary)

pred9 <- predict(mod9, newdata = test9[,-1], type = "response")

plotROC(test9$Attrition, pred9) # the model has AUC 83.91% which pretty good

Concordance(test9$Attrition, pred9)$Concordance 
# concordance is 0.8427

optcutoff9 <- optimalCutoff(test9$Attrition, pred9)[1]

sensitivity(test9$Attrition, pred9, threshold = optcutoff9)

# Senisitivity is 0.5304

confusionMatrix(test9$Attrition, pred9, threshold = optcutoff9)

# Drop variables  JobRoleResearch.Director,  JobRoleResearch.Scientist,
# Gender and run model 10


train10 <- train9[, -c(5,23,24)]
test10 <- test9[, -c(5,23,24)]

mod10 <- glm(Attrition ~ ., data = train10[,-1], family = "binomial")
summary(mod10) # AIC of mod10 is 2060

vif(mod10)

IV10 <- create_infotables(data = train10, y = "Attrition", bins = 10, parallel = FALSE)
IV_value10 <- data.frame(IV10$Summary)

pred10 <- predict(mod10, newdata = test10[,-1], type = "response")

plotROC(test10$Attrition, pred10) # the model has AUC 83.58% which pretty good

Concordance(test10$Attrition, pred10)$Concordance 
# concordance is 0.8417

optcutoff10 <- optimalCutoff(test10$Attrition, pred10)[1]

sensitivity(test10$Attrition, pred10, threshold = optcutoff10)

# Senisitivity is 0.5060

confusionMatrix(test10$Attrition, pred10, threshold = optcutoff10)

# Drop variables  JobLevel,  DistanceFromHome and run model 11


train11 <- train10[, -c(5,4)]
test11 <- test10[, -c(5,4)]

mod11 <- glm(Attrition ~ ., data = train11[,-1], family = "binomial")
summary(mod11) # AIC of mod11 is 2057

vif(mod11)

IV11 <- create_infotables(data = train11, y = "Attrition", bins = 10, parallel = FALSE)
IV_value11 <- data.frame(IV11$Summary)

pred11 <- predict(mod11, newdata = test11[,-1], type = "response")

plotROC(test11$Attrition, pred11) # the model has AUC 83.69% which pretty good

Concordance(test11$Attrition, pred11)$Concordance 
# concordance is 0.8425

optcutoff11 <- optimalCutoff(test11$Attrition, pred11)[1]

sensitivity(test11$Attrition, pred11, threshold = optcutoff11)

# Senisitivity is 0.4817

confusionMatrix(test11$Attrition, pred11, threshold = optcutoff11)

# Drop variables  MaritalStatusMarried,   avg_w.hrs.bucket and run model 12


train12 <- train11[, -c(12,20)]
test12 <- test11[, -c(12,20)]

mod12 <- glm(Attrition ~ ., data = train12[,-1], family = "binomial")
summary(mod12) # AIC of mod12 is 2088

vif(mod12)

IV12 <- create_infotables(data = train12, y = "Attrition", bins = 10, parallel = FALSE)
IV_value12 <- data.frame(IV12$Summary)

pred12 <- predict(mod12, newdata = test12[,-1], type = "response")

plotROC(test12$Attrition, pred12) # the model has AUC 85.1% which pretty good

Concordance(test12$Attrition, pred12)$Concordance 
# concordance is 0.8509

optcutoff12 <- optimalCutoff(test12$Attrition, pred12)[1]

sensitivity(test12$Attrition, pred12, threshold = optcutoff12)

# Senisitivity is 0.4268

confusionMatrix(test12$Attrition, pred12, threshold = optcutoff12)

# Drop variables  PercentSalaryHike and run model 13


train13 <- train12[, -6]
test13 <- test12[, -6]

mod13 <- glm(Attrition ~ ., data = train13[,-1], family = "binomial")
summary(mod13) # AIC of mod13 is 2090

vif(mod13)

IV13 <- create_infotables(data = train13, y = "Attrition", bins = 10, parallel = FALSE)
IV_value13 <- data.frame(IV13$Summary)

pred13 <- predict(mod13, newdata = test13[,-1], type = "response")

plotROC(test13$Attrition, pred13) # the model has AUC 85.21% which pretty good

Concordance(test13$Attrition, pred13)$Concordance 
# concordance is 0.8525

optcutoff13 <- optimalCutoff(test13$Attrition, pred13)[1]

sensitivity(test13$Attrition, pred13, threshold = optcutoff13)

# Senisitivity is 0.4024

confusionMatrix(test13$Attrition, pred13, threshold = optcutoff13)

# Drop variables  MonthlyIncome and run model 14


train14 <- train13[, -4]
test14 <- test13[, -4]

mod14 <- glm(Attrition ~ ., data = train14[,-1], family = "binomial")
summary(mod14) # AIC of mod13 is 2089

vif(mod14)

IV14 <- create_infotables(data = train14, y = "Attrition", bins = 10, parallel = FALSE)
IV_value14 <- data.frame(IV14$Summary)

pred14 <- predict(mod14, newdata = test14[,-1], type = "response")

plotROC(test14$Attrition, pred14) # the model has AUC 85.32% which pretty good

Concordance(test14$Attrition, pred14)$Concordance 
# concordance is 0.8532

optcutoff14 <- optimalCutoff(test14$Attrition, pred14)[1]

sensitivity(test14$Attrition, pred14, threshold = optcutoff14)

# Senisitivity is 0.4573

confusionMatrix(test14$Attrition, pred14, threshold = optcutoff14)

##############################################################################

