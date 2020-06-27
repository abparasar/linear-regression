#--------Import the libraries--------------
# for impute function to replace NA
library(Hmisc)
# for scatter plot and correlation
library(GGally)
#for vif( variable inflation factor)
library(car)


# ------- read the csv data -----------------
mashable_data<-read.csv("OnlineNewsPopularity.csv", stringsAsFactors = FALSE)

#First 6 rows of the data
head(mashable_data)

#structure of the data to see sample data and datatype
str(mashable_data)

#summary of data (check for NA Value for Missing value treatment) 
summary(mashable_data)

#-----------MIssing Value treatment-------------
# column n_tokens_content has NA values
summary(mashable_data$n_tokens_content)

# replace NA with mean impute function from hmisc library
mashable_data$n_tokens_content = impute(mashable_data$n_tokens_content, mean) 

summary(mashable_data$n_tokens_content)


#------------Outliers treatment (Do for all variable)-----------
#look for outliers
#box plot sets the outlier statistics coef is 1.5 time IQR from Q3
# value above ((Q3 - Q1) * 1.5) + Q3 is outlier 
outlier_values = boxplot.stats(mashable_data$num_hrefs)$out  

# Hmisc puts a * on missing value treatment, to solve that use as.numeric
boxplot(as.numeric(mashable_data$num_hrefs), main="No of token")

head(outlier_values)

# take a new variable to manipulate
x <- mashable_data$n_tokens_content

# find Q1 = qnt[1] and Q3 = qnt[2]
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)

# find 5 percentile and 95 percentile caps[1] and caps[2]
caps <- quantile(x, probs=c(.05, .95), na.rm = T)

#1.5 times of range
H <- 1.5 * IQR(x, na.rm = T)

#value below 1.5 times range from Q1 replaced by 5 percentile value
x[x < (qnt[1] - H)] <- caps[1]

#value aboe 1.5 times range from Q3 replaced by 95 percentile value
x[x > (qnt[2] + H)] <- caps[2]

# replacing the existing column
mashable_data$n_tokens_content<-x




#---------------- check correlation----------------

#ggpairs(data=mashable_data, columns=3:5, title="mashable data")
# creating a matrix of correlation
correlation_matrix = cor(mashable_data[,-1]) #removal of 1st categorical variable as correlation is applicable for numeric values
correlation_matrix
write.csv( correlation_matrix,'correlation.csv')


#---------------- multicollinearity -----------------


# first we will need to run the linear model
car_model<-lm(shares~ ., data=mashable_data[,-1])

#2 variables with very high multicollinearity. n_unique_tokens  n_non_stop_unique_tokens 
vif(car_model)


# removal of variables that have high multicollinearity
names(mashable_data)
car_model<-lm(shares~., data=mashable_data[,-c(1,5)])
vif(car_model)


#-------------- test and learn data ---------------

smp_size = 0.75 * nrow(mashable_data)
set.seed(123)
train_ind <- sample(seq_len(nrow(mashable_data)), size = smp_size)
train <- mashable_data[train_ind, ]
test <- mashable_data[-train_ind, ]


#--------------model creation----------
linearMod <- lm(shares ~ ., data=train[,-c(1,5)])  # build linear regression model on full data
# Check the model output
summary(linearMod)
names(mashable_data)


#---------Remove variable with low p value one by one VVIP ------------------

linearMod <- lm(shares ~ ., data=train[,-c(1,2,3,4,5,7,10,11,12,14,15,16,19,20,21, 22)])
# Check the model output
summary(linearMod)

# ------------predict target variable/ scoring ---------------------
# predict shares
shares_pred <- predict(linearMod, test)  
head(shares_pred)


# ---------------- Evaluation/ Validation ----------------------------
actuals_preds <- data.frame(cbind(actuals=train$shares, predicteds=shares_pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)

#Evaluation predictions
mape <- mean((abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals),na.rm = TRUE)
mape





































