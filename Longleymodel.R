
#callingLongley's Economic Regression from the R data#
install.packages("mlbench",repos = "http://cran.us.r-project.org")

#Data Gathering
#loading "longley" data 


#Data preparation and we'll be  analysing the data


data( "longley"  ,  packages="mlbench")

#calling "mlbench" into the library
library("mlbench")


data("longley")

ss=longley

#assigning object ss to the data longley
str(ss)

summary(ss)
#no missing value



#outlier checking and trearmemt 

#checking outlieer in GNP.deflator
boxplot.stats(ss$GNP.deflator)$out
boxplot(ss$GNP.deflator)
#NO OUTLIER


#checking outlieer in GNP
boxplot.stats(ss$GNP)$out
#NO OUTLIER

boxplot.stats(ss$Unemployed)$out
#NO OUTLIER

View(ss)


#checking outlieer Armed.Forces
boxplot.stats(ss$Armed.Forces)$out
#NO OUTLIER


#checking outlieer in Population
boxplot.stats(ss$Population)$out
#NO OUTLIER

#checking outlieer in Year 
boxplot.stats(ss$Year)$out
#NO OUTLIER

#checking outlieer in Employed  
boxplot.stats(ss$Employed)$out 
#NO OUTLIER


#install.packages("ggplot")
#install.packages("GGally") 
#Visualizing correlation 
ggpairs(data=ss,columns = 1:7)
install.packages("car")


#visulisation of multicollinerity
#running linear regression on the whole data and derive vif
modell=lm(Employed~. , data=ss)
library(car)

vif(modell)


#creation of train and test data

## 75% of the sample size

smp_size <- floor(0.75 * nrow(ss)) 


set.seed(100)
train_ind = sample(seq_len(nrow(ss)), size = smp_size) 
train =ss[train_ind, ]
test = ss[-train_ind, ]

#TREATING MULTICOLLINEARITY AND DERIVING VIF
#running linear regression on train data



linear_m=lm(Employed~.,data=train)



vif(linear_m)
linear_mod=lm(Employed~GNP.deflator+Unemployed+Armed.Forces+Population+Year,data=train)

vif(linear_mod)


linear_mod1=lm(Employed~GNP.deflator+Unemployed+Armed.Forces+Population,data=train)
vif(linear_mod1)


linear_mod2=lm(Employed~Unemployed+Armed.Forces+Population,data=train)
vif(linear_mod2)


linear_mod3=lm(Employed~Unemployed+Population,data=train)

#checking diagnosis
vif(linear_mod3)
summary(linear_mod3)

# predict Employment on test data
Employ_pred <- predict(linear_mod3, test) 
head(Employ_pred)


actuals_preds <- data.frame(cbind(actuals=train$Employed, predicteds=Employ_pred))  
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)
#Evaluation of predictions

mape <- mean((abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals),na.rm = TRUE)
mape





