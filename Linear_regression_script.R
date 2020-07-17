library(readr)
##Import data set- Property_Price_Train

PTB<- read_csv("E:/R lectuters/28 mar 2020/project/Property_Price_Train.csv")
Property_Price_Train=PTB

library(psych)
describe(Property_Price_Train)
names(Property_Price_Train)
table(str(Property_Price_Train))

a= describe(Property_Price_Train$Sale_Price)
View(a)
b= describe(PTB$Sale_Price)
View(b)
dim(PTB)
Property_Price_Train['Sale_Price'].describe
library(VIM)
s=colSums(is.na(PTB))
View(s)
nrow(PTB)

# code to remove outliers from sale price
boxplot(Property_Price_Train$Sale_Price)$out
outliers <- boxplot(Property_Price_Train$Sale_Price, plot=T)$out
Property_Price_Train[which(Property_Price_Train$Sale_Price %in% outliers),]
Property_Price_Train <- Property_Price_Train[-which(Property_Price_Train$Sale_Price %in% outliers),]
boxplot(Property_Price_Train$Sale_Price)

# histogram before removing outliers
par(mfrow= c(2,1))
hist(PTB$Sale_Price, xlab= 'Sale_Price' , main= 'Histogram of Sale_Price', col= heat.colors(60) )

# histogram after removing outliers
hist(Property_Price_Train$Sale_Price, xlab= 'Sale_Price',
     main= 'Histogram of Sale_Price After Transformation' , col= topo.colors(100) )


#with and without outliers scatter plot
par(mfrow= c(1,2))
plot(Sale_Price~ Grade_Living_Area, main="With Outliers"  , data= PTB, pch="*", col="red", cex=2)
abline(lm(Sale_Price ~ Grade_Living_Area, data=PTB), col="blue", lwd=3, lty=2)
plot(Sale_Price~ Grade_Living_Area, main="Outliers removed"  , data= Property_Price_Train, pch="*", col="blue", cex=2)
abline(lm(Sale_Price ~ Grade_Living_Area, data=Property_Price_Train), col="red", lwd=3, lty=2)
par(mfrow= c(1,1))

##Correclation matrix

library(corrplot)
library(PerformanceAnalytics)
my_data <- Property_Price_Train[, c('Sale_Price',	'Total_Basement_Area',	'Grade_Living_Area',		'Garage_Size',	'Overall_Material',	'Construction_Year'	)] 
chart.Correlation(my_data, histogram=TRUE, pch=19)

# ggplot
library(ggplot2)
ggplot(Property_Price_Train, aes(y = Sale_Price, x = Grade_Living_Area)) +
  labs(title = 'Sale_Price and Grade_Living_Area',
       x = 'Grade_Living_Area', 
       y = 'Sale_Price') + geom_point(aes(color= Basement_Height)) +
  geom_density_2d()

ggplot(Property_Price_Train, aes(y = Sale_Price, x = Construction_Year)) +
  labs(title = 'Sale_Price and Construction_Year',
       x = 'Construction_Year',
       y = 'Sale_Price') + geom_point(aes(color= House_Condition)) +
  geom_smooth()




# Month wise funy bar plot 
barplot.default(table(Property_Price_Train$Month_Sold ),  col = topo.colors(12), xlab =  "Month_of_Sale", ylab= "Count of Sale"  )



#pairs pnel
library(psych)
aa= Property_Price_Train[, c('Sale_Price', 'Overall_Material',	'Grade_Living_Area',	'Total_Basement_Area',)]
pairs.panels(aa) 




#Create Train and Test

p<-Property_Price_Train
hist(p$Sale_Price)
set.seed(100)
sam<-sample(x=1:nrow(p),size=0.8*nrow(p))
train<-p[sam,]
View(train)
head(train)
dim(train)
names(train)

str(train)
sales<- train$Sale_Price
sales

# 20% of data= Test
test  <- p[-sam, ]   
head(test)
dim(test)

cor(p[c('Overall_Material','Sale_Price','Year_Sold')])

model1<-lm(sales~train$Overall_Material+train$Total_Basement_Area+train$Grade_Living_Area+train$Garage_Size+train$Construction_Year)#0.77
model1
summary(model1)

model2<-lm(sales~train$Grade_Living_Area+train$Total_Basement_Area+train$Overall_Material)#.74
model2
summary(model2)

pred1<-predict(model1)
pred1
summary(pred1)
summary(train$Sale_Price)
summary(error1)
pred2<-predict(model2)
pred2

library(psych)
pairs.panels(test[c("Total_Basement_Area","Grade_Living_Area","Garage_Size","Sale_Price","Construction_Year",'Overall_Material')])

plot(train$Sale_Price,train$Overall_Material)

# Error

error1<-residuals(model1)
error1
summary(error1)

error2<-residuals(model2)
error2
summary(error2)

#ASSUMPTIONS OF MODEL 1

#Assumption 1:Normality of error

hist(error1)
boxplot(error1,col='red',horizontal=T)

#Assumption 2:Linearity

scatterplot(train$Overall_Material,error1,col="red")

#Assumption 3:Independance of error

obs_no<-c(1:1118)
train$obs_no<-NULL
train$obs_no<-obs_no
train

par(mfrow= c(2,1))
scatterplot(train$obs_no,error1)
scatterplot(train$obs_no,error2)
#Assumption 4:Constant of variance

plot(error1,pred1)

# Multicollinearty =VIF

library(car)

vif(model1)

# ASSUMPTIONS OF MODEL 2

#Assumption 1:Normality of error

hist(error2)
boxplot(error2,col='red',horizontal=T)

#Assumption 2:Linearity

plot(train$Overall_Material,error2,col="red")

#Assumption 3:Independance of error

obs_no<-c(1:1118)
train$obs_no<-NULL
train$obs_no<-obs_no
train

plot(train$obs_no,error2)

#Assumption 4:Constant of variance

plot(error2,pred2)

# Multicollinearty =VIF

library(car)

vif(model2)

select (prediction1$V1~ test$Sale_Price )

#PREDICTING TEST SET

prediction1 <- predict(model1,test)
View(prediction1)
summary(prediction1)
head(prediction1)

prediction2<-predict(model2,test)
prediction2
summary(prediction2)
head(prediction2)

#RMSE values for both models


e<-test$Sale_Price-prediction1
e
sqde<-e*e
sqde
sum<-sum(sqde)
sum
Mse<-sum/1118
Mse
rmse=sqrt(Mse)
rmse

e2<-test$Sale_Price-prediction2
sqde2<-e2*e2
sum2<-sum(sqde2)
sum2
Mse2<-sum2/1118
Mse2
rmse2=sqrt(Mse2)
rmse2

