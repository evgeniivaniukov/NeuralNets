# Simple NeuralNets

#1 Example
lasagnatriers.df <- read.csv("lasagnatriers.csv",header = T)
library(neuralnet)


set.seed(1)
train.rows.l <- sample(rownames(lasagnatriers.df), dim(lasagnatriers.df)[1]*0.6)
train.data.l <- lasagnatriers.df[train.rows.l,]

valid.rows.l<-setdiff(rownames(lasagnatriers.df), train.rows.l)
valid.data.l<-lasagnatriers.df[valid.rows.l,]

set.seed(1)
nn.lasagna<-neuralnet(Have_Tried_No + Have_Tried_Yes ~ 
                        Age_S + Weight_S + Income_S + Pay_Type + Gender + Live_Alone, data=train.data.l, linear.output=F, hidden=3)

plot(nn.lasagna)

library(caret,e1071)
valid.pred.l = compute(nn.lasagna, valid.data.l[,c(4:9)])
valid.class.l=apply(valid.pred.l$net.result, 
                    1,which.max)-1
confusionMatrix(as.factor(valid.class.l),       
                as.factor(lasagnatriers.df[valid.rows.l, ]$Have_Tried_Yes))
                
#2 Example 

toyotacorolla.df <- read.csv("Corollapp.csv",header = T)
head(toyotacorolla.df)

set.seed(1)
train.rows.toyota <- sample(rownames(toyotacorolla.df), dim(toyotacorolla.df)[1]*0.6)
train.data.toyota <- toyotacorolla.df[train.rows.toyota,]

valid.rows.toyota<-setdiff(rownames(toyotacorolla.df), train.rows.toyota)
valid.data.toyota<-toyotacorolla.df[valid.rows.toyota,]

library(neuralnet)

set.seed(1)

nn.toyota<- neuralnet(Price_S ~ Mfg_Year_S + KM_S + Fuel_D + Automatic + Sport_Model, 
                      data=train.data.toyota, linear.output = T, hidden = c(2,2))
plot(nn.toyota)


predict.toyota<-compute(nn.toyota, valid.data.toyota[,c(8,10,12,16,38)])
MSE.nn.toyota<-sum((valid.data.toyota$Price_S-
                 predict.toyota$net.result)^2)/nrow(valid.data.toyota)
MSE.nn.toyota

max<-max(toyotacorolla.df$Price)
min<-min(toyotacorolla.df$Price)

unscaled<- 0.712*(max-min)+min
unscaled


