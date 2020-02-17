# Simple NeuralNets
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
