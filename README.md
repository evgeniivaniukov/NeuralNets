# Simple NeuralNets
lasagnatriars.df <- read.csv("lasagnatriers.csv",header = T)
head(lasagnatriars.df)
library(neuralnet)



set.seed(1)

train.rows.lasagna <- sample(rownames(lasagnatriars.df), dim(lasagnatriars.df)[1]*0.6)
train.data.lasagna <- lasagnatriars.df[train.rows.lasagna,]

set.seed(1)

nn.lasagna <- neuralnet(Have_Tried_No + Have_Tried_Yes~ 
                          Age_S + Weight_S + Income_S + Pay_Type + Gender + Live_Alone, 
                        data = train.data.lasagna, linear.output = F, hidden = 3)
  
  
plot(nn.lasagna)
lasagnaNC.df <- read.csv("lasagnaNC.csv",header = T)
names(lasagnaNC.df)
head(lasagnaNC.df)
compute(nn.lasagna,lasagnaNC.df)
