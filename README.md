# Neural Networks Project, Evgenii Vaniukov 


•	Pre-processed the dataset in RStudio; partitioned the dataset to train the Neural Network model
•	Performed the three neural network fittings (models differed in hidden structures); plotted models
•	Constructed and compared confusion matrices; recommended which model to choose


```{r}
Taykopp.df<-read.csv("Taykopp.csv", header = T)
library(neuralnet)
library(caret)
library(e1071)


```

#Taks 1

```{r}
normalizing <- function(x) {
            min<-min(x)
            max<-max(x)
            return((x - min)/(max-min))}

```

```{r}
#Scaling numeric variables
Taykopp.df<- mutate(Tayko.df, Freq_scaled = normalizing(Tayko.df$Freq),
last_update_days_ago_scaled = normalizing(Tayko.df$last_update_days_ago),
First_update_days_ago_scaled = normalizing(Tayko.df$X1st_update_days_ago))

```

```{r}
#Creating new columns for Purchase_Yes and Purchase_No
Purchase_Yes<- ifelse(Taykopp.df$Purchase == 1, "1", "0")
Purchase_No <- ifelse(Taykopp.df$Purchase== 0, "1","0" )

Taykopp.df<- Taykopp.df %>% mutate(Purchase_No, Purchase_Yes)
write.csv(Taykopp.df, "Taykopp.csv")
```


#Task 2

```{r}
#Partitioning Dataset into 60% training and 40% validation datasets
set.seed(1)

train.rows.Tayko <- sample(rownames(Taykopp.df), dim(Taykopp.df)[1]*0.6)
train.data.Tayko <- Taykopp.df[train.rows.Tayko,]

valid.rows.Tayko<-setdiff(rownames(Taykopp.df), train.rows.Tayko)
valid.data.Tayko<-Taykopp.df[valid.rows.Tayko,]

```


#Task 3 & 4 

```{r}
#Neural Network 1 (one hidden layer with two nodes)

set.seed(1)
nn1.Tyko <- neuralnet(Purchase_No+Purchase_Yes~US+Web_order+Address_is_res+Freq_scaled+last_update_days_ago_scaled+First_update_days_ago_scaled, data=train.data.Tayko, linear.output = F, threshold = 0.05, hidden = 2)
plot(nn1.Tyko, rep = "best")

valid.pred.Tayko = compute(nn1.Tyko, valid.data.Tayko[,c(2:5,7:9)])
valid.class.Tayko=apply(valid.pred.Tayko$net.result, 
                        1,which.max)-1
confusionMatrix(as.factor(valid.class.Tayko),       
                as.factor(Taykopp.df[valid.rows.Tayko,]$Purchase))

```

```{r}
#Neural Network 2 (one hidden layer with three nodes)

set.seed(1)
nn2.Tyko <- neuralnet(Purchase_No+Purchase_Yes~US+Web_order+Address_is_res+Freq_scaled+last_update_days_ago_scaled+First_update_days_ago_scaled, data=train.data.Tayko, linear.output = F, threshold = 0.05, hidden = 3)
plot(nn2.Tyko, rep = "best")

valid.pred.Tayko = compute(nn2.Tyko, valid.data.Tayko[,c(2:5,7:9)])
valid.class.Tayko=apply(valid.pred.Tayko$net.result, 
                        1,which.max)-1
confusionMatrix(as.factor(valid.class.Tayko),       
                as.factor(Taykopp.df[valid.rows.Tayko,]$Purchase))

```

```{r}
#Neural Network 3 (two hidden layers with two nodes)

set.seed(1)
nn3.Tyko <- neuralnet(Purchase_No+Purchase_Yes~US+Web_order+Address_is_res+Freq_scaled+last_update_days_ago_scaled+First_update_days_ago_scaled, data=train.data.Tayko, linear.output = F, threshold = 0.05, hidden = c(2,2))
plot(nn3.Tyko, rep = "best")

valid.pred.Tayko = compute(nn3.Tyko, valid.data.Tayko[,c(2:5,7:9)])
valid.class.Tayko=apply(valid.pred.Tayko$net.result, 
                        1,which.max)-1
confusionMatrix(as.factor(valid.class.Tayko),       
                as.factor(Taykopp.df[valid.rows.Tayko,]$Purchase))

```

