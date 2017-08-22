#----remove objects and environment variables---#
rm(list=ls())

#----get working directory---#
getwd()

#---- set working directory---#
setwd("C:/Users/Garima/Documents/R/instacart")

#----import required libraries ---#
library(data.table)
library(dplyr)
library(tidyr)
library(readr)
library(arules)
library(arulesViz)
library(methods)
library(sqldf)
library(ggplot2)

#----set memory size---#
memory.limit(size=500000)

# Load Data ---------------------------------------------------------------
path <- "C:/Users/Garima/Documents/R/instacart"
aisles <- fread(file.path(path, "aisles.csv"))
departments <- fread(file.path(path, "departments.csv"))
orderp <- fread(file.path(path, "order_products__prior.csv"))
orders_products <- fread(file.path(path, "MyData.csv"))
prd <- fread(file.path(path, "prd1.csv"))


#----check number of reordered-----
table(orders_products$reordered)

#---load data again----------
ordert <- fread(file.path(path, "order_products__train.csv"))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          
orders <- fread(file.path(path, "orders.csv"))
products <- fread(file.path(path, "products.csv"))

#----divide the orders table based on eval_set---#
orders_train=orders[orders$eval_set=="train",]
orders_test=orders[orders$eval_set=="test",]

#----------imputing days_since_prior_order
orders$days_since_prior_order[is.na(orders$days_since_prior_order)]=0 

#--------minimum number of cart items---
orderp %>% 
  group_by(order_id) %>% 
  summarize(n_items = last(add_to_cart_order)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(stat="count",fill="magenta") + 
  geom_rug()+
  coord_cartesian(xlim=c(0,80))

#-------multiplot function-------
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#--------------multiplot graphs-------
p1=orders %>% 
  ggplot(aes(x=order_hour_of_day)) + 
  geom_histogram(stat="count",fill="magenta")
p2=orders %>% 
  ggplot(aes(x=order_dow)) + 
  geom_histogram(stat="count",fill="magenta")
p3=orders %>% 
  ggplot(aes(x=days_since_prior_order)) + 
  geom_histogram(stat="count",fill="magenta")
multiplot(p1, p2,p3, cols=3)

#----------check for product id-------
sqldf("Select product_id from products where product_name='Bag of Organic Bananas'")

#------check order_id for the above product_id
de=sqldf("Select order_id from orderp where product_id='13176'")

#combine orders and product on above order_id
combine_banana=sqldf("Select * from de LEFT JOIN orders_products on de.order_id=orders_products.order_id")

#check time when max order took place
combine_banana %>% 
  ggplot(aes(x=order_hour_of_day)) + 
  geom_histogram(stat="count",fill="magenta")

# Reshape data ------------------------------------------------------------
orders_train$order_hour_of_day=as.factor(orders_train$order_hour_of_day)
system.time(tcom <- 	merge(orders_train, ordert, all.x=TRUE))
aisles$aisle <- as.factor(aisles$aisle)
departments$department <- as.factor(departments$department)
orders$eval_set <- as.factor(orders$eval_set)
products$product_name <- as.factor(products$product_name)
table(orders_products$reordered)
table(ordert$reordered)
str(orders_products)

#-----Directly Applying Model to data

#---train data---
train1=tcom[sample(nrow(tcom), replace=F),]

#--test data------
test1=orders_test[sample(nrow(orders_test), replace=F),]

#------XGBoost Algo---------
library(xgboost)
params <- list(
  "objective"           = "reg:logistic",
  "eval_metric"         = "logloss",
  "eta"                 = 0.1,
  "max_depth"           = 6,
  "min_child_weight"    = 10,
  "gamma"               = 0.70,
  "subsample"           = 0.76,
  "colsample_bytree"    = 0.95,
  "alpha"               = 2e-05,
  "lambda"              = 10
)

subtrain1 <- train1
#subtrain[,-11]
subtrain1$order_hour_of_day=as.numeric(subtrain1$order_hour_of_day)
subtrain1$reordered=as.numeric(subtrain1$reordered)
X <- xgb.DMatrix(as.matrix(subtrain1 %>% select(-reordered,-order_id, -product_id,-eval_set)), label = subtrain1$reordered)
model <- xgboost(data = X, params = params, nrounds = 80)
#------Apply model

test1$V1=as.numeric(test1$V1)
test1$order_hour_of_day=as.numeric(test1$order_hour_of_day)
X <- xgb.DMatrix(as.matrix(test1 %>% select(-order_id, -product_id,-reordered)))
test1$V1 <- predict(model, X)

xtab=table(observed=test1$reordered, predicted=test1$V1)
library(caret)
library(e1071)
confusionMatrix(xtab)
write.csv(xtab, file = "XGBOOST.csv", row.names = TRUE)
write(capture.output(summary(rmodel)), "xgboost.txt")
#end--------

#-----C50 model--------
library(C50)
train1=train1[,-3]
train1$reordered=as.factor(train1$reordered)
train1$order_hour_of_day=as.numeric(train1$order_hour_of_day)
test1$reordered=as.factor(test1$reordered)
rmodel=C5.0(reordered ~ .,data=train1,rules=TRUE)
summary(rmodel)
test_pred=predict(rmodel, test1[,-10], type="class")
library(caret)
xtab1=table(observed=test1$reordered, predicted=test_pred)
write.csv(xtab1, file = "C50.csv", row.names = TRUE)
library(e1071)
confusionMatrix(xtab1)
write(capture.output(summary(rmodel)), "C50Rules.txt")
save(rmodel, file="DT.rda")
load("DT.rda")

#-------rpart model---
library(rpart)
library(Metrics)

train1$reordered=as.numeric(train1$reordered)
fit=rpart(reordered~.,data=train1, method="anova")
write(capture.output(summary(fit)), "fit.txt")
test1$reordered=as.numeric(test1$reordered)
predictions=predict(fit,test1[,-10])
test1$V2 <- (test1$V1 > 1.4) * 1
xtab2=table(observed=test1$reordered, predicted=predictions)
library(e1071)
confusionMatrix(xtab2)
mape=function(y,yhat)
  mean(abs((y-yhat)/y))
mape1=mape(test1$reordered,predictions)
library(DMwR)
regr.eval(test1$reordered, predictions, stats=c('mae','mape'))
save.image("regression_model.R")
#load("regression_model.R")

#--------Applied but didnt work--------
library(randomForest)
library(caret)
library(inTrees)
fit_classify=randomForest(reordered~., train, importance=TRUE, do.trace=500)
pred=predict(fit_classify,test[,-10])
xtab1=table(observed=test$reordered, predicted=pred)
confusionMatrix(xtab1)
getTree(fit_classify,2)
treeList=RF2List(fit_classify)
exec=extractRules(treeList,train[,1:9])
exec[1:2,]
#Directly apply to model end-----------------------
products <- products %>% 
  inner_join(aisles) %>% inner_join(departments) %>% 
  select(-aisle_id, -department_id)

ordert$user_id <- orders$user_id[match(ordert$order_id, orders$order_id)]

prd$prod_reorder_probability <- prd$prod_second_orders / prd$prod_first_orders
prd$prod_reorder_times <- 1 + prd$prod_reorders / prd$prod_first_orders
prd$prod_reorder_ratio <- prd$prod_reorders / prd$prod_orders

prd <- prd %>% select(-prod_reorders, -prod_first_orders, -prod_second_orders)

rm(products)
gc()

rm(us,aisles, departments)


#---using apriori algorithm to predict rules and next instacart basket items-----------------
trans =orders_products %>% 
  group_by(product_id) %>% 
  left_join(products,by="product_id")

write.csv(trans, file = "transactions.csv")
trans1=read.transactions("transactions.csv", format = "single", sep = ",",cols = c(2,6))
inspect(trans[1:10])
apriorirules = apriori(trans1, parameter = list(support = 0.001, confidence = 0.25))
summary(apriorirules)
inspect(apriorirules[5:20])
#---end
