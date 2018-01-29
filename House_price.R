# House prize prediction

train = read.csv("train.csv",stringsAsFactors = F)
test = read.csv("test.csv",stringsAsFactors = F)

#checking the levels of variables in test and train datasets , they should be equal 
#checking character variables 



charatr=sapply(train,is.character)
new.char.dataset=train[,charatr]


# checking if all are good to convert in factor
for(i in 1:ncol(new.char.dataset)){
  print(unique(new.char.dataset[,i]))
  
}

#now back to main datasets train & test
colSums(is.na(train))
colSums(is.na(test))


# Remove the target variable not found in test set
SalePrice = train$SalePrice 
train$SalePrice = NULL

# Combine data sets
full_data = rbind(train,test)

#for loop for imputing missing values and converting characters to factors at a time


for(col in  1:80){
  if(class(full_data[,col])=="character"){
    new_col = full_data[,col]
    new_col[which(is.na(new_col),T)]="missing"
    full_data[col] =  as.factor(new_col)
    
  }
}


#now the levels of train & test datasets will be same
# Separate out our train and test sets

train = full_data[1:nrow(train),]
train$SalePrice = SalePrice  
test = full_data[(nrow(train)+1):nrow(full_data),]


summary(train)


# imputing negative value for missing numerical values 
# # Fill remaining NA values with -1

# We will be using a tree-based model in this example so the scale of our numbers shouldn't affect 
#our model and assigning the NA's to -1 will essentially allow -1 to act as a numeric flag for NA values. 
#If we were using a model that scales numeric variables by a learned parameter like linear regression, 
#we might want to use a different solution such as imputing missing values and we'd also want to consider 
#centering, scaling and normalizing the numeric features so that they are on the same scale and have
#distributions that are roughly normal.

train[is.na(train)] = -1
test[is.na(test)] = -1

#checking correlation in train data


#high correlation cor>0.5
for(col in colnames(train)){
  if(is.numeric(train[,col])){
    if(abs(cor(train[,col],train$SalePrice))>0.5){
  print(col)
  print(abs(cor(train[,col],train$SalePrice)))
  }
}
}


# low correaltion cor<0.1
for(col in colnames(train)){
  if(is.numeric(train[,col])){
    if(abs(cor(train[,col],train$SalePrice))<0.1){
      print(col)
    print(cor(train[,col],train$SalePrice))
  }
}
}

# checking multicollinearity b/w any TWO numerical values

#cors = cor(train[ , sapply(train, is.numeric)])
#high_cor = which(abs(cors) > 0.6 & (abs(cors) < 1))
#rows = rownames(cors)[((high_cor-1) %/% 38)+1]
#cols = colnames(cors)[ifelse(high_cor %% 38 == 0, 38, high_cor %% 38)]
#vals = cors[high_cor]

#cor_data = data.frame(cols=cols, rows=rows, correlation=vals)
#cor_data

#Multicollinearity 
# dependent variable not needed b/c correlation with that is already checked
SalePrice = train$SalePrice
train$SalePrice = NULL


for(i in colnames(train)){
  if(is.numeric(train[,i])){
  
  for(j in colnames(train)){
    if(is.numeric(train[,j])){
    if(i!=j){
    cor = cor(train[,i],train[,j])
    if(cor>0.6 & cor<1){
    
      print(c(i,j,cor))
    }
    }
    }
  }
  }
}


train$SalePrice = SalePrice # adding back the dependent variable

# plotting density plot to check normality 

for (col in colnames(train)){
  if(is.numeric(train[,col])){
    plot(density(train[,col]), main=col)
  }
}

######################
#prediction!!!

# Add variable that combines above grade living area with basement sq footage
train$total_sq_footage = train$GrLivArea + train$TotalBsmtSF
test$total_sq_footage = test$GrLivArea + test$TotalBsmtSF

# Add variable that combines above ground and basement full and half baths
train$total_baths = train$BsmtFullBath + train$FullBath + (0.5 * (train$BsmtHalfBath + train$HalfBath))
test$total_baths = test$BsmtFullBath + test$FullBath + (0.5 * (test$BsmtHalfBath + test$HalfBath))

# Remove Id since it should have no value in prediction
train$Id = NULL    
test$Id = NULL

library(caret)
library(plyr)
library(xgboost)
library(Metrics)

#Next let's create the control object and tuning variable grid we need to pass to our
#caret model. The target metric used to judge this competition is root mean squared logarithmic 
#error or RMSLE. Caret optimizes root mean squared error for regression by default, so if we want
#to optimize for RMSLE we should pass in a custom summary function via our caret control object. 
#The R package "Metrics" has a function for computing RMSLE so we can use that to compute the performance
#metric inside our custom summary function.

# Create custom summary function in proper format for caret
custom_summary = function(data, lev = NULL, model = NULL){
  out = rmsle(data[, "obs"], data[, "pred"])
  names(out) = c("rmsle")
  out
}

# Create control object
control = trainControl(method = "cv",  # Use cross validation
                       number = 5,     # 5-folds
                       summaryFunction = custom_summary                      
)

# Create grid of tuning parameters
grid = expand.grid(nrounds=c(100, 200, 400, 800), # Test 4 values for boosting rounds
                   max_depth= c(4, 6),           # Test 2 values for tree depth
                   eta=c(0.1, 0.05, 0.025),      # Test 3 values for learning rate
                   gamma= c(0.1), 
                   colsample_bytree = c(1), 
                   min_child_weight = c(1),
                   subsample=0.7)

set.seed(12)

xgb_tree_model =  train(SalePrice~.,      # Predict SalePrice using all features
                        data=train,
                        method="xgbTree",
                        trControl=control, 
                        tuneGrid=grid, 
                        metric="rmsle",     # Use custom performance metric
                        maximize = FALSE)   # Minimize the metric

xgb_tree_model$results

xgb_tree_model$bestTune

varImp(xgb_tree_model)

test_predictions = predict(xgb_tree_model, newdata=test)
test_predictions




submission = read.csv("sample_submission.csv")
submission$SalePrice = test_predictions
write.csv(submission, "home_prices_xgb_sub1.csv", row.names=FALSE)

getwd()

























