
df <- read.csv( 'StudentsPerformance.csv', na.strings="" )

#display the first 5 rows of data
head(df, n = 5)
#show the structure of data
str(df)
#show statistics about data
summary(df)

###############################################

############### data cleaning #################

## fill NA/s 

# showing how many NA/s in each column
colSums( is.na(df) )

table(df$internet)

#fill Na with yes as it appears more than no
df$internet[ is.na(df$internet) ] <- "yes"

# check is now, NA is done. 
colSums( is.na(df) )

## fill NA, done.


## check the uniform format.

chars_colnames <- names(df) [sapply(df, is.character)]
num_colnames <- names(df)[sapply(df, is.numeric)][-1]

for(colname in chars_colnames){
  
  print( table( df[colname]) )
}

# we observe that sex column have F and Female And M and Male
# we must make it uniformed to Male and Female

df$sex[ df$sex == "M" ] <- "Male"
df$sex[ df$sex == "F" ] <- "Female"

# check it, now all columns have uniform format.
for(colname in chars_colnames){
  print(table(df[colname]) )
} 

## check the uniform format, done.


## check duplicated.

print(paste("the number of duplicated rows:", sum( duplicated(df) ) ))

## check duplicated, done.

## remove and convert cats columns to factor.

# remove id col.
df <- df[ names(df)[-1] ]
head(df, n=5)


# convert cats columns to factor.
df[chars_colnames] <- lapply( df[chars_colnames], as.factor)

str(df)
summary(df)


# convert cats columns to factor, done.

# take a look to outliers
outlier_length <- function(data) {
  
  q1 <- quantile(data, 0.25)
  q3 <- quantile(data, 0.75)
  
  # Calculate interquartile range
  iqr <- q3 - q1
  
  # Define outliers boundaries
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  # Identify and calculate length of outliers
  outliers <- data[data < lower_bound | data > upper_bound]
  
  outlier_length <-length(outliers)
  
  # Return results
  return(outlier_length)
}

for(colname in num_colnames){
  temp_prec <- ( outlier_length(df[[colname]]) / dim(df)[1]) * 100
  print(paste("the prec of outlier of", colname, " : ", round(temp_prec, 0), "% of the total data." ))
}

###############################################

############ data visualization ###############

str(df)

make_pie = function(colname){
  
  temp_col <- table( df[colname] )
  
  piepercent <- round(100 * temp_col / sum(temp_col), 1)
  
  pie(
    piepercent, 
    labels = paste0(names(temp_col)," ",piepercent, "%"),
    
    col = rainbow(length(temp_col)), 
    main = paste("the Percetage of", colname,"in Data")
  )

}

par(mfrow = c(2,3) )
for (colname in chars_colnames){
  make_pie(colname);
}

str(df)

make_hist = function(colname){

  hist(
     df[[colname]], 
     col="lightblue",
     main = paste("The Distribuiton of", colname),
     xlab = colname
     )
  
}

par(mfrow=c(3,3))

for (colname in num_colnames){
  make_hist(colname);
}


make_box = function(colname){
  
  boxplot(
    df[[colname]], 
    col="lightblue",
    main = paste("The Distribuiton of", colname),
    xlab = colname
    )
  
}


par(mfrow = c(3,3))

for (colname in num_colnames){
  make_box(colname);
}

library(heatmaply)

heatmaply_cor(cor(df[num_colnames]))


###############################################

################# ML Modeling #################

str(df)
library(caret)

set.seed(9999)

train_test_split = function(train_per=.8){
  
  train_index <- sample(nrow(df), train_per * nrow(df))
  
  train <- df[train_index, ]
  test <- df[-train_index, ]
  
  return(list(train, test))
}

train_test <- train_test_split()
train <- train_test[[1]]
test <- train_test[[2]]

dim(train)
dim(test)


fit <- lm( G1 ~ . , data= train)
fit

summary(fit)

attributes(fit)

fit$coefficients

g1_predictions <- predict(fit, newdata = test[names(df)[-which(names(df) == "G1")]])

# Calculate Mean Squared Error
mse <- mean((test$G1 - g1_predictions)^2)

# Print the MSE
cat("Mean Squared Error (MSE):", mse, "\n")

# Calculate R-squared
r_squared <- 1 - sum((test$G1 - g1_predictions)^2) / sum((test$G1 - mean(test$G1))^2)

# Print the R-squared
cat("R-squared:", r_squared, "\n")



train_and_predict_lm <- function(target, train, test){
  
  fit <- lm( reformulate(names(train)[which(names(train) != target)], response = target),
             data= train)

  g_predictions <- predict(fit, newdata = test[names(df)[which(names(df) != target)]])
  
  target_variable <- test[[target]]
  
  # Calculate Mean Squared Error
  mse <- mean((target_variable - g1_predictions)^2)
  
  
  # Calculate R-squared
  r_squared <- 1 - sum((target_variable - g_predictions)^2) / sum((target_variable - mean(target_variable))^2)
  
  return(c(mse, r_squared))
}


train_and_predict_lm("G1", train, test)
train_and_predict_lm("G2", train, test)
train_and_predict_lm("G3", train, test)


#install.packages("rpart")
#install.packages("rpart.plot")

library(rpart)
library(rpart.plot)


train_and_predict_tree <- function(target, train, test){
  
  tree <- rpart( 
    reformulate(names(train)[-which(names(train) == target)], response = target),
    data= train,
    method = "anova"
    )
  
  rpart.plot(tree)
  
  title(
    main =  paste("Decision Tree to Predict: ", target),
    font.main = 4,
    cex.main = .9,   # Adjust this value for smaller or larger text
    adj = 0     # Adjust adj parameter for positioning (0, 0) is top-left
  )  
  
  g_predictions <- predict(tree, newdata = test[names(df)[-which(names(df) == target)]])
  target_variable <- test[[target]]
  
  # Calculate Mean Squared Error
  mse <- mean((target_variable - g1_predictions)^2)
  
  
  # Calculate R-squared
  r_squared <- 1 - sum((target_variable - g_predictions)^2) / sum((target_variable - mean(target_variable))^2)
  
  return(c(mse, r_squared))
}

par(mfrow = c(1,1))

train_and_predict_tree("G1", train, test)
train_and_predict_tree("G2", train, test)
train_and_predict_tree("G3", train, test)
