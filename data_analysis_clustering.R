library(data.table);
library(arrow);
library(dplyr);
library(tidyr); 
library(MLmetrics);
library(Matrix);
library(psych)
library(ggplot2)
library(caret)
library(corrplot)
install.packages("car")
library(car)



book_example <- read_parquet("C:/Users/Public/c439ef22282f412ba39e9137a3fdabac.parquet")
head(book_example)

# count total missing values 
print("Count of total missing values  ")
sum(is.na(book_example))


# count of duplicated data
sum(duplicated(book_example))

#describing summary of the data
describe(book_example)


# scattter plot of book_example
qplot(seq_along(book_example$ask_size1), book_example$ask_size1)

# scattter plot of book_example
qplot(seq_along(book_example$ask_size2), book_example$ask_size2)

# frquency distribution of ask_size1
hist(book_example$ask_size1)

## removing outlier of ask_size1
mn_ask_size1 = mean(book_example$ask_size1)
mn_ask_size1

rm_outlier <- subset(book_example, book_example$ask_size1<mn_ask_size1) 
rm_outlier
#describing summary of the data
describe(rm_outlier)


# frquency distribution of ask_size1 after outlier removal
hist(rm_outlier$ask_size1)

# frquency distribution of ask_size2 
hist(rm_outlier$ask_size2)

## removing outlier of ask_size2
mn_ask_size2 = mean(rm_outlier$ask_size2)
mn_ask_size2

rm_outlier_new <- subset(rm_outlier, rm_outlier$ask_size2<mn_ask_size2) 
rm_outlier_new
#describing summary of the data
describe(rm_outlier_new)

# frquency distribution of ask_size2 after outlier removal 
hist(rm_outlier_new$ask_size2)


# Box-Cox transformation using caret package
bc_trans <- BoxCoxTrans(rm_outlier_new$ask_size1)
transformed_data <- predict(bc_trans, rm_outlier_new$ask_size1)

# plot histogram of transformed data
ggplot(data.frame(x = transformed_data), aes(x)) +
  geom_histogram(binwidth = 0.8, color = "black", fill = "lightblue") +
  ggtitle("Histogram of Box-Cox Transformed Data")


#create a list with the BoxCox objects
g <- apply(rm_outlier_new, 2, BoxCoxTrans)

#use map2 from purr to apply the models to new data

z <- purrr::map2(g, rm_outlier_new, function(x, y) predict(x, y)) 

#here the transformation is performed on the same data on 
#which I estimated the BoxCox lambda for

B_trans = as.data.frame(do.call(cbind, z)) #to convert to data frame

trasformed_data = data.frame(B_trans)
head(trasformed_data)


## data scaling
scale(trasformed_data)


corrplot(cor(trasformed_data), method = "number")

# creating constant vector for VIF
const_vector <- rep(1, 307116)
df<- data.frame(const_vector) 
df

updated <- cbind(df, trasformed_data)

model <- lm(updated$const_vector ~ updated$seconds_in_bucket + updated$bid_price1 + updated$ask_price1 + updated$bid_price2 + updated$ask_price2 + updated$bid_size1 + updated$ask_size1 + updated$bid_size2 + updated$ask_size2, data=updated)

#calculate the VIF for each predictor variable in the model
vif(model)


# dropping correlated features
imp_features = select(updated, seconds_in_bucket,bid_size1,ask_size1,bid_size2,ask_size2)

write.csv(imp_features,"C:/Users/Public/cleaned_data.csv") 
