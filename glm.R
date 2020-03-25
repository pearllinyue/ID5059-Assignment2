#####data manipulation 
#import train and test data
library(readr)
#setwd("~/Desktop/ID5059/Practicals/P02")
traindata <- read_csv("train.csv", na = c("-1","-1.0"))
testdata <- read_csv("test.csv", na = c("-1","-1.0"))

#combine dataset for equal treatment for odd cases
library(dplyr)
combinedata <- bind_rows(traindata %>% mutate(dset = 'traindata'),
                         testdata %>% mutate(dset = 'testdata',
                                             target = NA))
combinedata <- combinedata %>% mutate(dset = factor(dset))

#calculate the percentage of missing values for every variable
permiss <- function(x) {sum(is.na(x)) / length(x)*100 }
trainmissing <- apply(traindata, 2, permiss)
testmissing <- apply(testdata, 2, permiss)
trainmissing 

#draw correlation coefficient plot 
library(dplyr)
library(corrplot)

traindata %>%
  select(-starts_with("ps_calc"), -ps_ind_10_bin, -ps_ind_11_bin, -ps_car_10_cat, -id) %>%
  mutate_at(vars(ends_with("cat")), funs(as.integer)) %>%
  mutate_at(vars(ends_with("bin")), funs(as.integer)) %>%
  mutate(target = as.integer(target)) %>%
  cor(use="complete.obs", method = "spearman") %>%
  corrplot(type="lower", tl.col = "black",  diag=FALSE) #NA rows excluded, all variables as integers
#further from 0 = larger correlation -> look for large, bright circles
#most of the correlation are within groups
#there's no correlation with target

#plot large correlations
traindata %>%
  select(ps_ind_12_bin, ps_ind_14, ps_ind_16_bin, ps_ind_17_bin, ps_ind_18_bin, ps_reg_02,
         ps_reg_03, ps_car_12, ps_car_13, ps_car_14, ps_car_15, ps_car_02_cat, ps_car_04_cat) %>%
  mutate_at(vars(ends_with("cat")), funs(as.integer)) %>%
  mutate_at(vars(ends_with("bin")), funs(as.integer)) %>%
  cor(use="complete.obs", method = "spearman") %>%
  corrplot(type="lower", tl.col = "black",  diag=FALSE, method = "number")
#strong correlation between 'ps_ind_12_bin' & 'ps_ind_14' (0.94)
#other positive and negative strong ones

#drop variables with over 40% missing values & highly collinear variables
dropvars <- c('ps_car_03_cat', 'ps_car_05_cat', 'ps_car_13', 'ps_reg_03', 'ps_car_14', 'ps_ind_17_bin', 'ps_ind_18_bin', 'ps_ind_12_bin')
#install.packages("h2o")
library(h2o)
traindata <- traindata[ , !(names(traindata) %in% dropvars)]
testdata <- testdata[ , !(names(testdata) %in% dropvars)]

#replce missing continuous with mean
car_12_mean <- mean(combinedata$ps_car_12, na.rm = TRUE)
traindata$ps_car_12[is.na(traindata$ps_car_12)] <- car_12_mean
testdata$ps_car_12[is.na(testdata$ps_car_12)] <- car_12_mean

#replace missing ordinal with mode
getthemode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
} 
#refer to https://www.tutorialspoint.com/r/r_mean_median_mode.htm
car_11_mode <- getthemode(combinedata$ps_car_11)
traindata$ps_car_11[is.na(traindata$ps_car_11)] <- car_11_mode
testdata$ps_car_11[is.na(testdata$ps_car_11)] <- car_11_mode

#replace missing categorical with -1 
traindata$ps_ind_02_cat[is.na(traindata$ps_ind_02_cat)] <- -1
testdata$ps_ind_02_cat[is.na(testdata$ps_ind_02_cat)] <- -1
traindata$ps_ind_04_cat[is.na(traindata$ps_ind_04_cat)] <- -1
testdata$ps_ind_04_cat[is.na(testdata$ps_ind_04_cat)] <- -1
traindata$ps_ind_05_cat[is.na(traindata$ps_ind_05_cat)] <- -1
testdata$ps_ind_05_cat[is.na(testdata$ps_ind_05_cat)] <- -1
traindata$ps_car_01_cat[is.na(traindata$ps_car_01_cat)] <- -1
testdata$ps_car_01_cat[is.na(testdata$ps_car_01_cat)] <- -1
traindata$ps_car_02_cat[is.na(traindata$ps_car_02_cat)] <- -1
testdata$ps_car_02_cat[is.na(testdata$ps_car_02_cat)] <- -1
traindata$ps_car_07_cat[is.na(traindata$ps_car_07_cat)] <- -1
testdata$ps_car_07_cat[is.na(testdata$ps_car_07_cat)] <- -1
traindata$ps_car_09_cat[is.na(traindata$ps_car_09_cat)] <- -1
testdata$ps_car_09_cat[is.na(testdata$ps_car_09_cat)] <- -1
traindata$ps_car_11_cat[is.na(traindata$ps_car_11_cat)] <- -1
testdata$ps_car_11_cat[is.na(testdata$ps_car_11_cat)] <- -1

#set factor(target) and logical variables
traindata <- traindata %>%
  mutate_at(vars(ends_with("cat")), funs(factor)) %>%
  mutate_at(vars(ends_with("bin")), funs(as.logical)) %>%
  mutate(target = as.factor(target))
testdata <- testdata %>%
  mutate_at(vars(ends_with("cat")), funs(factor)) %>%
  mutate_at(vars(ends_with("bin")), funs(as.logical))
traindata <- traindata %>%
  mutate_at(vars(ends_with("bin")), funs(as.numeric))
testdata <- testdata %>%
  mutate_at(vars(ends_with("bin")), funs(as.numeric))

#save the imputed datasets
write.csv(traindata, "train_imputed.csv")
write.csv(testdata,"test_imputed.csv")

#balance undersampling train dataset
#install.packages("unbalanced")
library(unbalanced)
trainbalanced <- ubUnder(traindata[3:51], traindata$target)
trainbalanced <- trainbalanced[-3]
trainbalanced <- as.data.frame(trainbalanced)
trainbalanced <- trainbalanced %>%
  mutate_at(vars(ends_with("bin")), funs(as.numeric))

#reset colnames of original
colnames(trainbalanced)[colnames(trainbalanced) == "Y"] <- "target"
col <- colnames(trainbalanced)
for (i in 1:length(col) - 1) {
  col[i] <- substr(col[i], start = 3, stop = nchar(col[i]))
}
colnames(trainbalanced) <- col 

#for original order
trainbalanced <- trainbalanced[,c(50,1:49)]
write.csv(trainbalanced, "trainbalanced.csv")



#####fit the glm 
#import balanced data into R
library(data.table)
train_data <- fread("trainbalanced.csv")
test_data <- fread("test.csv")

#set column 'target' as factor
train_data$target <- as.factor(train_data$target)

#set seed 5059 for reproduceability
set.seed(5059)
#install.packages("h2o")
library(h2o)
h2o.init(ip = "localhost", port = 54321, nthreads = -1) 
#install JAVA Version 8 (Update 201 for this)
#refer to http://docs.h2o.ai/h2o/latest-stable/h2o-r/docs/reference/h2o.init.html

#transfer both datasets to h2o
train_hex <- as.h2o(train_data,  destination_frame ="train_data")
test_hex <- as.h2o(test_data,  destination_frame ="test_data")

#set a variable 'y' to show the outcome 'target'
y <- "target"

#remaining variables should be independent due to the fact that id variables have been removed
x <- colnames(train_hex[,-1])


#fit a glm logistic regression model using h2o
###1 glm with 5-fold, without lambda search
set.seed(5059)
system.time(glm_5_fold <- h2o.glm(x = x, 
                           y = y, 
                           training_frame = train_hex,
                           solver = 'IRLSM',
                           nfolds = 5,
                           keep_cross_validation_predictions = TRUE,
                           fold_assignment = "Stratified",
                           family = "binomial",
                           model_id = 'glm_5_fold')
)

#result
h2o.performance(glm_5_fold)
h2o.varimp(glm_5_fold)
deviance1 <- 1 - (h2o.null_deviance(glm_5_fold) - h2o.residual_deviance(glm_5_fold)) / h2o.null_deviance(glm_5_fold)
deviance1

#prediction
predict_test_1 <- h2o.predict(glm_5_fold, test_hex)
predict_test_1 <- predict_test_1[ , 'p1']

#submission
submission1 <- data.frame(id = test_data$id, target = as.vector(predict_test_1))
fwrite(submission1, "glm_5_fold.csv", row.names = F)



###2 glm with 10-fold, with lambda search
set.seed(5059)
system.time(glm_10_fold_lambda <- h2o.glm(x = x, 
                                y = y, 
                                training_frame = train_hex,
                                nfolds = 10,
                                keep_cross_validation_predictions = TRUE,
                                fold_assignment = "Stratified",
                                family = "binomial", 
                                model_id = 'glm_10_fold_lambda',
                                lambda_search = TRUE,
                                balance_classes = TRUE)
)

#result
h2o.performance(glm_10_fold_lambda)
h2o.varimp(glm_10_fold_lambda)
deviance2 <- 1 - (h2o.null_deviance(glm_10_fold_lambda) - h2o.residual_deviance(glm_10_fold_lambda)) / h2o.null_deviance(glm_10_fold_lambda)
deviance2

#prediction
predict_test_2 <- h2o.predict(glm_10_fold_lambda, test_hex)
predict_test_2 <- predict_test_2[ , 'p1']

#submission
submission2 <- data.frame(id = test_data$id, target = as.vector(predict_test_2))
fwrite(submission2, "glm_10_fold_lambda.csv", row.names = F)



###3 glm with 5-fold, with lambda search
set.seed(5059)
system.time(glm_5_fold_lambda <- h2o.glm(x = x, 
                                          y = y, 
                                          training_frame = train_hex,
                                          nfolds = 5,
                                          keep_cross_validation_predictions = TRUE,
                                          fold_assignment = "Stratified",
                                          family = "binomial", 
                                          model_id = 'glm_5_fold_lambda',
                                          lambda_search = TRUE,
                                          balance_classes = TRUE)
)

#result
h2o.performance(glm_5_fold_lambda)
h2o.varimp(glm_5_fold_lambda)
deviance3 <- 1 - (h2o.null_deviance(glm_5_fold_lambda) - h2o.residual_deviance(glm_5_fold_lambda)) / h2o.null_deviance(glm_5_fold_lambda)
deviance3

#prediction
predict_test_3 <- h2o.predict(glm_5_fold_lambda, test_hex)
predict_test_3 <- predict_test_3[, 'p1']

#submission
submission3 <- data.frame(id = test_data$id, target = as.vector(predict_test_3))
fwrite(submission3, "glm_5_fold_lambda.csv", row.names = F)