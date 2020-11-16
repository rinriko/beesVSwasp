library(EBImage)
library(keras)
library(OpenImageR)
library(imager)
library(magick)
# =================
resizeImg <- function(fpath) {
  tryCatch({
    img <- load.image(fpath)
    print(dim(img))
    img <- grayscale(img, method = "Luma", drop = TRUE)
    y <- imager::resize(img, 32, 32)
    print(dim(y))
    # y <- array_reshape(y, c(32, 32, 1))
    as.vector(y)
  }, error = function(e) {
    print(paste("MY_ERROR:  ", e))
    print(fpath)
    if(dim(img)[4]>3){
      rgbimg<-rm.alpha(img)
      print(dim(rgbimg))
      img <- grayscale(rgbimg, method = "Luma", drop = TRUE)
      y <- imager::resize(img, 32, 32)
      print(dim(y))
      # y <- array_reshape(y, c(32, 32, 1))
      as.vector(y)
    }else{
      gimg<-img[,,,1]
      print(dim(gimg))
      img <- grayscale(gimg, method = "Luma", drop = TRUE)
      y <- imager::resize(img, 32, 32)
      print(dim(y))
      # y <- array_reshape(y, c(32, 32, 1))
      as.vector(y)
    }
  })
}

# resizeImg <- function(fpath) {
#   tryCatch({
#     img <- load.image(fpath)
#     print(dim(img))
#     img <- grayscale(img, method = "Luma", drop = TRUE)
#     svdGray <- svd(img)
#     comp <- sapply(svdGray, function(i) {
#       compressed = i$u[, 1:35] %*% diag(i$d[1:35]) %*% t(i$v[, 1:35])
#     }, simplify = 'array')
#     y <- imager::resize(comp, 32, 32)
#     print(dim(y))
#     # y <- array_reshape(y, c(32, 32, 1))
#     as.vector(y)
#   }, error = function(e) {
#     print(paste("MY_ERROR:  ", e))
#     print(fpath)
#     if(dim(img)[4]>3){
#       rgbimg<-rm.alpha(img)
#       print(dim(rgbimg))
#       img <- grayscale(rgbimg, method = "Luma", drop = TRUE)
#       svdGray <- svd(img)
#       comp <- sapply(svdGray, function(i) {
#         compressed = i$u[, 1:35] %*% diag(i$d[1:35]) %*% t(i$v[, 1:35])
#       }, simplify = 'array')
#       y <- imager::resize(comp, 32, 32)
#       print(dim(y))
#       # y <- array_reshape(y, c(32, 32, 1))
#       as.vector(y)
#     }else{
#       gimg<-img[,,,1]
#       print(dim(gimg))
#       img <- grayscale(gimg, method = "Luma", drop = TRUE)
#       svdGray <- svd(img)
#       comp <- sapply(svdGray, function(i) {
#         compressed = i$u[, 1:35] %*% diag(i$d[1:35]) %*% t(i$v[, 1:35])
#       }, simplify = 'array')
#       y <- imager::resize(comp, 32, 32)
#       print(dim(y))
#       # y <- array_reshape(y, c(32, 32, 1))
#       as.vector(y)
#     }
#   })
# }
path = "/Users/minmin/TTU/Programming with R for Data Analysis and Machine Learning/Project/beesVSwasp/Dataset/kaggle_bee_vs_wasp"

df <-
  read.csv(paste(
    path,
    .Platform$file.sep,
    list.files(path, pattern = ".csv",  all.files = FALSE),
    sep = ""
  ))

df <- data.frame(df)
# change format file path
df$path <- gsub("\\\\", .Platform$file.sep, df$path)
# change file path to fullpath
df$path <-paste(path, .Platform$file.sep, df$path, sep = "")
# fix photo quality
# df.drop(df[df$id == 5310].index, inplace = True) 
df[df$id == 5310, ]$photo_quality <- 0
df[df$id == 5310, ]
# drop column id
df <- df[, !names(df) %in% c("id"), drop = FALSE]
# select high photo quality
df.photo_quality <- df[df$photo_quality == 1, , drop = FALSE]
df.bee <- df.photo_quality[df.photo_quality$is_bee == 1, , drop = FALSE]
df.w <- df.photo_quality[df.photo_quality$is_wasp == 1, , drop = FALSE]
df.photo_quality <- rbind(df.bee,df.w )
dim(df.photo_quality)
table(df.photo_quality$label)
# -------------------------------
df.pt1 <- df.photo_quality[df.photo_quality$is_validation == 0, , drop = FALSE]
df.pt2 <- df.photo_quality[df.photo_quality$is_final_validation == 0, , drop = FALSE]
df.photo_quality.train  <- rbind(df.pt1,df.pt2 )
df.pv <-df.photo_quality[df.photo_quality$is_validation == 1, , drop = FALSE]
df.pt <-df.photo_quality[df.photo_quality$is_final_validation == 1, , drop = FALSE]
df.photo_quality.test <- rbind(df.pv,df.pt)
df.photo_quality.test
dim(df.photo_quality.test)
dim(df.photo_quality.train)
df.photo_quality.train <- df.photo_quality.train[sample(nrow(df.photo_quality.train), size = 4000, replace = FALSE, prob = NULL),]
# df.photo_quality.validation <- df.photo_quality.validation[sample(nrow(df.photo_quality.validation), size = 200, replace = FALSE, prob = NULL),]
df.photo_quality.test <- df.photo_quality.test[sample(nrow(df.photo_quality.test), size = 1000, replace = FALSE, prob = NULL),]
table(df.photo_quality.train$label)
# reset index df.photo_quality.train
row.names(df.photo_quality.train) <- NULL
# table(df.photo_quality.validation$label)
# # reset index df.photo_quality.validation
# row.names(df.photo_quality.validation) <- NULL
table(df.photo_quality.test$label)
# reset index df.photo_quality.test
row.names(df.photo_quality.test) <- NULL
# --------------------------------
# df.train <-
#   df[df$is_validation == 0 && df$is_final_validation == 0, , drop = FALSE]
# df.validation <- df[df$is_validation == 1, , drop = FALSE]
# df.test <- df[df$is_final_validation == 1, , drop = FALSE]
# df.test <- rbind(df.validation,df.test)
# df.train <- df.train[sample(nrow(df.train), size = 4000, replace = FALSE, prob = NULL),]
# # df.photo_quality.validation <- df.photo_quality.validation[sample(nrow(df.photo_quality.validation), size = 200, replace = FALSE, prob = NULL),]
# df.test <- df.test[sample(nrow(df.test), size = 1000, replace = FALSE, prob = NULL),]
# table(df.train$label)
# # reset index df.train
# row.names(df.train) <- NULL
# table(df.validation$label)
# # reset index df.validation
# row.names(df.validation) <- NULL
# table(df.test$label)
# # reset index df.test
# row.names(df.test) <- NULL
# -----------------------------------

img <- lapply(df.photo_quality.train$path,resizeImg)
dim(df.photo_quality.train)
# dt_train<-array_reshape(t, c(nrow(df.photo_quality.train),256*256))
data_train <- list()
for (i in 1:nrow(df.photo_quality.train)) {data_train <- rbind(data_train, data.frame(matrix(img[[i]],nrow = 1)))}
str(data_train)
data_train <- as.data.frame(data_train)
data_train
# t <- as.matrix(dt_train)
# data.frame(dt_train)
label_train <- df.photo_quality.train$label
# dt_train <- cbind(label, data_train)
# str(as.data.frame(dt_train))
# dt_train<-as.data.frame(dt_train)
img <- lapply(df.photo_quality.test$path,resizeImg)
data_test <- list()
for (i in 1:nrow(df.test)) {data_test <- rbind(data_test, data.frame(matrix(img[[i]],nrow = 1)))}
label_test <- df.photo_quality.test$label
# img
# ==================
# https://valentinitnelav.github.io/satellite-image-classification-r/
# Packages for spatial data processing & visualization
library(rgdal)
library(gdalUtils)
library(raster)
library(sf)
library(sp)
library(RStoolbox)
library(getSpatialData)
library(rasterVis)
library(mapview)

library(RColorBrewer)
library(plotly)
library(grDevices)

# Machine learning packages
library(caret)
library(randomForest)
library(ranger)
library(MLmetrics)
library(nnet)
library(NeuralNetTools)
library(LiblineaR)
library(keras)


# Packages for general data processing and parallel computation
library(data.table)
library(dplyr)
library(stringr)
library(doParallel)
library(snow)
library(parallel)


# ============================================================================================================
# path = "/Users/minmin/TTU/Programming with R for Data Analysis and Machine Learning/Project/beesVSwasp/Dataset/kaggle_bee_vs_wasp"
# dt <- read.csv(paste(path, .Platform$file.sep, list.files(path, pattern = ".csv",  all.files = FALSE), sep = ""))
# dt <- data.frame(dt)
# dt <- as.data.table(dt$label, )
# dt
# dt$label
# ============================================================================================================
set.seed(321)
# A stratified random split of the data
# idx_train <- createDataPartition(dt$label,
#                                  p = 0.7, # percentage of data as training
#                                  list = FALSE)
# dt_train <- dt[idx_train]
# dt_test <- dt[-idx_train]
# dt_train$label
# table(dt_train$label)
# table(dt_test$label)

# ============================================================================================================
# create cross-validation folds (splits the data into n random groups)
n_folds <- 10
set.seed(321)
folds <- createFolds(1:nrow(df.photo_quality.train), k = n_folds)
# Set the seed at each resampling iteration. Useful when running CV in parallel.
seeds <- vector(mode = "list", length = n_folds + 1) # +1 for the final model
for(i in 1:n_folds) seeds[[i]] <- sample.int(1000, n_folds)
seeds[n_folds + 1] <- sample.int(1000, 1) # seed for the final model
# ============================================================================================================
ctrl <- trainControl(summaryFunction = multiClassSummary,
                     method = "cv",
                     number = n_folds,
                     search = "grid",
                     classProbs = TRUE, # not implemented for SVM; will just get a warning
                     savePredictions = TRUE,
                     index = folds,
                     seeds = seeds)
# ============================================================================================================
# Register a doParallel cluster, using 3/4 (75%) of total CPU-s
cl <- makeCluster(3/4 * detectCores())
registerDoParallel(cl)
# dt = as.data.frame(dt_train)
model_rf <- caret::train(y=label_train,x=data_train, method = "rf",
                         importance = TRUE, # passed to randomForest()
                         # run CV process in parallel;
                         # see https://stackoverflow.com/a/44774591/5193830
                         allowParallel = TRUE,
                         tuneGrid = data.frame(mtry = c(2, 3, 4, 5, 8)),
                         trControl = ctrl)
stopCluster(cl); remove(cl)
# Unregister the doParallel cluster so that we can use sequential operations
# if needed; details at https://stackoverflow.com/a/25110203/5193830
registerDoSEQ()
saveRDS(model_rf, file = "/Users/minmin/TTU/Programming with R for Data Analysis and Machine Learning/Project/beesVSwasp/cache/model_rf.rds")
# ============================================================================================================
model_rf$times$everything # total computation time
plot(model_rf) # tuning results
# ============================================================================================================
dim(data_test)
dim(label_test)
label_test
cm_rf <- confusionMatrix(data = predict(model_rf, newdata = data_test),
                         as.factor(label_test))
cm_rf
# ============================================================================================================
# ============================================================================================================
# ============================================================================================================

# Grid of tuning parameters
svm_grid <- expand.grid(cost = c(0.2, 0.5, 1),
                        Loss = c("L1", "L2"))

cl <- makeCluster(3/4 * detectCores())
registerDoParallel(cl)
model_svm <- caret::train(y=label_train,x=data_train,  method = "svmLinear3", 
                          allowParallel = TRUE,
                          tuneGrid = svm_grid,
                          trControl = ctrl)
stopCluster(cl); remove(cl)
registerDoSEQ()
# Warning message:
# In train.default(x, y, weights = w, ...) :
#   Class probabilities were requested for a model that does not implement them
# (see why above)
saveRDS(model_svm, file = "/Users/minmin/TTU/Programming with R for Data Analysis and Machine Learning/Project/beesVSwasp/cache/model_svm.rds")



model_svm$times$everything
plot(model_svm)
# The confusion matrix using the test dataset
cm_svm <- confusionMatrix(data = predict(model_svm, newdata = data_test),
                          as.factor(label_test))
cm_svm





# ============================================================================================================



