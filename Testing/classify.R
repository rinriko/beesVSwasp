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

# Packages for general data processing and parallel computation
library(data.table)
library(dplyr)
library(stringr)
library(doParallel)
library(snow)
library(parallel)


# ============================================================================================================
path = "/Users/minmin/TTU/Programming with R for Data Analysis and Machine Learning/Project/beesVSwasp/Dataset/kaggle_bee_vs_wasp"
dt <- read.csv(paste(path, .Platform$file.sep, list.files(path, pattern = ".csv",  all.files = FALSE), sep = ""))
dt <- data.frame(dt)
dt <- as.data.table(dt$label, )
dt
dt$label
# ============================================================================================================
set.seed(321)
# A stratified random split of the data
idx_train <- createDataPartition(dt$label,
                                 p = 0.7, # percentage of data as training
                                 list = FALSE)
dt_train <- dt[idx_train]
dt_test <- dt[-idx_train]
dt_train$label
table(dt_train$label)
table(dt_test$label)

# ============================================================================================================
# create cross-validation folds (splits the data into n random groups)
n_folds <- 10
set.seed(321)
folds <- createFolds(1:nrow(dt_train), k = n_folds)
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
model_rf <- caret::train(label ~ . , data = as.matrix(dt_train), method = "rf",
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
saveRDS(model_rf, file = "./cache/model_rf.rds")
# ============================================================================================================
model_rf$times$everything # total computation time
plot(model_rf) # tuning results
# ============================================================================================================
cm_rf <- confusionMatrix(data = predict(model_rf, newdata = dt_test),
                         dt_test$class)
cm_rf
# ============================================================================================================
# ============================================================================================================
# ============================================================================================================

