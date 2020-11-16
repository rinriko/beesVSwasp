# This program will take a set of photos including bees or wasps, and determine if the photo is a bee or a
# wasp.
# By: Victoria Obrien, Jordan Ketring

# 11/11/2020: version 1: inputs color images, separates color channels, performs image compression via svd
# 11/13/2020: version 2: if an image is not rgb - converts to 3 channel rgb, resizes all images to 250x250,
#                       saves compressed/resized images into respective file folders, converts all images to
#                       grayscale and saves them to a file folder
# 11/15/2020: version 3: Narrowed some of the goals, added functionaliry to sort bees and wasp pictures and
#                        pair them with labels. Added neural network model

# ============================================================================================================
# manual functions
# ============================================================================================================

# reads in images and converts them to cimg (useable format)
readImages <- function(fullImPath, myIms, nImages){
  x <- c(1:nImages)
  for (val in x){
    myIms[[val]] <- magick2cimg(image_resize(image_read(fullImPath[[val]]), '256x256!'))
    # if the image is not an rgb image (grayscale), convert it to RGB
    if (dim(myIms[[val]])[4] != 3){
      myIms[[val]] <- cimg(array(myIms[[val]], c(dim(myIms[[val]])[1], dim(myIms[[val]])[2], 1, 3)))
    }
  }
  return(myIms)
}


# ============================================================================================================
# libraries
# ============================================================================================================
library(imager)
library(gsubfn)
library(magick)
library(geometry)

install.packages("remotes")
remotes::install_github("rstudio/tensorflow")

library(tensorflow)

install_tensorflow(version = "2.0.0b1", method = "conda", envname = "r-reticulate")

library(keras)

# ============================================================================================================
# Set the path and read in the .csv containing all the images info and class labels
# ============================================================================================================
# get path to .csv file
path <- "C:\\Users\\joket\\Documents\\github repos\\beesVSwasp\\Dataset\\kaggle_bee_vs_wasp"

# read in csv file
myData <- read.csv(paste(path, "\\", list.files(path, pattern = ".csv"), sep = ""))
BeeWaspData <- data.frame(myData)

# ============================================================================================================
# read in color images
# ============================================================================================================
#create list of paths to use
imagePaths <- list()
is_beeVector <- list()
is_waspVector <- list()

#Loop to filter images of bees and wasps - Version with only n pictures of bees and wasps each
n = 10
nbee = 1
nwasp = 1
for(i in c(1:nrow(BeeWaspData))){
  if (BeeWaspData$is_final_validation[[i]] == 0) {
    if(BeeWaspData$is_bee[[i]] == 1 && nbee <= n){
      imagePaths[2*nbee - 1] <- paste(path, "\\", BeeWaspData$path[[i]], sep = "")
      is_beeVector[2*nbee - 1] <- BeeWaspData$is_bee[[i]]
      is_waspVector[2*nbee - 1] <- BeeWaspData$is_wasp[[i]]
      nbee = nbee + 1
    }
    else if(BeeWaspData$is_wasp[[i]] == 1 && nwasp <= n){
      imagePaths[2*nwasp] <- paste(path, "\\", BeeWaspData$path[[i]], sep = "")
      is_beeVector[2*nwasp] <- BeeWaspData$is_bee[[i]]
      is_waspVector[2*nwasp] <- BeeWaspData$is_wasp[[i]]
      nwasp = nwasp + 1
    }
  }
}

#find test images----------------------------------------------------------

#Read the images from the image paths
nImages = length(imagePaths)

myIms <- list()
colorIms <- readImages(imagePaths, myIms, nImages)

# check to see if it read in properly -- change colorIms[[value]]
plot(colorIms[[2]])
plot(as.cimg(colorIms[[2]][,,1])) # check color channel information

# ============================================================================================================
# Resize and Reshape
# ============================================================================================================
#Pre allocate dataframe to put following vectors in
imageData <- data.frame(images = rep(NA,nImages),imageVectors = rep(NA,nImages))

# Reshape each image into a vector
#for (i in c(1:nImages)){imageData$imageVectors[[i]] <- array_reshape(colorIms[[i]], c(256, 256, 3))}

for (i in c(1:nImages)){colorIms[[i]] <- array_reshape(colorIms[[i]], c(256, 256, 3))}

trainVars <- NULL
for (i in c(1:nImages)) {trainVars <- rbind(trainVars, colorIms[[i]])}

#version that preallocates above vector to make more efficient
trainVars <- matrix(data = NA, nrow = nImages, ncol = 196608, byrow = FALSE, dimnames = NULL)
for (i in c(1:nImages)) {trainVars[[i]] <- rbind(colorIms[[i]])}

trainCat <- to_categorical(is_beeVector)

# ============================================================================================================
# Create Model
# ============================================================================================================
model <- keras_model_sequential()
model %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(196608)) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 2, activation = 'softmax')

summary(model)

model %>%
  compile(loss = 'binary_crossentropy',
          optimizer = optimizer_rmsprop(),
          metrics = c('accuracy'))

history <- model %>%
  fit(trainVars,
      trainCat,
      epochs = 30,
      batch_size = 100,
      validation_split = 0.2)

model %>% evaluate(trainVars, trainCat)
pred <- model %>% predict_classes(trainVars)
