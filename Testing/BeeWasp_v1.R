# This program will take a set of photos including bees, wasps, other and determine if the
# photo is a bee, wasp, or other. This program will use SVD to compress the images and LDA to 
# classify the images
# By: Victoria Obrien

# 11/11/2020: version 1: inputs color images, separates color channels, performs image compression

###############################################################################################
# manual functions
###############################################################################################


# function to get path from the user
get_path <- function(){
  path <- readline(prompt = "Enter the path to the folder with your photos: ")
  return(path)
}

# reads in images and converts them to cimg (useable format)
readImages <- function(fullImPath, myIms, nImages){
  x <- c(1:nImages)
  for (val in x){
    myIms[[val]] <- magick2cimg(image_read(fullImPath[[val]]))
  }
  return(myIms)
}


###############################################################################################
# libraries
###############################################################################################
library(imager)
library(gsubfn)
library(magick)
library(geometry)


###############################################################################################
# read in the .csv containing all the images info and class labels
###############################################################################################
# get path to .csv file from user
path <- get_path()
# read in csv file
myData <- read.csv(paste(path, "\\", list.files(path, pattern = ".csv"), sep = ""))
BeeWaspData <- data.frame(myData)
fullImPath <- paste(path, "\\", BeeWaspData$path, sep = "")
# number of images we want to read in --> used 100 for testing purposes
nImages = as.integer(100)

###############################################################################################
# read in color images
###############################################################################################
myIms <- list()
colorIms <- readImages(fullImPath, myIms, nImages)

# check to see if it read in properly -- change colorIms[[value]]
plot(colorIms[[2]])
plot(as.cimg(colorIms[[2]][,,1])) # check color channel information

###############################################################################################
# compress images using SVD
###############################################################################################
# separate images into RGB components
R <- list()
G <- list()
B <- list()

for (i in c(1:nImages)){
  R[[i]] <- colorIms[[i]][,,1]
  G[[i]] <- colorIms[[i]][,,2]
  B[[i]] <- colorIms[[i]][,,3]
}

# test to see individual RGB components
plot(as.cimg(B[[2]]))

# perform SVD on each color channel
svdR<-list()
svdG<-list()
svdB<-list()
svdRGB<-list(svdR,svdG,svdB)

for (j in c(1:nImages)){
  svdR[[j]]<-svd(R[[j]])
  svdG[[j]]<-svd(G[[j]])
  svdB[[j]]<-svd(B[[j]])
  svdRGB[[j]]<-list(svdR[[j]],svdG[[j]],svdB[[j]])
}

# compresses image, results in rank of 35, heavily based on 
# https://rpubs.com/himank369123/476208
comp <- list()
for(k in c(1:nImages)){
    comp[[k]] <- sapply(svdRGB[[k]], function(i){
      compressed = i$u[,1:35] %*% diag(i$d[1:35]) %*% t(i$v[,1:35])
    }, simplify = 'array')
    comp[[k]] <- as.cimg(comp[[k]])
}

# compare the compressed image with the input image
par(mfrow=c(1,2))
plot(comp[[2]],axes=FALSE,main=paste("Compressed Image of Rank ", 35))
plot(colorIms[[2]],axes=FALSE,main="Original Image")

