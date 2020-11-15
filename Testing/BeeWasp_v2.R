# This program will take a set of photos including bees, wasps, other and determine if the
# photo is a bee, wasp, or other. This program will use SVD to compress the images and LDA to 
# classify the images
# By: Victoria Obrien

# 11/11/2020: version 1: inputs color images, separates color channels, performs image compression via svd
# 11/13/2020: version 2: if an image is not rgb - converts to 3 channel rgb, resizes all images to 250x250,
#                       saves compressed/resized images into respective file folders, converts all images to
#                       grayscale and saves them to a file folder

###############################################################################################
# manual functions
###############################################################################################


# function to get path to csv file from the user
get_path <- function(){
  path <- readline(prompt = "Enter the path to the containing .csv file: ")
  return(path)
}

# function to get path to specific insect folder
get_fpath <- function(){
  folderPath <- readline(prompt = "Enter the path to the folder with your desired set of photos: ")
  return(folderPath)
}

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
# get path to image folder from user
folderPath <- get_fpath()
# read in csv file
myData <- read.csv(paste(path, "\\", list.files(path, pattern = ".csv"), sep = ""))
BeeWaspData <- data.frame(myData)
#fullImPath <- paste(folderPath, "\\", BeeWaspData$path, sep = "")
fullImPath <- paste(folderPath, "\\", list.files(folderPath, pattern = ".jpg"), sep = "")
# number of images we want to read in --> used 100 for testing purposes
nImages = as.integer(length(list.files(folderPath, pattern = ".jpg")))

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

# remove color image list to clear up some memory
rm(colorIms)

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

# remove R, G, B channels to clear up some more memory
rm(R)
rm(G)
rm(B)

# compresses image, results in rank of 35, heavily based on 
# https://rpubs.com/himank369123/476208
compIm <- list()
for(k in c(1:nImages)){
  compIm[[k]] <- sapply(svdRGB[[k]], function(i){
    compressed = i$u[,1:35] %*% diag(i$d[1:35]) %*% t(i$v[,1:35])
  }, simplify = 'array')
  compIm[[k]] <- as.cimg(compIm[[k]])
}

# test to see the compressed image
plot(compIm[[2]])

# remove svd lists to clear up more memory
rm(svdB)
rm(svdG)
rm(svdR)
rm(svdRGB)

###############################################################################################
# save pre-processed images to folders
###############################################################################################
# paths to each compressed folder
compressedPaths <- list("C:\\Users\\vicky\\Documents\\BeesVsWasps\\compressed\\c_bee1",
                        "C:\\Users\\vicky\\Documents\\BeesVsWasps\\compressed\\c_bee2",
                        "C:\\Users\\vicky\\Documents\\BeesVsWasps\\compressed\\c_other_insect",
                        "C:\\Users\\vicky\\Documents\\BeesVsWasps\\compressed\\c_other_noinsect",
                        "C:\\Users\\vicky\\Documents\\BeesVsWasps\\compressed\\c_wasp1",
                        "C:\\Users\\vicky\\Documents\\BeesVsWasps\\compressed\\c_wasp2")

# save compressed images to the compressed folder
# change compressedPaths[[n]] to select a folder
for (l in c(1:nImages)){
  save.image(compIm[[l]], paste(compressedPaths[[6]], "\\", "c_", basename(fullImPath[l]), sep =""))
}

# paths to each compressed grayscale folder
compressedGrayPaths <- list("C:\\Users\\vicky\\Documents\\BeesVsWasps\\compressed_grey\\gc_bee1",
                            "C:\\Users\\vicky\\Documents\\BeesVsWasps\\compressed_grey\\gc_bee2",
                            "C:\\Users\\vicky\\Documents\\BeesVsWasps\\compressed_grey\\gc_other_insect",
                            "C:\\Users\\vicky\\Documents\\BeesVsWasps\\compressed_grey\\gc_other_noinsect",
                            "C:\\Users\\vicky\\Documents\\BeesVsWasps\\compressed_grey\\gc_wasp1",
                            "C:\\Users\\vicky\\Documents\\BeesVsWasps\\compressed_grey\\gc_wasp2")

# save compressed gray images to the compressed gray folder
# change compressedPaths[[n]] to select a folder
for (m in c(1:nImages)){
  save.image(grayscale(compIm[[m]], method = "Luma", drop = TRUE), paste(compressedGrayPaths[[6]], "\\", "gc_", basename(fullImPath[m]), sep =""))
}
