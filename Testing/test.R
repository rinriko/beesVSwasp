library(EBImage)
library(keras)
library(OpenImageR)
library(imager)
library(magick)
path = "/Users/minmin/TTU/Programming with R for Data Analysis and Machine Learning/Project/beesVSwasp/Dataset/kaggle_bee_vs_wasp"
# path = "Users\\minmin\\TTU\\Programming with R for Data Analysis and Machine Learning\\Project\\beesVSwasp\\Dataset\\kaggle_bee_vs_wasp"
# df <- read.csv(,stringsAsFactors = FALSE)

file.path("/home/user/", "project")

df <-
  read.csv(paste(
    path,
    .Platform$file.sep,
    list.files(path, pattern = ".csv",  all.files = FALSE),
    sep = ""
  ))
BeeWaspData <- data.frame(df)
BeeWaspData$label
fullImPath <-
  paste(path, .Platform$file.sep, BeeWaspData$path, sep = "")
# fullImPath <- system.file(path, .Platform$file.sep, BeeWaspData$path, package = "OpenImageR")
fullImPath
fullImPath <- gsub("\\\\", .Platform$file.sep, fullImPath)
fullImPath

# ==================================================================================================================================
img <- readImage(fullImPath[2])
? apply

imgx <- lapply(fullImPath, function(fpath) {
  # x <- system.file(fpath, package="EBImage")
  print(fpath)
  tryCatch({
    img <- readImage(fpath)
  }, error = function(e) {
    print(paste("MY_ERROR:  ",e))
    fpath <- gsub(".jpg", ".png", fpath)
    print(fpath)
    img <- readImage(fpath)
  })
    # R <- img[,,1]
    # G <- img[,,2]
    # B <- img[,,3]
    # svdR<-svd(R)
    # svdG<-svd(G)
    # svdB<-svd(B)
    # svdRGB<-list(svdR,svdG,svdB)
    # comp <- sapply(svdRGB, function(i){
    #   compressed = i$u[,1:35] %*% diag(i$d[1:35]) %*% t(i$v[,1:35])
    # }, simplify = 'array')
    # print(dim(comp))
    # comp <- as.cimg(comp)
  y <- EBImage::resize(img, w = 256, h = 256)
  # y <- EBImage::resize(comp, w = 128, h = 128)
  
  z <- imageData(y)
})
  
  
  
  
  # img <- lapply(fullImPath,function(x){
  #   img <- readImage(x)
  #   R <- img[,,1]
  #   G <- img[,,2]
  #   B <- img[,,3]
  #   svdR<-svd(R)
  #   svdG<-svd(G)
  #   svdB<-svd(B)
  #   svdRGB<-list(svdR,svdG,svdB)
  #   comp <- sapply(svdRGB, function(i){
  #     compressed = i$u[,1:35] %*% diag(i$d[1:35]) %*% t(i$v[,1:35])
  #   }, simplify = 'array')
  #   comp <- as.cimg(comp)
  # })
  
  # 
  # 
  # dim(img)
  # 
  # 
  # # ======================
  # ? resize
  # # img<- resize(img,128,128)
  # imageShow(img)
  # ?  ? readImage
  # 
  # 
  # # }
  # R <- img[, , 1]
  # G <- img[, , 1]
  # B <- img[, , 1]
  # 
  # # test to see individual RGB components
  # plot(as.cimg(B))
  # 
  # # perform SVD on each color channel
  # svdR <- svd(R)
  # svdG <- svd(G)
  # svdB <- svd(B)
  # svdRGB <- list(svdR, svdG, svdB)
  # 
  # comp <- sapply(svdRGB, function(i) {
  #   compressed = i$u[, 1:35] %*% diag(i$d[1:35]) %*% t(i$v[, 1:35])
  # }, simplify = 'array')
  # dim(comp)
  # imageShow(comp)
  # comp <- as.cimg(comp)
  # plot(comp)
  