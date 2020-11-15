install.packages("magick")
install.packages("imager")

library(magick)
#path of the images folder which we want to process
folderpath="E:/Texas Tech/Fall 2020/R Programing CS5311/Project/kaggle_bee_vs_wasp/bee1"


library(imager)
#Here we took images from bees1 folder of the dataset
bees1 <- list.files(path=folderpath, pattern = "*.jpg", full.names=TRUE)

#Checking memory limnit to process images
memory.limit()

#increasing memory limit upto 3 times, 
#otherwise it cannot load imnages pops out an error
memory.limit(15000)

#Loading 100 images to  a variable/list since whole folder takes very much time and space
for (i in 1:100){
b1dataorg <- lapply(bees1, load.image )}

#checking for the image
print(b1dataorg[1])
plot(b1dataorg[[1]])

#reducing the image size of first 100 images 
rsbees1<-list()
for (i in 1:100){rsbees1[[i]] <- resize(b1dataorg[[i]], round(256), round(256))}
print(rsbees1[1])
plot(rsbees1[[1]])

#creating temp folder to save the reduce graphic (cimage) into png format
#through which we can find out their pixel values in both rgb and b/w format

for (i in 1:100)
  {
  rsimages[i] <-tempfile(fileext=".png")
  save.image(rsbees1[[i]],rsimages[i])
  load.image(rsimages[i])
}



#finding pixel values of the images
cimg<-list()
cval<-list()
bw.val<-list()

library(raster)
cimg<-lapply(rsbees1[[i]], brick)

for (i in 1:100){
  cimg[i] <- brick(rsimages[i])
  #color values of the image
  cval[[i]] <- getValues(cimg[[i]])
  #bw values of the respective image
  bw.val[[i]] <- cval[[i]][,1]*0.21 + cval[[i]][,1]*0.72 + cval[[i]][,1]*0.07
}


