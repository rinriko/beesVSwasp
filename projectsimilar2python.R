install.packages("superml")
install.packages("imager")

#importing the dataset: enter the csv path of the dataset beesVswasp
data<-read.csv("E:/Texas Tech/Fall 2020/R Programing CS5311/Project/kaggle_bee_vs_wasp/labels.csv")

#Checking dataset info
str(data)

#importing Machine learing package
library(superml)
lbl = LabelEncoder$new()
#lbl$fit(data$label)

#Classifying the images with the help of their features values

#Extracting valid images rows and cols from main dataset
valid_img <-data[is_validation=='1']

#Extracting final valid images rows and cols from main dataset
final_valid <- data[is_final_validation=='1']

#extracting images for training from main dataset
train_img <- data[is_validation=='0' & is_final_validation =='0']

#viewing first four rows of the training dataset 
w <- train_img[1:4, ]

library(imager)
dir = '/kaggle/input/bee-vs-wasp/kaggle_bee_vs_wasp'
lis <- list.files(path = dir)

#path of the above 4 training  images 
print(w$path)
lis<-list()
lis<-w$path

#loading actual images from their respective paths
path <- system.file(package="imager") %>% paste0("/extdata")
lis_img<-load.dir(path)

#ploting those 4 images that we extract from training images

for(i in 1:length(lis_img))
  {
  print(lis_img[[i]])
  plot(lis_img[[i]])
  }

#########################################################
#rest of the code need to follow from the shared link
