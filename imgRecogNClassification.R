install.packages('Keras')
install.packages("imager")

library(EBImage)
library(imager)
library(keras)


#Enter you path of the dataset bnpics
folderpath="C:/Users/abdul/OneDrive/Desktop/Hilal/bnwpics"
pics <- list.files(path=folderpath, pattern = "*.jpg", full.names=TRUE)

mypic <- list()
for (i in 1:length(pics)) {mypic[[i]] <- readImage(pics[i])}

#Testing the data which we imported
print(mypic[[1]])
EBImage::display(mypic[[1]])
summary(mypic[[1]])
hist(mypic[[1]])
hist(mypic[[3]])
str(mypic)

#Resizing
rspics<-list()
for (i in 1:length(pics)) {rspics[[i]] <- EBImage::resize(mypic[[i]], 28, 28)}
str(rspics)

#Reshaping
rshapedpic<-list()
for (i in 1:length(pics)) {rshapedpic[[i]] <- array_reshape(rspics[[i]], c(28, 28, 3))}

# Row Bind

#Traing X
trainx <- NULL
#Adding first 80 images of bees in trainx
for (i in 1:80) {trainx <- rbind(trainx, rshapedpic[[i]])}
str(trainx)

#adding next 80 images of wasp in trainx
for (i in 101:180) {trainx <- rbind(trainx, rshapedpic[[i]])}
str(trainx)

#Testing x
testx<-NULL
#Adding next 20 images of bees in trainx
for (i in 81:100) {testx <- rbind(testx, rshapedpic[[i]])}
str(testx)
#Adding next 20 images of wasp in trainx
for (i in 181:200) {testx <- rbind(testx, rshapedpic[[i]])}
str(testx)

#Bees='0' and Wasp='1'

#Training y
#Assigning 80 times 0 to first and 80 times 1 for training y
trainy <- c(replicate(80, 0),replicate(80, 1))

#testing y
#Assigning 20 times 0 to first and 20 times 1 for training y
testy <- c(replicate(20, 0),replicate(20, 1))


#One Hot Encoding

library(tensorflow)
# After this Use Command "install_tensorflow()"

trainLabels<-NULL
trainLabels <- to_categorical(trainy)

testLabels <- NULL
testLabels <- to_categorical(testy)

# Model
#Establishing the neural net with different layers as layer_dense and uses 'relu'and 'soft' activation function
model <- keras_model_sequential()
model %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(2352)) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 2, activation = 'softmax')
summary(model)
 

# Compile
model %>%
  compile(loss = 'binary_crossentropy',
          optimizer = optimizer_rmsprop(),
          metrics = c('accuracy'))


# Fit Model
history <- model %>%
  fit(trainx,
      trainLabels,
      epochs = 30,
      batch_size = 32,
      validation_split = 0.2)

plot(history)

# Evaluation & Prediction - train data
model %>% evaluate(trainx, trainLabels)

pred <- model %>% predict_classes(trainx)
print(pred)

#Confusion matrix
table(Predicted = pred, Actual = trainy)

#probability values of the classes
prob <- model %>% predict_proba(trainx)
print(prob)

cbind(prob, Prected = pred, Actual= trainy)

#checking: for the image 
EBImage::display(mypic[[200]])
