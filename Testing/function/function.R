# ================= function ================
compressedAndResizeImg <- function(fpath) {
  tryCatch({
    img <- readImage(fpath)
  }, error = function(e) {
    print(paste("MY_ERROR:  ", e))
    fpath <- gsub(".jpg", ".png", fpath)
    print(fpath)
    img <- readImage(fpath)
  })
  if (dim(img) == 3) {
    R <- img[, , 1]
    G <- img[, , 2]
    B <- img[, , 3]
    svdR <- svd(R)
    svdG <- svd(G)
    svdB <- svd(B)
    svdRGB <- list(svdR, svdG, svdB)
    comp <- sapply(svdRGB, function(i) {
      compressed = i$u[, 1:35] %*% diag(i$d[1:35]) %*% t(i$v[, 1:35])
    }, simplify = 'array')
  }
  else{
    svdGray <- svd(img)
    comp <- sapply(svdGray, function(i) {
      compressed = i$u[, 1:35] %*% diag(i$d[1:35]) %*% t(i$v[, 1:35])
    }, simplify = 'array')
  }
  y <- EBImage::resize(comp, w = 256, h = 256)
  z <- imageData(y)
}
resizeImg <- function(fpath) {
  tryCatch({
    img <- load.image(fpath)
    print(dim(img))
    img <- grayscale(img, method = "Luma", drop = TRUE)
    y <- imager::resize(img, 256, 256)
    print(dim(y))
    y <- array_reshape(y, c(256, 256, 1))
  }, error = function(e) {
    print(paste("MY_ERROR:  ", e))
    print(fpath)
    if(dim(img)[4]>3){
      rgbimg<-rm.alpha(img)
      print(dim(rgbimg))
      img <- grayscale(rgbimg, method = "Luma", drop = TRUE)
      y <- imager::resize(img, 256, 256)
      print(dim(y))
      y <- array_reshape(y, c(256, 256, 1))
    }else{
      gimg<-img[,,,1]
      print(dim(gimg))
      img <- grayscale(gimg, method = "Luma", drop = TRUE)
      y <- imager::resize(img, 256, 256)
      print(dim(y))
      y <- array_reshape(y, c(256, 256, 1))
    }
  })
  
  #   y <- imager::resize(img, 256, 256)
  #   print(y)
  #   y<-array_reshape(y, c(256,256,3))
  #   z <- imageData(y)
}

compressedAndResizeImgGray <- function(fpath) {
  tryCatch({
    img <- load.image(fpath)
    # img <- readImage(fpath)
  }, error = function(e) {
    print(paste("MY_ERROR:  ", e))
    fpath <- gsub(".jpg", ".png", fpath)
    print(fpath)
    img <- load.image(fpath)
    # img <- readImage(fpath)
  })
  
  img <- grayscale(img, method = "Luma", drop = TRUE)
  svdGray <- svd(img)
  comp <- sapply(svdGray, function(i) {
    compressed = i$u[, 1:35] %*% diag(i$d[1:35]) %*% t(i$v[, 1:35])
  }, simplify = 'array')
  
  y <- EBImage::resize(comp, w = 256, h = 256)
  z <- imageData(y)
}
resizeImgGray <- function(fpath) {
  tryCatch({
    img <- load.image(fpath)
    # img <- readImage(fpath)
  }, error = function(e) {
    print(paste("MY_ERROR:  ", e))
    fpath <- gsub(".jpg", ".png", fpath)
    print(fpath)
    img <- load.image(fpath)
    # img <- readImage(fpath)
  })
  
  img <- grayscale(img, method = "Luma", drop = TRUE)
  y <- EBImage::resize(img, w = 256, h = 256)
  z <- imageData(y)
}