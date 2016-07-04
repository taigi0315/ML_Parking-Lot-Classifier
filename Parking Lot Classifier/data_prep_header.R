# ===== useful functions =====
col2gry <- function(img) { #720*1280 // to make it simple, we use gray scaled image
  new_img = matrix(0,nrow=dim(img)[1],ncol=dim(img)[2])
  for(i in 1:dim(img)[1]) {
    for(j in 1:dim(img)[2]){
      new_img[i,j]=sum(img[i,j,])/3
    }
  }
  return (new_img)
}

subImage<-function(img,x1,y1,x2,y2) {
  return (img[y2:y1,x1:x2])
}
