# ===== Program start =====
rm(list=ls())
library(jpeg)
source('logistic_header.R')
source('data_prep_header.R')
setwd('./images')

# ===== Data prep part =====
fs=list.files('.',pattern = '*.jpg')   #get a list of file name
plot(1:2,xlab='',ylab='',type='n', axes=FALSE)
dataList=c();
for(i in 1:length(fs)){
  j = floor((i-1)/15)
  img = readJPEG(fs[i],native=FALSE)
  gry=col2gry(img)
  lot = subImage(gry,615,310,656,268) # location of selected lot
  dataList = rbind(dataList,as.vector(lot))
  #rasterImage(lot,  (1+0.06*((i-1)%%15))  ,2-((j+1)*0.06), (1+0.06*(((i-1)%%15)+1)) ,2-(j*0.06))
}
dataList = cbind(rep(1,nrow(dataList)), dataList)     #cbind '0' bias(constant '1')
y = c(1,1,0,0,0,0,0,1,1,0,0,0,1,0,0,     #since data does not come with y value, manually set y value
      1,1,1,0,0,1,1,0,1,0,0,0,1,1,1,
      1,1,0,1,1,1,1,1,1,1,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,1,1,0,1,0,0,
      0,0,1,0,0,1,1,1,1,0,0,0,0,1,0,
      1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,
      0,0,0,0,0,1,0,0,0,1,0,1,1,1,1,
      1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,
      1,0,0,0,1,0,0,0,0,0,0,1,1,1,0,
      0,0,0,0,1,0,1,0,0,0,0,0,0,0,0)
# ===== Classifier part =====
setFold = 10
cv_result <- rep(1, setFold)
fold_index <- cut(seq(1,nrow(dataList)),breaks=setFold,labels=FALSE) #index for cross-validation

for(i in 1:setFold){
  index <- which(fold_index==i,arr.ind=TRUE)
  test_X <- dataList[index, ]
  test_y <- y[index]
  training_X <- dataList[-index, ]
  training_y <- y[-index]
  #learning setup
  initialTheta = matrix(0,ncol(dataList),1)    #set up initial theta
  iters = 300;    #set iteration number
  alpha = 0.01;   #set alpha, running rate
  
  theta = gradientDescent(training_X, training_y, initialTheta, alpha, iters);   
  prediction = sigmoid(test_X %*% theta)
  prediction[prediction <  0.5] = 0
  prediction[prediction >= 0.5] = 1
  
  acc = sum(as.vector(prediction) == test_y) / length(test_y)
  cv_result[i] = acc
}
print (paste("Accuracy : ",sum(cv_result)/length(cv_result)))
