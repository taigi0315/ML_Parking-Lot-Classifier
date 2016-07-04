sigmoid <- function(z){
  z = exp(-z);
  g = 1./(1 + z);
  return (g)
}

computeCost <- function(x, y, theta){
  m = nrow(x);
  h = sigmoid(x %*% theta);
  left = t(-y) %*% log(h);
  right = t(1-y) %*% log(1-h);
  J = (1/m) * (left - right);
  return (J)
}

gradientDescent <- function(x, y, initialTheta, alpha, num_iters){
  m = nrow(x)
  J_history = matrix(0,nrow = num_iters, ncol = 1);
  theta = initialTheta;
  
  for (iter in 1:num_iters){
    h = sigmoid(x %*% theta);    #get hyphosesis with this theta
    error = h-y;
    thetaChange = (alpha * (1/m)) * (t(x) %*% error);  
    # vectorized, same with : 1/m * SUM(error * x)
    theta = theta - thetaChange;    #theta, grad
    J_history[iter] = computeCost(x, y, theta);
  }
  
  print(paste0("J history : ", J_history));
  return (theta);
}