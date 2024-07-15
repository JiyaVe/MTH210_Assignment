### ONLY past your predict.y function here
# x = vector of length 100
predict.y <- function(x)
{
  x <- as.matrix(x)
  load("fit_params.Rdata")
  f.x <- x %*% beta.ridge
  return(f.x)
}

X <- read.csv("assign4_train.csv")
X <- X[,-1]
predict.y(X)

x <- as.matrix(data[,2:101])
x
predict.y(x)

