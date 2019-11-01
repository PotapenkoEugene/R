cor_pearson <- function(data){
  cov <- sum((data[,1] - mean(data[,1]))*(data[,2] - mean(data[,2]))) / (nrow(data) - 1)
  cor <- cov / ((sd(data[,1]))*sd(data[,2]))
  return(cor)
  }


# проверка
vector1 <- rnorm(30)
vector2 <- rnorm(30)
data <- data.frame(vector1, vector2)

cor_pearson(data)
cor(data)
