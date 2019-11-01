cor_pearson_spearman <- function(data, method = 'pearson'){
  if(method == 'spearman'){
    data <- data.frame(rank(data[,1]),rank(data[,2]))
  }
  cov <- sum((data[,1] - mean(data[,1]))*(data[,2] - mean(data[,2]))) / (nrow(data) - 1)
  cor <- cov / ((sd(data[,1]))*sd(data[,2]))
  return(cor)
}

# Проверка
vector1 <- rnorm(30)
vector2 <- rnorm(30)
data <- data.frame(vector1, vector2)
  
cor_pearson_spearman(data, method = 'spearman')
cor(data, method ="spearman")

cor_pearson_spearman(data)
cor(data)