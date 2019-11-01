cor_spearman <- function(data){
  rank_1 <- rank(data[,1])
  rank_2 <- rank(data[,2])
      cov <- sum((rank_1 - mean(rank_1))*(rank_2 - mean(rank_2))) / (nrow(data) - 1)
      cor <- cov / ((sd(rank_1))*sd(rank_2))
  return(cor)
}


# проверка 
vector1 <- rnorm(30)
vector2 <- rnorm(30)
data <- data.frame(vector1, vector2)

cor_spearman(data)
cor(data, method ="spearman")
