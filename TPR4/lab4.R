
data <- read.table("table.txt", header = TRUE, sep = " ", check.names = FALSE, stringsAsFactors = FALSE)


v <- colnames(data)
v <- as.integer(v)

data$S <- ""

weigth <- 0
for(i in nrow(data):1){
  data$S[i] <- weigth
  weigth <- weigth + 1
}
data$S <- as.integer(data$S)
na <- NULL
nb <- NULL
nc <- NULL
nd <- NULL
for(j in 1:(ncol(data)-1)){
    na <- append(na, (data$S[which(data[,j]=="a")]))
    nb <- append(nb, (data$S[which(data[,j]=="b")]))
    nc <- append(nc, (data$S[which(data[,j]=="c")]))
    nd <- append(nd, (data$S[which(data[,j]=="d")]))
}
na <- na*v
na <- sum(na)
nb <- nb*v
nb <- sum(na)
nc <- nc*v
nc <- sum(nc)
nd <- nd*v
nd <- sum(nd)

summ <- function(x){
  x <- x*v
  x <- sum(x)
}
