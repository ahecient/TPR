Borde <- function(data){
  v <- colnames(data)
  v <- as.integer(v)
  
  data$S <- ""
  
  weigth <- 0
  
  for(i in nrow(data):1){
    data$S[i] <- weigth
    weigth <- weigth + 1
  }
  
  data$S <- as.integer(data$S)
  lst <- list()
  for(j in 1:(ncol(data)-1)){
    lst[["a"]] <- append(lst[["a"]], (data$S[which(data[,j]=="a")]))
    lst[["b"]] <- append(lst[["b"]], (data$S[which(data[,j]=="b")]))
    lst[["c"]] <- append(lst[["c"]], (data$S[which(data[,j]=="c")]))
    lst[["d"]] <- append(lst[["d"]], (data$S[which(data[,j]=="d")]))
  }
  
  lst <- lapply(lst, "*", v)
  lst <- lapply(lst, function(x) sum(x))
  lst <- lst[order(-unlist(lst))]
  lst <- as.matrix(lst)
  
  return(t(lst))
}


data <- read.table("table.txt", header = TRUE, sep = " ", check.names = FALSE, stringsAsFactors = FALSE)
Borde(data)

v <- colnames(data)
v <- as.integer(v)
lst <- list("a" = 0, "b" = 0, "c" = 0, "d" = 0)
for(j in 1:ncol(data)){                               #< - win
  
  if(which(data[,j]=="a")<which(data[,j]=="b")) lst[["a"]] <- lst[["a"]]+1
  else lst[["b"]] <- lst[["b"]]-1
  
  if(which(data[,j]=="a")<which(data[,j]=="c")) lst[["a"]] <- lst[["a"]]+1
  else lst[["c"]] <- lst[["c"]]-1
  
  if(which(data[,j]=="a")<which(data[,j]=="d")) lst[["a"]] <- lst[["a"]]+1
  else lst[["d"]] <- lst[["d"]]-1
  
  if(which(data[,j]=="b")<which(data[,j]=="d")) lst[["b"]] <- lst[["b"]]+1
  else lst[["d"]] <- lst[["d"]]-1
  
  if(which(data[,j]=="b")<which(data[,j]=="c")) lst[["b"]] <- lst[["b"]]+1
  else lst[["c"]] <- lst[["c"]]-1
  
  if(which(data[,j]=="c")<which(data[,j]=="c")) lst[["c"]] <- lst[["c"]]+1
  else lst[["d"]] <- lst[["d"]]-1
}

