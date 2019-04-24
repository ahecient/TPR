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
  
  return(names(lst))
}


table <- read.table("table.txt", header = TRUE, sep = " ", check.names = FALSE, stringsAsFactors = FALSE)
Borde(table)

