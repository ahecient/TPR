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

Compl <- function(data){
  voters <- colnames(data)
  voters <- as.integer(voters)
  lst_complend <- list()
  candidate <- sort(unique(data[,1]))
  for(i in 1:length(candidate)){
    nul <- 0
    lst_complend[[candidate[i]]] <- nul
  }
  
  for(i in 1:length(candidate)){
    for(j in 1:length(candidate)){
      first_candidate <- 0
      second_candidate <- 0
      if(i != j & i < j){
        for(k in 1:ncol(data)){
          if(which(data[,k]==candidate[i]) < which(data[,k]==candidate[j])) first_candidate <- first_candidate+voters[k]
          else second_candidate <- second_candidate+voters[k]
        }
        if(first_candidate > second_candidate) lst_complend[[candidate[i]]] <- lst_complend[[candidate[i]]] + 1 
        else lst_complend[[candidate[j]]] <- lst_complend[[candidate[j]]] - 1
        #print(paste(string[i], string[j], sep = ":"))
        #print(c(y,x))
      }
    }
  }
  
  lst_complend <- lst_complend[order(-unlist(lst_complend))]
  lst_complend <- as.matrix(lst_complend)
  
  return(t(lst_complend))
}

data <- read.table("table.txt", header = TRUE, sep = " ", check.names = FALSE, stringsAsFactors = FALSE)
Borde(data)
Compl(data)


