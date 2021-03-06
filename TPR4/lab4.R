
Borde <- function(data){
  voters <- colnames(data)        
  voters <- as.integer(voters)
  
  data$weight <- ""              
  
  weigth <- 0
  
  for(i in nrow(data):1){       
    data$weight[i] <- weigth
    weigth <- weigth + 1
  }
  
  data$weight <- as.integer(data$weight)
  lst <- list()
  candidate <- sort(unique(data[,1]))   
  for(i in 1:length(candidate)){         
    for(j in 1:(ncol(data)-1)){         
      lst[[candidate[i]]] <- append(lst[[candidate[i]]], (data$weight[which(data[,j]==candidate[i])])) 
    
    }
  }
  
  lst <- lapply(lst, "*", voters) 
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
        if(first_candidate > second_candidate) 
        {
          lst_complend[[candidate[i]]] <- lst_complend[[candidate[i]]] + 1  
          lst_complend[[candidate[j]]] <- lst_complend[[candidate[j]]] - 1
        }
        else {
          lst_complend[[candidate[j]]] <- lst_complend[[candidate[j]]] + 1
          lst_complend[[candidate[i]]] <- lst_complend[[candidate[i]]] - 1
        }
        print(paste(candidate[i], candidate[j], sep = ":"))
        print(c(first_candidate,second_candidate))

      }
    }
  }
  print(lst_complend)
  
  lst_complend <- lst_complend[order(-unlist(lst_complend))]
  lst_complend <- as.matrix(lst_complend)
  
  return(t(lst_complend))
}

Parallel <- function(data) {
  voters <- colnames(data)
  voters <- as.integer(voters)
  
  lst_parallel <- list()
  candidate <- sort(unique(data[,1]))
  
  while(length(candidate)!=1){
    for(i in 1:length(candidate)){
      for(j in 1:length(candidate)){
        first_candidate <- 0
        second_candidate <- 0
        if(i != j & i < j){
          for(k in 1:ncol(data)){
            if(which(data[,k]==candidate[i]) < which(data[,k]==candidate[j])) first_candidate <- first_candidate+voters[k]
            else second_candidate <- second_candidate+voters[k]
          }
          if(first_candidate > second_candidate){
            lst_parallel <- append(lst_parallel,list(paste(first_candidate,second_candidate, sep = ":"), paste(candidate[i],candidate[j], sep = ">")))
            data <- as.data.frame(sapply(data, function(x) x[x != candidate[j]]))
            candidate <- candidate[ candidate != candidate[j]]
            
          }
          else{
            lst_parallel <- append(lst_parallel,list(paste(first_candidate,second_candidate, sep = ":"), paste(candidate[i],candidate[j], sep = "<")))
            data <- as.data.frame(sapply(data, function(x) x[x != candidate[i]]))
            candidate <- candidate[ candidate != candidate[i]]
          }
          break
        }
      }
    }
  }
  return(c(lst_parallel, paste("Winner:", candidate, sep = " ")))
}

data <- read.table("table.txt", header = TRUE, sep = " ", check.names = FALSE, stringsAsFactors = FALSE)
Borde(data)
Compl(data)
Parallel(data)
