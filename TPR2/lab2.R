library(tm)

min <- function(v){
  v.na <- v
  v.na[v==0] <- NA
  return(which.min(v.na))
}

NFA <- function(file){
  list_containers <- list()
  list_elements <- list()
  
  for(i in 1:length(file)){
    if((do.call(sum, list_elements)+file[i]) <= 100){
      list_elements <- append(list_elements, file[i]) 
    }
    else{
      list_containers <-append(list_containers, list(list_elements))
      list_elements <- NULL
      list_elements <- list()
      list_elements <- append(list_elements, file[i])
    }
  }
  save(list_containers, file = "NFA.RData")
  containers_count <- length(list_containers)
  return(containers_count)
}

FFA <- function(file){

  list_containers <- list()
  list_elements <- list()
  
  for(i in 1:length(file)){
    mark <- TRUE
    if((do.call(sum, list_elements)+file[i]) <= 100){
      list_elements <- append(list_elements, file[i]) 
    }
    else{
      list_containers <-append(list_containers, list(list_elements))
      for (j in 1:length(list_containers)){
        if((do.call(sum, list_containers[[j]])+file[i]) <= 100){
          list_containers[[j]] <- append(list_containers[[j]], file[i])
          mark <- FALSE
          break
        }
      }
      list_elements <- NULL
      list_elements <- list()
      if (mark){
        list_elements <- append(list_elements, file[i])
      }
    }
  }
  save(list_containers, file = "FFA.RData")
  containers_count <- length(list_containers)
  return(containers_count)
}

WFA <- function(file){
  list_containers <- list()
  list_elements <- list()
  
  for(i in 1:length(file)){
    h <- integer()
    mark <- TRUE
    if((do.call(sum, list_elements)+file[i]) <= 100){
      list_elements <- append(list_elements, file[i]) 
    }
    else{
      list_containers <-append(list_containers, list(list_elements))
      for (j in 1:length(list_containers)){
        h <- append(h, (100 - do.call(sum, list_containers[[j]])))
      }
      max_f <- which(h == max(h), arr.ind = TRUE)
      if((do.call(sum, list_containers[[max_f[1]]])+file[i]) <= 100){
        list_containers[[max_f[1]]] <- append(list_containers[[max_f[1]]], file[i])
        mark <- FALSE
      }
      list_elements <- NULL
      list_elements <- list()
      if (mark){
        list_elements <- append(list_elements, file[i])
      }
    }
  }
  save(list_containers, file = "WFA.RData")
  containers_count <- length(list_containers)
  return(containers_count)
}

BFA <- function(file){
  list_containers <- list()
  list_elements <- list()
  
  for(i in 1:length(file)){
    h <- integer()
    mark <- TRUE
    if((do.call(sum, list_elements)+file[i]) <= 100){
      list_elements <- append(list_elements, file[i]) 
    }
    else{
      list_containers <-append(list_containers, list(list_elements))
      for (j in 1:length(list_containers)){
        print(file[i])
        h <- append(h, (100 - do.call(sum, list_containers[[j]])))
        print(h)
      }
      print(min(h))
      if((do.call(sum, list_containers[[min(h)]])+file[i]) <= 100){
        print("Enter")
        list_containers[[min(h)]] <- append(list_containers[[min(h)]], file[i])
        mark <- FALSE
      }  
      list_elements <- NULL
      list_elements <- list()
      if (mark){
        list_elements <- append(list_elements, file[i])
      }
    }
  }
  save(list_containers, file = "BFA.RData")
  containers_count <- length(list_containers)
  return(containers_count)
}

h[length(h)]

  
file <- scan(file = "table.txt", nlines = 1, skip = 2)
NFA(file)
FFA(file)
WFA(file)
BFA(file)
load("NFA.RData")
load("FFA.RData")
load("WFA.RData")
load("BFA.RData")


