library(tm)
library(sjmisc)

min <- function(v){
  v[length(v)] <- 0
  v.na <- v
  v.na[v==0] <- NA
  return(which.min(v.na))
}

min_count_containers <- function(file){
  M <- ceiling(sum(file)/100)
  return(M)
}

NFA <- function(file){
  count <- 0
  list_containers <- list()
  list_elements <- list()
  
  for(i in 1:length(file)){
    count <- count + 1
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
  save(list_containers, list_elements, file = "NFA.RData")
  containers_count <- length(list_containers)
  return(c(containers_count, count))
}

FFA <- function(file){
  count <- 0
  list_containers <- list()
  list_elements <- list()
  
  for(i in 1:length(file)){
    mark <- TRUE
    count <- count + 1
    if((do.call(sum, list_elements)+file[i]) <= 100){
      list_elements <- append(list_elements, file[i]) 
    }
    else{
      list_containers <-append(list_containers, list(list_elements))
      for (j in length(list_containers):1){
        count <- count + 1
        if((do.call(sum, list_containers[[j]])+file[i]) <= 100){
          list_containers[[j]] <- append(list_containers[[j]], file[i])
          mark <- FALSE
          break
        }
      }
      list_elements <- NULL
      list_elements <- list()
      #count <- count + 1
      if (mark){
        list_elements <- append(list_elements, file[i])
      }
    }
  }
  save(list_containers, file = "FFA.RData")
  containers_count <- length(list_containers)
  return(c(containers_count, count))
}

WFA <- function(file){
  count <- 0
  list_containers <- list()
  list_elements <- list()
  
  for(i in 1:length(file)){
    h <- integer()
    mark <- TRUE
    count <- count +1
    if((do.call(sum, list_elements)+file[i]) <= 100){
      list_elements <- append(list_elements, file[i]) 
    }
    else{
      list_containers <-append(list_containers, list(list_elements))
      for (j in 1:length(list_containers)){
        count <- count + 1
        h <- append(h, (100 - do.call(sum, list_containers[[j]])))
      }
      max_f <- which(h == max(h), arr.ind = TRUE)
      count <- count + 1
      if((do.call(sum, list_containers[[max_f[1]]])+file[i]) <= 100){
        list_containers[[max_f[1]]] <- append(list_containers[[max_f[1]]], file[i])
        mark <- FALSE
      }
      list_elements <- NULL
      list_elements <- list()
      #count <- count +1
      if (mark){
        list_elements <- append(list_elements, file[i])
      }
    }
  }
  save(list_containers, file = "WFA.RData")
  containers_count <- length(list_containers)
  return(c(containers_count, count))
}

BFA <- function(file){
  count <- 0
  list_containers <- list()
  list_elements <- list()
  
  for(i in 1:length(file)){
    h <- integer()
    mark <- TRUE
    count <- count + 1
    if((do.call(sum, list_elements)+file[i]) <= 100){
      list_elements <- append(list_elements, file[i]) 
    }
    else{
      list_containers <-append(list_containers, list(list_elements))
      for (j in 1:length(list_containers)){
        count <- count + 1
        h <- append(h, (100 - do.call(sum, list_containers[[j]])))
      }
      if(!is_empty(min(h))){
        count <- count + 1
        if((do.call(sum, list_containers[[min(h)]])+file[i]) <= 100){
          list_containers[[min(h)]] <- append(list_containers[[min(h)]], file[i])
          mark <- FALSE
        }  
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
  return(c(containers_count, count))
}
  
file <- scan(file = "table.txt", nlines = 1)
min_count_containers(file)
NFA(file)
FFA(file)
WFA(file)
BFA(file)
load("NFA.RData")
load("FFA.RData")
load("WFA.RData")
load("BFA.RData")


