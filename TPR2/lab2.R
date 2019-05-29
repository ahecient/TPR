library(tm)
library(sjmisc)

min_count_containers <- function(file){
  M <- ceiling(sum(file)/100)
  return(M)
}

check_last_cont <- function(list){
  if(!is_empty(list)) return (1)
  else return (0)
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
  NFA_count <- length(list_containers)+check_last_cont(list_elements)
  NFA_complexity <- count
  save(list_containers,NFA_count,NFA_complexity, file = "NFA.RData")
  return(c(NFA_count, NFA_complexity))
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
      if (mark){
        list_elements <- append(list_elements, file[i])
      }
    }
  }
  FFA_count <- length(list_containers)+check_last_cont(list_elements)
  FFA_complexity <- count
  save(list_containers,FFA_count, FFA_complexity, file = "FFA.RData")
  return(c(FFA_count, FFA_complexity))
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
      if (mark){
        list_elements <- append(list_elements, file[i])
      }
    }
  }
  WFA_count <- length(list_containers)+check_last_cont(list_elements)
  WFA_complexity <- count
  save(list_containers,WFA_count, WFA_complexity, file = "WFA.RData")
  return(c(WFA_count, WFA_complexity))
}

BFA <- function(file){
  list_containers <- list()
  list_elements <- list()
  count <- 0
  
  for(i in 1:length(file)){
    h <- integer()
    mark <- TRUE
    count <- count + 1
    if((do.call(sum, list_elements)+file[i]) <= 100){
      list_elements <- append(list_elements, file[i]) 
    }
    else{
      index_min <- 0
      min_f <- 100
      list_containers <- append(list_containers, list(list_elements))
      for(j in 1:length(list_containers)){
        h <- append(h, (100 - do.call(sum, list_containers[[j]])))
        count <- count + 1
        if(h[length(h)] >= file[i]){
          count <- count + 1
          if(h[length(h)] <= min_f){
            min_f <- h[length(h)]
            index_min <- length(h)
          }
        }
      }
      if(index_min!=0){
        list_containers[[index_min]] <- append(list_containers[[index_min]], file[i])
        mark <- FALSE
      }
      list_elements <- NULL
      list_elements <- list()
      if (mark){
        list_elements <- append(list_elements, file[i])
      }
    }
  }
  BFA_count <- length(list_containers)+check_last_cont(list_elements)
  BFA_complexity <- count
  save(list_containers, BFA_count, BFA_complexity, file = "BFA.RData")
  return(c(BFA_count, BFA_complexity))
}







result_desc <- data.frame(data = character(), NFA_count = integer(), 
                     FFA_count = integer(), WFA_count = integer(), BFA = integer(),
                     NFA_complexity = integer(), FFA_complexity = integer(),
                     WFA_complexity = integer(), BFA_complexity = integer())

file <- scan(file = "table.txt", nlines = 1)
file <- sort(file, decreasing = TRUE)
min_count_containers(file)
NFA(file)
FFA(file)
WFA(file)
BFA(file)
load("NFA.RData")
load("FFA.RData")
load("WFA.RData")
load("BFA.RData")
result_desc <- rbind(result_desc, data.frame(data = "1 рядок", NFA_count = NFA_count, FFA_count = FFA_count,
                                   WFA_count = WFA_count, BFA_count = BFA_count,
                                   NFA_complexity = NFA_complexity, FFA_complexity = FFA_complexity,
                                   WFA_complexity = WFA_complexity, BFA_complexity = BFA_complexity))

file <- scan(file = "table.txt", nlines = 1, skip = 1)
file <- sort(file, decreasing = TRUE)
min_count_containers(file)
NFA(file)
FFA(file)
WFA(file)
BFA(file)
load("NFA.RData")
load("FFA.RData")
load("WFA.RData")
load("BFA.RData")
result_desc <- rbind(result_desc, data.frame(data = "2 рядок", NFA_count = NFA_count, FFA_count = FFA_count,
                                   WFA_count = WFA_count, BFA_count = BFA_count,
                                   NFA_complexity = NFA_complexity, FFA_complexity = FFA_complexity,
                                   WFA_complexity = WFA_complexity, BFA_complexity = BFA_complexity))

file <- scan(file = "table.txt", nlines = 1, skip = 2)
file <- sort(file,decreasing = TRUE)
min_count_containers(file)
NFA(file)
FFA(file)
WFA(file)
BFA(file)
load("NFA.RData")
load("FFA.RData")
load("WFA.RData")
load("BFA.RData")
result_desc <- rbind(result_desc, data.frame(data = "3 рядок", NFA_count = NFA_count, FFA_count = FFA_count,
                                   WFA_count = WFA_count, BFA_count = BFA_count,
                                   NFA_complexity = NFA_complexity, FFA_complexity = FFA_complexity,
                                   WFA_complexity = WFA_complexity, BFA_complexity = BFA_complexity))

file <- scan(file = "table.txt", nlines = 3)
file <- sort(file, decreasing = TRUE)
min_count_containers(file)
NFA(file)
FFA(file)
WFA(file)
BFA(file)
load("NFA.RData")
load("FFA.RData")
load("WFA.RData")
load("BFA.RData")
result_desc <- rbind(result_desc, data.frame(data = "Всі рядки", NFA_count = NFA_count, FFA_count = FFA_count,
                                   WFA_count = WFA_count, BFA_count = BFA_count,
                                   NFA_complexity = NFA_complexity, FFA_complexity = FFA_complexity,
                                   WFA_complexity = WFA_complexity, BFA_complexity = BFA_complexity))

