library(rowr)
library(taRifx)


ClothesTable <- read.table("Clothes.txt", stringsAsFactors = FALSE, sep = "\t", dec = ".", header = TRUE, check.names = FALSE)
SeasonsTable <- read.table("Seasons.txt", stringsAsFactors = FALSE, sep = "\t", header = TRUE, check.names = FALSE)
SetTable <- read.table("Set.txt", stringsAsFactors = FALSE, sep = "\t", header = TRUE, check.names = FALSE, dec = ".")

Cost1kg <- 10



mapping_fun <- function(x) {
  vec <- SetTable$T
  vec_range <- range(vec)
  if (x > vec_range[2]) {
    return(vec_range[2])
  }
  else {
    return(min(vec[vec > x]))
  }
}

SeasonsTable$mapping <- sapply(SeasonsTable$T, mapping_fun)

for(i in 1:nrow(SeasonsTable)){
  for(j in 1:nrow(SetTable)){
    if(SeasonsTable$mapping[i]==SetTable$T[j]){
      SeasonsTable$mapping[i] <- SetTable$H[j]
    }
  }
}

Strategy <- function(month, P){
  CostTable <- data.frame(stringsAsFactors = FALSE)
  
  for(i in month){
    result <- integer()
    for(j in 1:nrow(SetTable)){
      if(SeasonsTable$mapping[i]==SetTable$H[j]){
        result <- append(result, 0)
      }
      if(SeasonsTable$mapping[i]!=SetTable$H[j]){
        a <- SeasonsTable$mapping[i]
        summ <- integer()
        
        if(is.na(SetTable$Headdress[j]) & !is.na(SetTable$Headdress[a])){
          summ <- append(summ, ClothesTable$Cost[which(grepl(SetTable$Headdress[a], ClothesTable$Clothes))])
        }
        else{
          if(!is.na(SetTable$Headdress[j]) & !is.na(SetTable$Headdress[a]) & SetTable$Headdress[j]!=SetTable$Headdress[a]){
            summ <- append(summ, ClothesTable$Cost[which(grepl(SetTable$Headdress[a], ClothesTable$Clothes))])
          } 
        }
        
        
        if(is.na(SetTable$Outerwear[j]) & !is.na(SetTable$Outerwear[a])){
          summ <- append(summ, ClothesTable$Cost[which(grepl(SetTable$Outerwear[a], ClothesTable$Clothes))])
        }
        else{
          if(!is.na(SetTable$Outerwear[j]) & !is.na(SetTable$Outerwear[a]) & SetTable$Outerwear[j]!=SetTable$Outerwear[a]) {
            summ <- append(summ, ClothesTable$Cost[which(grepl(SetTable$Outerwear[a], ClothesTable$Clothes))])
          }
        }
        
        
        if(is.na(SetTable$Gloves[j]) & !is.na(SetTable$Gloves[a])){
          summ <- append(summ, ClothesTable$Cost[which(grepl(SetTable$Gloves[a], ClothesTable$Clothes))])
        }
        else{
          if(!is.na(SetTable$Gloves[a]) & !is.na(SetTable$Gloves[j]) & SetTable$Gloves[j]!=SetTable$Gloves[a]) {
            summ <- append(summ, ClothesTable$Cost[which(grepl(SetTable$Gloves[a], ClothesTable$Clothes))])
          }
        }
        
        
        if(is.na(SetTable$Trousers[j]) & !is.na(SetTable$Trousers[a])){
          summ <- append(summ, ClothesTable$Cost[which(grepl(SetTable$Trousers[a], ClothesTable$Clothes))])
        }
        else{
          if(!is.na(SetTable$Trousers[j]) & !is.na(SetTable$Trousers[a]) & SetTable$Trousers[j]!=SetTable$Trousers[a]) {
            summ <- append(summ, ClothesTable$Cost[which(grepl(SetTable$Trousers[a], ClothesTable$Clothes))])
          }
        }
        
        
        if(is.na(SetTable$Footwear[j]) & !is.na(SetTable$Footwear[a])){
          summ <- append(summ, ClothesTable$Cost[which(grepl(SetTable$Footwear[a], ClothesTable$Clothes))])
        }
        else{
          if(!is.na(SetTable$Footwear[j]) & !is.na(SetTable$Footwear[a]) & SetTable$Footwear[j]!=SetTable$Footwear[a]) {
            summ <- append(summ, ClothesTable$Cost[which(grepl(SetTable$Footwear[a], ClothesTable$Clothes))])
          }
        }
        
        count <- length(summ)
        summ <- sum(summ)+count*2
        
        result <- append(result, summ)
      }
    }
    CostTable <- cbind.fill(CostTable,a = result, fill = NA)
  }
  
  CostTable <- CostTable[,-1]
  colnames(CostTable) <- SeasonsTable$Month[month]
  CostTable$Cost <- SetTable$Weight*Cost1kg
  
  
  Result <- integer()
  for(i in 1:nrow(CostTable)){
    E <- integer()
    for(j in 1:(ncol(CostTable)-1)){
      E <- append(E, (CostTable[i,j]+CostTable$Cost[i])*P)
    }
    Result <- append(Result, sum(-E))
    print(c(paste("E", i, sep = ""), sum(-E)))
  }
  print(c("Best strategy:", which.max(Result)))
}

##---------------FIRST----------------

allmonth <- c(1:12)
Strategy(allmonth, 0.083)

##--------------SECOND-----------------

##-----WINTER

winter <- c(1,2,12)
Strategy(winter, 1/3)

##-----SPRING

spring <- c(3,4,5)
Strategy(spring, 1/3)

#------SUMMER

summer <- c(6,7,8)
Strategy(summer, 1/3)

#------AUTUMN

autumn <- c(9,10,11)
Strategy(autumn, 1/3)

##--------------THIRD-----------------


