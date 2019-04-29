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

for(i in 1:nrow(SeasonsTable)){
  for(j in 1:nrow(SetTable)){
    if(SeasonsTable$mapping[i]!=SetTable$H[j]){
      a <- SeasonsTable$mapping[i]
      summ <- integer()
      print(c(SetTable$H[j], SeasonsTable$Month[i]))
      if(!is.na(SetTable$Headdress[j]) & !is.na(SetTable$Headdress[a]) & SetTable$Headdress[j]!=SetTable$Headdress[a]){
        print(c(SetTable$Headdress[a],ClothesTable$Cost[which(grepl(SetTable$Headdress[a], ClothesTable$Clothes))] ))
        summ <- append(summ, ClothesTable$Cost[which(grepl(SetTable$Headdress[a], ClothesTable$Clothes))])
      } 
      if(!is.na(SetTable$Outerwear[j]) & !is.na(SetTable$Outerwear[a]) & SetTable$Outerwear[j]!=SetTable$Outerwear[a]) {
        print(c(SetTable$Outerwear[a],ClothesTable$Cost[which(grepl(SetTable$Outerwear[a], ClothesTable$Clothes))] ))
        summ <- append(summ, ClothesTable$Cost[which(grepl(SetTable$Outerwear[a], ClothesTable$Clothes))])
      }
      if(!is.na(SetTable$Gloves[j]) & !is.na(SetTable$Gloves[a]) & SetTable$Gloves[j]!=SetTable$Gloves[a]) {
        print(c(SetTable$Gloves[a],ClothesTable$Cost[which(grepl(SetTable$Gloves[a], ClothesTable$Clothes))] ))
        summ <- append(summ, ClothesTable$Cost[which(grepl(SetTable$Gloves[a], ClothesTable$Clothes))])
      }
      if(!is.na(SetTable$Trousers[j]) & !is.na(SetTable$Trousers[a]) & SetTable$Trousers[j]!=SetTable$Trousers[a]) {
        print(c(SetTable$Trousers[a],ClothesTable$Cost[which(grepl(SetTable$Trousers[a], ClothesTable$Clothes))] ))
        summ <- append(summ, ClothesTable$Cost[which(grepl(SetTable$Trousers[a], ClothesTable$Clothes))])
      }
      if(!is.na(SetTable$Footwear[j]) & !is.na(SetTable$Footwear[a]) & SetTable$Footwear[j]!=SetTable$Footwear[a]) {
        print(c(SetTable$Footwear[a],ClothesTable$Cost[which(grepl(SetTable$Footwear[a], ClothesTable$Clothes))] ))
        summ <- append(summ, ClothesTable$Cost[which(grepl(SetTable$Footwear[a], ClothesTable$Clothes))])
      }
      count <- length(summ)
      summ <- sum(summ)+count*2
      print(c("SUM", summ))
      
    }
  }
}
which(grepl(SetTable$Headdress[SeasonsTable$mapping[5]], ClothesTable$Clothes))
