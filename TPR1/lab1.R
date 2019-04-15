library(tm)
library(ggplot2)


Pareto <- function(file){
  test <- data.frame(A = character(), Q1 = integer(), Q2 = integer(), stringsAsFactors = FALSE)
  for(i in 1:length(file)){ 
    test <- rbind(test,data.frame(A = paste("A", i, sep = ""), Q1 = file[i]%/%10, Q2 = file[i]%%10, stringsAsFactors = FALSE))
  }
  test$mark <- TRUE
  
  finalPareto <- data.frame(A = character(), Q1 = integer(), Q2 = integer(), Domination = character(), stringsAsFactors = FALSE)
  
  for (i in 1:nrow(test)){
    for (j in 1:nrow(test)){ 
      if(test$mark[j] & test$mark[i] & i != j & test$Q1[i]>=test$Q1[j] & test$Q2[i]>=test$Q2[j] & (test$Q1[i]!=test$Q1[j] | test$Q2[i]!=test$Q2[j])){
        finalPareto <-rbind(finalPareto, data.frame(A = test$A[j], Q1 = test$Q1[j], Q2 = test$Q2[j], Domination = test$A[i], stringsAsFactors = FALSE))
        test$mark[j] <- FALSE
      }
      else
        if(test$mark[i] & test$mark[i] & i != j & test$Q1[i]<=test$Q1[j] & test$Q2[i]<=test$Q2[j] & (test$Q1[i]!=test$Q1[j] | test$Q2[i]!=test$Q2[j])){
          finalPareto <-rbind(finalPareto, data.frame(A = test$A[i], Q1 = test$Q1[i], Q2 = test$Q2[i], Domination = test$A[j], stringsAsFactors = FALSE))
          test$mark[i] <- FALSE
        }
    }
    if(test$mark[i])  finalPareto <-rbind(finalPareto, data.frame(A = test$A[i], Q1 = test$Q1[i], Q2 = test$Q2[i], Domination = "", stringsAsFactors = FALSE))
  }
  test$mark <- NULL 
  finalPareto <- finalPareto[order(finalPareto$Q1, -finalPareto$Q2),]
  graphPareto <- ggplot(finalPareto[!duplicated(finalPareto[2:3]),], aes(x=Q1, y=Q2, color=Domination))+geom_point()+geom_line(data=finalPareto[finalPareto$Domination=="", ])+geom_text(aes(label=A),hjust=1.2, vjust=1.2)
  
  save(test, finalPareto, graphPareto, file = "Pareto.RData")
}      

Slater <- function(file){
  test <- data.frame(A = character(), Q1 = integer(), Q2 = integer(), stringsAsFactors = FALSE)
  for(i in 1:length(file)){
    test <- rbind(test,data.frame(A = paste("A", i, sep = ""), Q1 = file[i]%/%10, Q2 = file[i]%%10, stringsAsFactors = FALSE))
  }
  test$mark <- TRUE
  
  finalSlater <- data.frame(A = character(), Q1 = integer(), Q2 = integer(), Domination = character(), stringsAsFactors = FALSE)
  
  for (i in 1:nrow(test)){
    for (j in 1:nrow(test)){
      if(test$mark[j] & test$mark[i] & i != j & test$Q1[i]>test$Q1[j] & test$Q2[i]>test$Q2[j]){
        finalSlater <-rbind(finalSlater, data.frame(A = test$A[j], Q1 = test$Q1[j], Q2 = test$Q2[j], Domination = test$A[i], stringsAsFactors = FALSE))
        test$mark[j] <- FALSE
      }
      else
        if(test$mark[i] & test$mark[i] & i != j & test$Q1[i]<test$Q1[j] & test$Q2[i]<test$Q2[j]){
          finalSlater <-rbind(finalSlater, data.frame(A = test$A[i], Q1 = test$Q1[i], Q2 = test$Q2[i], Domination = test$A[j], stringsAsFactors = FALSE))
          test$mark[i] <- FALSE
        }
      
    }
    if(test$mark[i])  finalSlater <-rbind(finalSlater, data.frame(A = test$A[i], Q1 = test$Q1[i], Q2 = test$Q2[i], Domination = "", stringsAsFactors = FALSE))
  }
  
  test$mark <- NULL
  finalSlater <- finalSlater[order(finalSlater$Q1, -finalSlater$Q2),]
  graphSlater <- ggplot(finalSlater[!duplicated(finalSlater[2:3]),], aes(x=Q1, y=Q2, color=Domination))+geom_point()+geom_line(data=finalSlater[finalSlater$Domination=="", ])+geom_text(aes(label=A),hjust=1.2, vjust=1.2)
  
  save(test, finalSlater, graphSlater, file = "Slater.RData")
}

file1 <- scan(file = "table.txt")

Pareto(file1) 
load("Pareto.RData") 

Slater(file1)
load("Slater.RData")

graphPareto
graphSlater

