library(tm)
library(ggplot2)


Pareto <- function(file){
  test <- data.frame(A = character(), Q1 = integer(), Q2 = integer(), stringsAsFactors = FALSE) #создаем таблицу для преобразования данных, в нужный нам вид
  for(i in 1:length(file)){ #добавляем строчки в таблицу. у нас есть число 32 например. Q1 = 3, Q2 = 2. Тоесть Q1 = целое от деления на 10, Q2 = остаток от деления на 10
    test <- rbind(test,data.frame(A = paste("A", i, sep = ""), Q1 = file[i]%/%10, Q2 = file[i]%%10, stringsAsFactors = FALSE))
  }
  test$mark <- TRUE #чисто для алгоритма пометка 
  
  finalPareto <- data.frame(A = character(), Q1 = integer(), Q2 = integer(), Domination = character(), stringsAsFactors = FALSE) #создает финальную таблицу, в которій помечено, кто над кем доминирует
  
  for (i in 1:nrow(test)){
    for (j in 1:nrow(test)){ #запускаем алгоритм
      #всё так же например у нас есть 32 и 83. 8 > 3 and 3 > 2, значит 83 доминирует над 32. То, НАД ЧЕМ доминирую мы "исключаем" из выборки (mark = false)
      #если мы не можем определить, кто доминирует, например у нас 32 и 81, 8 > 3 and 2 > 1 - оставляем оба.
      #так проверяем все пары. 
      #условия вхождения в иф - пометки у двух пар - ТРУ. После выхода - тот, над кем доминируют - ФОЛС.
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
    #Добавляе в таблицу те пары, где доминанта не определили
    if(test$mark[i])  finalPareto <-rbind(finalPareto, data.frame(A = test$A[i], Q1 = test$Q1[i], Q2 = test$Q2[i], Domination = "", stringsAsFactors = FALSE))
  }
  test$mark <- NULL #удаляем поетки
  finalPareto <- finalPareto[order(finalPareto$Q1, -finalPareto$Q2),] #сортирируем для красоты сединения точек
  #рисуем график, убираем дупликаті точек, по х - кью1, по у - кью2, выделяем разными цветами по колонке "доминация", что бы потом соединить пужное.
  #добавляем точки. добавляем линии где "доминации" нет, тоесть для тех пар, где мы её не смогли определить (эти пары и создает нужное нам множество)
  #линия - граница Парето
  #добавляем подписи точек
  graphPareto <- ggplot(finalPareto[!duplicated(finalPareto[2:3]),], aes(x=Q1, y=Q2, color=Domination))+geom_point()+geom_line(data=finalPareto[finalPareto$Domination=="", ])+geom_text(aes(label=A),hjust=1.2, vjust=1.2)
  
  save(test, finalPareto, graphPareto, file = "Pareto.RData") #сохраняем нужные нам данные в фАЙЛ
}      
#аналогично, но другой знак
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

file1 <- scan(file = "table.txt")  #все элементы в файле 
#file1 <- scan(file = "table.txt", nlines = 1) первая строчка
#file1 <- scan(file = "table.txt", nlines = 1, skip = 1) вторая строчка
#file1 <- scan(file = "table.txt", nlines = 1, skip = 2) третья сточка

Pareto(file1) #применяем функцию для файла 
load("Pareto.RData") #загружаем файл

Slater(file1)
load("Slater.RData")

graphPareto #вывод графиков
graphSlater

