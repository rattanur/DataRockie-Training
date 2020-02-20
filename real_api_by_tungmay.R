
##----------------create time and distance matrix using Google Maps-----------------------

## set path
setwd("file_path")

##----------------------------------------------------------------------------------------
## install package (only for the fisrt time)
install.packages("gmapsdistance")
install.packages("devtools")

## call packages
library(gmapsdistance)
library(tidyverse)
library(openxlsx)

##----------------------------------------------------------------------------------------
## setting the API key
set.api.key("paste_your_API_key")

##----------------------------------------------------------------------------------------
## read file
library(openxlsx)
df <- read.xlsx("CWT.xlsx")

## create input for google maps
create_node <- function(){
  node <- c()
  #destination <- c()
  p <- paste(toString(df$Latitude[1]),"+",(toString(df$Longitude[1])),sep = "")
  node <- append(node, p, after = length(node))
  for (irow in 2:nrow(df)){
    p1 <- paste(toString(df$Latitude[irow]),"+",(toString(df$Longitude[irow])),sep = "")
    node <- append(node, p1, after = length(node))
  }
  return(node)
}

## create distance and time matrix function
matrixs <- function(){
  time <- data.frame(matrix(ncol = 0,nrow = length(origin)))
  a <- length(origin)-1
  time <- cbind(time, data.frame(c(0:length(origin)))) 
  distance <- cbind(distance, data.frame(c(0:length(origin)))) 
  for (i in 1:length(origin)) { 
    t <- data.frame(matrix(ncol = 1,nrow = 0))
    d <- data.frame(matrix(ncol = 1,nrow = 0))
    for (j in 1:length(origin)) { 
      from_google <- gmapsdistance(origin[i], destination[j], mode = "driving", avoid = "tolls",traffic_model = "best_guess")
      ## R can select traffic made = optimistic/best_guess/pessimistic
      Ti <- data.frame(from_google$Time / 60) #convert to minute
      D <- data.frame(from_google$Distance / 1000) #convert to kilometer
      t <- rbind(t,Ti)
      d <- rbind(d,D)
    }
    time <- cbind(time,t)
    distance <- cbind(distance,d)
  }
  colnames(time) <- c("",0:a) 
  colnames(distance) <- c("",0:a) 
  my_list <- list("time" = time, "distance" = distance)
  return(my_list)
}

## export function
export <- function(tim, dis){
  sheet1 = addWorksheet(wb1, "1")
  writeData(wb1, sheet = sheet1, x = tim)
  sheet2 = addWorksheet(wb2, "1")
  writeData(wb2, sheet = sheet2, x = dis)
}

## export
wb1 = createWorkbook()
wb2 = createWorkbook()
saveWorkbook(wb1, "time_matrix.xlsx")
saveWorkbook(wb2, "distance_matrix.xlsx")

## main
wb1 = createWorkbook()
wb2 = createWorkbook()
origin <- create_node() 
destination <- create_node() 
mat <- matrixs()
tim <- mat$time
dis <- mat$distance
export(tim, dis)
