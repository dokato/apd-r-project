library("ggplot2")
library("dplyr")
library("igraph")

trim <- function(x) gsub("^\\s+|^\n|\\s+$", "", x)

load("apd.Rda")
tags.lst<-strsplit(apddata$keypl,",")

# trim(unlist(wx[3]))
# gsub(".","",unlist(wx[3]))
# gsub(",","",unlist(wx[3]))
# tolower(unlist(wx[3]))
# 
# dfx <- data.frame(v1= character(0), v2= character(0), strength= numeric(0))
# 
# g <- graph.empty()
# 
# g["a","b", attr="weight"] <- 2
# g["c","a"] <- g["c","a"] + 4
# length(V(g)[V(g)$name=='a' ]) #check if exists

graph.tags <- graph.empty(directed=FALSE)

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# weight of the vertex

for (i in 1:length(tags.lst)){
  cat(i, " ")
  row <- trim(unlist(tags.lst[i]))
  if (sum(is.na(row))){
    next
  }
  row<-gsub("\\.","",row)
  row<-gsub(",","",row)
  row<-tolower(row)
  
  for (k in 1:length(row)){
    if (length(V(graph.tags)[V(graph.tags)$name==row[k]])==0){
      graph.tags <- graph.tags + vertex(row[k], val=1)
    }
    else{
      #graph.tags[row[k]] <- graph.tags[row[k]] + 1
      currval <-get.vertex.attribute(graph.tags,'val',row[k])
      graph.tags <- set.vertex.attribute(graph.tags,'val',row[k],currval+1)
    }
  }
  for (k in 1:length(row)){
    for (j in 1:length(row)){
      if (graph.tags[row[k], row[j]]==0){
        graph.tags[row[k], row[j], attr="weight"] <- 1
      }
      else{
        graph.tags[row[k], row[j]] <- graph.tags[row[k], row[j]] + 1
      }
    }
  }
}
