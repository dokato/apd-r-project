library("ggplot2")
library("dplyr")
library("igraph")

trim <- function(x) gsub("^\\s+|^\n|\\s+$", "", x)

load("apd.Rda")
ixs<-grep("[<>]", apddata$keypl)
tags.lst<-strsplit(apddata$keypl[-ixs],",")


graph.tags <- graph.empty(directed=FALSE)

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
      if (k==j){
        next
      }
      if (graph.tags[row[k], row[j]]==0){
        graph.tags[row[k], row[j], attr="weight"] <- 1
      }
      else{
        graph.tags[row[k], row[j]] <- graph.tags[row[k], row[j]] + 1
      }
    }
  }
}

nodes <- data.frame(cbind(V(graph.tags), as.character(V(graph.tags))))
edges <- t(Vectorize(get.edge, vectorize.args='id')(graph.tags, 1:ecount(graph.tags)))
gr.xml <- write.gexf(nodes, edges)


saveAsGEXF = function(g, filepath="converted_graph.gexf"){
  require(igraph)
  require(rgexf)
  # gexf nodes require two column data frame (id, label)
  # check if the input vertices has label already present
  # if not, just have the ids themselves as the label
  if(is.null(V(g)$label))
    V(g)$label <- enc2utf8(as.character(V(g)$name))
  # similarily if edges does not have weight, add default 1 weight
  if(is.null(E(g)$weight))
    E(g)$weight <- rep.int(1, ecount(g))
  nodes <- data.frame(cbind(V(g), V(g)$label))
  edges <- t(Vectorize(get.edge, vectorize.args='id')(g, 1:ecount(g)))
  # combine all node attributes into a matrix (and take care of & for xml)
  vAttrNames <- setdiff(list.vertex.attributes(g), "label")
  nodesAtt <- data.frame(sapply(vAttrNames, function(attr) sub("&", "&",get.vertex.attribute(g, attr))),
                         stringsAsFactors = FALSE)
  # combine all edge attributes into a matrix (and take care of & for xml
  eAttrNames <- setdiff(list.edge.attributes(g), "weight")
  edgesAtt <- data.frame(sapply(eAttrNames, function(attr) sub("&", "&",get.edge.attribute(g, attr))),
                         stringsAsFactors = FALSE)
  
  # generate the gexf object
  
  output <- write.gexf(nodes, edges,
                       edgesWeight=E(g)$weight,
                       edgesAtt = edgesAtt,                      
                       nodesAtt = nodesAtt)
  
  print(output, filepath, replace=T)
  
}

write.graph(graph.tags, "apdtagsgraph.graphml", format="graphml")
