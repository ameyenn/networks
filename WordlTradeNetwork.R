####Exploriing igraph network plots using World Trade Agreement data
####Writen by Andrew Meyenn, 12/7/2022

library(data.table)
library(factoextra)
library(tidyverse)
library(tidygraph)
library(dplyr)
library(igraph)
library(ggraph)
library(networkD3)

setwd("C:/Users/Cathie/SkyDrive/Rprogs/FTA")
df<-fread("./fta1.csv") 
#data from https://aric.adb.org/fta-all
#Note: this is just test data and should not be used for serious work.

colnames(df)

#we just going to use 2 columns Source and Target

df[1:10,1:2]

#the Source and Target list countries, in same row means there is a trade
#agreement

#Step 1 is creating a matrix of the 2 columns.
#Method 1 - select and store as matrix, column 1 of the matrix 
#will store the from Vertex and column 2 the two vertex
#edges will connect these

gMatrix <- df %>%
  select(Source, Target) %>%
  as.matrix 
 
#Method 2 is to se cbind
#gMatrix <- cbind(df$Source, df$Target)

#Step 2 need to great the edge list, comes from iGraph package
#see https://igraph.org/r/doc/ it is a massive package
#best tutorial https://kateto.net/networks-r-igraph

#create the graph object and set the edges to connect
grNet<-graph_from_edgelist(gMatrix, directed = FALSE)
grNet #to see the edges conencting the vertices

#basic plot
plot(grNet) #click ZOOM to get a large view

#the degree of a node r vertex is how many edges come in.
#the larger here the more agreements. For directed you can do IN and OUT mode

#this will show CHINA has 31 and USA 26

sort(degree(grNet))

#we can colour and change size of edges and vertices, and use different layouts
#see below for more options

lay1 <- layout_with_graphopt(grNet) #see the tutorial above

plot(grNet, layout=lay1, 
     vertex.size=degree(grNet),
     vertex.color="red")

#see the vertexes    
V(grNet)   

#see the edges
E(grNet)

#can look at Vetices
V(grNet)[2]$name

#can add an attribute
#AIM: set an attribute of Exports & Imports for each Vertex.
colnames(df)

#we search for name match (they are unique) and then set new attributes
for (i in 1:length(V(grNet))){
  x <- which(V(grNet)$name %in% df[i, 3])
  V(grNet)[x]$export<-df[i,5]
  V(grNet)[x]$import<-df[i,6]
} 

#plot the graph setting the vertex size = imports/exports
#multiply by scale of 2 as quotent im/ex not big
ex<-as.numeric(unlist(V(grNet)$export))
im<-as.numeric(unlist(V(grNet)$import))
plot(grNet, 
     vertex.size=(im/ex)*2,
     vertex.color="red")

#plot showing export relative size, scale 0.00002
plot(grNet, 
     vertex.size=(ex)*0.00002,
     vertex.color="red")

#clustering
set.seed(123)
#can use cluster_optimal
clusters <- cluster_louvain(grNet)
V(grNet)$clust<-clusters$membership

#list of layouts to try one at a time
lay1 <- layout_with_fr(grNet)
lay1 <- layout_with_kk(grNet)
lay1<-layout_with_graphopt(grNet)
lay1<-layout_with_lgl
lay1<-layout_with_mds

# clust is the label, not change in arrow head size
plot(clusters,grNet, 
     vertex.label = V(grNet)$clust, 
     vertex.size=degree(grNet, mode="in"), 
     edge.arrow.size = .2, layout=lay1)

#ggplot gives a whole new range of options, here is basic example
ggraph(grNet) +
  geom_edge_link() +   # add edges to the plot
  geom_node_point() 

#nice big graph, max screen, select View and select fit
#select change all vertexes, select one, right click and change colour to white
#the output is included as an image file in a sheet of the datafile
tkid <- tkplot(grNet)

#option comes network3D, drag nodes around
networkData <- data.frame(df[,1], df[,2])
simpleNetwork(networkData)

#3d plot can move around, in the tkplot window
rglplot(grNet, layout=layout.fruchterman.reingold(grNet, dim=3), vertex.color="white")

#dendrogram
hrg <- fit_hrg(grNet)
plot_dendrogram(hrg)

#some other things to check and references
#https://www.r-bloggers.com/2018/12/network-centrality-in-r-an-introduction/

#centrality - this an undirected graph so either give a measure of most connected
centr_degree(grNet)
degree(grNet) #we saw this above, adds IN and OUT together undirected

#whole graph, bigger value shows greater influence of vertex, in more shortest paths.
#centralization value gives a measure of how close nodes are to each other
closeness(grNet, mode="all")

#betweenness - gives a measure per vertex of how many shortest paths go through it
#i.e. can get from A to F via C, between is on C.
#must be a better way to do this - create a vector, add to dataframe
#add new col of country names
bt<-betweenness(grNet)
b<-as.data.frame(bt)
b$c<-row.names(b)
#which one has highest betweenness other than looking?
b[order(-b$bt),]
b[which.max(b$bt), 2]

#check paths - warning, paths don't imply a trade agreement route

V(grNet)
distances(grNet)
shortest_paths(grNet,1) #1 is the position in the vertex list
