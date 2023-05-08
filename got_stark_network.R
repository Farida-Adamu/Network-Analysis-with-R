################################################
##Analyzing the House Stark main characters in GOT
################################################

library(igraph)
library(dplyr)

### Read the data from external files
got_nodes <- read.csv(file="got-nodes.csv")
got_edges <- read.csv(file="got-edges.csv")


#subset the datarframe for only the main characters in House Stark 
#We select only characters that made it to the very final episodes. 
edges <- got_edges %>% filter(Source == "Arya" | 
                                Source == "Jon" | 
                                Source == "Sansa")
### Generate a network from the dataframes
network <- graph.data.frame(edges,
                           # vertices = got_nodes,
                            directed=TRUE)
is.igraph(network)

#information about the vertices
V(network)

#information about the edges or connections
E(network)

#labels
V(network)$label <- V(network)$name
V(network)$label

#number of degrees
V(network)$degree<- degree(network)
V(network)$degree

#histogram
hist(V(network)$degree,
     col = 'grey',
     main = 'Histogram of node degree',
     ylab = 'frequency',
     xlab = 'degree of vertices')
#the histagram shows us thre are more characters with smaller degrees
#than with higher degrees in the game of thrones series.
#there are few nodes with high connections and many notes with small connecyions

#set.seed(222)

# Set font family to Arial
#par(family = "Arial")


# Plot network with vertex labels
plot(network, layout = layout.fruchterman.reingold,
     vertex.size = V(network)$degree, 
     vertex.label.dist= 0.5,
     edge.arrow.size = 0.1,
    vertex.label.cex = 0.5)


# Saving the network with high resolution
jpeg(file="network_stark.jpg", width = 6, height = 6, units = 'in', res = 300)
plot(network, layout = layout.fruchterman.reingold,
     vertex.size = V(network)$degree, 
     vertex.label.dist= 0.5,
     edge.arrow.size = 0.1,
     vertex.label.cex = 0.5)
dev.off()


###---------------DETECTING COMMUNITIES------------------

## Communities according to the Louvain algorithm
louvain <- cluster_louvain(as.undirected(network))

jpeg(file="starK_network_louvain.jpg", width = 6, height =
       6, units = 'in', res = 300)
plot(network, layout=layout.fruchterman.reingold,
     vertex.size=4, vertex.label.dist=0.5,
     vertex.color=membership(louvain),
     edge.arrow.size=0.5)
dev.off()

##C Communities accroding to the edge-between. algorithm
edge_between<-cluster_edge_betweenness(as.undirected(network))
jpeg(file="stark_network_between.jpg", width = 6, height = 6, units = 'in', res = 300)
plot(network, layout=layout.fruchterman.reingold, vertex.size=4, 
     vertex.label.dist=0.5, vertex.color=membership(edge_between), edge.arrow.size=0.5)
dev.off()



#-------------------HUB and AUTHORITIES
# Hub and authorities
hs <- hub_score(network)$vector
as <- authority.score(network)$vector
par(mfrow=c(1,2))
set.seed(123)
plot(network,
     vertex.size=hs*20,
     main = 'Hubs',
     #vertex.color = rainbow(52),
     edge.arrow.size=0.1,
     layout = layout.fruchterman.reingold)

plot(network,
     vertex.size=as*20,
     main = 'Authorities',
     #vertex.color = rainbow(52),
     edge.arrow.size=0.1,
     layout = layout.fruchterman.reingold)
par(mfrow=c(1,1))


# Community detection to detect densely connected nodes
unet <- graph.data.frame(edges, directed = F)
cnet <- cluster_edge_betweenness(network)
jpeg(file="conn_stark.jpg", width = 6, height = 6, units = 'in', res = 300)
plot(cnet,
     unet,
     vertex.size = V(network)$degree,
     vertex.label.cex = 0.5,
     vertex.label.dist=0.5, 
    edge.arrow.size=0.3)
dev.off()

# Create a data frame with network statistics
network_stats <- data.frame(
  Nodes = vcount(network),
  Edges = ecount(network),
  MeanDegree = mean(degree(network, mode ="all")),
  SDDegree = sd(degree(network, mode ="all")),
  Isolates = sum(degree(network, mode ="all")==0),
  AvgClustering = transitivity(network, type="global")
)

# View the network statistics table
network_stats
library(knitr)
# Create a table with the network statistics
kable(network_stats)

## Distances 
diameter(network); mean_distance(network, directed=F)

### Connectivity
indegree<-degree(network, mode="in")
outdegree<-degree(network, mode="out")
degree <-degree(network, mode="all")

mean(indegree)
mean(outdegree)
mean(degree)

reciprocity(network)

## Assortativity
## ...using simple correlation coefficient
es <-get.edges(network, E(network))
deg<-degree(network, mode="all")
assortativity= cor(deg[es[,1] ], deg[es[,2] ])
assortativity

## Positioning:
# Global centrality
betweenness= betweenness(network, directed = FALSE)
betweenness

eigenvector.centrality= evcent(network)
eigenvector.centrality
hist(eigenvector.centrality$vector)

# Local centrality
outdegree= degree(network, mode="out")
indegree= degree(network, mode="in")
deg= degree(as.undirected(network))

# Clustering coefficient
clustering = transitivity(as.undirected(network), type="local")
clustering
