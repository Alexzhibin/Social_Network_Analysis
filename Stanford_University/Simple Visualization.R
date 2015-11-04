fradj = read.delim("http://stanford.edu/~messing/Krack-High-Tec-ADVICE.tab", header = TRUE, row.names = 1)
colnames(fradj) = 1:21
library(igraph)
fradj = as.matrix(fradj)
frnet = graph.adjacency(fradj)
frnet
plot.igraph(frnet)
#But the nodes are placed randomly on the graph. We can see more with a better layout. At least in igraph, the best layout algorithm seems to be Fruchterman-Reingold 
plot.igraph(frnet, layout=layout.fruchterman.reingold)

# let's assign the vertex (node) names as the labels
V(frnet)$name
V(frnet)$label = letters[as.numeric(V(frnet)$name)]
plot.igraph(frnet, layout=layout.fruchterman.reingold)

       
g1
plot.igraph(g1)
