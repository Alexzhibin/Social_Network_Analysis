###A network can consist of differnet 'classes' of node.such as two-mode network
df <- data.frame( person = c('Sam','Sam','Sam','Greg','Tom','Tom','Tom','Mary','Mary'), group = c('a','b','c','a','b','c','d','b','d'), stringsAsFactors = F)

M = as.matrix( table(df) ) 
m = table( df )
M = as.matrix( m ) 

#one-mode representation of ties between rows entities
Mrow = M %*% t(M) 

#one-mode representation of ties between cols entities
Mcol = t(M) %*% M

###Analysis of Two Mode Data and Mobility
# Load the "igraph" library
library("igraph") 

# (1) Read in the data files, NA data objects coded as "na" 
magact96 = read.delim("http://stanford.edu/~messing/mag_act96.txt", 
                      na.strings = "na")
magact97 = read.delim("http://stanford.edu/~messing/mag_act97.txt", 
                      na.strings = "na")
magact98 = read.delim("http://stanford.edu/~messing/mag_act98.txt", 
                      na.strings = "na")


magattrib = magact96[,1:4]

g96 = as.matrix(magact96[,-(1:4)]); row.names(g96) = magact96$ID.
g97 = as.matrix(magact97[,-(1:4)]); row.names(g97) = magact97$ID.
g98 = as.matrix(magact98[,-(1:4)]); row.names(g98) = magact98$ID.


i96 <- graph.incidence(g96, mode=c("all") )
i97 <- graph.incidence(g97, mode=c("all") )
i98 <- graph.incidence(g98, mode=c("all") )

V(i96)$color[1:1295] = rgb(1,0,0,.5)
V(i96)$color[1296:1386] = rgb(0,1,0,.5)

V(i96)$label = V(i96)$name
V(i96)$label.color = rgb(0,0,.2,.5)
V(i96)$label.cex = .4
V(i96)$size = 6
V(i96)$frame.color = NA

E(i96)$color = rgb(.5,.5,0,.2)

plot(i96, layout=layout.fruchterman.reingold)


#1.First, we'll remove isloates, by deleting all nodes with a degree of 0, meaning that they have zero edges. 
#2.Then, we'll suppress labels for students and make their nodes smaller and more transparent. 
#3.Then we'll make the edges more narrow more transparent.
i96 = delete.vertices(i96, V(i96)[ degree(i96)==0 ])
V(i96)$label[1:857] = NA
V(i96)$color[1:857] =  rgb(1,0,0,.1)
V(i96)$size[1:857] = 2 

E(i96)$width = .3
E(i96)$color = rgb(.5,.5,0,.1)

pdf("i96.2.pdf")
plot(i96, layout=layout.kamada.kawai)
dev.off()

pdf("i96.3.pdf")
plot(i96, layout=layout.fruchterman.reingold.grid)
dev.off()

pdf("i96.4.pdf")
plot(i96, layout=layout.fruchterman.reingold)
dev.off()

#I personally prefer the Fruchterman-Reingold layout in this case.
#The nice thing about this layout is that it really emphasizes centrality--the nodes that are most central are nearly always placed in the middle of the plot.



################
#Two mode to one mode data transformation
################
g96e = t(g96) %*% g96
g97e = t(g97) %*% g97
g98e = t(g98) %*% g98

i96e = graph.adjacency(g96e, mode = "undirected")

#Now we need to tansform the graph so that multiple edges become an attribute ( E(g)$weight ) of each unique edge:
E(i96e)$weight <- count.multiple(i96e)
i96e <- simplify(i96e)

#Now we'll set the other plotting parameters as we did above:
# Set vertex attributes
V(i96e)$label = V(i96e)$name
V(i96e)$label.color = rgb(0,0,.2,.8)
V(i96e)$label.cex = .6
V(i96e)$size = 6
V(i96e)$frame.color = NA
V(i96e)$color = rgb(0,0,1,.5)

# Set edge gamma according to edge weight
egam = (log(E(i96e)$weight)+.3)/max(log(E(i96e)$weight)+.3)
E(i96e)$color = rgb(.5,.5,0,egam)
#We set edge gamma as a function of how many edges exist between two nodes, or in this case, how many students each group has in common. 
pdf("i96e.pdf")
plot(i96e, main = "layout.kamada.kawai", layout=layout.kamada.kawai)
plot(i96e, main = "layout.fruchterman.reingold", layout=layout.fruchterman.reingold) 
dev.off()
##I like the Kamada-Kawai layout for this graph, because the center of the graph is too busy otherwise. 


##############
#Group overlap networks and plots
#############
## a percent overlap graph
ol96 = g96e/diag(g96e)
ol97 = g97e/diag(g97e)
ol98 = g98e/diag(g98e)

# sum the matricies and set any NA cells to zero:
magall = ol96 + ol97 + ol98
magall[is.na(magall)] = 0

#Note that magall now consists of a percent overlap matrix, but because we've summed over 3 years, the maximun is now 3 instead of 1. 
#Let's compute average club size, by taking the mean across each value in each diagonal:
magdiag = apply(cbind(diag(g96e), diag(g97e), diag(g98e)), 1, mean ) 

#Finally, we'll generate centrality measures for magall.
#When we create the igraph object from our matrix, we need to set weighted=T because otherwise igraph dichotomizes edges at 1. 
#This can distort our centrality measures because now edges represent  more than binary connections--they represent the percent of membership overlap.  
magallg = graph.adjacency(magall, weighted=T)
# Degree 
V(magallg)$degree = degree(magallg)
# Betweenness centrality
V(magallg)$btwcnt = betweenness(magallg)
#Before we plot this, we should probably filter some of the edges, otherwise our graph will probably be too busy to make sense of visually.  
# Take a look at the distribution of connection strength by plotting the density of the magall matrix:
plot(density(magall))

#Nearly all of the edge weights are below 1--or in other words, the percent overlap for most clubs is less than 1/3. 
#Let's filter at 1, so that an edge will consists of group overlap of more than 1/3 of the group's members in question.

magallgt1 = magall
magallgt1[magallgt1<1] = 0
magallggt1 = graph.adjacency(magallgt1, weighted=T)
# Removes loops:
magallggt1 <- simplify(magallggt1, remove.multiple=FALSE, remove.loops=TRUE)

#Before we do anything else, we'll create a custom layout based on Fruchterman.-Ringold
#wherein we adjust the coordates by hand using the tkplot gui tool to make sure all of the labels are visible. This is very useful if you want to create a really sharp-looking network visualization for publication.
magallggt1$layout = layout.fruchterman.reingold(magallggt1)
V(magallggt1)$label = V(magallggt1)$name
tkplot(magallggt1)
#Save the layout coordinates to the graph object:
#3 mean the third plot
magallggt1$layout = tkplot.getcoords(3)


# Set vertex attributes
V(magallggt1)$label = V(magallggt1)$name
V(magallggt1)$label.color = rgb(0,0,.2,.6)
V(magallggt1)$size = 6
V(magallggt1)$frame.color = NA
V(magallggt1)$color = rgb(0,0,1,.5)

# Set edge attributes
E(magallggt1)$arrow.size = .3

# Set edge gamma according to edge weight
egam = (E(magallggt1)$weight+.1)/max(E(magallggt1)$weight+.1)
E(magallggt1)$color = rgb(.5,.5,0,egam)

#One thing that we can do with this graph is to set label size as a function of degree, which adds a "tag-cloud"-like element to the visualization:
  
V(magallggt1)$label.cex = V(magallggt1)$degree/(max(V(magallggt1)$degree)/2)+ .3
#note, unfortunately one must play with the formula above to get the 
#ratio just right

#Let's plot the results:

pdf("magallggt1customlayout.pdf")
plot(magallggt1)
dev.off()

#This visualization reveals much more information about our network than our cresent-star visualization. 

#####
#Block
#####
blocks = cohesive.blocks(i96e) 
plot(blocks, i96e, vertex.size=1.1, vertex.label.cex=.6, edge.color=rgb(.4,.4,0,.3))

######
#Maximal CLiques
######
cl <- maximal.cliques(i96e)
length(cl)
colbar <- rainbow(length(cl) + 1)
for (i in 1:length(cl)) {
   V(i96e)[cl[[i]]]$color <- colbar[i+1] 
}
plot(i96e, mark.groups=cl,vertex.size=.3, vertex.label.cex=.6, edge.color=rgb(.4,.4,0,.3))

######
#Largest CLiques
######
cl <- largest.cliques(i96e)
length(cl)
colbar <- rainbow(length(cl) + 1)
for (i in 1:length(cl)) {
  V(i96e)[cl[[i]]]$color <- colbar[i+1] 
}
plot(i96e, mark.groups=cl,vertex.size=.3, vertex.label.cex=.5, edge.color=rgb(.4,.4,0,.3))

