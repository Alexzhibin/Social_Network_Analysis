################
#Visualize each set of relationships on the same layout
###############
advadj = read.delim("Krack-High-Tec-ADVICE.tab.txt", row.names = 1)
colnames(advadj) = 1:21
advadj = as.matrix(advadj)
rptadj = read.delim("Krack-High-Tec-REPORTS_TO.tab.txt", row.names = 1)
colnames(rptadj) = 1:21
rptadj = as.matrix(rptadj)

#In igraph, you need one graph object per set of relationships.
advnet = graph.adjacency(advadj)
rptnet = graph.adjacency(rptadj)

#Now check the length of each edgelist.(Link)
length(E(advnet))
length(E(rptnet))
#The ADVICE network (advnet object) has 189 edges and the REPORTS_TO network (rptnet object) only has 19

#we will apply the layout optimization algorithm to the ADVICE network 
#because it is usually much easier to read - sparse network visualizations are easier to read without layout optimization.
la = layout.fruchterman.reingold( advnet )  
plot.igraph( advnet, layout = la )

# A little cluttered though.  So let's transform the ADVICE graph so that multiple edges become an edge-attribute ( E(g)$weight ) :
E(advnet)$weight <- count.multiple(advnet)
advnet <- simplify(advnet)
la = layout.fruchterman.reingold( advnet, weights = E( advnet )$weight )   
plot.igraph( advnet, layout = la )
