## @knitr grafo2019 ---------------------------------------------------------------

weighted_edgelist_2019 <- edgelist |> 
  filter(year ==2019) |> 
  graph.data.frame(directed = FALSE) |> 
  as_edgelist() |> 
  as.data.frame() |> 
  group_by(V1,V2) |> 
  summarize(weight = n())

graph2019 <- graph.data.frame(weighted_edgelist_2019, directed=FALSE)
plot(graph2019)

AdjList2019 <- as_adj_list(graph2019)
AdjList2019

cluster<-cluster_louvain(graph2019)
co_autori<- membership(cluster)
cluster_colors <- rainbow(length(unique(co_autori)))
V(graph2019)$color <- cluster_colors[co_autori]
plot(graph2019, vertex.label = V(graph2019)$name, vertex.size = 10, main = "Grafo con nodi colorati per cluster")



subgraph2019 <- induced_subgraph(graph2019, 
                                 V(graph2019)[degree(graph2019) > 3])


## @knitr grafo2018 ---------------------------------------------------------------

weighted_edgelist_2018 <- edgelist |> 
  filter(year ==2018) |> 
  graph.data.frame(directed = FALSE) |> 
  as_edgelist() |> 
  as.data.frame() |> 
  group_by(V1,V2) |> 
  summarize(weight = n())

graph2018<- graph_from_data_frame(weighted_edgelist_2018, directed = FALSE)
plot(graph2018)
AdjList2018<- as_adj_list(graph2018)
V(graph2018)
E(graph2018)


## @knitr creazione sottografo 2018 ---------------------------------------------------------------

sgrafo_2018 <- subgraph(graph2018, V(graph2018)[degree(graph2018)] > 3)

V(sgrafo_2018)$label<- NA #elimino eticchetta nodi
V(sgrafo_2018)
E(sgrafo_2018)
plot(sgrafo_2018)

##graph by centrality

# Calculate betweenness centrality for all nodes
centrality_scores <- betweenness(graph2018)

# Define a centrality threshold
centrality_threshold <- median(centrality_scores)

# Filter nodes based on the centrality threshold
nodes_to_keep <- V(graph2018)[centrality_scores > centrality_threshold]

# Create the subgraph
subgraph2018 <- induced_subgraph(graph2018, nodes_to_keep)

V(subgraph2018)$label<- NA #elimino eticchetta nodi
V(subgraph2018)
E(subgraph2018)
plot(subgraph2018)


# Ggraph ------------------------------------------------------------------

ggraph(subgraph2018, layout = 'fr') + 
  geom_edge_link() + 
  geom_node_point(aes(size = degree(subgraph2018))) + 
  theme(legend.position = 'bottom')



#sul sottografo creo i cluster
cluster_2018<- cluster_louvain(sgrafo_2018)
autori_2018<- membership(cluster_2018)
c_colors<-rainbow(length(unique(autori_2018)))
V(sgrafo_2018)$color <- c_colors[autori_2018]
plot(sgrafo_2018, vertex.label = V(sgrafo_2018)$label, vertex.size = 10, 
     main = "Louvain 2018")
modularità_18<- modularity(cluster_2018) #0.96

#provo un altri 2 metodi per la creazione dei cluster
cluster_2_18<-cluster_walktrap(sgrafo_2018)
plot(cluster_2_18, sgrafo_2018, main="walktrap 2018") 
cluster_3_18<- cluster_label_prop(sgrafo_2018)
plot(cluster_3_18, sgrafo_2018, main="label porpagation 2018")

## @knitr misure descrittive 2018-------------------------------------------------
nodi_18<-V(sgrafo_2018)
legami_18<- E(sgrafo_2018)
densità_18<- edge_density(sgrafo_2018)
degree_18<-degree(sgrafo_2018)
max_degr_18<-which.max(degree(sgrafo_2018))
min_degr_18<- which.min(degree(sgrafo_2018))
closness_18<- closeness(sgrafo_2018, normalized = TRUE)
clo_max_18<- which.max(closeness(sgrafo_2018))
clo_min_18<- which.min(closeness(sgrafo_2018))
betw_18<- betweenness(sgrafo_2018, normalized = TRUE)
betw_max_18<-which.max(betweenness(sgrafo_2018))
betw_min_18<-which.min(betweenness(sgrafo_2018))
diametro_18<- diameter(sgrafo_2018)
e_betw_18<-edge_betweenness(sgrafo_2018)
E(sgrafo_2018)[which.max(edge_betweenness(sgrafo_2018))]



## @knitr grafo2019 ---------------------------------------------------------------

weighted_edgelist_2019 <- edgelist |> 
  filter(year ==2019) |> 
  graph.data.frame(directed = FALSE) |> 
  as_edgelist() |> 
  as.data.frame() |> 
  group_by(V1,V2) |> 
  summarize(weight = n())

graph2019 <- graph.data.frame(weighted_edgelist_2019, directed=FALSE)
plot(graph2019)
AdjList2019 <- as_adj_list(graph2019)
AdjList2019

## @knitr creazione sottografo 2019---------------------------------------------------------------

sottografo_degree<-function(graph,soglia){
  total_degree<-degree(graph, mode="total")
  node_to_include<-which(total_degree >= soglia)
  sottografo<-induced_subgraph(graph, node_to_include)
  return(sottografo)
}
soglia<- 3
sgrafo_2019<-sottografo_degree(graph2019, soglia)
V(sgrafo_2019)$label<- NA #elimino eticchetta nodi
plot(sgrafo_2019)


#sul sottografo creo i cluster
cluster_2019<- cluster_louvain(sgrafo_2019)
autori_2019<- membership(cluster_2019)
c_colors<-rainbow(length(unique(autori_2019)))
V(sgrafo_2019)$color <- c_colors[autori_2019]
plot(sgrafo_2019, vertex.label = V(sgrafo_2019)$label, vertex.size = 10, 
     main = "Louvain 2019")
modularità_19<- modularity(cluster_2019) #0.94

#provo un altri 2 metodi per la creazione dei cluster
set.seed(100)
cluster_2_19<-cluster_walktrap(sgrafo_2019)
plot(cluster_2_19, sgrafo_2019, main="walktrap 2019") 
cluster_3_19<- cluster_label_prop(sgrafo_2019)
plot(cluster_3_19, sgrafo_2019, main="label porpagation 2019")

## @knitr misure descrittive 2019-------------------------------------------------
nodi_19<-V(sgrafo_2019)
legami_19<- E(sgrafo_2019)
densità_19<- edge_density(sgrafo_2019)
degree_19<-degree(sgrafo_2019)
max_degr_19<-which.max(degree(sgrafo_2019))
min_degr_19<- which.min(degree(sgrafo_2019))
closness_19<- closeness(sgrafo_2019, normalized = TRUE)
clo_max_19<- which.max(closeness(sgrafo_2019))
clo_min_19<- which.min(closeness(sgrafo_2019))
betw_19<- betweenness(sgrafo_2019, normalized = TRUE)
betw_max_19<-which.max(betweenness(sgrafo_2019))
betw_min_19<-which.min(betweenness(sgrafo_2019))
diametro_19<- diameter(sgrafo_2019)
e_betw<-edge_betweenness(sgrafo_2019)
E(sgrafo_2019)[which.max(edge_betweenness(sgrafo_2019))]


## @knitr grafo2020 ---------------------------------------------------------------

weighted_edgelist_2020 <- edgelist |> 
  filter(year ==2020) |> 
  graph.data.frame(directed = FALSE) |> 
  as_edgelist() |> 
  as.data.frame() |> 
  group_by(V1,V2) |> 
  summarize(weight = n())

graph2020 <- graph.data.frame(weighted_edgelist_2020, directed=FALSE)
plot(graph2020)
AdjList2020 <- as_adj_list(graph2020)
AdjList2020

## @knitr creazione sottografo 2020---------------------------------------------------------------

sottografo_degree<-function(graph,soglia){
  total_degree<-degree(graph, mode="total")
  node_to_include<-which(total_degree >= soglia)
  sottografo<-induced_subgraph(graph, node_to_include)
  return(sottografo)
}
soglia<- 3
sgrafo_2020<-sottografo_degree(graph2020, soglia)
V(sgrafo_2020)$label<- NA #elimino eticchetta nodi
plot(sgrafo_2020)
#sul sottografo creo i cluster
set.seed(100)
cluster_2020<- cluster_louvain(sgrafo_2020)
autori_2020<- membership(cluster_2020)
c_colors<-rainbow(length(unique(autori_2020)))
V(sgrafo_2020)$color <- c_colors[autori_2020]
plot(sgrafo_2020, vertex.label = V(sgrafo_2020)$label, vertex.size = 10, 
     main = "Louvain 2020")
modularità_20<- modularity(cluster_2020) #0.94

#provo un altri 2 metodi per la creazione dei cluster
set.seed(100)
cluster_2_20<-cluster_walktrap(sgrafo_2020)
plot(cluster_2_20, sgrafo_2020, main="walktrap 2020") 
cluster_3_20<- cluster_label_prop(sgrafo_2020)
plot(cluster_3_20, sgrafo_2020, main="label porpagation 2020")


## @knitr misure descrittive 2020-------------------------------------------------
nodi_20<-V(sgrafo_2020)
legami_20<- E(sgrafo_2020)
densità_20<- edge_density(sgrafo_2020)
degree_20<-degree(sgrafo_2020)
max_degr_20<-which.max(degree(sgrafo_2020))
min_degr_20<- which.min(degree(sgrafo_2020))
closness_20<- closeness(sgrafo_2020, normalized = TRUE)
clo_max_20<- which.max(closeness(sgrafo_2020))
clo_min_20<- which.min(closeness(sgrafo_2020))
betw_20<- betweenness(sgrafo_2020, normalized = TRUE)
betw_max_20<-which.max(betweenness(sgrafo_2020))
betw_min_20<-which.min(betweenness(sgrafo_2020))
diametro_20<- diameter(sgrafo_2020)
e_betw_20<-edge_betweenness(sgrafo_2020)
E(sgrafo_2020)[which.max(edge_betweenness(sgrafo_2020))]



# Test sulla robustezza dei cluster 2020 ---------------------------------------
set.seed(100)
comp_1<-robinCompareFast(sgrafo_2020, method1 = "louvain", method2 = "walktrap",
                         measure = "vi")
plot_1<- plotRobin(graph=sgrafo_2020, model1 = comp_1$Mean1, model2 = comp_1$Mean2,
                   legend = c("louvain", "walktrap"))
comp_2<- robinCompareFast(sgrafo_2020, method1 = "louvain", method2="labelPro",
                          measure = "vi")
comp_3<- robinCompareFast(sgrafo_2020, method1 = "walktrap", method2 = "labelPro",
                          measure = "vi")






