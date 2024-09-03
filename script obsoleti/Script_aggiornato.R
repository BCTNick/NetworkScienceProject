# Librerie ----------------------------------------------------------------
library(readr)
library(igraph)
library(tidyverse)
library(predictrace)
library(ggraph)
library(RColorBrewer)
library(tidygraph)
library(robin)
library(dplyr)

# Importazione ------------------------------------------------------------

authors_final <- read_csv("data/authors_final.csv", 
                          col_names = FALSE)
edges_final <- read_csv("data/edges_final.csv", 
                        col_names = FALSE)

# Pre-analisi -------------------------------------------------------------

papers_per_year <- edges_final |> ##capire quanti paper sono stati scritti nei singoli anni
  distinct(X3, X4) |> 
  group_by(X3) |> 
  summarise(paper_count = n())

# Predire genere ----------------------------------------------------------

gender <- predict_gender(word(authors_final$X2,1)) 
gender_copia <- gender
sum(is.na(gender$likely_gender))

na_primi <- c()

for (i in 1:nrow(authors_final)) {
  if (is.na(gender$likely_gender[i])) {
    # Se likely_gender è NA, applica predict_gender sulla seconda parola
    gender[i,] <- predict_gender(word(authors_final$X2[i], 2))
    na_primi <- c(na_primi, i)
  }
}
authors_complete <- cbind(authors_final, gender[3]) |> 
  rename(gender = likely_gender)


# Pulizia -----------------------------------------------------------------

just_names <- authors_final[, c("X1", "X2")] ##teniamo solo i nomi degli autori

edgelist <- edges_final |> 
  rename(V1 = X1,
         V2 = X2,
         year = X3,
         paper_id = X4) |> ##rinomina le colonne
  
  merge(just_names, by.x = "V1", by.y = "X1") |> 
  select(X2, V2, year, paper_id) |> 
  rename(V1 = X2) |> ##sostituiamo i nomi del vertice1
  
  merge(just_names, by.x = "V2", by.y = "X1") |> 
  select(V1, X2, year, paper_id) |> 
  rename(V2 = X2) ##sostituiamo i nomi del vertice2



# Sottografo complessivo basato sulla centralità -------------------------------

weighted_edgelist <- edgelist |> 
  graph.data.frame(directed = FALSE) |> 
  as_edgelist() |> 
  as.data.frame() |> 
  group_by(V1,V2) |> 
  summarize(weight = n())

graph <- graph.data.frame(weighted_edgelist, directed = FALSE)
V(graph)

##subgraph by centrality

centrality_scores <- betweenness(graph, normalized = TRUE)

centrality_threshold <- median(centrality_scores)

nodes_to_keep <- V(graph)[centrality_scores > centrality_threshold]

subgraph_betweenness <- induced_subgraph(graph, nodes_to_keep)

V(subgraph_betweenness)$label<- NA #elimino eticchetta nodi

V(subgraph_betweenness)

# Misure descrittive del sottografo---------------------------------------------

V(subgraph_betweenness)

E(subgraph_betweenness)

densità<- edge_density(subgraph_betweenness)

diametro<- diameter(subgraph_betweenness)

# Misure descrittive top 15 -----------------------------------------------

degree_nodi<- degree(subgraph_betweenness)

V(subgraph_betweenness)$label<-1:vcount(subgraph_betweenness)

node_degree<- degree(subgraph_betweenness)

degree_dat_fra<- data.frame(node= V(subgraph_betweenness)$label, 
                            degree=node_degree)

ordi_degree_df<- degree_dat_fra[order(-degree_dat_fra$degree),]

top_15_nodi<-head(ordi_degree_df,15)

print(top_15_nodi)


# Calcolo della betweenness centrality di ogni nodo
node_betweenness <- betweenness(subgraph_betweenness, normalized = TRUE)

# Creazione di un data frame con i nodi e le rispettive betweenness centrality
betweenness_df <- data.frame(node = V(subgraph_betweenness)$name, 
                             betweenness = node_betweenness)

# Selezionare i nodi con lo stesso ordine dei primi 15 nodi estratti dal grado
top_15_betweenness <- betweenness_df[order(-betweenness_df$betweenness), ][1:15, ]

# Visualizzare i primi 15 nodi con la betweenness centrality più alta
print(top_15_betweenness)

# Plot sottografo ---------------------------------------------------------
#sottogrfo genereale prima dell'identificazione dei cluster, si puo intravedere una struttura
#da esplorare. Idee per esplorare: distribuzione del genere, cambio negli anni.

ggraph(subgraph_betweenness, layout = 'nicely') +
  geom_edge_link() + 
  geom_node_point(aes(size = node_degree)) + 
  geom_node_text(aes(label = ifelse(name %in% top_15_nodi$name, name, '')), 
                 repel = TRUE, 
                 size = 3) + 
  theme(legend.position = 'bottom')


# Sottografo per genere ---------------------------------------------------
set.seed(42)

#frequenza per genere
freq_genere<-table(authors_complete$gender)

#calcolo un oggetto tidygraph
tg_genere <- as_tbl_graph(subgraph_betweenness)

# Unire i dati degli autori con il grafo
graph_genere <- tg_genere |> 
  activate(nodes) |> 
  left_join(authors_complete[,c("X2", "gender")], by = c("name" = "X2"))

# Visualizzare il grafo con ggraph
ggraph(graph_genere, layout = "fr") +
  geom_edge_link() +
  geom_node_point(aes(color = gender), size = 5) +
  geom_node_text(aes(label = ifelse(name %in% top_nodes$name, name, '')), 
                 repel = TRUE, 
                 size = 3) +
  scale_color_manual(values = c("male" = "#3faea8", "female" = "orchid", 
                                "NA" ="lightgrey")) +
  theme() +
  labs(title = "Grafo con Nodi Colorati in Base al Genere")



# Cluster con metodo Louvain ----------------------------------------------
set.seed(42)

layout<-layout_with_kk(subgraph_betweenness)

community_lou<- (cluster_louvain(subgraph_betweenness))

autori <- membership(community_lou)

modularita<- modularity(community_lou)

##aggiungo le informazione della community ai nodi
V(subgraph_betweenness)$community_lou<- membership(community_lou)

V(subgraph_betweenness)$degree<- degree(subgraph_betweenness)

V(subgraph_betweenness)$label<- NA

#calcolo il degree di ogni nodo, servirà per la costruzione del grafico
node_degrees <- degree(subgraph_betweenness)

nodes_df <- data.frame(name = V(subgraph_betweenness)$name, 
                       degree = node_degrees)

top_nodes <- nodes_df %>% top_n(15, degree)

##converto il grafo in un oggetto tidygraph 
tg<-as_tbl_graph(subgraph_betweenness) #serve per detectare le communities di appartenenza

##visualizzo cluster
#plot(subgraph_betweenness, vertex.color=membership(community_lou), 
     #vertex.size=8,vertex.label.cex=0.3)


# Grafico del sottografo sulla base dei cluster (louvain), grandezza del nodo --------
ggraph(tg, layout = "fr") + 
  geom_edge_link(aes(edge_alpha = 0.5), color = "black") + 
  geom_node_point(aes(color = factor(community_lou), size = degree)) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes$name, name, '')), 
                 repel = TRUE, 
                 size = 3) +
  scale_color_discrete(name = "Community") + 
  #  scale_size_continuous(range = c(3, 10)) +
  labs(title=" Sottografo con cluster Louvain")+
  theme_graph() +
  theme(legend.position = "none")

# Cluster con metodo Walktrap ---------------------------------------------
set.seed(42)

layout<-layout_with_kk(subgraph_betweenness)

community_wk<- cluster_walktrap(subgraph_betweenness)

##aggiungo le informazione della community ai nodi
V(subgraph_betweenness)$community_wk<- membership(community_wk)

V(subgraph_betweenness)$degree<- degree(subgraph_betweenness)

V(subgraph_betweenness)$label<- NA

##converto il grafo in un oggetto tidygraph 
tg<-as_tbl_graph(subgraph_betweenness) #serve per detectare le communities di appartenenza

##visualizzo cluster
plot(subgraph_betweenness, vertex.color=membership(community_wk), 
     vertex.size=8,vertex.label.cex=0.3)


# Grafico grafo sulla base dei cluster walktrap ---------------------------

ggraph(tg, layout = "kk")+ 
  geom_edge_link(aes(edge_alpha = 0.5), color = "black") + 
  geom_node_point(aes(color = factor(community_wk), size = degree)) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes$name, name, '')), 
                 repel = TRUE, 
                 size = 3) +
  scale_color_discrete(name = "Community") + 
  #  scale_size_continuous(range = c(3, 10)) +
  labs(title=" Sottografo con cluster Walktrap")+
  theme_graph() +
  theme(legend.position = "none") 
# Cluster metodo label propagation ----------------------------------------
set.seed(42)

layout<-layout_with_kk(subgraph_betweenness)

community_lp<- cluster_label_prop(subgraph_betweenness)

##aggiungo le informazione della community ai nodi
V(subgraph_betweenness)$community_lp<- membership(community_lp)

V(subgraph_betweenness)$degree<- degree(subgraph_betweenness)

V(subgraph_betweenness)$label<- NA

##converto il grafo in un oggetto tidygraph 
tg<-as_tbl_graph(subgraph_betweenness)

##visualizzo cluster
plot(subgraph_betweenness, vertex.color=membership(community_lp),
     vertex.size=8,vertex.label.cex=0.3)

# Grafico grafo sulla base dei cluster label propagation ------------------

ggraph(tg, layout = "fr") + 
  geom_edge_link(aes(edge_alpha = 0.5), color = "black") + 
  geom_node_point(aes(color = factor(community_lp), size = degree)) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes$name, name, '')), 
                 repel = TRUE, 
                 size = 3) +
  scale_color_discrete(name = "Community") + 
  #  scale_size_continuous(range = c(3, 10)) +
  labs(title=" Sottografo con cluster Label Propagation")+
  theme_graph() +
  theme(legend.position = "none") 



# Confronto metodi cluster ------------------------------------------------
library(robin)
#Walktrap
set.seed(100)

compW <- robinCompare(subgraph_betweenness, method1="louvain",
                          method2="walktrap", measure="vi")
#LabelProp
set.seed(100)

compLa <- robinCompare(subgraph_betweenness, method1="louvain",
                           method2="labelProp", measure="vi")
#Louvain
set.seed(100)

compLo<- robinCompare(subgraph_betweenness, method1 = "walktrap",
                          method2 = "louvain", measure="vi")

##Plottiamo 
plot1<- plotRobin(subgraph_betweenness, model1=compW$Mean1, 
                  model2 = compW$Mean2,legend=c("louvain", "walktrap"))

plot2<-plotRobin(subgraph_betweenness, model1=compLa$Mean1, 
                 model2 = compLa$Mean2,legend = c("louvain", "label propagation"))

plot3<- plotRobin(subgraph_betweenness, model1 = compLo$Mean1,
                  model2 = compLo$Mean2, legend = c("walktrap", "louvain"))

gridExtra::grid.arrange(plot1, plot2, plot3)  

# Estrazione componenti ---------------------------------------------------

## calcolo le componenti massime sul sottografo iniziale
componenti<- components(subgraph_betweenness)
componenti$membership #estraggo i membri

#estraggo la componente più grande
comp_1<-largest_component(subgraph_betweenness, mode = c("weak", "strong"))

#verifico la sua connessione
connessione<-is_connected(comp_1)

#conto tutte le componenti presenti nel sottografo

num_comp<-count_components(subgraph_betweenness, mode = c("weak", "strong"))

#estraggo i membri della prima componente
membri_comp1<-components(comp_1)$membership 

densità_comp1<-edge_density(comp_1,loops = FALSE)

degree_comp1<- degree(comp_1)

degree_max1<- which.max(degree_comp1)

degree_min1<- which.min(degree_comp1)

diametro_comp1<-diameter(comp_1)

#transitività_comp<- transitivity(comp_1)

betw_comp1<- betweenness(comp_1, normalized = TRUE)

betw_comp1_max<- which.max(betw_comp1)

betw_comp1_min<- which.min(betw_comp1)


#verifico se ci sono legami multipli
any_multiple(comp_1)

#verifico se ci sono self_loop
any_loop(comp_1)

#controllo i vertici tagliati
vertici_tagliati<-articulation.points(comp_1)

#verifico se all'interno ci sono delle biconnessioni
is_biconnected(comp_1)

#cluster sulla prima componente uso Louvain
layout_with_kk(comp_1)
cl_comp1<- cluster_louvain(comp_1)

membri_cl<-membership(cl_comp1)           

clustercomp_importance <- table(membri_cl)

top_cluster_comp <- names(sort(clustercomp_importance, decreasing = TRUE))[1:15]

#assegno i colori dei cluster sulla base della loro importanza
c_colors <- brewer.pal(10, "Set3")

V(comp_1)$color <- ifelse(membri_cl %in% top_cluster_comp, 
                        c_colors[match(membri_cl, top_cluster_comp)], "red")

E(comp_1)$color <- V(comp_1)$color[get.edge.ids(comp_1, t(as_edgelist(comp_1)))]

plot(comp_1, vertex.label = V(comp_1)$label, vertex.size = 10, 
     main = "Componente 1")

#effettuo plot dei cluster della prima componente
plot(cl_comp1, comp_1, main="Cluster prima comp")

ggraph(tg, layout = "kk") + 
  geom_edge_link(aes(edge_alpha = 0.5), color = "black") + 
  geom_node_point(aes(color = factor(cl_comp1), size = degree)) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes$name, name, '')), 
                 repel = TRUE, 
                 size = 3) +
  scale_color_discrete(name = "Community") + 
  #  scale_size_continuous(range = c(3, 10)) +
  labs(title=" Sottografo con cluster Louvain")+
  theme_graph() +
  theme(legend.position = "none")
#per estrarre le altre componenti
#estrazione<-lapply(seq_along(componenti$csize) [componenti$csize==18], 
                  function(x) V(subgraph_betweenness)$name 
                   [componenti$membership %in% x])

estrazione_2<- decompose(subgraph_betweenness, mode=c("weak", "strong"), 
                               max.comps = , min.vertices = 0)
#unlist
comp2_u<-unlist(comp_2)
#verifico la sua connessione
connessione_2<-is_connected(comp2_u)

#estraggo i membri della prima componente
membri_comp2<-components(comp_2)$membership 

densità_comp1<-edge_density(comp_1,loops = FALSE)

degree_comp1<- degree(comp_1)

degree_max1<- which.max(degree_comp1)

degree_min1<- which.min(degree_comp1)

diametro_comp1<-diameter(comp_1)



