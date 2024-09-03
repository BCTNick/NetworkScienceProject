
# LIBRERIE ----------------------------------------------------------------

library(readr)
library(igraph)
library(tidyverse)
library(predictrace)
library(ggraph)
library(RColorBrewer)
library(httr)
library(jsonlite)
library(tidygraph)
library(ggforce)
library(robin)


# IMPORT DEI DATI ---------------------------------------------------------

authors_final <- read_csv("data/authors_final.csv", 
                          col_names = FALSE)
edges_final <- read_csv("data/edges_final.csv", 
                        col_names = FALSE)

# PAPERS PER ANNI ---------------------------------------------------------
papers_per_year <- edges_final |> 
  distinct(X3, X4) |> 
  group_by(X3) |> 
  summarise(paper_count = n())
#GRAFICO
ggplot(papers_per_year, aes(x = factor(X3), y = paper_count, fill = factor(X3))) +
  geom_bar(stat = "identity") +
  labs(title = "Numero di articoli per anno",
       x = "Anno",
       y = "Conteggio degli articoli") +
   theme_classic() +
  scale_fill_brewer(palette = "Set3")+
  theme(legend.position = "none")


# DETERMINARE AFFILIAZIONI -------------------------------------------------------------------------
##IMPUTAZIONE MANUALE
authors_final <- authors_final  |> 
  mutate(X3 = case_when(
    row_number() == 2 ~ "Nokia Bell Labs",
    row_number() == 5 ~ "Texas A&M University",
    row_number() == 7 ~ "Qatar Computing Research Institute",
    row_number() == 20 ~ "Indiana University Bloomington",
    row_number() == 31 ~ "Bocconi University",
    row_number() == 41 ~ "University of South Florida",
    row_number() == 51 ~ "Clemson University",
    row_number() == 60 ~ "University of Colorado Boulder",
    row_number() == 70 ~ "Max Planck Institute for Human Development",
    row_number() == 80 ~ "Yale University",
    row_number() == 113 ~ "Microsoft Research India",
    row_number() == 245 ~ "Center for Terahertz Waves and College of Precision Instrument and Optoelectronics Engineering, Tianjin University",
    row_number() == 320 ~ "Harvard University",
    row_number() == 437 ~ "Institute of Applied Physics and Computational Mathematics",
    row_number() == 791 ~ "Delft University of Technology",
    row_number() == 798 ~ "Delft University of Technology",
    row_number() == 1262 ~ "Halmstad University",
    row_number() == 1521 ~ "Google Research India",
    row_number() == 1636 ~ "Harvard University",
    row_number() == 1718 ~ "Dhirubhai Ambani Institute of Information and Communication Technology",
    row_number() == 90 ~ "University of Waterloo",
    row_number() == 102 ~ "Tsinghua University",
    row_number() == 112 ~ "Indian Institute of Technology Kharagpur",
    row_number() == 122 ~ "Qatar University",
    row_number() == 133 ~ "University of Edinburgh",
    row_number() == 143 ~ "Dartmouth College",
    row_number() == 153 ~ "Stanford University",
    row_number() == 164 ~ "University of Iowa",
    row_number() == 175 ~ "EPFL",
    row_number() == 185 ~ "InferLink Corporation",
    row_number() == 200 ~ "Purdue University",
    row_number() == 210 ~ "Simon Fraser University",
    row_number() == 220 ~ "Cornell University",
    row_number() == 230 ~ "Ecole Normale Supérieure",
    row_number() == 240 ~ "Pennsylvania State University",
    row_number() == 250 ~ "VU University Amsterdam",
    row_number() == 266 ~ "Carnegie Mellon University",
    row_number() == 277 ~ "Pennsylvania State University",
    row_number() == 288 ~ "Sciences Po",
    row_number() == 298 ~ "Osaka University",
    row_number() == 311 ~ "Sabanci University",
    row_number() == 581 ~ "Shenzhen Technology University",
    row_number() == 647 ~ "University of Chinese Academy of Sciences",
    row_number() == 264 ~ "University of Chinese Academy of Sciences",
    row_number() == 721 ~ "University of the West Indies",
    row_number() == 286 ~ "Sciences Po",
    row_number() == 1241 ~ "Ruhr-University Bochum",
    row_number() == 1497 ~ "Universidade Federal de Viçosa (UFV)",
    row_number() == 1734 ~ "McMaster University",
    row_number() == 1642 ~ "University of Michigan",
    row_number() == 1659 ~ "LCR Honda IDEMITSU",
    row_number() == 1368 ~ "University of Oxford",
    row_number() == 896 ~ "Indian Institute of Technology Bombay",
    row_number() == 816 ~ "Kyoto University",
    row_number() == 949 ~ "University of Exeter",
    row_number() == 1333 ~ "Indiana University",
    row_number() == 1467 ~ "National University of Singapore",
    row_number() == 1258 ~ "University of California, Berkeley",
    row_number() == 486 ~ "University of Geneva",
    row_number() == 1341 ~ "Sabanci University",
    row_number() == 1827 ~ "University of North Carolina at Chapel Hill",
    row_number() == 1120 ~ "University of Exeter",
    row_number() == 42 ~ "University of California, Los Angeles",
    row_number() == 714 ~ "University of California, Los Angeles",
    row_number() == 309 ~ "University of Zurich",
    row_number() == 1235 ~ "Chulalongkorn University",
    row_number() == 807 ~ "University of California, Los Angeles",
    row_number() == 753 ~ "University of East Anglia",
    row_number() == 524 ~ "Kyoto University",
    row_number() == 1127 ~ "University of the West of England",
    row_number() == 1534 ~ "University of Michigan",
    row_number() == 739 ~ "Max Planck Institute for Demographic Research",
    row_number() == 681 ~ "University of Geneva",
    row_number() == 1198 ~ "University of Illinois at Urbana-Champaign",
    row_number() == 1240 ~ "Ruhr-University Bochum",
    row_number() == 1378 ~ "Harvard Medical School",
    row_number() == 1222 ~ "Imperial College London",
    TRUE ~ X3 )) |> 
  mutate(X3 = ifelse(X3 == "Qatar Computing Research institute", 
                     "Qatar Computing Research Institute", 
                     X3))


##IMPUTAZIONE ALGORITMO 
# Funzione per trovare l'ente più comune
get_most_common <- function(entities) {
  entities <- entities[entities != "None"]
  if (length(entities) == 0) {
    return("None")
  }
  
  # Contare le occorrenze di ogni ente
  entity_counts <- table(entities)
  
  # Ottenere il massimo conteggio
  max_count <- max(entity_counts)
  
  # Trovare gli enti con il massimo conteggio
  most_common_entities <- names(entity_counts[entity_counts == max_count])
  
  # Scegliere uno degli enti più comuni casualmente
  return(sample(most_common_entities, 1))
}


for (i in 1:nrow(authors_final)) {
  if (authors_final$X3[i] == "None") {
    # Codice dell'autore senza ente
    author_code <- authors_final$X1[i]
    
    # Trovare tutti i codici degli autori con cui è collegato
    connected_authors <- unique(c(edges_final$X2[edges_final$X1 == author_code],
                                  edges_final$X1[edges_final$X2 == author_code]))
    
    # Estrarre gli enti degli autori collegati
    connected_entities <- authors_final$X3[authors_final$X1 %in% connected_authors]
    
    # Assegnare l'ente più comune
    authors_final$X3[i] <- get_most_common(connected_entities)
  }
}


# ASSEGNAZIONE GENERE -----------------------------------------------------
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


# PREPARAZIONE DEI DATI ---------------------------------------------------

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


# EDGE LIST PESATA --------------------------------------------------------

weighted_edgelist <- edgelist |> ##edgelist pesata
  graph.data.frame(directed = FALSE) |> 
  as_edgelist() |> 
  as.data.frame() |> 
  group_by(V1,V2) |> 
  summarize(weight = n())

graph <- graph.data.frame(weighted_edgelist, directed = FALSE)


# SUB-GRAFO E MISURE ---------------------------------------------------------------
centrality_scores <- betweenness(graph)
nodes_to_keep <- V(graph)[centrality_scores > median(centrality_scores)]
subgraph_betweenness <- induced_subgraph(graph, nodes_to_keep)

centrality_scores <- betweenness(graph, normalized = TRUE)
centrality_threshold <- median(centrality_scores)
nodes_to_keep <- V(graph)[centrality_scores > centrality_threshold]
subgraph_betweenness <- induced_subgraph(graph, nodes_to_keep)
V(subgraph_betweenness)$label<- NA #elimino eticchetta nodi

# MISURE DESCRITTIVE  -----------------------------------------------------

V(subgraph_betweenness)
E(subgraph_betweenness)
densità<- edge_density(subgraph_betweenness)
diametro<- diameter(subgraph_betweenness)


# TOP 15 ------------------------------------------------------------------

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

#calcolo il degree di ogni nodo, servirà per la costruzione del grafico
node_degrees <- degree(subgraph_betweenness)


nodes_df <- data.frame(name = V(subgraph_betweenness)$name, 
                       degree = node_degrees)

top_nodes <- nodes_df %>% top_n(15, degree)

# CONFRONTO CLUSTERING ----------------------------------------------------

#Louvain-Walktrap
set.seed(100)
compW <- robinCompare(subgraph_betweenness, method1="louvain",
                      method2="walktrap", measure="vi")
#Louvain-LabelProp
set.seed(100)
compLa <- robinCompare(subgraph_betweenness, method1="louvain",
                       method2="labelProp", measure="vi")

##Plottiamo 
plot1<- plotRobin(subgraph_betweenness, model1=compW$Mean1, 
                  model2 = compW$Mean2,legend=c("louvain", "walktrap"))

plot2<-plotRobin(subgraph_betweenness, model1=compLa$Mean1, 
                 model2 = compLa$Mean2,legend = c("louvain", "label propagation"))

gridExtra::grid.arrange(plot1, plot2) 


# PLOT DEL SOTTOGRAFO  ----------------------------------------------------

set.seed(100)
subgraph_betweenness_cluster <- cluster_louvain(subgraph_betweenness)
autori <- membership(subgraph_betweenness_cluster)
cluster_importance <- table(autori)
top_clusters <- names(sort(cluster_importance, decreasing = TRUE))[1:10]

#assegna i colori ai cluster
c_colors <- brewer.pal(10, "Set3")
V(subgraph_betweenness)$color <- ifelse(autori %in% top_clusters, c_colors[match(autori, top_clusters)], "grey")
E(subgraph_betweenness)$color <- V(subgraph_betweenness)$color[get.edge.ids(subgraph_betweenness, t(as_edgelist(subgraph_betweenness)))]

# Calcolo dei gradi dei nodi per etichette
node_degrees <- degree(subgraph_betweenness)
top_nodes <- data.frame(name = V(subgraph_betweenness)$name, degree = node_degrees) |> 
  top_n(20, degree)

set.seed(100)
# Creazione del grafico
ggraph(subgraph_betweenness, layout = 'fr') + 
  geom_edge_link(aes(color = I(color))) + 
  geom_node_point(aes(size = node_degrees, color = I(color))) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes$name, name, '')), 
                 repel = TRUE, 
                 size = 3) + 
  theme(legend.position = 'none')


# SOTTOGRAFO PER GENERE ---------------------------------------------------
#frequenza per genere
freq_genere<-table(authors_complete$gender)

#calcolo un oggetto tidygraph
tg_genere <- as_tbl_graph(subgraph_betweenness)

# Unire i dati degli autori con il grafo
graph_genere <- tg_genere |> 
  activate(nodes) |> 
  left_join(authors_complete[,c("X2", "gender")], by = c("name" = "X2"))

set.seed(100)
# Visualizzare il grafo con ggraph
ggraph(graph_genere, layout = "fr") +
  geom_edge_link(color = "grey") +
  geom_node_point(aes(size = node_degrees, color = gender)) +
  geom_node_text(aes(label = ifelse(name %in% top_nodes$name, name, '')), 
                 repel = TRUE, 
                 size = 4,
                 color = "black") +
  scale_color_manual(values = c("male" = "#3faea8", "female" = "orchid", 
                                "NA" ="lightgrey")) +
  theme(legend.position = 'none')


# GRAFO ISTITUTI ----------------------------------------------------------

edgelist_universita <- edgelist |> 
  left_join(authors_complete, by = c("V1" = "X2"))  |> 
  rename(universita1 = X3)  |> 
  left_join(authors_complete, by = c("V2" = "X2"))  |> 
  rename(universita2 = X3)  |> 
  select(universita1, universita2) |> 
  filter(universita1 != universita2)|> 
  filter(universita1 != "None")

weighted_edgelist_uni <- edgelist_universita |> 
  graph.data.frame(directed = FALSE) |> 
  as_edgelist() |> 
  as.data.frame() |> 
  group_by(V1, V2) |> 
  summarize(weight = n())

graphU<- graph.data.frame(weighted_edgelist_uni, directed = FALSE)

##subgraph by centrality
centrality_scoresU <- betweenness(graphU)
nodes_to_keepU <- V(graphU)[centrality_scoresU > median(centrality_scoresU)]
subgraph_betweennessU <- induced_subgraph(graphU, nodes_to_keepU)

##misure
V(graphU)
E(graphU)
densitàU<- edge_density(graphU)
diametroU<- diameter(graphU)

degree_nodiU<- degree(graphU)
V(graphU)$label<-1:vcount(graphU)

degree_dat_fraU<- data.frame(node= V(graphU)$label, 
                            degree=degree_nodiU)

ordi_degree_dfU<- degree_dat_fraU[order(-degree_dat_fraU$degree),]
top_10_nodiU<-head(ordi_degree_dfU,10)
print(top_10_nodiU)

node_betweennessU <- betweenness(graphU, normalized = TRUE)

# Creazione di un data frame con i nodi e le rispettive betweenness centrality
betweenness_dfU <- data.frame(node = V(graphU)$name, 
                             betweenness = node_betweennessU)

# Selezionare i nodi con lo stesso ordine dei primi 15 nodi estratti dal grado
top_10_betweennessU <- betweenness_dfU[order(-betweenness_dfU$betweenness), ][1:10, ]


# Visualizzare i primi 15 nodi con la betweenness centrality più alta
print(top_10_betweennessU)


set.seed(100)
# Calcola il clustering per il sottografo
subgraph_betweenness_clusterU <- cluster_louvain(subgraph_betweennessU)
autoriU <- membership(subgraph_betweenness_clusterU)
cluster_importanceU <- table(autoriU)
top_clustersU <- names(sort(cluster_importanceU, decreasing = TRUE))[1:5]

#attributo colore al node in base al cluster
c_colors <- brewer.pal(10, "Set3")
V(subgraph_betweennessU)$color <- ifelse(autoriU %in% top_clustersU, c_colors[match(autoriU, top_clustersU)], "grey")
E(subgraph_betweennessU)$color <- V(subgraph_betweennessU)$color[get.edge.ids(subgraph_betweennessU, t(as_edgelist(subgraph_betweennessU)))]

#attributo cluster al node
V(subgraph_betweennessU)$cluster <- subgraph_betweenness_clusterU$membership

# Scegliere i nodi con degree piu alto
top_nodesU <- data.frame(name = V(subgraph_betweennessU)$name, degree = degree(subgraph_betweennessU)) |> 
  top_n(20, degree)

subgraph_betweennessU<-as_tbl_graph(subgraph_betweennessU)
set.seed(1000)
ggraph(subgraph_betweennessU, layout = 'nicely') + 
  geom_edge_link(aes(color = I(color))) + 
  geom_node_point(aes(size = degree(subgraph_betweennessU), color = I(color))) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodesU$name, name, ''),), 
                 repel = TRUE, 
                 size = 3) + 
  theme(legend.position = 'none')


# ############ DIVISIONE IN ANNI ############ -----------------------------


# 2018 --------------------------------------------------------------------

edgelist_2018 <- edgelist |> 
  filter( year == 2018)

#grafo GENERE 2018
graph18 <- graph_from_data_frame(edgelist_2018, directed = FALSE)
V(graph18)
E(graph18)
#misure 2018
densità_18<- edge_density(graph18)

diametro_18<- diameter(graph18)


#top 15 del 2018
degree_nodi<- degree(graph18)

V(graph18)$label<-1:vcount(graph18)

node_degree<- degree(graph18)

degree_dat_fra<- data.frame(node= V(graph18)$label, 
                            degree=node_degree)

ordi_degree_df<- degree_dat_fra[order(-degree_dat_fra$degree),]

top_10_nodi18<-head(ordi_degree_df,10)

node_betweenness <- betweenness(graph18, normalized = TRUE)

betweenness_df_18 <- data.frame(node = V(graph18)$name, 
                                betweenness = node_betweenness)

# Selezionare i nodi con lo stesso ordine dei primi 10 nodi estratti dal grado
top_10_betweenness_18 <- betweenness_df_18[order(-betweenness_df_18$betweenness), ][1:10, ]
###grafo
node_degrees <- degree(graph18)


nodes_df_18 <- data.frame(name = V(graph18)$name, 
                          degree = node_degrees)

top_nodes_18 <- nodes_df_18 %>% top_n(10, degree)


#cluster per capire gruppi di autori
set.seed(42)
community_lou_18 <- cluster_louvain(graph18)
autori_18 <- membership(community_lou_18)
cluster_importance_18 <- table(autori_18)
top_clusters_18 <- names(sort(cluster_importance_18, decreasing = TRUE))[1:17]

#assegna i colori ai cluster
c_colors <- brewer.pal(10, "Set3")
V(graph18)$color <- ifelse(autori_18 %in% top_clusters_18, c_colors[match(autori_18, top_clusters_18)], "grey")



V(graph18)$degree<- degree(graph18)

V(graph18)$label<- NA
##converto il grafo in un oggetto tidygraph 
tg18<-as_tbl_graph(graph18) #serve per detectare le communities di appartenenza
# Grafico del sottografo sulla base dei cluster (louvain), grandezza del nodo --------
ggraph(tg18, layout = "fr") + 
  geom_edge_link() + 
  geom_node_point(aes(color = I(color), size = degree)) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes_18$name, name, '')), 
                 repel = TRUE, 
                 size = 3) +
  scale_color_discrete(name = "Community") + 
  theme(legend.position = "none")

#genere anno 2018

#calcolo un oggetto tidygraph
tg_genere_18 <- as_tbl_graph(graph18)

# Unire i dati degli autori con il grafo
graph_genere_18 <- tg_genere_18 |> 
  activate(nodes) |> 
  left_join(authors_complete[,c("X2", "gender")], by = c("name" = "X2"))
# Calcola il numero di maschi e femmine
nodes_with_gender <- graph_genere_18 |> 
  activate(nodes) |> 
  as_tibble()


gender_count_18 <- nodes_with_gender |> 
  group_by(gender) |> 
  summarise(count = n())
print(gender_count_18)


set.seed(42)
# Visualizzare il grafo con ggraph
ggraph(graph_genere_18, layout = "fr") +
  geom_edge_link() +
  geom_node_point(aes(color = gender), size = degree(graph_genere_18)) +
  geom_node_text(aes(label = ifelse(name %in% top_nodes_18$name, name, '')), 
                 repel = TRUE, 
                 size = 3) +
  scale_color_manual(values = c("male" = "#3faea8", "female" = "orchid", 
                                "NA" ="lightgrey")) +
  theme(legend.position = "none") 


#grafo AFFILIAZIONI 2018

edgelist_universita18 <- edgelist |> 
  filter( year == 2018)|> 
  left_join(authors_complete, by = c("V1" = "X2"))  |> 
  rename(universita1 = X3)  |> 
  left_join(authors_complete, by = c("V2" = "X2"))  |> 
  rename(universita2 = X3)  |> 
  select(universita1, universita2) |> 
  filter(universita1 != universita2)|> 
  filter(universita1 != "None")

weighted_edgelist_uni18 <- edgelist_universita18 |> 
  graph.data.frame(directed = FALSE) |> 
  as_edgelist() |> 
  as.data.frame() |> 
  group_by(V1, V2) |> 
  summarize(weight = n())

graph18U<- graph.data.frame(weighted_edgelist_uni18, directed = FALSE)

set.seed(100)
# Calcola il clustering 
graph_betweenness_cluster18U <- cluster_louvain(graph18U)
autori18U <- membership(graph_betweenness_cluster18U)
cluster_importance18U <- table(autori18U)
top_clusters18U <- names(sort(cluster_importance18U, decreasing = TRUE))[1:10]

#attributo colore al node in base al cluster
c_colors <- brewer.pal(10, "Set3")
V(graph18U)$color <- ifelse(autori18U %in% top_clusters18U, c_colors[match(autori18U, top_clusters18U)], "grey")
E(graph18U)$color <- V(graph18U)$color[get.edge.ids(graph18U, t(as_edgelist(graph18U)))]

#attributo cluster al node
V(graph18U)$cluster <- graph_betweenness_cluster18U$membership

# Scegliere i nodi con degree piu alto
top_nodes18U <- data.frame(name = V(graph18U)$name, degree = degree(graph18U)) |> 
  top_n(20, degree)

graph18U<-as_tbl_graph(graph18U)
set.seed(1000)
ggraph(graph18U, layout = 'nicely') + 
  geom_edge_link() + 
  geom_node_point(aes(size = degree(graph18U), color = I(color))) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes18U$name, name, ''),), 
                 repel = TRUE, 
                 size = 3) + 
  theme(legend.position = 'none')


# 2019 --------------------------------------------------------------------

edgelist_2019 <- edgelist |> 
  filter( year == 2019)
#grafo 2019
graph19 <- graph.data.frame(edgelist_2019, directed = FALSE)
V(graph19)
E(graph19)
#misure 2019
densità_19<- edge_density(graph19)
diametro_19<- diameter(graph19)
#top 10 del 2019
degree_nodi<- degree(graph19)
V(graph19)$label<-1:vcount(graph19)
node_degree<- degree(graph19)
degree_dat_fra<- data.frame(node= V(graph19)$label, 
                            degree=node_degree)

ordi_degree_df<- degree_dat_fra[order(-degree_dat_fra$degree),]

top_10_nodi19<-head(ordi_degree_df,10)

node_betweenness <- betweenness(graph19, normalized = TRUE)

betweenness_df_19 <- data.frame(node = V(graph19)$name, 
                                betweenness = node_betweenness)

# Selezionare i nodi con lo stesso ordine dei primi 15 nodi estratti dal grado
top_10_betweenness_19 <- betweenness_df_19[order(-betweenness_df_19$betweenness), ][1:10, ]
###grafo
node_degrees <- degree(graph19)


nodes_df_19 <- data.frame(name = V(graph19)$name, 
                          degree = node_degrees)

top_nodes_19 <- nodes_df_19 %>% top_n(10, degree)

ggraph(graph19, layout = 'fr') +
  geom_edge_link() + 
  geom_node_point(aes(size = node_degrees)) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes_19$name, name, '')), 
                 repel = TRUE, 
                 size = 3) + 
  theme(legend.position = 'bottom')

#cluster per capire gruppi di autori
set.seed(42)

community_lou_19<- (cluster_louvain(graph19))

autori_19 <- membership(community_lou_19)

modularita_19<- modularity(community_lou_19)

##aggiungo le informazione della community ai nodi
V(graph19)$community_lou_19<- membership(community_lou_19)

V(graph19)$degree<- degree(graph19)

V(graph19)$label<- NA
##converto il grafo in un oggetto tidygraph 
tg19<-as_tbl_graph(graph19) #serve per detectare le communities di appartenenza
# Grafico del sottografo sulla base dei cluster (louvain), grandezza del nodo --------
ggraph(tg19, layout = "fr") + 
  geom_edge_link(aes(edge_alpha = 0.5), color = "black") + 
  geom_node_point(aes(color = factor(community_lou_19), size = degree)) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes_19$name, name, '')), 
                 repel = TRUE, 
                 size = 3) +
  scale_color_discrete(name = "Community") + 
  #  scale_size_continuous(range = c(3, 10)) +
  labs(title=" Sottografo con cluster Louvain anno 2019")+
  theme_graph() +
  theme(legend.position = "none")

#genere anno 2019

#calcolo un oggetto tidygraph
tg_genere_19 <- as_tbl_graph(graph19)

# Unire i dati degli autori con il grafo
graph_genere_19 <- tg_genere_19 |> 
  activate(nodes) |> 
  left_join(authors_complete[,c("X2", "gender")], by = c("name" = "X2"))

# Calcola il numero di maschi e femmine
nodes_with_gender <- graph_genere_19 |> 
  activate(nodes) |> 
  as_tibble()


gender_count_19 <- nodes_with_gender |> 
  group_by(gender) |> 
  summarise(count = n())
print(gender_count_19)

#calcolo un oggetto tidygraph
tg_genere_19 <- as_tbl_graph(graph19)

# Unire i dati degli autori con il grafo
graph_genere_19 <- tg_genere_19 |> 
  activate(nodes) |> 
  left_join(authors_complete[,c("X2", "gender")], by = c("name" = "X2"))
# Visualizzare il grafo con ggraph
ggraph(graph_genere_19, layout = "fr") +
  geom_edge_link() +
  geom_node_point(aes(color = gender), size = 5) +
  geom_node_text(aes(label = ifelse(name %in% top_nodes_19$name, name, '')), 
                 repel = TRUE, 
                 size = 3) +
  scale_color_manual(values = c("male" = "#3faea8", "female" = "orchid", 
                                "NA" ="lightgrey")) +
  theme() +
  labs(title = "Grafo con Nodi Colorati in Base al Genere 2019")


# AFFILIAZIONI 2019

edgelist_universita19U <- edgelist |> 
  filter( year == 2019)|> 
  left_join(authors_complete, by = c("V1" = "X2"))  |> 
  rename(universita1 = X3)  |> 
  left_join(authors_complete, by = c("V2" = "X2"))  |> 
  rename(universita2 = X3)  |> 
  select(universita1, universita2) |> 
  filter(universita1 != universita2)|> 
  filter(universita1 != "None")

weighted_edgelist_uni19U <- edgelist_universita19U |> 
  graph.data.frame(directed = FALSE) |> 
  as_edgelist() |> 
  as.data.frame() |> 
  group_by(V1, V2) |> 
  summarize(weight = n())

graph19U<- graph.data.frame(weighted_edgelist_uni19U, directed = FALSE)

set.seed(100)
# Calcola il clustering 
graph_betweenness_cluster19U <- cluster_louvain(graph19U)
autori19U <- membership(graph_betweenness_cluster19U)
cluster_importance19U <- table(autori19U)
top_clusters19U <- names(sort(cluster_importance19U, decreasing = TRUE))[1:10]

#attributo colore al node in base al cluster
c_colors <- brewer.pal(10, "Set3")
V(graph19U)$color <- ifelse(autori19U %in% top_clusters19U, c_colors[match(autori19U, top_clusters19U)], "grey")
E(graph19U)$color <- V(graph19U)$color[get.edge.ids(graph19U, t(as_edgelist(graph19U)))]

#attributo cluster al node
V(graph19U)$cluster <- graph_betweenness_cluster19U$membership

# Scegliere i nodi con degree piu alto
top_nodes19U <- data.frame(name = V(graph19U)$name, degree = degree(graph19U)) |> 
  top_n(20, degree)

graph19U<-as_tbl_graph(graph19U)
set.seed(1000)
ggraph(graph19U, layout = 'nicely') + 
  geom_edge_link() + 
  geom_node_point(aes(size = degree(graph19U), color = I(color))) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes19U$name, name, ''),), 
                 repel = TRUE, 
                 size = 3) + 
  theme(legend.position = 'none')


# 2020 --------------------------------------------------------------------

#grafo 2020

edgelist_2020 <- edgelist |> 
  filter( year == 2020)
#grafo 2020
graph20 <- graph.data.frame(edgelist_2020, directed = FALSE)
V(graph20)
E(graph20)
#misure 2020
densità_20<- edge_density(graph20)

diametro_20<- diameter(graph20)


#top 10 del 2020
degree_nodi<- degree(graph20)

V(graph20)$label<-1:vcount(graph20)

node_degree<- degree(graph20)

degree_dat_fra<- data.frame(node= V(graph20)$label, 
                            degree=node_degree)

ordi_degree_df<- degree_dat_fra[order(-degree_dat_fra$degree),]

top_10_nodi20<-head(ordi_degree_df,10)

node_betweenness <- betweenness(graph20, normalized = TRUE)

betweenness_df_20 <- data.frame(node = V(graph20)$name, 
                                betweenness = node_betweenness)

# Selezionare i nodi con lo stesso ordine dei primi 15 nodi estratti dal grado
top_10_betweenness_20 <- betweenness_df_20[order(-betweenness_df_20$betweenness), ][1:10, ]
###grafo
node_degrees <- degree(graph20)


nodes_df_20 <- data.frame(name = V(graph20)$name, 
                          degree = node_degrees)

top_nodes_20 <- nodes_df_20 %>% top_n(10, degree)

ggraph(graph20, layout = 'fr') +
  geom_edge_link() + 
  geom_node_point(aes(size = node_degrees)) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes_20$name, name, '')), 
                 repel = TRUE, 
                 size = 3) + 
  theme(legend.position = 'bottom')

#cluster per capire gruppi di autori
set.seed(42)



community_lou_20<- (cluster_louvain(graph20))

autori_20 <- membership(community_lou_20)

modularita_20<- modularity(community_lou_20)

##aggiungo le informazione della community ai nodi
V(graph20)$community_lou_20<- membership(community_lou_20)

V(graph20)$degree<- degree(graph20)

V(graph20)$label<- NA
##converto il grafo in un oggetto tidygraph 
tg20<-as_tbl_graph(graph20) #serve per detectare le communities di appartenenza
# Grafico del sottografo sulla base dei cluster (louvain), grandezza del nodo --------
ggraph(tg20, layout = "fr") + 
  geom_edge_link(aes(edge_alpha = 0.5), color = "black") + 
  geom_node_point(aes(color = factor(community_lou_20), size = degree)) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes_20$name, name, '')), 
                 repel = TRUE, 
                 size = 3) +
  scale_color_discrete(name = "Community") + 
  #  scale_size_continuous(range = c(3, 10)) +
  labs(title=" Sottografo con cluster Louvain anno 2020")+
  theme_graph() +
  theme(legend.position = "none")

#genere anno 2020
#calcolo un oggetto tidygraph
tg_genere_20 <- as_tbl_graph(graph20)

# Unire i dati degli autori con il grafo
graph_genere_20 <- tg_genere_20 |> 
  activate(nodes) |> 
  left_join(authors_complete[,c("X2", "gender")], by = c("name" = "X2"))

# Calcola il numero di maschi e femmine
nodes_with_gender <- graph_genere_20 |> 
  activate(nodes) |> 
  as_tibble()


gender_count_20 <- nodes_with_gender |> 
  group_by(gender) |> 
  summarise(count = n())
print(gender_count_20)


#calcolo un oggetto tidygraph
tg_genere_20 <- as_tbl_graph(graph20)

# Unire i dati degli autori con il grafo
graph_genere_20<- tg_genere_20 |> 
  activate(nodes) |> 
  left_join(authors_complete[,c("X2", "gender")], by = c("name" = "X2"))

# Visualizzare il grafo con ggraph
ggraph(graph_genere_20, layout = "fr") +
  geom_edge_link() +
  geom_node_point(aes(color = gender), size = 5) +
  geom_node_text(aes(label = ifelse(name %in% top_nodes_20$name, name, '')), 
                 repel = TRUE, 
                 size = 3) +
  scale_color_manual(values = c("male" = "#3faea8", "female" = "orchid", 
                                "NA" ="lightgrey")) +
  theme() +
  labs(title = "Grafo con Nodi Colorati in Base al Genere 2020")

#affiliazioni 2020
edgelist_universita20U <- edgelist |> 
  filter( year == 2020)|> 
  left_join(authors_complete, by = c("V1" = "X2"))  |> 
  rename(universita1 = X3)  |> 
  left_join(authors_complete, by = c("V2" = "X2"))  |> 
  rename(universita2 = X3)  |> 
  select(universita1, universita2) |> 
  filter(universita1 != universita2)|> 
  filter(universita1 != "None")

weighted_edgelist_uni20U <- edgelist_universita20U |> 
  graph.data.frame(directed = FALSE) |> 
  as_edgelist() |> 
  as.data.frame() |> 
  group_by(V1, V2) |> 
  summarize(weight = n())

graph20U<- graph.data.frame(weighted_edgelist_uni20U, directed = FALSE)

set.seed(100)
# Calcola il clustering 
graph_betweenness_cluster20U <- cluster_louvain(graph20U)
autori20U <- membership(graph_betweenness_cluster20U)
cluster_importance20U <- table(autori20U)
top_clusters20U <- names(sort(cluster_importance20U, decreasing = TRUE))[1:10]

#attributo colore al node in base al cluster
c_colors <- brewer.pal(10, "Set3")
V(graph20U)$color <- ifelse(autori20U %in% top_clusters20U, c_colors[match(autori20U, top_clusters20U)], "grey")
E(graph20U)$color <- V(graph20U)$color[get.edge.ids(graph20U, t(as_edgelist(graph20U)))]

#attributo cluster al node
V(graph20U)$cluster <- graph_betweenness_cluster20U$membership

# Scegliere i nodi con degree piu alto
top_nodes20U <- data.frame(name = V(graph20U)$name, degree = degree(graph20U)) |> 
  top_n(20, degree)

graph20U<-as_tbl_graph(graph20U)
set.seed(1000)
ggraph(graph20U, layout = 'nicely') + 
  geom_edge_link() + 
  geom_node_point(aes(size = degree(graph20U), color = I(color))) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes20U$name, name, ''),), 
                 repel = TRUE, 
                 size = 3) + 
  theme(legend.position = 'none')

# 2021 --------------------------------------------------------------------

edgelist_2021 <- edgelist |> 
  filter( year == 2021)
#grafo 2021
graph21 <- graph.data.frame(edgelist_2021, directed = FALSE)
V(graph21)
E(graph21)
#misure 2021
densità_21<- edge_density(graph21)

diametro_21<- diameter(graph21)


#top 10 del 2021
degree_nodi<- degree(graph21)

V(graph21)$label<-1:vcount(graph21)

node_degree<- degree(graph21)

degree_dat_fra<- data.frame(node= V(graph21)$label, 
                            degree=node_degree)

ordi_degree_df<- degree_dat_fra[order(-degree_dat_fra$degree),]

top_10_nodi21<-head(ordi_degree_df,10)

node_betweenness <- betweenness(graph21, normalized = TRUE)

betweenness_df_21 <- data.frame(node = V(graph21)$name, 
                                betweenness = node_betweenness)

# Selezionare i nodi con lo stesso ordine dei primi 15 nodi estratti dal grado
top_10_betweenness_21 <- betweenness_df_21[order(-betweenness_df_21$betweenness), ][1:10, ]
###grafo
node_degrees <- degree(graph21)


nodes_df_21 <- data.frame(name = V(graph21)$name, 
                          degree = node_degrees)

top_nodes_21 <- nodes_df_21 %>% top_n(10, degree)

#cluster per capire gruppi di autori
set.seed(42)
community_lou_21 <- cluster_louvain(graph21)
autori_21 <- membership(community_lou_21)
cluster_importance_21 <- table(autori_21)
top_clusters_21 <- names(sort(cluster_importance_21, decreasing = TRUE))[1:17]

#assegna i colori ai cluster
c_colors <- brewer.pal(10, "Set3")
V(graph21)$color <- ifelse(autori_21 %in% top_clusters_21, c_colors[match(autori_21, top_clusters_21)], "grey")



V(graph21)$degree<- degree(graph21)

V(graph21)$label<- NA
##converto il grafo in un oggetto tidygraph
tg21<-as_tbl_graph(graph21) #serve per detectare le communities di appartenenza
# Grafico del sottografo sulla base dei cluster (louvain), grandezza del nodo --------
ggraph(tg21, layout = "fr") +
  geom_edge_link() +
  geom_node_point(aes(color = I(color), size = degree)) +
  geom_node_text(aes(label = ifelse(name %in% top_nodes_18$name, name, '')),
                 repel = TRUE,
                 size = 3) +
  scale_color_discrete(name = "Community") +
  theme(legend.position = "none")


#genere anno 2021

#calcolo un oggetto tidygraph
tg_genere_21 <- as_tbl_graph(graph21)

# Unire i dati degli autori con il grafo
graph_genere_21 <- tg_genere_21 |> 
  activate(nodes) |> 
  left_join(authors_complete[,c("X2", "gender")], by = c("name" = "X2"))
# Calcola il numero di maschi e femmine
nodes_with_gender <- graph_genere_21 |> 
  activate(nodes) |> 
  as_tibble()


gender_count_21 <- nodes_with_gender |> 
  group_by(gender) |> 
  summarise(count = n())
print(gender_count_21)


set.seed(42)
# Visualizzare il grafo con ggraph
ggraph(graph_genere_21, layout = "fr") +
  geom_edge_link() +
  geom_node_point(aes(color = gender), size = degree(graph_genere_21)) +
  geom_node_text(aes(label = ifelse(name %in% top_nodes_21$name, name, '')), 
                 repel = TRUE, 
                 size = 3) +
  scale_color_manual(values = c("male" = "#3faea8", "female" = "orchid", 
                                "NA" ="lightgrey")) +
  theme(legend.position = "none")


#AFFILIAZIONI 2021

edgelist_universita21U <- edgelist |> 
  filter( year == 2021)|> 
  left_join(authors_complete, by = c("V1" = "X2"))  |> 
  rename(universita1 = X3)  |> 
  left_join(authors_complete, by = c("V2" = "X2"))  |> 
  rename(universita2 = X3)  |> 
  select(universita1, universita2) |> 
  filter(universita1 != universita2)|> 
  filter(universita1 != "None")

weighted_edgelist_uni21U <- edgelist_universita21U |> 
  graph.data.frame(directed = FALSE) |> 
  as_edgelist() |> 
  as.data.frame() |> 
  group_by(V1, V2) |> 
  summarize(weight = n())

graph21U<- graph.data.frame(weighted_edgelist_uni21U, directed = FALSE)

set.seed(100)
# Calcola il clustering 
graph_betweenness_cluster21U <- cluster_louvain(graph21U)
autori21U <- membership(graph_betweenness_cluster21U)
cluster_importance21U <- table(autori21U)
top_clusters21U <- names(sort(cluster_importance21U, decreasing = TRUE))[1:10]

#attributo colore al node in base al cluster
c_colors <- brewer.pal(10, "Set3")
V(graph21U)$color <- ifelse(autori21U %in% top_clusters21U, c_colors[match(autori21U, top_clusters21U)], "grey")
E(graph21U)$color <- V(graph21U)$color[get.edge.ids(graph21U, t(as_edgelist(graph21U)))]

#attributo cluster al node
V(graph21U)$cluster <- graph_betweenness_cluster21U$membership

# Scegliere i nodi con degree piu alto
top_nodes21U <- data.frame(name = V(graph21U)$name, degree = degree(graph21U)) |> 
  top_n(20, degree)

graph21U<-as_tbl_graph(graph21U)
set.seed(1000)
ggraph(graph21U, layout = 'nicely') + 
  geom_edge_link() + 
  geom_node_point(aes(size = degree(graph21U), color = I(color))) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes21U$name, name, ''),), 
                 repel = TRUE, 
                 size = 3) + 
  theme(legend.position = 'none')

# 2022 --------------------------------------------------------------------

edgelist_2022 <- edgelist |> 
  filter( year == 2022)
#grafo 2022
graph22 <- graph.data.frame(edgelist_2022, directed = FALSE)
V(graph22)
E(graph22)
#misure 2022
densità_22<- edge_density(graph22)

diametro_22<- diameter(graph22)


#top 10 del 2022
degree_nodi<- degree(graph22)

V(graph22)$label<-1:vcount(graph22)

node_degree<- degree(graph22)

degree_dat_fra<- data.frame(node= V(graph22)$label, 
                            degree=node_degree)

ordi_degree_df<- degree_dat_fra[order(-degree_dat_fra$degree),]

top_10_nodi22<-head(ordi_degree_df,10)

node_betweenness <- betweenness(graph22, normalized = TRUE)

betweenness_df_22 <- data.frame(node = V(graph22)$name, 
                                betweenness = node_betweenness)

# Selezionare i nodi con lo stesso ordine dei primi 15 nodi estratti dal grado
top_10_betweenness_22 <- betweenness_df_22[order(-betweenness_df_22$betweenness), ][1:10, ]
###grafo
node_degrees <- degree(graph22)


nodes_df_22 <- data.frame(name = V(graph22)$name, 
                          degree = node_degrees)

top_nodes_22 <- nodes_df_22 %>% top_n(10, degree)

ggraph(graph22, layout = 'fr') +
  geom_edge_link() + 
  geom_node_point(aes(size = node_degrees)) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes_22$name, name, '')), 
                 repel = TRUE, 
                 size = 3) + 
  theme(legend.position = 'bottom')

#cluster per capire gruppi di autori
set.seed(42)

community_lou_22<- (cluster_louvain(graph22))

autori_22 <- membership(community_lou_22)

modularita_22<- modularity(community_lou_22)

##aggiungo le informazione della community ai nodi
V(graph22)$community_lou_22<- membership(community_lou_22)

V(graph22)$degree<- degree(graph22)

V(graph22)$label<- NA
##converto il grafo in un oggetto tidygraph 
tg22<-as_tbl_graph(graph22) #serve per detectare le communities di appartenenza
# Grafico del sottografo sulla base dei cluster (louvain), grandezza del nodo --------
ggraph(tg22, layout = "fr") + 
  geom_edge_link(aes(edge_alpha = 0.5), color = "black") + 
  geom_node_point(aes(color = factor(community_lou_22), size = degree)) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes_22$name, name, '')), 
                 repel = TRUE, 
                 size = 3) +
  scale_color_discrete(name = "Community") + 
  #  scale_size_continuous(range = c(3, 10)) +
  labs(title=" Sottografo con cluster Louvain anno 2022")+
  theme_graph() +
  theme(legend.position = "none")

#genere anno 2022
#calcolo un oggetto tidygraph
tg_genere_22 <- as_tbl_graph(graph22)

# Unire i dati degli autori con il grafo
graph_genere_22 <- tg_genere_22 |> 
  activate(nodes) |> 
  left_join(authors_complete[,c("X2", "gender")], by = c("name" = "X2"))

# Calcola il numero di maschi e femmine
nodes_with_gender <- graph_genere_22 |> 
  activate(nodes) |> 
  as_tibble()


gender_count_22 <- nodes_with_gender |> 
  group_by(gender) |> 
  summarise(count = n())
print(gender_count_22)


#calcolo un oggetto tidygraph
tg_genere_22 <- as_tbl_graph(graph22)

# Unire i dati degli autori con il grafo
graph_genere_22 <- tg_genere_22 |> 
  activate(nodes) |> 
  left_join(authors_complete[,c("X2", "gender")], by = c("name" = "X2"))

# Visualizzare il grafo con ggraph
ggraph(graph_genere_22, layout = "fr") +
  geom_edge_link() +
  geom_node_point(aes(color = gender), size = 5) +
  geom_node_text(aes(label = ifelse(name %in% top_nodes_22$name, name, '')), 
                 repel = TRUE, 
                 size = 3) +
  scale_color_manual(values = c("male" = "#3faea8", "female" = "orchid", 
                                "NA" ="lightgrey")) +
  theme() +
  labs(title = "Grafo con Nodi Colorati in Base al Genere 2022")

#AFFILIAZIONI 2022
edgelist_universita22U <- edgelist |> 
  filter( year == 2022)|> 
  left_join(authors_complete, by = c("V1" = "X2"))  |> 
  rename(universita1 = X3)  |> 
  left_join(authors_complete, by = c("V2" = "X2"))  |> 
  rename(universita2 = X3)  |> 
  select(universita1, universita2) |> 
  filter(universita1 != universita2)|> 
  filter(universita1 != "None")

weighted_edgelist_uni22U <- edgelist_universita22U |> 
  graph.data.frame(directed = FALSE) |> 
  as_edgelist() |> 
  as.data.frame() |> 
  group_by(V1, V2) |> 
  summarize(weight = n())

graph22U<- graph.data.frame(weighted_edgelist_uni22U, directed = FALSE)

set.seed(100)
# Calcola il clustering 
graph_betweenness_cluster22U <- cluster_louvain(graph22U)
autori22U <- membership(graph_betweenness_cluster22U)
cluster_importance22U <- table(autori22U)
top_clusters22U <- names(sort(cluster_importance22U, decreasing = TRUE))[1:10]

#attributo colore al node in base al cluster
c_colors <- brewer.pal(10, "Set3")
V(graph22U)$color <- ifelse(autori22U %in% top_clusters22U, c_colors[match(autori22U, top_clusters22U)], "grey")
E(graph22U)$color <- V(graph22U)$color[get.edge.ids(graph22U, t(as_edgelist(graph22U)))]

#attributo cluster al node
V(graph22U)$cluster <- graph_betweenness_cluster22U$membership

# Scegliere i nodi con degree piu alto
top_nodes22U <- data.frame(name = V(graph22U)$name, degree = degree(graph22U)) |> 
  top_n(20, degree)

graph22U<-as_tbl_graph(graph22U)
set.seed(1000)
ggraph(graph22U, layout = 'nicely') + 
  geom_edge_link() + 
  geom_node_point(aes(size = degree(graph22U), color = I(color))) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes22U$name, name, ''),), 
                 repel = TRUE, 
                 size = 3) + 
  theme(legend.position = 'none')

# 2023 --------------------------------------------------------------------

edgelist_2023 <- edgelist |> 
  filter( year == 2023)
#grafo 2023
graph23 <- graph.data.frame(edgelist_2023, directed = FALSE)
V(graph23)
E(graph23)
#misure 2023
densità_23<- edge_density(graph23)

diametro_23<- diameter(graph23)


#top 10 del 2023
degree_nodi<- degree(graph23)

V(graph23)$label<-1:vcount(graph23)

node_degree<- degree(graph23)

degree_dat_fra<- data.frame(node= V(graph23)$label, 
                            degree=node_degree)

ordi_degree_df<- degree_dat_fra[order(-degree_dat_fra$degree),]

top_10_nodi23<-head(ordi_degree_df,10)

node_betweenness <- betweenness(graph23, normalized = TRUE)

betweenness_df_23 <- data.frame(node = V(graph23)$name, 
                                betweenness = node_betweenness)

# Selezionare i nodi con lo stesso ordine dei primi 15 nodi estratti dal grado
top_10_betweenness_23 <- betweenness_df_23[order(-betweenness_df_23$betweenness), ][1:10, ]
###grafo
node_degrees <- degree(graph23)


nodes_df_23 <- data.frame(name = V(graph23)$name, 
                          degree = node_degrees)

top_nodes_23 <- nodes_df_23 %>% top_n(10, degree)


#cluster per capire gruppi di autori
set.seed(42)
community_lou_23 <- cluster_louvain(graph23)
autori_23 <- membership(community_lou_23)
cluster_importance_23 <- table(autori_23)
top_clusters_23 <- names(sort(cluster_importance_23, decreasing = TRUE))[1:17]

#assegna i colori ai cluster
c_colors <- brewer.pal(10, "Set3")
V(graph23)$color <- ifelse(autori_23 %in% top_clusters_23, c_colors[match(autori_23, top_clusters_23)], "grey")



V(graph23)$degree<- degree(graph23)

V(graph23)$label<- NA
##converto il grafo in un oggetto tidygraph
tg23<-as_tbl_graph(graph23) #serve per detectare le communities di appartenenza
# Grafico del sottografo sulla base dei cluster (louvain), grandezza del nodo --------
ggraph(tg23, layout = "fr") +
  geom_edge_link() +
  geom_node_point(aes(color = I(color), size = degree)) +
  geom_node_text(aes(label = ifelse(name %in% top_nodes_18$name, name, '')),
                 repel = TRUE,
                 size = 3) +
  scale_color_discrete(name = "Community") +
  theme(legend.position = "none")


#genere anno 2023

#calcolo un oggetto tidygraph
tg_genere_23 <- as_tbl_graph(graph23)

# Unire i dati degli autori con il grafo
graph_genere_23 <- tg_genere_23 |> 
  activate(nodes) |> 
  left_join(authors_complete[,c("X2", "gender")], by = c("name" = "X2"))
# Calcola il numero di maschi e femmine
nodes_with_gender <- graph_genere_23 |> 
  activate(nodes) |> 
  as_tibble()


gender_count_23 <- nodes_with_gender |> 
  group_by(gender) |> 
  summarise(count = n())
print(gender_count_23)


set.seed(42)
# Visualizzare il grafo con ggraph
ggraph(graph_genere_23, layout = "fr") +
  geom_edge_link() +
  geom_node_point(aes(color = gender), size = degree(graph_genere_23)) +
  geom_node_text(aes(label = ifelse(name %in% top_nodes_23$name, name, '')), 
                 repel = TRUE, 
                 size = 3) +
  scale_color_manual(values = c("male" = "#3faea8", "female" = "orchid", 
                                "NA" ="lightgrey")) +
  theme(legend.position = "none")

#AFFILIAZIONI 2023

edgelist_universita23U <- edgelist |> 
  filter( year == 2023)|> 
  left_join(authors_complete, by = c("V1" = "X2"))  |> 
  rename(universita1 = X3)  |> 
  left_join(authors_complete, by = c("V2" = "X2"))  |> 
  rename(universita2 = X3)  |> 
  select(universita1, universita2) |> 
  filter(universita1 != universita2)|> 
  filter(universita1 != "None")

weighted_edgelist_uni23U <- edgelist_universita23U |> 
  graph.data.frame(directed = FALSE) |> 
  as_edgelist() |> 
  as.data.frame() |> 
  group_by(V1, V2) |> 
  summarize(weight = n())

graph23U<- graph.data.frame(weighted_edgelist_uni23U, directed = FALSE)

set.seed(100)
# Calcola il clustering 
graph_betweenness_cluster23U <- cluster_louvain(graph23U)
autori23U <- membership(graph_betweenness_cluster23U)
cluster_importance23U <- table(autori23U)
top_clusters23U <- names(sort(cluster_importance23U, decreasing = TRUE))[1:10]

#attributo colore al node in base al cluster
c_colors <- brewer.pal(10, "Set3")
V(graph23U)$color <- ifelse(autori23U %in% top_clusters23U, c_colors[match(autori23U, top_clusters23U)], "grey")
E(graph23U)$color <- V(graph23U)$color[get.edge.ids(graph23U, t(as_edgelist(graph23U)))]

#attributo cluster al node
V(graph23U)$cluster <- graph_betweenness_cluster23U$membership

# Scegliere i nodi con degree piu alto
top_nodes23U <- data.frame(name = V(graph23U)$name, degree = degree(graph23U)) |> 
  top_n(20, degree)

graph23U<-as_tbl_graph(graph23U)
set.seed(1000)
ggraph(graph23U, layout = 'nicely') + 
  geom_edge_link() + 
  geom_node_point(aes(size = degree(graph23U), color = I(color))) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes23U$name, name, ''),), 
                 repel = TRUE, 
                 size = 3) + 
  theme(legend.position = 'none')

############ FINE #########