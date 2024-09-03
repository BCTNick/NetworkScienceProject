## @knitr librerie ---------------------------------------------------------

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


## @knitr importazione-------------------------------------------------------------------------

authors_final <- read_csv("data/authors_final.csv", 
                          col_names = FALSE)
edges_final <- read_csv("data/edges_final.csv", 
                        col_names = FALSE)


## @knitr imputazione università a mano -------------------------------------------

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


## @kintr determinare_genere -----------------------------------------------------

gender <- predict_gender(word(authors_final$X2,1)) 

# Se likely_gender è NA, applica predict_gender sulla seconda parola
for (i in 1:nrow(authors_final)) {
  if (is.na(gender$likely_gender[i])) {
    gender[i,] <- predict_gender(word(authors_final$X2[i], 2))
  }
}

#aggiungi il genere alla lista autori
authors_complete <- cbind(authors_final, gender[3]) |> 
  rename(gender = likely_gender) 


## @knitr determinare uni ------------------------------------------

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


for (i in 1:nrow(authors_complete)) {
  if (authors_complete$X3[i] == "None") {
    # Codice dell'autore senza ente
    author_code <- authors_complete$X1[i]
    
    # Trovare tutti i codici degli autori con cui è collegato
    connected_authors <- unique(c(edges_final$X2[edges_final$X1 == author_code],
                                  edges_final$X1[edges_final$X2 == author_code]))
    
    # Estrarre gli enti degli autori collegati
    connected_entities <- authors_complete$X3[authors_complete$X1 %in% connected_authors]
    
    # Assegnare l'ente più comune
    authors_complete$X3[i] <- get_most_common(connected_entities)
  }
}

## @knitr pulizia -------------------------------------------------------------------------

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


## @knitr analisi del grafo-------------------------------------------------------------

papers_per_year <- edges_final |> ##capire quanti paper sono stati scritti nei singoli anni
  distinct(X3, X4) |> 
  group_by(X3) |> 
  summarise(paper_count = n())

weighted_edgelist <- edgelist |> ##edgelist pesata
  graph.data.frame(directed = FALSE) |> 
  as_edgelist() |> 
  as.data.frame() |> 
  group_by(V1,V2) |> 
  summarize(weight = n())

graph <- graph.data.frame(weighted_edgelist, directed = FALSE)


## @knitr subgraph by centrality
centrality_scores <- betweenness(graph)
nodes_to_keep <- V(graph)[centrality_scores > median(centrality_scores)]
subgraph_betweenness <- induced_subgraph(graph, nodes_to_keep)

## @knitr Confronto metodi cluster ------------------------------------------------

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


## @knitr plot del sottografo con louvain ---------------------------------
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


## @knitr Sottografo per genere ---------------------------------------------------

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

## @knitr grafo università --------------------------------------------------------

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

graph <- graph.data.frame(weighted_edgelist_uni, directed = FALSE)

##subgraph by centrality
centrality_scores <- betweenness(graph)
nodes_to_keep <- V(graph)[centrality_scores > median(centrality_scores)]
subgraph_betweenness <- induced_subgraph(graph, nodes_to_keep)


set.seed(100)
# Calcola il clustering per il sottografo
subgraph_betweenness_cluster <- cluster_louvain(subgraph_betweenness)
autori <- membership(subgraph_betweenness_cluster)
cluster_importance <- table(autori)
top_clusters <- names(sort(cluster_importance, decreasing = TRUE))[1:5]

#attributo colore al node in base al cluster
c_colors <- brewer.pal(10, "Set3")
V(subgraph_betweenness)$color <- ifelse(autori %in% top_clusters, c_colors[match(autori, top_clusters)], "grey")
E(subgraph_betweenness)$color <- V(subgraph_betweenness)$color[get.edge.ids(subgraph_betweenness, t(as_edgelist(subgraph_betweenness)))]

#attributo cluster al node
V(subgraph_betweenness)$cluster <- subgraph_betweenness_cluster$membership

# Scegliere i nodi con degree piu alto
top_nodes <- data.frame(name = V(subgraph_betweenness)$name, degree = degree(subgraph_betweenness)) |> 
  top_n(20, degree)

subgraph_betweenness<-as_tbl_graph(subgraph_betweenness)
set.seed(1000)
ggraph(subgraph_betweenness, layout = 'nicely') + 
  geom_edge_link(aes(color = I(color))) + 
  geom_node_point(aes(size = degree(subgraph_betweenness), color = I(color))) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes$name, name, ''),), 
                 repel = TRUE, 
                 size = 3) + 
  theme(legend.position = 'none')

## @knitr grafo università 2018 --------------------------------------------------------

edgelist_universita <- edgelist |> 
  filter( year == 2018)|> 
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

graph <- graph.data.frame(weighted_edgelist_uni, directed = FALSE)

set.seed(100)
# Calcola il clustering 
graph_betweenness_cluster <- cluster_louvain(graph)
autori <- membership(graph_betweenness_cluster)
cluster_importance <- table(autori)
top_clusters <- names(sort(cluster_importance, decreasing = TRUE))[1:10]

#attributo colore al node in base al cluster
c_colors <- brewer.pal(10, "Set3")
V(graph)$color <- ifelse(autori %in% top_clusters, c_colors[match(autori, top_clusters)], "grey")
E(graph)$color <- V(graph)$color[get.edge.ids(graph, t(as_edgelist(graph)))]

#attributo cluster al node
V(graph)$cluster <- graph_betweenness_cluster$membership

# Scegliere i nodi con degree piu alto
top_nodes <- data.frame(name = V(graph)$name, degree = degree(graph)) |> 
  top_n(20, degree)

graph<-as_tbl_graph(graph)
set.seed(1000)
ggraph(graph, layout = 'nicely') + 
  geom_edge_link() + 
  geom_node_point(aes(size = degree(graph), color = I(color))) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes$name, name, ''),), 
                 repel = TRUE, 
                 size = 3) + 
  theme(legend.position = 'none')


## @knitr grafo università 2019 --------------------------------------------------------

edgelist_universita <- edgelist |> 
  filter( year == 2019)|> 
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

graph <- graph.data.frame(weighted_edgelist_uni, directed = FALSE)

set.seed(100)
# Calcola il clustering 
graph_betweenness_cluster <- cluster_louvain(graph)
autori <- membership(graph_betweenness_cluster)
cluster_importance <- table(autori)
top_clusters <- names(sort(cluster_importance, decreasing = TRUE))[1:10]

#attributo colore al node in base al cluster
c_colors <- brewer.pal(10, "Set3")
V(graph)$color <- ifelse(autori %in% top_clusters, c_colors[match(autori, top_clusters)], "grey")
E(graph)$color <- V(graph)$color[get.edge.ids(graph, t(as_edgelist(graph)))]

#attributo cluster al node
V(graph)$cluster <- graph_betweenness_cluster$membership

# Scegliere i nodi con degree piu alto
top_nodes <- data.frame(name = V(graph)$name, degree = degree(graph)) |> 
  top_n(20, degree)

graph<-as_tbl_graph(graph)
set.seed(1000)
ggraph(graph, layout = 'nicely') + 
  geom_edge_link() + 
  geom_node_point(aes(size = degree(graph), color = I(color))) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes$name, name, ''),), 
                 repel = TRUE, 
                 size = 3) + 
  theme(legend.position = 'none')


## @knitr grafo università 2020 --------------------------------------------------------

edgelist_universita <- edgelist |> 
  filter( year == 2020)|> 
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

graph <- graph.data.frame(weighted_edgelist_uni, directed = FALSE)

set.seed(100)
# Calcola il clustering 
graph_betweenness_cluster <- cluster_louvain(graph)
autori <- membership(graph_betweenness_cluster)
cluster_importance <- table(autori)
top_clusters <- names(sort(cluster_importance, decreasing = TRUE))[1:10]

#attributo colore al node in base al cluster
c_colors <- brewer.pal(10, "Set3")
V(graph)$color <- ifelse(autori %in% top_clusters, c_colors[match(autori, top_clusters)], "grey")
E(graph)$color <- V(graph)$color[get.edge.ids(graph, t(as_edgelist(graph)))]

#attributo cluster al node
V(graph)$cluster <- graph_betweenness_cluster$membership

# Scegliere i nodi con degree piu alto
top_nodes <- data.frame(name = V(graph)$name, degree = degree(graph)) |> 
  top_n(20, degree)

graph<-as_tbl_graph(graph)
set.seed(1000)
ggraph(graph, layout = 'nicely') + 
  geom_edge_link() + 
  geom_node_point(aes(size = degree(graph), color = I(color))) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes$name, name, ''),), 
                 repel = TRUE, 
                 size = 3) + 
  theme(legend.position = 'none')


## @knitr grafo università 2021 --------------------------------------------------------

edgelist_universita <- edgelist |> 
  filter( year == 2021)|> 
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

graph <- graph.data.frame(weighted_edgelist_uni, directed = FALSE)

set.seed(100)
# Calcola il clustering 
graph_betweenness_cluster <- cluster_louvain(graph)
autori <- membership(graph_betweenness_cluster)
cluster_importance <- table(autori)
top_clusters <- names(sort(cluster_importance, decreasing = TRUE))[1:10]

#attributo colore al node in base al cluster
c_colors <- brewer.pal(10, "Set3")
V(graph)$color <- ifelse(autori %in% top_clusters, c_colors[match(autori, top_clusters)], "grey")
E(graph)$color <- V(graph)$color[get.edge.ids(graph, t(as_edgelist(graph)))]

#attributo cluster al node
V(graph)$cluster <- graph_betweenness_cluster$membership

# Scegliere i nodi con degree piu alto
top_nodes <- data.frame(name = V(graph)$name, degree = degree(graph)) |> 
  top_n(20, degree)

graph<-as_tbl_graph(graph)
set.seed(1000)
ggraph(graph, layout = 'nicely') + 
  geom_edge_link() + 
  geom_node_point(aes(size = degree(graph), color = I(color))) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes$name, name, ''),), 
                 repel = TRUE, 
                 size = 3) + 
  theme(legend.position = 'none')


## @knitr grafo università 2022 --------------------------------------------------------

edgelist_universita <- edgelist |> 
  filter(year == 2022) |> 
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

graph <- graph.data.frame(weighted_edgelist_uni, directed = FALSE)

set.seed(100)
# Calcola il clustering 
graph_betweenness_cluster <- cluster_louvain(graph)
autori <- membership(graph_betweenness_cluster)
cluster_importance <- table(autori)
top_clusters <- names(sort(cluster_importance, decreasing = TRUE))[1:10]

#attributo colore al node in base al cluster
c_colors <- brewer.pal(10, "Set3")
V(graph)$color <- ifelse(autori %in% top_clusters, c_colors[match(autori, top_clusters)], "grey")
E(graph)$color <- V(graph)$color[get.edge.ids(graph, t(as_edgelist(graph)))]

#attributo cluster al node
V(graph)$cluster <- graph_betweenness_cluster$membership

# Scegliere i nodi con degree piu alto
top_nodes <- data.frame(name = V(graph)$name, degree = degree(graph)) |> 
  top_n(20, degree)

graph<-as_tbl_graph(graph)
set.seed(1000)
ggraph(graph, layout = 'nicely') + 
  geom_edge_link() + 
  geom_node_point(aes(size = degree(graph), color = I(color))) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes$name, name, ''),), 
                 repel = TRUE, 
                 size = 3) + 
  theme(legend.position = 'none')


## @knitr grafo università 2023 --------------------------------------------------------

edgelist_universita <- edgelist |> 
  filter(year == 2023) |> 
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

graph <- graph.data.frame(weighted_edgelist_uni, directed = FALSE)

set.seed(100)
# Calcola il clustering 
graph_betweenness_cluster <- cluster_louvain(graph)
autori <- membership(graph_betweenness_cluster)
cluster_importance <- table(autori)
top_clusters <- names(sort(cluster_importance, decreasing = TRUE))[1:10]

#attributo colore al node in base al cluster
c_colors <- brewer.pal(10, "Set3")
V(graph)$color <- ifelse(autori %in% top_clusters, c_colors[match(autori, top_clusters)], "grey")
E(graph)$color <- V(graph)$color[get.edge.ids(graph, t(as_edgelist(graph)))]

#attributo cluster al node
V(graph)$cluster <- graph_betweenness_cluster$membership

# Scegliere i nodi con degree piu alto
top_nodes <- data.frame(name = V(graph)$name, degree = degree(graph)) |> 
  top_n(20, degree)

graph<-as_tbl_graph(graph)
set.seed(1000)
ggraph(graph, layout = 'nicely') + 
  geom_edge_link() + 
  geom_node_point(aes(size = degree(graph), color = I(color))) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes$name, name, ''),), 
                 repel = TRUE, 
                 size = 3) + 
  theme(legend.position = 'none')
print("hello")
