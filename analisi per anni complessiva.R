
# ANALISI PER ANNI --------------------------------------------------------


# analisi di base ---------------------------------------------------------

# @ knitr Librerie ----------------------------------------------------------------
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
ggplot(papers_per_year, aes(x = factor(X3), y = paper_count, fill = factor(X3))) +
  geom_bar(stat = "identity") +
  labs(title = "Numero di articoli per anno",
       x = "Anno",
       y = "Conteggio degli articoli") +
  theme_classic() +
  scale_fill_brewer(palette = "Set3")

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




# ANNO 2018 ---------------------------------------------------------------


edgelist_2018 <- edgelist |> 
  filter( year == 2018)
#grafo 2018
graph18 <- graph.data.frame(edgelist_2018, directed = FALSE)
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

top_nodes_18 <- nodes_df_18 %>% top_n(15, degree)

ggraph(graph18, layout = 'fr') +
  geom_edge_link() + 
  geom_node_point(aes(size = node_degrees)) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes_18$name, name, '')), 
                 repel = TRUE, 
                 size = 3) + 
  theme(legend.position = 'bottom')




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
  geom_edge_link(aes(edge_alpha = 0.5)) + 
  geom_node_point(aes(color = I(color), size = degree)) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes_18$name, name, '')), 
                 repel = TRUE, 
                 size = 3) +
  scale_color_discrete(name = "Community") + 
  #  scale_size_continuous(range = c(3, 10)) +
  labs(title="Grafo con cluster Louvain anno 2018")+
  theme_graph() +
  theme(legend.position = "none")




#genere anno 2018
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

# Visualizzare il grafo con ggraph
ggraph(graph_genere_18, layout = "fr") +
  geom_edge_link() +
  geom_node_point(aes(color = gender), size = 5) +
  geom_node_text(aes(label = ifelse(name %in% top_nodes_18$name, name, '')), 
                 repel = TRUE, 
                 size = 3) +
  scale_color_manual(values = c("male" = "#3faea8", "female" = "orchid", 
                                "NA" ="lightgrey")) +
  theme() +
  labs(title = "Grafo con Nodi Colorati in Base al Genere 2018")

# ANNO 2019 ---------------------------------------------------------------

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



# ANNO 2020 ---------------------------------------------------------------

#2020
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



# ANNO 2021 ---------------------------------------------------------------

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

ggraph(graph21, layout = 'fr') +
  geom_edge_link() + 
  geom_node_point(aes(size = node_degrees)) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes_21$name, name, '')), 
                 repel = TRUE, 
                 size = 3) + 
  theme(legend.position = 'bottom')

#cluster per capire gruppi di autori
set.seed(42)



community_lou_21<- (cluster_louvain(graph21))

autori_21 <- membership(community_lou_21)

modularita_21<- modularity(community_lou_21)

##aggiungo le informazione della community ai nodi
V(graph21)$community_lou_21<- membership(community_lou_21)

V(graph21)$degree<- degree(graph21)

V(graph21)$label<- NA
##converto il grafo in un oggetto tidygraph 
tg21<-as_tbl_graph(graph21) #serve per detectare le communities di appartenenza
# Grafico del sottografo sulla base dei cluster (louvain), grandezza del nodo --------
ggraph(tg21, layout = "fr") + 
  geom_edge_link(aes(edge_alpha = 0.5), color = "black") + 
  geom_node_point(aes(color = factor(community_lou_21), size = degree)) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes_21$name, name, '')), 
                 repel = TRUE, 
                 size = 3) +
  scale_color_discrete(name = "Community") + 
  #  scale_size_continuous(range = c(3, 10)) +
  labs(title=" Sottografo con cluster Louvain anno 2021")+
  theme_graph() +
  theme(legend.position = "none")

#genere anno 2021
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


#calcolo un oggetto tidygraph
tg_genere_21 <- as_tbl_graph(graph21)

# Unire i dati degli autori con il grafo
graph_genere_21 <- tg_genere_21 |> 
  activate(nodes) |> 
  left_join(authors_complete[,c("X2", "gender")], by = c("name" = "X2"))

# Visualizzare il grafo con ggraph
ggraph(graph_genere_21, layout = "fr") +
  geom_edge_link() +
  geom_node_point(aes(color = gender), size = 5) +
  geom_node_text(aes(label = ifelse(name %in% top_nodes_21$name, name, '')), 
                 repel = TRUE, 
                 size = 3) +
  scale_color_manual(values = c("male" = "#3faea8", "female" = "orchid", 
                                "NA" ="lightgrey")) +
  theme() +
  labs(title = "Grafo con Nodi Colorati in Base al Genere 2021")




# ANNO 2022 ---------------------------------------------------------------

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



# ANNO 2023 ---------------------------------------------------------------


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

ggraph(graph23, layout = 'fr') +
  geom_edge_link() + 
  geom_node_point(aes(size = node_degrees)) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes_23$name, name, '')), 
                 repel = TRUE, 
                 size = 3) + 
  theme(legend.position = 'bottom')

#cluster per capire gruppi di autori
set.seed(42)



community_lou_23<- (cluster_louvain(graph23))

autori_23 <- membership(community_lou_23)

modularita_23<- modularity(community_lou_23)

##aggiungo le informazione della community ai nodi
V(graph23)$community_lou_23<- membership(community_lou_23)

V(graph23)$degree<- degree(graph23)

V(graph23)$label<- NA
##converto il grafo in un oggetto tidygraph 
tg23<-as_tbl_graph(graph23) #serve per detectare le communities di appartenenza
# Grafico del sottografo sulla base dei cluster (louvain), grandezza del nodo --------
ggraph(tg23, layout = "fr") + 
  geom_edge_link(aes(edge_alpha = 0.5), color = "black") + 
  geom_node_point(aes(color = factor(community_lou_23), size = degree)) + 
  geom_node_text(aes(label = ifelse(name %in% top_nodes_23$name, name, '')), 
                 repel = TRUE, 
                 size = 3) +
  scale_color_discrete(name = "Community") + 
  #  scale_size_continuous(range = c(3, 10)) +
  labs(title=" Sottografo con cluster Louvain anno 2023")+
  theme_graph() +
  theme(legend.position = "none")

#genere anno 2023
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


#calcolo un oggetto tidygraph
tg_genere_23 <- as_tbl_graph(graph23)

# Unire i dati degli autori con il grafo
graph_genere_23<- tg_genere_23 |> 
  activate(nodes) |> 
  left_join(authors_complete[,c("X2", "gender")], by = c("name" = "X2"))

# Visualizzare il grafo con ggraph
ggraph(graph_genere_23, layout = "fr") +
  geom_edge_link() +
  geom_node_point(aes(color = gender), size = 5) +
  geom_node_text(aes(label = ifelse(name %in% top_nodes_23$name, name, '')), 
                 repel = TRUE, 
                 size = 3) +
  scale_color_manual(values = c("male" = "#3faea8", "female" = "orchid", 
                                "NA" ="lightgrey")) +
  theme() +
  labs(title = "Grafo con Nodi Colorati in Base al Genere 2023")

