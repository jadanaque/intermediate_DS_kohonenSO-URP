library(dplyr)
library(magrittr)
library(RColorBrewer)

pal <- colorRampPalette(brewer.pal(9, "YlOrRd"))  # define my palette

plot(vino.som, type = "changes")
plot(vino.som, type = "counts", palette.name = pal)
plot(vino.som, type = "dist.neighbours", palette.name = pal)
plot(vino.som, type = "codes")  # default for plot(vino.som)
par(mar = c(rep(4, 3), 2))

plot(vino.som, type = "quality", palette.name = pal)

display.brewer.all()
pal2 <- colorRampPalette(brewer.pal(11, "BrBG"))
plot(vino.som, type = "property", property = getCodes(vino.som)[, 1],
     main = colnames(getCodes(vino.som))[1],
     palette.name = pal2)

windows(width = 15, height = 10)
par(mfrow = c(2, 3))
for (i in 1:6){
  plot(vino.som, type = "property", property = getCodes(vino.som)[, i],
       main = colnames(getCodes(vino.som))[i],
       palette.name = pal2)
}

# Now the same heatmaps, but in their original scales
means <- sapply(datos[, 1:13], mean)
std_devs <- sapply(datos[, 1:13], sd)

plot(vino.som, type = "property", property = ((getCodes(vino.som)[, 1]) * std_devs[1]) + means[1],
     main = colnames(getCodes(vino.som))[1],
     palette.name = pal2)


# Now, let's compute the means per node and draw the heatmaps
means_per_node <- datos[, 1:13] %>%
                    bind_cols(data.frame(node = vino.som$unit.classif)) %>%
                    group_by(node) %>%
                    summarise_all(mean)

missing_nodes <- which(!(1:nrow(getCodes(vino.som)) %in% means_per_node$node))

means_per_node <- means_per_node %>%
  bind_rows(data.frame(node = missing_nodes)) %>%
  arrange(node)

plot(vino.som, type = "property", property = means_per_node[, 2] %>% unclass %>% unlist,
     main = names(means_per_node)[2],
     palette.name = pal2)

windows(width = 15, height = 10)
par(mfrow = c(2, 3))
for (i in (1:6)+1){
  plot(vino.som, type = "property", property = means_per_node[, i] %>% unclass %>% unlist,
       main = names(means_per_node)[i],
       palette.name = pal2)
}

# Clustering ----
# Determining a suitable number of clusters
vino_codebook <- getCodes(vino.som)
wss <- (nrow(vino_codebook) - 1) * sum(apply(vino_codebook, 2, var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(vino_codebook, centers = i)$withinss)
}
plot(1:15, wss, type = "b")

# Defining clusters using the Hierarchical Clustering algorithm
som_cluster <- cutree(hclust(dist(vino_codebook)), 3)  # cluster memberships (for each node)
plot(vino.som, type = "mapping", bgcol = som_cluster + 1, main = "Clusters")
add.cluster.boundaries(vino.som, som_cluster)

## Trying out better colors for the plot above
plot(vino.som, type = "mapping", bgcol = brewer.pal(3, "Set2")[som_cluster], main = "Clusters")
add.cluster.boundaries(vino.som, som_cluster)

# Some summaries of the original (training) data by cluster
datos %>%
  mutate(cluster = som_cluster[vino.som$unit.classif]) %>%
  group_by(cluster) %>%
  summarise_all(mean)

# Now, the heatmap from above, with the cluster boundaries
windows(width = 15, height = 10)
par(mfrow = c(4, 4))
for (i in (1:13)+1){
  plot(vino.som, type = "property", property = means_per_node[, i] %>% unclass %>% unlist,
       main = names(means_per_node)[i],
       palette.name = pal2)
  add.cluster.boundaries(vino.som, som_cluster)
}