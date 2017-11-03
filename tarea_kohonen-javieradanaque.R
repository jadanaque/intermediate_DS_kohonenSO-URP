library(readxl)
library(kohonen)

hatcon_df <- read_excel("DATAS/HATCON.xlsx")
hatcon_df <- hatcon_df[, c(1:7, 9:10)]  # eliminamos variables categóricas

hatcon_sc <- scale(hatcon_df)

hatcon_som <- som(hatcon_sc, grid = somgrid(4, 4, topo = "hexagonal"), rlen = 200)

summary(hatcon_som)
plot(hatcon_som, type = "changes")
plot(hatcon_som, type = "counts", palette.name = pal)
plot(hatcon_som, type = "mapping")
plot(hatcon_som, type = "dist.neighbours", palette.name = pal)
plot(hatcon_som, type = "codes")

windows(15, 10)
par(mfrow = c(3, 3))
for (i in 1:9) {
  plot(hatcon_som, type = "property", property = getCodes(hatcon_som)[, i],
       main = colnames(getCodes(hatcon_som))[i],
       palette.name = pal2)
}

purchase_outcm_node1 <- tapply(hatcon_df$X9, hatcon_som$unit.classif, mean)

plot(hatcon_som, type = "property", property = purchase_outcm_node1,
     main = "Purchase Outcome per Node (mean)",
     palette.name = pal2)

# Probar quitando x9 (Purchase Outcomes). A ver si las variables se agrupan naturalmente entre grandes y pequeños compradores
hatcon_sc <- scale(hatcon_df[, -8])

hatcon_som <- som(hatcon_sc, grid = somgrid(4, 4, topo = "hexagonal"), rlen = 200)

plot(hatcon_som, type = "counts", palette.name = pal)
plot(hatcon_som, type = )

windows(15, 10)
par(mfrow = c(3, 3))
for (i in 1:8) {
  plot(hatcon_som, type = "property", property = getCodes(hatcon_som)[, i],
       main = colnames(getCodes(hatcon_som))[i],
       palette.name = pal2)
}

purchase_outcm_node <- tapply(hatcon_df$X9, hatcon_som$unit.classif, mean)

plot(hatcon_som, type = "property", property = purchase_outcm_node,
     main = "Purchase Outcome per Node",
     palette.name = pal2)
