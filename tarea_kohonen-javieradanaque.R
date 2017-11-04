library(readxl)
library(kohonen)

hatcon_df <- read_excel("DATAS/HATCON.xlsx")
hatcon_df <- hatcon_df[, c(1:7, 9:10)]  # eliminamos variables categóricas

hatcon_sc <- scale(hatcon_df)

hatcon_som <- som(hatcon_sc, grid = somgrid(4, 4, topo = "hexagonal"), rlen = 200)

summary(hatcon_som)
names(hatcon_som)

plot(hatcon_som, type = "counts")
plot(hatcon_som, type = "mapping")
plot(hatcon_som, type = "quality")
plot(hatcon_som, type = "codes")

# Supervised version
hatcon_df <- read_excel("DATAS/HATCON.xlsx")
hatcon_df <- hatcon_df[, c(1:10)]  # eliminamos variables categóricas, excepto X8 (Size of firm: Large or Small)

hatcon_sc <- scale(hatcon_df[, c(1:7, 9:10)])

hatcon_som_superv <- xyf(hatcon_sc, classvec2classmat(hatcon_df$X8),
                         grid = somgrid(4, 4, "hexagonal"), rlen = 200)

plot(hatcon_som_superv, type = "counts")
plot(hatcon_som_superv, type = "quality")
plot(hatcon_som_superv, type = "codes")
plot(hatcon_som_superv, type = "mapping", col = hatcon_df$X8 + 1)
