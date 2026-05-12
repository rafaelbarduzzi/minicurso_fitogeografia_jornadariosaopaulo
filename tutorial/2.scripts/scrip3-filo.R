#============#
# 1. Head ####
#============#

#intalando e/ou carregando pacotes e carregando banco de dados

if (!require(librarian)) install.packages(c("librarian"))
librarian::shelf(phytools,ape,tidyverse,ggplot2,beepr,
                 remotes, picante, viridis, sf)

#BiocManager::install("ggtree")
#remotes::install_github("YuLab-SMU/ggtree", force = TRUE) #alternartiva
library(ggtree)

# Para as analises de biodiversidade considerando as relacoes filogeneticas,
# temos que ter acesso a filogenia do grupo
tree <- read.tree("1.dataset/tree-Vasconcelos2020.txt")

#===========================#
# 2. Manipulando árvores ####
#===========================#

# Quais sao os componentes da arvore?
tree
tree$tip.label #especies incluidas nas arvores
tree$Nnode #numero de nós
max(nodeHeights(tree)) #calcula o valor total da altura de todos os nós

## 2.1 Plotando árvores ####
ggtree(tree)

ggtree(tree, size = 0.2) + 
  theme_tree2() + #adiciona escala
  geom_tiplab(size = 1) + #adiciona os nomes dos tips
  geom_nodepoint(aes(subset = node %in% 
                       c(390, 458, 471, 477, 501, 503, 524, 526, 535, 
                         596, 604, 610, 612, 615, 634, 636, 646, 657,
                         662, 663, 693, 748)), color = "black", 
                 size = 3, 
                 alpha = 0.5) #adiciona os nós

ggtree(tree, size = 0.2, layout = "circular") + #arvore circular
  geom_tiplab(size = 1) + 
  geom_nodepoint(aes(subset = node %in% 
                       c(390, 458, 471, 477, 501, 503, 524, 526, 535, 
                         596, 604, 610, 612, 615, 634, 636, 646, 657,
                         662, 663, 693, 748)), color = "darkblue", 
                 size = 4, 
                 alpha = 0.5) +  
  geom_text(aes(label = ifelse(node %in% c(390, 458, 471, 477, 501, 503, 524, 526, 535, 
                                           596, 604, 610, 612, 615, 634, 636, 646, 657,
                                           662, 663, 693, 748),
                               node, NA)),
            size = 2, color = "white") 

## 2.2 Podando grupos externos ####

outgroup <- c(tree$tip.label[!grepl("Mimosa", tree$tip.label)])

pruned_tree <- drop.tip(tree, outgroup)

ggtree(pruned_tree, size = 0.2, layout = "circular") +
  geom_tiplab(aes(label = sub("Mimosa", "M", label)), 
              size=1.5)

#============================================#
# 4. Analises de diversidade filogenetica ####
#============================================#

## 4.1 Matrix de presenca e ausencia filogenetica ####
comm_matrix

#podando a matrix para conter apenas especies que ocorrem na filogenia
comm_matrix_phyl <- comm_matrix[ , colnames(comm_matrix) %in% 
                                   pruned_tree$tip.label]

# colnames(comm_matrix[!comm_matrix %in% comm_matrix_phyl])
# colnames(comm_matrix[comm_matrix %in% comm_matrix_phyl])

## 4.2 calculando diversidade filogenetica ####
set.seed(42)
pd_stats <- ses.pd(comm_matrix_phyl, pruned_tree,
                   include.root = TRUE,
                   null.model = "taxa.label")

# atribuindo id
pd_stats$id <- rownames(pd_stats)
pd_stats$id <- as.numeric(pd_stats$id)
# intersect(grid_cells$grid_id, pd_stats$id)

## 4.3 calculando os residuos ####
pd_stats$residuals <- lm(pd.obs ~ ntaxa, pd_stats)$res

# Arredondando valores
cols_to_round <- names(pd_stats)[!names(pd_stats) %in% 
                                   c("ntaxa", "pd.obs.p", "id")]

pd_stats[,cols_to_round] <- round(pd_stats[,cols_to_round], 2)

## 4.4 Combinando resultados de pd com objeto espacial ####
pd_poly <- dplyr::left_join(grid_cells, pd_stats,
                            by = c("grid_id" = "id"))

# write.csv(pd_stats, "3.outputs/pd_stats.csv", row.names = F)
# write.csv(pd_poly, "3.outputs/pd-poly", row.names = F)

# extensao das grids
st_bbox(grid_cells)

#=========================================#
# 5. Plots de diversidade filogenetica ####
#=========================================#

#PD
ggplot(pd_poly) +
  geom_sf(aes(fill = pd.obs), color = NA, alpha = 0.8) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1
  ) +
  coord_sf(
    xlim = c(-109.95498 , -27.95498),
    ylim = c(-57.10205, 16.79878)
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    panel.grid = element_blank()
  )

#SR
ggplot(pd_poly) +
  geom_sf(aes(fill = ntaxa), color = NA, alpha = 0.8) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1
  ) +
  coord_sf(
    xlim = c(-109.95498 , -27.95498),
    ylim = c(-57.10205, 16.79878)
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    panel.grid = element_blank()
  )

# Residuos
ggplot(pd_poly) +
  geom_sf(aes(fill = residuals), color = NA, alpha = 0.8) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1
  ) +
  coord_sf(
    xlim = c(-109.95498 , -27.95498),
    ylim = c(-57.10205, 16.79878)
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    panel.grid = element_blank()
  )

