library(factoextra)
library(haven)
options(max.print=5.5E5) 

load <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
} 

packages <- c("rio","factoextra","clustertend", "NbClust", "cluster", "caret", "skmeans", "fpc", "dendextend", "circlize", "ape")
load(packages)


## banco de dados presente no arquivo Pesquisa Bin?ria.sav
## definir diret?rio 
library(haven)
dados =  read.table("C://Users//amand//Downloads//Pesquisa Binária.txt")
dim(dados)
head(dados)
summary(dados)

dados.X = dados[,1:50]
head(dados.X)

dados.Y = dados[,51]  # variavel ser? usada para compara?ao 
table(dados.Y)
summary(dados.X)

#-------------------------------------------------------------------------------

# Encontra a estatistica Hopkins para dados dataset
set.seed(123)
hopkins(dados.X, n = nrow(dados.X)-1) 


## Metodo visual: mais avermelhado - mais similares, mais azulado - menos similares

fviz_dist(get_dist(dados.X), show_labels = FALSE)+labs(title = "dados data") # apresenta 3 grupos


## Determinando o numero otimo de clusters

# Elbow method (metodo do cotovelo)
fviz_nbclust(dados.X, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +
  labs(subtitle = "Elbow method")
# A Curva de Cotovelo ou M?todo Elbow Curve ? uma t?cnica usada para encontrar a quantidade 
# ideal de clusters K. Este m?todo testa a vari?ncia dos dados em rela??o ao n?mero de clusters.
# O valor ideal de K ? aquele que tem um menor Within Sum of Squares (WSS) e ao mesmo tempo
# o menor n?mero de clusters. De acordo com o grafico acima, 3 ? o numero ideal.


# Silhouette method
fviz_nbclust(dados.X, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

#---------------------------------------------------------

#### Agrupamentos hierarquicos  

# Agglomerative Nesting (AGNES)

res.agnes <- eclust(dados.X, "agnes", k = 3)
res.agnes$method

# Coeficiente aglomerativo: mede a qualidade do agrupamento encontrado
res.agnes$ac

grupos.agnes = res.agnes$cluster
grupos.agnes

fviz_dend(res.agnes, k = 3, k_colors = "jco", as.ggplot = TRUE, show_labels = FALSE, 
          rect = TRUE,rect_border = "jco", rect_fill = TRUE)



fviz_cluster(list(data = dados.X[,-47], cluster = grupos.agnes))

# PCA dados originais ( para compara??o )
fviz_pca_ind(prcomp(dados.X), title = "PCA - Pesqusa Binaria",
             habillage = dados.Y, palette = "jco",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom")


## Estatisticas de validacao de cluster


## Grafico da silhueta

fviz_silhouette(res.agnes, palette = "jco",ggtheme = theme_classic())

## informacao da Silhouette 
silinfo <- res.agnes$silinfo
names(silinfo)

## valores da silhouette de cada objeto
head(silinfo$widths, 10) # 10 primeiros
silinfo$widths  # todos

## silhouette media de cada cluster
silinfo$clus.avg.widths

## media total 
silinfo$avg.width

## tamanho de cada cluster
res.agnes$size

## Para encontrar os objetos com silhouette negativa
## Silhouette de cada observacao
sil <- res.agnes$silinfo$widths

## Objetos com silhouette negativa
neg_sil_index <- which(sil[, 'sil_width'] < 0)
sil[neg_sil_index, , drop = FALSE]    # pode ser citado no relatorio que os objetos nao possuem 
# silhueta negativa


## indice Dunn e outras estatisticas de validacao de cluster
## Estatistica para metodo de ward (agnes)
clust_stats_agnes = cluster.stats(d = dist(dados.X), dados.Y, res.agnes$cluster)
clust_stats_agnes


dc = cophenetic(res.agnes)
do = dist(dados.X)
corr_cof = cor(dc,do)
corr_cof

