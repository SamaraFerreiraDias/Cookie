#===============================================================================
#Pacotes
#===============================================================================

library(readr)      #Ler arquivo
library(dplyr)      #Tratar dados
library(cluster)    #K-means
library(ggplot2)    #Plotar gráficos
library(factoextra) #Metodo elbow
library(tidyverse)  #Manipulação de dataframe
library(corrplot)   #Plotar gráficos
library(GGally)
library(knitr)
library(arules)     #Agrupar pacotes

#===============================================================================
# Carregar Dataset
#===============================================================================

#Caracteristicas (Cor, aroma, sabor, Aceitação) como variáveis
dados_analise_C = read.csv2(file = 'dados_tratados_caracteristicas.csv')
#View(dados_analise_C)
#str(dados_analise_C)
dados_analise_C1 = dados_analise_C[,-1] #dataframe sem a variavel julgador
#View(dados_analise1)
dados_analise_C2 = dados_analise_C1[,-1]

#Tipos de biscoitos (Controle, F1, F2, F3 e AceitacaoGlobal) como variáveis
dados_analise_B = read.csv2(file = 'dados_tratados_biscoitos.csv')
#View(dados_analise_B)
#str(dados_analise_B) 

#===============================================================================
# Histograma para cada tributo
#===============================================================================

#Histograma para características
dados_analise_C2 %>% gather(Attributes, value, 1:4) %>% ggplot(aes(x=value, fill=Attributes)) +
  geom_histogram(colour="black", show.legend=FALSE) + facet_wrap(~Attributes, scales="free_x") +
  labs(x="Notas", y="Frequencia",title="Atributos Caracteristicas dos Biscoitos - Histogramas") + theme_bw()

#Histograma para tipos de biscoitos
dados_analise_B %>% gather(Attributes, value, 3:6) %>% ggplot(aes(x=value, fill=Attributes)) +
  geom_histogram(colour="black", show.legend=FALSE) + facet_wrap(~Attributes, scales="free_x") +
  labs(x="Notas", y="Frequencia",title="Avaliação dos Biscoitos - Histogramas") + theme_bw()

#===============================================================================
# Boxplot para cada atrituto (Atributo que mais se destacou)
#===============================================================================

#Boxplot para características
dados_analise_C1 %>% gather(Attributes, values, c(2:5)) %>% ggplot(aes(x=reorder(Attributes, values, FUN=median), y=values, fill=Attributes)) +
  geom_boxplot(show.legend=FALSE) + labs(title="Atributos dos Biscoitos - Boxplots") + theme_bw() +
  theme(axis.title.y=element_blank(), axis.title.x=element_blank()) + ylim(0, 35) + coord_flip()

#Boxplot para tipos de biscoitos
dados_analise_B %>% gather(Attributes, values, c(3:6)) %>% ggplot(aes(x=reorder(Attributes, values, FUN=median), y=values, fill=Attributes)) +
  geom_boxplot(show.legend=FALSE) + labs(title="Avaliação dos Biscoitos - Boxplots") + theme_bw() +
  theme(axis.title.y=element_blank(), axis.title.x=element_blank()) + ylim(0, 35) + coord_flip()

#===============================================================================
# Matriz de Correlacao 
#===============================================================================

corrplot(cor(dados_analise_C2), type="upper", method="ellipse", tl.cex=0.9) 

#===============================================================================
# Aplicação do K-means (K = 5)
#===============================================================================

#mudar a escala das variaveis
dados_analise_C3 = scale(dados_analise_C, center = TRUE, scale = TRUE)

#calcular o numero otimo de clusters (Elbow)
fviz_nbclust(dados_analise_C3, kmeans, method = 'wss', iter.max = 100)

#clusterizar os dados com o kmeans
k = 6
numIters = 100
resultado = kmeans(dados_analise_C3, centers = k, iter.max = numIters)
print(resultado$cluster)
previsao = resultado$cluster

#agrupamento 
ggpairs(cbind(dados_analise_C, Cluster=as.factor(previsao)),
        columns=1:7, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both") +
theme_bw()

#Plotar agrupamentos
plot(previsao)
clusplot(dados_analise_C3,previsao, color = T, lines = F, labels = 5)

#Anexar o id dos clusters no dataset original
dados_analise_C = dados_analise_C %>% mutate(cluster = previsao)
dados_analise_C$cluster = as.character(dados_analise_C$cluster)

dados_cluster = dados_analise_C

#Salvando o cluster
#write_csv2(dados_cluster, 'dados_cluster5.csv')

#===============================================================================
#*****************Analisado Resultados do cluster (k = 5)*********************
#***********Retirando os dados de julgadores que não gostam de biscoitos********
#===============================================================================

#Proporção de tipos de biscoitos por clusters
qtdePorProd = dados_cluster %>% group_by(Produto) %>% summarise(Qtde = n())

#write_csv2(qtdePorProd, 'dados_cluster5_qtdePorProd.csv')

qtdePorGrupo = dados_cluster %>% group_by(cluster) %>% summarise(Qtde = n()) %>%
  mutate(Prop = round(100 * Qtde / sum(Qtde), 2))

#Proporção de tipos de biscoitos por clusters
propPorGrupo = dados_cluster %>% group_by(cluster, Produto) %>% summarise(Qtde = n()) %>%
  mutate(Prop = round(100 * Qtde / sum(Qtde), 2))

#Calculando mediana das caracteristicas dos biscoitos
medianaPorGrupo = dados_cluster %>% group_by(cluster, Produto) %>%
  summarise(MedianaCor = median(Cor),
            MedianaAroma = median(Aroma),
            MedianaSabor = median(Sabor),
            MedianaAG = median(AceitacaoGlobal))

#write_csv2(medianaPorGrupo, 'dados_cluster5_medianas.csv')


#Excluir clusters com dados de pessoas que não gostam de biscoito
novo_cluster = dados_cluster %>% filter (cluster > "2") %>% mutate(cluster = NULL)

#===============================================================================
# Aplicação do K-means (K = 3)
#===============================================================================

#mudar a escala das variaveis
novo_cluster1 = scale(novo_cluster, center = TRUE, scale = TRUE)

#calcular o numero otimo de clusters (Elbow)
fviz_nbclust(novo_cluster1, kmeans, method = 'wss', iter.max = 100)

#clusterizar os dados com o kmeans
n = 3
resultado = kmeans(novo_cluster1, centers = n, iter.max = numIters)
print(resultado$cluster)
previsao = resultado$cluster

#agrupamento 
ggpairs(cbind(novo_cluster, Cluster=as.factor(previsao)),
        columns= 1:7, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both") +
  theme_bw()

#Plotar agrupamentos
plot(previsao)
clusplot(novo_cluster1,previsao, color = T, lines = F, labels = 5)

#Anexar o id dos clusters no dataset original
novo_cluster = novo_cluster %>% mutate(cluster = previsao)
novo_cluster$cluster = as.character(novo_cluster$cluster)

print(novo_cluster)

#Salvando o cluster
#write_csv2(novo_cluster, 'dados_cluster3.csv')

#===============================================================================
#***************** Analisado Resultados do cluster (k = 3)***********************
#===============================================================================

#Proporção de tipos de biscoitos por clusters
qtdePorProd = novo_cluster %>% group_by(Produto) %>% summarise(Qtde = n())
qtdePorGrupo = novo_cluster %>% group_by(cluster) %>% summarise(Qtde = n()) %>%
  mutate(Prop = round(100 * Qtde / sum(Qtde), 2))

#write_csv2(medianaPorGrupo, 'qtdePorProd_cluster3.csv')

#Proporção de tipos de biscoitos por clusters
propPorGrupo = novo_cluster %>% group_by(cluster, Produto) %>% summarise(Qtde = n()) %>%
  mutate(Prop = round(100 * Qtde / sum(Qtde), 2))

#Calculando mediana das caracteristicas dos biscoitos
medianaPorGrupo = novo_cluster %>% group_by(cluster, Produto) %>%
  summarise(MedianaCor = median(Cor),
            MedianaAroma = median(Aroma),
            MedianaSabor = median(Sabor),
            MedianaAG = median(AceitacaoGlobal))

#write_csv2(medianaPorGrupo, 'dados_cluster3_medianas.csv')








