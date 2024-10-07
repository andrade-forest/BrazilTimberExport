if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)
# dados
comex <- read.table("True-EXP_2010_2022.csv", h=T,sep=";", dec = ".")
#criando coluna volume. d = m/v

comex$Year= comex$Ano
comex$volume = (comex$Quilograma.Líquido / 700)
comex$Quilograma.Líquido=as.numeric(comex$Quilograma.Líquido)
##tirar o amapa
comex=filter(comex,UF.do.Produto!="Amapá")


str(comex)
##link da consulta: http://comexstat.mdic.gov.br/pt/geral/87024


#####1. Dinamica anual da exportação por estado - Quilo
comex_estado= comex %>%
  group_by(UF.do.Produto,Year) %>%
  summarise(
    mean = mean(Quilograma.Líquido, na.rm = TRUE),
    sum=sum(Quilograma.Líquido,na.rm = TRUE),
    sd = sd(Quilograma.Líquido, na.rm = TRUE),
    var= var(Quilograma.Líquido, na.rm=TRUE))

#####1. Dinamica anual da exportação por estado - Volume
comex_estado= comex %>%
  group_by(UF.do.Produto,Year) %>%
  summarise(
    sum=sum(volume,na.rm = TRUE))
#####1.2 Dinamica anual da exportação geral - Volume
comex_geral= comex %>%
  group_by(Year) %>%
  summarise(
    sum=sum(volume,na.rm = TRUE))

comex_geral$Real=comex_geral$sum
comex_geral$Year=as.factor(comex_geral$Year)


#####2. Dinamica anual dos produtos por estado
comex_produtos= comex %>%
  group_by(UF.do.Produto,Código.CUCI.Item) %>%
  summarise(
    mean = mean(Quilograma.Líquido, na.rm = TRUE),
    sum=sum(Quilograma.Líquido,na.rm = TRUE),
    sd = sd(Quilograma.Líquido, na.rm = TRUE),
    var= var(Quilograma.Líquido, na.rm=TRUE))

#####3. Dinamica anual dos produtos
comex_ranking_produtos= comex %>%
  group_by(Código.CUCI.Item) %>%
  summarise(
    mean = mean(Quilograma.Líquido, na.rm = TRUE),
    sum=sum(Quilograma.Líquido,na.rm = TRUE),
    sd = sd(Quilograma.Líquido, na.rm = TRUE),
    var= var(Quilograma.Líquido, na.rm=TRUE))
##Nós temos 35 produtos de madeira exportados na Amazônia Brasileira.

#####4. Paises
comex_ranking_paises= comex %>%
  group_by(Países,Descrição.CUCI.Item,Year) %>%
  summarise(
    mean = mean(volume, na.rm = TRUE),
    sum=sum(volume,na.rm = TRUE),
    sd = sd(volume, na.rm = TRUE),
    var= var(volume, na.rm=TRUE))
##Nós temos 163 paises compradores da madeira amazonica Brasileira.


##Paises
###ggrafico de linhas com separação por UF de origem
ggplot(data = comex_ranking_paises, aes(x = Year, y =sum, group = Países)) +
  geom_line(aes(colour = Países), size=0.9)+
  labs(y="Volume (m³)",x="Ano")

ggplot(data = comex_ranking_paises, aes(x = Year, y =sum, fill=Países)) +
  geom_bar(stat="identity") +
  labs(y="Volume",x="Ano", colour= "Países") +
  theme_bw()
