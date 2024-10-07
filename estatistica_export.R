##Estatisticas exportação
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)

# Selecionar as colunas 'UF' e 'Paises' para o novo DataFrame comex
comex <- EXP_2010_2023_20240119[, c('UF', 'Paises', 'kg', 'NCM', "Ano", "Valor_FOB")]

# Exibir o novo data frame
print(comex)
comex$volume= (comex$kg/700)

# Lista de estados desejados
estados_desejados <- c('Amazonas', 'Pará', 'Acre', 'Rondônia', 'Roraima', 'Amapá', 'Tocantins')

# Filtrar apenas os estados desejados
comex_filtrado <- comex %>%
  filter(UF %in% estados_desejados)

# Identificar os 10 países com maior volume
top_paises <- comex_agrega2 %>%
  top_n(10, sum) %>%
  select(Paises)

# 1. Calcular a média anual de cada país
media_anual <- comex_filtrado %>%
  group_by(Paises, Ano) %>%
  summarise(media_anual = mean(Valor_FOB))

# 2. Calcular o percentual de valor total que cada país contribui em relação ao todo
percentual_contribuicao <- comex_filtrado %>%
  group_by(Paises) %>%
  summarise(total_contribuido = sum(Valor_FOB)) %>%
  mutate(percentual = (total_contribuido / sum(total_contribuido)) * 100)

# Exibir os resultados
print("Média Anual por País:")
print(media_anual)

print("\nPercentual de Valor Total por País:")
print(percentual_contribuicao)

sum(comex_filtrado$Valor_FOB)


comex_filtrado$valor_m3= (comex_filtrado$Valor_FOB/comex_filtrado$volume)


#agrega
valores_produto= comex_filtrado %>%
  group_by(NCM) %>%
  summarise(
    sum=sum(valor_m3,na.rm = TRUE))
##tirar a media e o desvio tbm



#agrega
anual_vol= comex_filtrado %>%
  group_by(Ano) %>%
  summarise(
    sum=sum(volume,na.rm = TRUE))
##tirar a media e o desvio tbm