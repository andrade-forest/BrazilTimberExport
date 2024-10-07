##PACOTES NECESSÁRIOS
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)

# Selecionar as colunas 'UF' e 'Paises' para o novo DataFrame comex
comex <- EXP_2010_2023_20240119[, c('UF', 'Paises', 'kg', 'NCM', 'Ano')]

# Exibir o novo data frame
print(comex)

comex$volume= (comex$kg/700)


#PRODUÇÃO ANUAL DE TORAS
#filtro
#agrega
comex_agrega= comex %>%
  group_by(UF,Paises,Ano) %>%
  summarise(
    sum=sum(volume,na.rm = TRUE))

# Lista de estados desejados
estados_desejados <- c('Amazonas', 'Pará', 'Acre', 'Rondônia', 'Roraima', 'Amapá', 'Tocantins', 'Mato Grosso')

# Filtrar apenas os estados desejados
comex_filtrado <- comex_agrega %>%
  filter(UF %in% estados_desejados)

comex_filtrado= comex_filtrado %>%
  group_by(UF, Ano) %>%
  summarise(
    sum=sum(sum,na.rm = TRUE))

# Exibir o novo data frame
print(comex_filtrado)

# Verificar tipos de dados
comex_filtrado$Ano <- as.factor(comex_filtrado$Ano)
comex_filtrado$sum <- as.numeric(comex_filtrado$sum)
comex_filtrado$UF <- as.factor(comex_filtrado$UF)
# Ordenar dados por ano
comex_filtrado <- comex_filtrado %>%
  arrange(Ano)

# Verificar valores únicos de UF
unique_UF <- unique(comex_filtrado$UF)
print(unique_UF)

# Criar gráfico de linhas com reordenação da legenda, nome personalizado e escala do eixo y ajustada
linha_chart <- ggplot(comex_filtrado, aes(x = Ano, y = sum, group = UF, color = reorder(UF, -sum))) +
  geom_line() +
  labs(title = "Variação da exportação ao longo dos anos",
       x = "Ano",
       y = "Volume (m³)") +
  theme_minimal() +
  scale_color_discrete(name = "Estados") +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "k"))

# Exibir o gráfico de linhas
print(linha_chart)

library(broom)


