##PACOTES NECESSÁRIOS
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)

# Selecionar as colunas 'UF' e 'Paises' para o novo DataFrame comex
comex <- EXP_2010_2023_20240119[, c('UF', 'Paises', 'kg', 'NCM', 'Ano', 'Valor_FOB')]

# Exibir o novo data frame
print(comex)

comex$volume= (comex$kg/700)



#PRODUÇÃO ANUAL DE TORAS
#filtro
#agrega
comex_agrega= comex %>%
  group_by(UF, Paises, Ano) %>%
  summarise(
    sum=sum(Valor_FOB,na.rm = TRUE))

# Lista de estados desejados
estados_desejados <- c('Amazonas', 'Pará', 'Acre', 'Rondônia', 'Roraima', 'Amapá', 'Tocantins', 'Mato Grosso')

# Filtrar apenas os estados desejados
comex_filtrado <- comex_agrega %>%
  filter(UF %in% estados_desejados)

# Exibir o novo data frame
print(comex_filtrado)


#agrega
comex_agrega2= comex_filtrado %>%
  group_by(UF,Ano) %>%
  summarise(
    sum=sum(sum,na.rm = TRUE))
library(ggplot2)

# Se os dados estão em 'comex_agrega2'

# Vamos criar um gráfico de linha usando ggplot2
ggplot(comex_agrega2, aes(x = Ano, y = sum, color = UF)) +
  geom_line() +
  labs(x = "Ano", y = "Produção por Ano", color = "Países") +
  ggtitle("Produção por Ano em cada País") +
  theme_minimal()

# Agregando os dados por UF
estatisticas_por_estado <- comex_agrega2 %>%
  group_by(UF) %>%
  summarise(
    soma = sum(sum, na.rm = TRUE),
    media = mean(sum, na.rm = TRUE),
    mediana = median(sum, na.rm = TRUE),
    desvio_padrao = sd(sum, na.rm = TRUE)
  )

# Visualizando as estatísticas
print(estatisticas_por_estado)
