##PACOTES NECESSÁRIOS
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)

# Selecionar as colunas 'UF' e 'Paises' para o novo DataFrame comex
comex <- EXP_2010_2023_20240119[, c('UF','Valor_FOB')]

# Lista de estados desejados
estados_desejados <- c('Amazonas', 'Pará', 'Acre', 'Rondônia', 'Roraima', 'Amapá', 'Tocantins', 'Mato Grosso')

# Filtrar apenas os estados desejados
comex_filtrado <- comex %>%
  filter(UF %in% estados_desejados)

# Exibir o novo data frame
print(comex_filtrado)

sum(comex_filtrado$Valor_FOB)

##Valor por pais
# Selecionar as colunas 'UF' e 'Paises' para o novo DataFrame comex
comex <- EXP_2010_2023_20240119[, c('Paises','Valor_FOB', 'UF')]

# Lista de estados desejados
estados_desejados <- c('Amazonas', 'Pará', 'Acre', 'Rondônia', 'Roraima', 'Amapá', 'Tocantins', 'Mato Grosso')

# Filtrar apenas os estados desejados
comex_filtrado <- comex %>%
  filter(UF %in% estados_desejados)

#agrega
comex_filtrado= comex_filtrado %>%
  group_by(UF) %>%
  summarise(
    sum=sum(Valor_FOB,na.rm = TRUE))

# Criar gráfico de pizza personalizado com legenda ordenada por 'sum'
ggplot(comex_filtrado, aes(x = " ", y = sum, fill = reorder(UF, -sum))) +
  geom_bar(stat = "identity", width = 1) +
  theme_void() + 
  scale_fill_manual(values = cores) +  # Definir a paleta de cores
  labs(title = "Valor arrecadado por estado",
       fill = "Estados") +
  theme_minimal() +
  theme(legend.position = "right",  # Posicionar a legenda à direita
        axis.text = element_blank(),  # Ocultar rótulos do eixo
        axis.title = element_blank(),  # Ocultar título do eixo
        panel.grid = element_blank(),  # Ocultar linhas de grade
        panel.border = element_blank())  # Ocultar borda do gráfico
