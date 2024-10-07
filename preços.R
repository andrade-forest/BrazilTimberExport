##PACOTES NECESSÁRIOS
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, maps, tibble, ggplot2, car, agricolae, tidyverse, forcats)

# Selecionar as colunas 'UF' e 'Paises' para o novo DataFrame comex
comex <- EXP_2010_2023_20240119[, c('UF', 'Paises', 'kg', 'NCM', 'Ano', 'Valor_FOB')]

# Substituir vírgulas por pontos nas colunas 'kg' e 'Valor_FOB'
comex$kg <- as.numeric(gsub(".", ",", comex$kg))
comex$Valor_FOB <- as.numeric(gsub(".", ",", comex$Valor_FOB))
# Exibir o novo data frame
print(comex)

comex$volume= (comex$kg/700)

# Lista de estados desejados
estados_desejados <- c('Amazonas', 'Pará', 'Acre', 'Rondônia', 'Roraima', 'Amapá', 'Tocantins', 'Mato Grosso')

# Filtrar apenas os estados desejados
comex_uf <- comex %>%
  filter(UF %in% estados_desejados)

comex_uf$precom3= (comex_uf$Valor_FOB/comex_uf$volume)

comex_preco <- comex_uf %>%
  filter(!is.infinite(precom3) & !is.na(precom3)) %>%
  summarise(
    soma = sum(precom3),
    media = mean(precom3),
    mediana = median(precom3),
    max = max(precom3),
    min = min(precom3),
    desvio_padrao = sd(precom3)
  )

#pra ver se as bases estao boas
valores  <- comex_uf  %>%
  group_by(UF,) %>%
  summarise(Valor_FOB = sum(Valor_FOB))



# Criando o gráfico de barras
ggplot(valores, aes(x = UF, y = Valor_FOB)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Valores FOB por Estado",
       x = "Estado",
       y = "Valor FOB") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotacionando os rótulos do eixo x para melhor legibilidade


# Ordenando o dataframe por valores decrescentes para facilitar a visualização
valores <- valores %>% arrange(desc(Valor_FOB))

# Criando o gráfico de área empilhada
ggplot(valores, aes(x = UF, y = Valor_FOB, fill = UF, group = 1)) +
  geom_area() +
  labs(title = "Valores FOB por Estado (Gráfico de Área Empilhada)",
       x = "Estado",
       y = "Valor FOB",
       fill = "Estado") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


