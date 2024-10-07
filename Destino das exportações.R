##PACOTES NECESSÁRIOS
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)

# Selecionar as colunas 'UF' e 'Paises' para o novo DataFrame comex
comex <- EXP_2010_2023_20240119[, c('UF', 'Paises', 'kg', 'NCM')]

# Exibir o novo data frame
print(comex)

comex$volume= (comex$kg/700)


#PRODUÇÃO ANUAL DE TORAS
#filtro
#agrega
comex_agrega= comex %>%
  group_by(UF, Paises) %>%
  summarise(
    sum=sum(volume,na.rm = TRUE))

# Lista de estados desejados
estados_desejados <- c('Amazonas', 'Pará', 'Acre', 'Rondônia', 'Roraima', 'Amapá', 'Tocantins', 'Mato Grosso')

# Filtrar apenas os estados desejados
comex_filtrado <- comex_agrega %>%
  filter(UF %in% estados_desejados)

#agrega
comex_agrega2= comex_filtrado %>%
  group_by(Paises) %>%
  summarise(
    sum=sum(sum,na.rm = TRUE))


# Identificar os 10 países com maior volume
top_paises <- comex_agrega2 %>%
  top_n(10, sum) %>%
  select(Paises)

# Criar uma nova coluna para agrupar os outros países como "Outros"
comex_agrega2 <- comex_agrega2 %>%
  mutate(Categoria = ifelse(Paises %in% top_paises$Paises, as.character(Paises), "Outros"))

# Ordenar o DataFrame pelo volume (Sum)
comex_agrega2 <- comex_agrega2 %>%
  arrange(desc(sum))


#agrega
comex_agrega3= comex_agrega2 %>%
  group_by(Categoria) %>%
  summarise(
    sum=sum(sum,na.rm = TRUE))

# Calcular o percentual de Sum para cada Categoria
# Calcular a contribuição de cada país em relação ao total
comex_agrega3_percentual <- comex_agrega3 %>%
  mutate(Percentual = (sum / sum(sum)) * 100)

# Exibir o data frame com os percentuais
print(comex_agrega3_percentual)

##vendo quanto vale
# Selecionar as colunas 'UF' e 'Paises' para o novo DataFrame comex
valor <- EXP_2010_2023_20240119[, c('Paises', 'Valor_FOB', 'NCM', 'Ano')]

#agrega
dados= valor %>%
  group_by(Paises,Ano) %>%
  summarise(
    sum=sum(Valor_FOB,na.rm = TRUE))

# Supondo que seu data frame se chama 'dados' e possui as colunas 'sum', 'ano' e 'pais'

# 1. Calcular a média anual de cada país
media_anual <- dados %>%
  group_by(Paises, Ano) %>%
  summarise(media_anual = mean(sum))

# 2. Calcular o percentual de valor total que cada país contribui em relação ao todo
percentual_contribuicao <- dados %>%
  group_by(Paises) %>%
  summarise(total_contribuido = sum(sum)) %>%
  mutate(percentual = (total_contribuido / sum(total_contribuido)) * 100)

# Exibir os resultados
print("Média Anual por País:")
print(media_anual)

print("\nPercentual de Valor Total por País:")
print(percentual_contribuicao)
 sum(percentual_contribuicao$total_contribuido)

# Criar gráfico de pizza personalizado com legenda ordenada por 'sum'
destino_pizza<- ggplot(comex_agrega3, aes(x = "", y = sum, fill = reorder(Categoria, -sum))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = cores) +  # Definir a paleta de cores
  labs(title = "Destino das Exportações",
       fill = "Países") +
  theme_minimal() +
  theme(legend.position = "right",  # Posicionar a legenda à direita
        axis.text = element_blank(),  # Ocultar rótulos do eixo
        axis.title = element_blank(),  # Ocultar título do eixo
        panel.grid = element_blank(),  # Ocultar linhas de grade
        panel.border = element_blank())  # Ocultar borda do gráfico

# Criar gráfico de donut personalizado com legenda ordenada por 'sum'
destino_pizza<- ggplot(comex_agrega3, aes(x = "", y = sum, fill = reorder(Categoria, -sum))) +
  geom_bar(stat = "identity", width = 1) +
  geom_point(aes(x = 0, y = 0), size = 50, color = "white", fill = "white") +  # Adicionar ponto branco no centro
  coord_polar(theta = "y") +
  scale_fill_manual(values = cores) +  # Definir a paleta de cores
  labs(title = "Destino das Exportações",
       fill = "Países") +
  theme_minimal() +
  theme(legend.position = "right",  # Posicionar a legenda à direita
        axis.text = element_blank(),  # Ocultar rótulos do eixo
        axis.title = element_blank(),  # Ocultar título do eixo
        panel.grid = element_blank(),  # Ocultar linhas de grade
        panel.border = element_blank())  # Ocultar borda do gráfico

# Exibir o gráfico de pizza
print(destino_pizza)

#agrega
comex_agrega4= comex_filtrado %>%
  group_by(UF) %>%
  summarise(
    sum=sum(sum,na.rm = TRUE))

# Paleta de cores mais escuras de marrom ao amarelo
cores <- c("#8B4513", "#CD853F","#543C16", "#7E5E2B", "#8B6C1F", "#A08426", "#BAAE37", "#CDBF49", "#D9C874", "#E7DFA0", "#F6EDCC")

# Criar gráfico de pizza personalizado com legenda ordenada por 'sum'
ggplot(comex_agrega3, aes(x = "", y = sum, fill = reorder(Paises, -sum))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = cores) +  # Definir a paleta de cores
  labs(title = "Destino das Exportações",
       fill = "Estados") +
  theme_minimal() +
  theme(legend.position = "right",  # Posicionar a legenda à direita
        axis.text = element_blank(),  # Ocultar rótulos do eixo
        axis.title = element_blank(),  # Ocultar título do eixo
        panel.grid = element_blank(),  # Ocultar linhas de grade
        panel.border = element_blank())  # Ocultar borda do gráfico

# Criar gráfico de donut personalizado com legenda ordenada por 'sum'
ggplot(comex_agrega4, aes(x = "", y = sum, fill = reorder(UF, -sum))) +
  geom_bar(stat = "identity", width = 1) +
  geom_point(aes(x = 0, y = 0), size = 50, color = "white", fill = "white") +  # Adicionar ponto branco no centro
  coord_polar(theta = "y") +
  scale_fill_manual(values = cores) +  # Definir a paleta de cores
  labs(title = "Estados produtores de produtos exportados",
       fill = "Estados") +
  theme_minimal() +
  theme(legend.position = "right",  # Posicionar a legenda à direita
        axis.text = element_blank(),  # Ocultar rótulos do eixo
        axis.title = element_blank(),  # Ocultar título do eixo
        panel.grid = element_blank(),  # Ocultar linhas de grade
        panel.border = element_blank())  # Ocultar borda do gráfico


