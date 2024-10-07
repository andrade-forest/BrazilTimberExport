library(ggplot2)

dados2= destination %>%
  group_by(Blocos) %>%
  summarise(
    sum=sum(Volume,na.rm = TRUE))


# Criar gráfico de pizza personalizado com legenda ordenada por 'Volume'
ggplot(dados2, aes(x = "", y = sum, fill = reorder(Blocos, -sum))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = cores) +  # Definir a paleta de cores
  labs(title = "",
       fill = "Países") +
  theme_minimal() +
  theme(legend.position = "right",  # Posicionar a legenda à direita
        axis.text = element_blank(),  # Ocultar rótulos do eixo
        axis.title = element_blank(),  # Ocultar título do eixo
        panel.grid = element_blank(),  # Ocultar linhas de grade
        panel.border = element_blank())  # Ocultar borda do gráfico

dados= destination %>%
  group_by(Bloco) %>%
  summarise(
    sum=sum(Volume,na.rm = TRUE))

library(ggplot2)
library(ggarrange)
dados$percentual <- dados$sum / total * 100

# Criar gráfico de pizza personalizado com legenda ordenada por 'Volume'
ggplot(dados, aes(x = "", y = sum, fill = reorder(Bloco, -sum))) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(round(percentual), "%")), 
            position = position_stack(vjust = 0.6), # Ajuste aqui para harmonizar
            size = 5, color = "white", fontface = "bold", family = "sans", hjust = 0.5,
            show.legend = FALSE) +
  geom_point(aes(x = 0, y = 0), size = 50, color = "white", fill = "white") +  # Adicionar ponto branco no centro
  coord_polar(theta = "y") +
  scale_fill_manual(values = cores) +  # Definir a paleta de cores
  labs(title = "",
       fill = "Blocos compradores") +
  theme_minimal() +
  theme(legend.position = "right",  # Posicionar a legenda à direita
        axis.text = element_blank(),  # Ocultar rótulos do eixo
        axis.title = element_blank(),  # Ocultar título do eixo
        panel.grid = element_blank(),  # Ocultar linhas de grade
        panel.border = element_blank())  # Ocultar borda do gráfico



library(scales)

library(ggplot2)

library(ggplot2)

#library(ggplot2)
library(ggplot2)
library(scales)

library(ggplot2)
library(scales)

# Calcular o total
total <- sum(dados2$sum)

# Adicionar uma coluna com os percentuais
dados2$percentual <- dados2$sum / total * 100
library(ggplot2)
library(scales)

ggplot(dados2, aes(x = "", y = sum, fill = reorder(Blocos, -sum))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(percentual), "%")), 
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", fontface = "bold", family = "sans", hjust = 0.5,
            show.legend = FALSE) +
  scale_fill_manual(values = cores, name = "Países compradores") +
  labs(title = "",
       y = "Volume m³ (milhões)", x = "") +
  theme_minimal() +
  scale_y_continuous(labels = label_comma(scale = 1e-6, suffix = "M")) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


    
