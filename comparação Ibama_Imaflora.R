if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)

###COMPARAR IBAMA, IMAFLOR, COMEX E TOTAL
comparar3 <- read.table("total_comparar.csv", h=T,sep=",", dec = ".")
comparar3$Year= as.factor(comparar3$Year)

#ggrafico de linhas com separação por UF de origem
ggplot(data = comparar3, aes(x = Year, y =Real, group = Fonte)) +
  geom_line(aes(colour = Fonte))+
  labs(y="Volume (m³)",x="Ano")


# Definir a ordem desejada para a variável Fonte
ordem_fonte <- c("Comex", "Imaflora", "Ibama")

library(dplyr)

# Filtrar o dataframe para remover a linha "Total"
comparar3 <- comparar3 %>% filter(Fonte != "Total")

library(dplyr)

# Calcular a média do percentual para cada fonte
media_percentual <- comparar3 %>%
  group_by(Fonte) %>%
  summarise(Media_Percentual = mean(Percentual, na.rm = TRUE))


# Definir a ordem desejada para a variável Fonte
ordem_fonte <- c("Comex","Imaflora","Ibama")

# Converter a variável Fonte em um fator com a ordem desejada
media_percentual$Fonte <- factor(media_percentual$Fonte, levels = ordem_fonte)

# Definir as cores em tons de marrom
cores_marrom <- c("#A0522D", "#D2691E","#CD853F")

# Gráfico de barras do percentual em relação ao total absoluto
ggplot(media_percentual, aes(x = Fonte, y = Media_Percentual, fill = Fonte)) +
  geom_bar(stat = "identity") +
  labs(y = "Percentual do Total (%)", x = "Fonte do dado") +
  scale_fill_manual(values = cores_marrom) +
  theme_minimal()
