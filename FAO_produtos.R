# carregue o pacote ggplot2
library(ggplot2)
FAO <- read.table("FAO_wood.csv", h=T,sep=",", dec = ".")

# Agora, vamos criar um gráfico de barras para visualizar os dados
ggplot(FAO, aes(x = Item, y = Value, fill = Element)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Produção e Exportação de Produtos de Madeira no Brasil (2022)",
       x = "Produto de Madeira",
       y = "Volume (m3)",
       fill = "Tipo") +
  theme_minimal()



# Cria o gráfico de barras com a ordem das barras definida pela produção total
ggplot(FAO, aes(x = reorder(Item, Value), y = Value, fill = Element)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_fill_manual(values = c("Production" = "darkorange", "Export Quantity" = "brown")) +
  labs(title = "Produção e Exportação de Produtos de Madeira no Brasil (2022)",
       x = "Produto de Madeira",
       y = "Volume (m³)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  scale_y_continuous(labels = scales::comma_format(scale = 1, decimal.mark = ",", big.mark = "."))

FAO <- read.table("FAO.csv", h=T,sep=",", dec = ".")


# Cria o gráfico de barras com a ordem das barras definida pela produção total
ggplot(FAO, aes(x = reorder(Item, Value), y = Value, fill = Element)) +
  geom_bar(stat = "identity", position = "identity") +
  scale_fill_manual(values = c("Production" = "darkorange", "Export Quantity" = "brown")) +
  labs(title = "Produção e Exportação de Produtos de Madeira no Brasil (2022)",
       x = "Produto de Madeira",
       y = "Volume (m³)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  scale_y_continuous(labels = scales::comma_format(scale = 1, decimal.mark = ",", big.mark = "."))

