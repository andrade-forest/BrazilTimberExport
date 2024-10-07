if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)
# dados
toras <- read.table("export_3.csv", h=T,sep=",", dec = ".")

library(reprex)

#####1. Ajustando as variáveis
toras$ano=as.factor(toras$ano)

#####2 APENAS PRODUTO TORA
#2.1.Tirar tora da tabela
sem_tora=filter(toras,produto_padronizado!="tora")

export_filtrado <- sem_tora%>%
  filter(exportacao == "Sim" & exportado_mun_portos == "1")

export_filtrado2 <- sem_tora%>%
  filter(exportacao == "" & exportado_mun_portos == "1")

export_combinado <- rbind(export_filtrado, export_filtrado2)

#Espécies Florestais mais exportadas

export_combinado$ano <- factor(export_combinado$ano)

#origem_geral
ssp= export_combinado %>%
  group_by(nome_imaflora) %>%
  summarise(
    sum=sum(volume,na.rm = TRUE))

#CONJUNTO DE DADOS AGREGADOS 
#Novo conjunto de dados com os 5 maiores uf_destino e agrupar os demais em "Outros"
ssp_raking <- ssp %>%
  group_by(nome_imaflora) %>%
  summarize(total_sum = sum(sum)) %>%
  arrange(desc(total_sum)) %>%
  slice_head(n = 15) %>%
  mutate(nome_imaflora_class = as.character(nome_imaflora))

# Ordene as espécies pelo volume total
ssp_raking <- ssp_raking %>%
  arrange(desc(total_sum))



# Gráfico de barras
ggplot(ssp_raking, aes(x = total_sum, y = reorder(nome_imaflora, total_sum))) +
  geom_bar(stat = "identity", fill = "brown") +
  labs(title = "15 Principais espécies exportadas",
       x = "Volume (m³)",
       y = "Espécies") +
  scale_x_continuous(labels = scales::number_format(scale = 1e-3, suffix = " Mil")) +  # Alterado para mil
  scale_y_discrete(labels = c("handroanthus serratifolius" = "Handroanthus serratifolius", 
                              "hymenaea courbaril" = "Hymenaea courbaril", 
                              "manilkara huberi" = "Manilkara huberi", 
                              "Não classificado" = "Não classificado", 
                              "dinizia excelsa" = "Dinizia excelsa", 
                              "dipteryx odorata" = "Dipteryx odorata", 
                              "virola surinamensis" = "Virola surinamensis", 
                              "couratari guianensis" = "Couratari guianensis", 
                              "handroanthus ssp" = "Handroanthus ssp", 
                              "apuleia leiocarpa" = "Apuleia leiocarpa", 
                              "astronium lecointei" = "Astronium lecointei", 
                              "mezilaurus itauba" = "Mezilaurus itauba", 
                              "carapa guianensis" = "Carapa guianensis", 
                              "couratari oblongifolia" = "Couratari oblongifolia", 
                              "macrolobium pendulum" = "Macrolobium pendulum")) + 
  theme_minimal()

