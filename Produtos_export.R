if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, tibble, ggplot2, car, agricolae, tidyverse, forcats)
# dados
toras <- read.table("export_3.csv", h=T,sep=",", dec = ".")

library(reprex)

#####1. Ajustando as variáveis
toras$ano=as.factor(toras$ano)

#####3. Dinamica anual de todos os produtos
produtos_anual= toras %>%
  group_by(produto_padronizado,ano) %>%
  summarise(
    sum=sum(volume,na.rm = TRUE))

#####2 APENAS PRODUTO TORA
#2.1.Tirar tora da tabela
apenas_tora=filter(toras,produto_padronizado=="tora")

#####3.Verificar as estatísticas para tora e ver quanto representa

#####4. Dinamica anual apenas da tora
tora_ano= apenas_tora %>%
  group_by(ano) %>%
  summarise(
    mean = mean(volume, na.rm = TRUE),
    sum=sum(volume,na.rm = TRUE),
    sd = sd(volume, na.rm = TRUE),
    var= var(volume, na.rm=TRUE))

######5. OUTROS PRODUTOS NÃO TORA
sem_toras=filter(toras,produto_padronizado!="tora")

#####3. Dinamica anual dos produtos sem tora
sem_toras$ano=as.factor(sem_toras$ano)
toras_ano_produto= sem_toras %>%
  group_by(produto_padronizado, exportado_mun_portos) %>%
  summarise(
    mean = mean(volume, na.rm = TRUE),
    sum=sum(volume,na.rm = TRUE),
    sd = sd(volume, na.rm = TRUE),
    var= var(volume, na.rm=TRUE))

#####4. Dinamica anual
sem_toras$ano=as.factor(toras$ano)
sem_toras_ano= toras %>%
  group_by(ano) %>%
  summarise(
    mean = mean(volume, na.rm = TRUE),
    sum=sum(volume,na.rm = TRUE),
    sd = sd(volume, na.rm = TRUE),
    var= var(volume, na.rm=TRUE))


#####5. Dinamica anual e produtos
sem_toras$ano=as.factor(toras$ano)
com_tora_ano= toras %>%
  group_by(ano, produto_padronizado,exportado_mun_portos) %>%
  summarise(
    mean = mean(volume, na.rm = TRUE),
    sum=sum(volume,na.rm = TRUE),
    sd = sd(volume, na.rm = TRUE),
    var= var(volume, na.rm=TRUE))


sum(sem_toras$volume)

######################GRAFICOS###################

## 1. grafico linhas comparando produto exportado e produto nacional

ggplot(data = com_tora_ano, aes(x = ano, y =sum/1000, group = produto_padronizado)) +
  geom_line(aes(colour = produto_padronizado), size=0.8)+
  facet_wrap(~exportado_mun_portos)
  labs(y="Volume (dam³)",x="Ano") 

#####2. Geobar
ggplot(data=com_tora_ano, aes(x=ano, y=sum/1000, fill=produto_padronizado)) +
  geom_bar(stat="identity") +
  facet_wrap(~exportado_mun_portos)
  labs(y="Volume (dam³)",x="Ano", colour= "Produto") +
  theme_minimal()


##################################Exportação

toras <- read.table("produtos.csv", h=T,sep=",")

#####1. Ajustando as variáveis
toras$ano=as.factor(toras$ano)

#####2. Agrupando mercados nacional e internacional

toras_ano_ssp= toras %>%
  group_by(exportado_mun_portos, ano) %>%
  summarise(
    mean = mean(volume, na.rm = TRUE),
    sum=sum(volume,na.rm = TRUE),
    sd = sd(volume, na.rm = TRUE),
    var= var(volume, na.rm=TRUE))

####3. geobar exportação
toras_ano_ssp$exportado_mun_portos=as.character(toras_ano_ssp$exportado_mun_portos)

ggplot(data=toras_ano_ssp, aes(x=ano, y=sum, fill=exportado_mun_portos)) +
  geom_bar(stat="identity") +
  labs(y="Volume",x="Ano", colour= "exportado_mun_portos") +
  theme_bw()

ggplot(data = toras_ano_ssp, aes(x = ano, y =sum, group = exportado_mun_portos)) +
  geom_line(aes(colour = exportado_mun_portos), size=0.9)+
  labs(y="Volume (m³)",x="Ano", colour = "exportado_mun_portos") 

####Exportação sem tora

toras <- read.table("produtos_semtora2.csv", h=T,sep=",")
####1. Ajustando as variáveis
toras$ano=as.factor(toras$ano)
toras$Mercados=as.factor(toras$Mercados)
#####2. Agrupando mercados
toras_ano_ssp= toras %>%
  group_by(Mercados, ano) %>%
  summarise(
    mean = mean(volume, na.rm = TRUE),
    sum=sum(volume,na.rm = TRUE),
    sd = sd(volume, na.rm = TRUE),
    var= var(volume, na.rm=TRUE))
#######3. geomline

toras_ano_ssp$Mercados <- ordered(toras_ano_ssp$Mercados, levels = c("nao",
                                                                     "sim"))
ggplot(data = toras_ano_ssp, aes(x = ano, y =sum, group = Mercados)) +
  geom_line(aes(colour = Mercados), size=0.9)+
  scale_y_continuous(limits = c(0, 600000), breaks = seq(0, 600000, 50000))+
  labs(y="Volume (m³)",x="Ano") + scale_colour_manual("Mercados",
                                                      values = c("darkgreen", "darkorange"),
                                                      labels = c( "Nacional","Internacional"))

####4. geobar exportação
ggplot(data = toras_ano_ssp, aes(x = ano, y =sum, fill = Mercados)) +
  geom_bar(stat="identity") +
  scale_y_continuous(limits = c(0, 600000), breaks = seq(0, 600000, 50000))+
  labs(y="Volume (m³)",x="Ano") + scale_fill_manual("Mercados",
                                                    values = c("darkgreen", "darkorange"),
                                                    labels = c( "Nacional","Internacional"))

#####DADOS POR ESTADO

produtos_estado= toras %>%
  group_by(uf_destino, Mercados) %>%
  summarise(
    mean = mean(volume, na.rm = TRUE),
    sum=sum(volume,na.rm = TRUE),
    sd = sd(volume, na.rm = TRUE),
    var= var(volume, na.rm=TRUE))

