# TRABALHO 1 - QUESTÃO 2

library("read.dbc")
library("tidyverse")
library("foreign")
library("readr")
library("nortest")
library("EnvStats")
library("vcd")

# PARTE 1 ----


# Leitura dos bancos de dados
Sinasc_2019 <- read.dbc(file.choose())
Sinasc_2020 <- read.dbc(file.choose())
Sinasc_2021 <- read.dbf(file.choose())

# Filtro para o Estado de Tocantins
Sinasc_2021 <- Sinasc_2021 %>% 
               filter(CODMUNRES %in% Sinasc_2020$CODMUNRES)

# População Geral por Idade Estimada - Tocantins - 2020
Pop_TO_2020 <- read_delim("C:/Users/Eduardo/Desktop/Trabalho 1 - Demografia/Tocantins - Geral.txt", 
               delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
               .[2:20,]

# População Feminina por Idade Estimada - Tocantins - 2020
Pop_FEM_TO_2020 <- read_delim("C:/Users/Eduardo/Desktop/Trabalho 1 - Demografia/Tocantins - Mulheres.txt", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
                   .[2:20,]

# Filtragem dos bancos de dados 
Sinasc_2019 <- Sinasc_2019 %>% select(5,7,14,16,18,36) 
Sinasc_2020 <- Sinasc_2020 %>% select(5,7,14,16,18,36)
Sinasc_2021 <- Sinasc_2021 %>% select(6,8,15,17,19,38)


# 2021 - Escolaridade da Mãe
Sinasc_2021$ESC <- factor(Sinasc_2021$ESCMAE, 
                          label = c("Nenhuma","1 a 3 anos","4 a 7 anos",
                                    "8 a 11 anos","12 e mais","Ignorado"), 
                          level = c(1,2,3,4,5,9))

# 2021 - Tipo de Parto
Sinasc_2021$Parto <- factor(Sinasc_2021$PARTO, 
                            label = c("Vaginal","Cesáreo","Ignorado"), 
                            level = c(1,2,9))

# 2021 - Sexo
Sinasc_2021$Sexo <- factor(Sinasc_2021$SEXO, 
                            label = c("Ignorado","Masculino","Feminino"), 
                            level = c(0,1,2))

# 2021 - Idade da Mãe
Sinasc_2021$Idade <-  cut(as.numeric(as.vector(Sinasc_2021$IDADEMAE)), 
                          breaks=c(0,4,9,14,19,24,29,34,39,44,49,
                                   54,59,64,69,74,79,84,89,150), 
                      labels=c("00 |--| 04", "05 |--| 09","10 |--| 14", 
                               "15 |--| 19", "20 |--| 24","25 |--| 29",
                               "30 |--| 34", "35 |--| 39","40 |--| 44", 
                               "45 |--| 49", "50 |--| 54","55 |--| 59",
                               "60 |--| 64", "65 |--| 69","70 |--| 74",
                               "75 |--| 79", "80 |--| 84","85 |--| 89",
                               "+ 90"), right=T)

# 2020 - Sexo
Sinasc_2020$Sexo <- factor(Sinasc_2020$SEXO, 
                           label = c("Ignorado","Masculino","Feminino"), 
                           level = c(0,1,2))

# 2020 - Idade da Mãe
Sinasc_2020$Idade <-  cut(as.numeric(as.vector(Sinasc_2020$IDADEMAE)), 
                          breaks=c(0,4,9,14,19,24,29,34,39,44,49,
                                   54,59,64,69,74,79,84,89,150), 
                      labels=c("00 |--| 04", "05 |--| 09","10 |--| 14", 
                               "15 |--| 19", "20 |--| 24","25 |--| 29",
                               "30 |--| 34", "35 |--| 39","40 |--| 44", 
                               "45 |--| 49", "50 |--| 54","55 |--| 59",
                               "60 |--| 64", "65 |--| 69","70 |--| 74",
                               "75 |--| 79", "80 |--| 84","85 |--| 89",
                               "+ 90"), right=T)

# 2019 - Sexo
Sinasc_2019$Sexo <- factor(Sinasc_2019$SEXO, 
                           label = c("Ignorado","Masculino","Feminino"), 
                           level = c(0,1,2))

# 2019 - Idade da Mãe
Sinasc_2019$Idade <-  cut(as.numeric(as.vector(Sinasc_2019$IDADEMAE)), 
                          breaks=c(0,4,9,14,19,24,29,34,39,44,49,
                                   54,59,64,69,74,79,84,89,150), 
                      labels=c("00 |--| 04", "05 |--| 09","10 |--| 14", 
                               "15 |--| 19", "20 |--| 24","25 |--| 29",
                               "30 |--| 34", "35 |--| 39","40 |--| 44", 
                               "45 |--| 49", "50 |--| 54","55 |--| 59",
                               "60 |--| 64", "65 |--| 69","70 |--| 74",
                               "75 |--| 79", "80 |--| 84","85 |--| 89",
                               "+ 90"), right=T)

table(Sinasc_2021$Sexo)
# PARTE 2 ----

# TAXA BRUTA DE NATALIDADE - TBN

TBN_2019 <- 1000 * length(Sinasc_2019$PARTO) / sum(Pop_TO_2020$`2020`)
TBN_2019

TBN_2020 <- 1000 * length(Sinasc_2020$PARTO) / sum(Pop_TO_2020$`2020`)
TBN_2020

TBN_2021 <- 1000 * length(Sinasc_2021$PARTO) / sum(Pop_TO_2020$`2020`)
TBN_2021


# TAXA FECUNDIDADE GERAL - TFG

TFG_2019 <- 1000 * length(Sinasc_2019$PARTO) / sum(Pop_FEM_TO_2020$`2020`[4:10])
TFG_2019

TFG_2020 <- 1000 * length(Sinasc_2020$PARTO) / sum(Pop_FEM_TO_2020$`2020`[4:10])
TFG_2020

TFG_2021 <- 1000 * length(Sinasc_2021$PARTO) / sum(Pop_FEM_TO_2020$`2020`[4:10])
TFG_2021


# TAXAS ESPECÍFICAS DE FECUNDIDADE - nfx

Tab1 <- Sinasc_2019 %>% 
        group_by(Idade) %>% 
        summarise(Nasc_2019 = n())

Tab2 <- Sinasc_2020 %>% 
        group_by(Idade) %>% 
        summarise(Nasc_2020 = n())

Tab3 <- Sinasc_2021 %>% 
        group_by(Idade) %>% 
        summarise(Nasc_2021 = n())

# Nascidos Vivos por Idade
Nasc_Idade <- cbind(Tab1[2:8,],Tab2[2:8,],Tab3[2:8,]) %>% 
              select(1,2,4,6)

nfx_2019 <- 1000 * Nasc_Idade$Nasc_2019 / Pop_FEM_TO_2020$`2020`[4:10]
nfx_2019

nfx_2020 <- 1000 * Nasc_Idade$Nasc_2020 / Pop_FEM_TO_2020$`2020`[4:10]
nfx_2020

nfx_2021 <- 1000 * Nasc_Idade$Nasc_2021 / Pop_FEM_TO_2020$`2020`[4:10]
nfx_2021


# TAXA DE FECUNDIDADE TOTAL - TFT ou ÍNDICE SINTÉTICO DE FECUNDIDADE

TFT_2019 <- 5 * sum(nfx_2019 / 1000)
TFT_2019

TFT_2020 <- 5 * sum(nfx_2020 / 1000)
TFT_2020

TFT_2021 <- 5 * sum(nfx_2021 / 1000)
TFT_2021


# TAXAS ESPECÍFICAS DE FECUNDIDADE FEMININA

Tab4 <- Sinasc_2019 %>% 
        filter(Sexo == "Feminino") %>%
        group_by(Idade) %>% 
        summarise(Nasc_2019 = n())

Tab5 <- Sinasc_2020 %>% 
        filter(Sexo == "Feminino") %>%
        group_by(Idade) %>% 
        summarise(Nasc_2020 = n())

Tab6 <- Sinasc_2021 %>% 
        filter(Sexo == "Feminino") %>%
        group_by(Idade) %>% 
        summarise(Nasc_2021 = n())

# Nascidos Vivos por Idade
Nasc_Fem_Idade <- cbind(Tab4[2:8,],Tab5[2:8,],Tab6[2:8,]) %>% 
                  select(1,2,4,6)


TEFF_2019 <- 1000 * Nasc_Fem_Idade$Nasc_2019 / Pop_FEM_TO_2020$`2020`[4:10]
TEFF_2019

TEFF_2020 <- 1000 * Nasc_Fem_Idade$Nasc_2020 / Pop_FEM_TO_2020$`2020`[4:10]
TEFF_2020

TEFF_2021 <- 1000 * Nasc_Fem_Idade$Nasc_2021 / Pop_FEM_TO_2020$`2020`[4:10]
TEFF_2021


# TAXA BRUTA DE REPRODUÇÃO

TBR_2019 <- 5 * sum(TEFF_2019 / 1000)
TBR_2019

TBR_2020 <- 5 * sum(TEFF_2020 / 1000)
TBR_2020

TBR_2021 <- 5 * sum(TEFF_2021 / 1000)
TBR_2021


# TAXA LÍQUIDA DE REPRODUÇÃO

nLx <- c(4.923377874, 4.912320715, 4.898980062, 4.881555024, 
         4.85779413, 4.823499343, 4.770804287)

TLR_2019 <- sum((TEFF_2019 * nLx) / 1000)
TLR_2019

TLR_2020 <- sum((TEFF_2020 * nLx) / 1000)
TLR_2020

TLR_2021 <- sum((TEFF_2021 * nLx) / 1000)
TLR_2021


# Parte 3 ----

# Estudo de Associação - Idade da Mãe e Escolaridade da Mãe

# Medidas de posição e variabilidade

Sinasc_2021$Idade_numeric <- Sinasc_2021$IDADEMAE %>% 
                             as.vector() %>% 
                             as.numeric()

Sinasc_2021 %>%
  filter(!is.na(ESC)) %>% 
  group_by(ESC) %>%
  summarise(media = mean(Idade_numeric),
            dv_padrão = sd(Idade_numeric),
            num = n(),
            min = min(Idade_numeric),
            max = max(Idade_numeric),
            q25 = quantile(Idade_numeric, probs = 0.25),
            mediana = median(Idade_numeric),
            q75 = quantile(Idade_numeric, probs = 0.75))

# Boxplot 
Sinasc_2021 %>%
  filter(!is.na(ESC), ESC != "Ignorado") %>% 
  ggplot(aes(x = ESC, y = Idade_numeric)) +
  geom_boxplot(fill = c("#329ea8"), width = 0.5) +
    stat_summary(fun = "mean", geom = "point", shape = 23,
                 size = 3, fill = "white") +
    labs(x = "Escolaridade da Mãe", y = "Idade da Mãe") +
    theme_bw()


## Cálculo de Associação - R2
media_variancias = (39*6.71^2 + 310*7.33^2 + 2672*7.34^2 +
                    15037*6.24^2 + 5520*5.62^2) / 23578
media_variancias

R2 = 1 - (media_variancias / var(Sinasc_2021$Idade_numeric))
R2 # 0.11625


# Teste de hipóteses: ANOVA e Kruskal-Wallis

# Ho: u1 = u2 = u3 = u4 = u5
# Ha: Ao menos uma das médias é diferente

dados_teste <- Sinasc_2021 %>%
               filter(!is.na(ESC), ESC != "Ignorado") %>% 
               .[runif(200,10,55),]

# Normalidade dos dados rejeitada
shapiro.test(dados_teste$Idade_numeric[runif(300,10,60)])
lillie.test(dados_teste$Idade_numeric[runif(300,10,60)])  # Lilliefors
ad.test(dados_teste$Idade_numeric[runif(300,10,60)])

# ANOVA
summary(aov(dados_teste$Idade_numeric~dados_teste$ESC))

# Kruskall-Wallis
kruskal.test(dados_teste$Idade_numeric~dados_teste$ESC)

# Comparações
pairwise.t.test(dados_teste$Idade_numeric, dados_teste$ESC, 
                p.adjust.method = "bonferroni")

pairwise.wilcox.test(dados_teste$Idade_numeric, dados_teste$ESC, 
                     p.adjust.method = "bonf")



# Estudo de Associação - Tipo de Parto e Escolaridade da Mãe


# Qui-Quadrado

table(Sinasc_2021$ESC, Sinasc_2021$Parto) 
prop.table(table(Sinasc_2021$ESC, Sinasc_2021$Parto),1) 

valores <- c(23,165,1460,7224,1496,16,144,1201,7800,4022)

tabela_x2 <- matrix(valores, byrow = F, ncol = 2)

aa <- chisq.test(tabela_x2)
aa$expected
aa

assocstats(tabela_x2)

associacao <- (0.19)/((2-1)/2)**(1/2)
associacao # 0.2687


# Gráficos 
dados_teste <- Sinasc_2021 %>%
               filter(!is.na(ESC), 
                      ESC != "Ignorado",
                      !is.na(Parto))

ggplot(dados_teste) + # freq relativa
  geom_bar(mapping = aes(x = ESC, fill = Parto),
           position = "fill") +
  labs(title = "",
       x ="Escolaridade da Mãe", 
       y ="Frequência Relativa") +
  theme(legend.position = "right", 
        legend.title = element_blank()) +
  scale_color_manual(values = c("#13bdbd","#ed5858")) +
  scale_fill_manual(values = c("#13bdbd","#ed5858")) +
  scale_y_continuous(breaks = seq(from = 0,to = 1,
                     by = 0.2), limits = c(0,1)) +
  theme_minimal()

nfx_2000 <- c(142.82,198.75,122.54,61.61,30.12,11.11,1.91)
nfx_2005 <- c(118.91,167.84,116.67,57.12,27.29,8.80,0.83)
nfx_2011 <- c(90.30,123.68,102.30,64.34,30.53,7.82,0.63)
Idades <- c("15 a 19","20 a 24","25 a 29","30 a 34",
            "35 a 39","40 a 44","45 a 49")

nfx_data <- cbind(nfx_2000,nfx_2005,nfx_2011,
                  nfx_2019,nfx_2020,nfx_2021) %>% 
            data.frame(Idades = Idades)

sum(Pop_FEM_TO_2020$`2011`[4:10])
