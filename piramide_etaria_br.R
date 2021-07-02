
library(openxlsx) # Abrir base de dados
library(dplyr) # Mexer na base de dados
library(ggplot2) # Fazer os gráficos
library(reshape2)
library(plyr)
library(stringr) # Tirar espaços em branco das strings

## Piramide Etaria do Brasil

# Abre base de dados

dados_piramide <- read.xlsx("C:/Users/gabri/Documents/Demografia/Dados_piramides.xlsx")

# Tira espaços em branco entre os valores da população

dados_piramide$Pop <- as.numeric(gsub(" ","",dados_piramide$Pop))

# Tira espaços em branco na variável sexo

dados_piramide$Sexo <- gsub(" ","",dados_piramide$Sexo)

## 1970

dados_piramide %>%
  filter(Ano == 1970) %>%
  mutate(Pop = ifelse(Sexo == "Homens", Pop * (-1),Pop)) %>%
  ggplot(aes(x = Grupos.de.idade, y = Pop, fill = Sexo)) + 
  geom_bar(subset = .(Sexo == "Mulheres"), stat = "identity") + 
  geom_bar(subset = .(Sexo == "Homens"), stat = "identity") + 
  scale_y_continuous(breaks = seq(-15000000, 15000000, 5000000), 
                     labels = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")) + 
  coord_flip() + 
  scale_fill_manual(values = c("Mulheres" = "red", "Homens" = "blue")) +
  labs(title = "Pirâmide Etária - Brasil 1970", y = "População (em milhões)",
       x = "Faixa Etária") +
  theme_bw()

## 1980

dados_piramide %>%
  filter(Ano == 1980) %>%
  mutate(Pop = ifelse(Sexo == "Homens", Pop * (-1),Pop)) %>%
  ggplot(aes(x = Grupos.de.idade, y = Pop, fill = Sexo)) + 
  geom_bar(subset = .(Sexo == "Mulheres"), stat = "identity") + 
  geom_bar(subset = .(Sexo == "Homens"), stat = "identity") + 
  scale_y_continuous(breaks = seq(-15000000, 15000000, 5000000), 
                     labels = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")) + 
  coord_flip() + 
  scale_fill_manual(values = c("Mulheres" = "red", "Homens" = "blue")) +
  labs(title = "Pirâmide Etária - Brasil 1980", y = "População (em milhões)",
       x = "Faixa Etária") +
  theme_bw()

## 1991

dados_piramide %>%
  filter(Ano == 1991) %>%
  mutate(Pop = ifelse(Sexo == "Homens", Pop * (-1),Pop)) %>%
  ggplot(aes(x = Grupos.de.idade, y = Pop, fill = Sexo)) + 
  geom_bar(subset = .(Sexo == "Mulheres"), stat = "identity") + 
  geom_bar(subset = .(Sexo == "Homens"), stat = "identity") + 
  scale_y_continuous(breaks = seq(-15000000, 15000000, 5000000), 
                     labels = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")) + 
  coord_flip() + 
  scale_fill_manual(values = c("Mulheres" = "red", "Homens" = "blue")) +
  labs(title = "Pirâmide Etária - Brasil 1991", y = "População (em milhões)",
       x = "Faixa Etária") +
  theme_bw()

## 2000

dados_piramide %>%
  filter(Ano == 2000) %>%
  mutate(Grupos.de.idade = factor(Grupos.de.idade, levels = c(
    "0 a 4 anos","5 a 9 anos","10 a 14 anos","15 a 19 anos","20 a 24 anos","25 a 29 anos",
    "30 a 34 anos","35 a 39 anos","40 a 44 anos","45 a 49 anos","50 a 54 anos","55 a 59 anos",
    "60 a 64 anos","65 a 69 anos","70 a 74 anos","75 a 79 anos","80 ou mais"
  ))) %>%
  mutate(Pop = ifelse(Sexo == "Homens", Pop * (-1),Pop)) %>%
  ggplot(aes(x = Grupos.de.idade, y = Pop, fill = Sexo)) + 
  geom_bar(subset = .(Sexo == "Mulheres"), stat = "identity") + 
  geom_bar(subset = .(Sexo == "Homens"), stat = "identity") + 
  scale_y_continuous(breaks = seq(-15000000, 15000000, 5000000), 
                     labels = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")) + 
  coord_flip() + 
  scale_fill_manual(values = c("Mulheres" = "red", "Homens" = "blue")) +
  labs(title = "Pirâmide Etária - Brasil 2000", y = "População (em milhões)",
       x = "Faixa Etária") +
  theme_bw()

## 2010

dados_piramide %>%
  filter(Ano == 2010) %>%
  mutate(Pop = ifelse(Sexo == "Homens", Pop * (-1),Pop)) %>%
  ggplot(aes(x = Grupos.de.idade, y = Pop, fill = Sexo)) + 
  geom_bar(subset = .(Sexo == "Mulheres"), stat = "identity") + 
  geom_bar(subset = .(Sexo == "Homens"), stat = "identity") + 
  scale_y_continuous(breaks = seq(-15000000, 15000000, 5000000), 
                     labels = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")) + 
  coord_flip() + 
  scale_fill_manual(values = c("Mulheres" = "red", "Homens" = "blue")) +
  labs(title = "Pirâmide Etária - Brasil 2010", y = "População (em milhões)",
       x = "Faixa Etária") +
  theme_bw()

## 2018

dados_piramide %>%
  filter(Ano == 2018) %>%
  mutate(Grupos.de.idade = factor(Grupos.de.idade, levels = c(
    "0 a 4 anos","5 a 9 anos","10 a 14 anos","15 a 19 anos","20 a 24 anos","25 a 29 anos",
    "30 a 34 anos","35 a 39 anos","40 a 44 anos","45 a 49 anos","50 a 54 anos","55 a 59 anos",
    "60 a 64 anos","65 a 69 anos","70 a 74 anos","75 a 79 anos","80 anos ou mais"
  ))) %>%
  mutate(Pop = ifelse(Sexo == "Homens", Pop * (-1),Pop)) %>%
  ggplot(aes(x = Grupos.de.idade, y = Pop, fill = Sexo)) + 
  geom_bar(subset = .(Sexo == "Mulheres"), stat = "identity") + 
  geom_bar(subset = .(Sexo == "Homens"), stat = "identity") + 
  scale_y_continuous(breaks = seq(-15000000, 15000000, 5000000), 
                     labels = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")) + 
  coord_flip() + 
  scale_fill_manual(values = c("Mulheres" = "red", "Homens" = "blue")) +
  labs(title = "Pirâmide Etária - Brasil 2018", y = "População (em milhões)",
       x = "Faixa Etária") +
  theme_bw()

## Dados 2030

dados_piramide %>%
  filter(Ano == 2030) %>%
  mutate(Grupos.de.idade = factor(Grupos.de.idade, levels = c(
    "0 a 4 anos","5 a 9 anos","10 a 14 anos","15 a 19 anos","20 a 24 anos","25 a 29 anos",
    "30 a 34 anos","35 a 39 anos","40 a 44 anos","45 a 49 anos","50 a 54 anos","55 a 59 anos",
    "60 a 64 anos","65 a 69 anos","70 a 74 anos","75 a 79 anos","80 a 84 anos","85 a 89 anos",
    "90 anos ou mais"
  ))) %>%
  mutate(Pop = ifelse(Sexo == "Homens", Pop * (-1),Pop)) %>%
  ggplot(aes(x = Grupos.de.idade, y = Pop, fill = Sexo)) + 
  geom_bar(subset = .(Sexo == "Mulheres"), stat = "identity") + 
  geom_bar(subset = .(Sexo == "Homens"), stat = "identity") + 
  scale_y_continuous(breaks = seq(-15000000, 15000000, 5000000), 
                     labels = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")) + 
  coord_flip() + 
  scale_fill_manual(values = c("Mulheres" = "red", "Homens" = "blue")) +
  labs(title = "Pirâmide Etária - Brasil 2030", y = "População (em milhões)",
       x = "Faixa Etária") +
  theme_bw()

## Dados 2040

dados_piramide %>%
  filter(Ano == 2040) %>%
  mutate(Grupos.de.idade = factor(Grupos.de.idade, levels = c(
    "0 a 4 anos","5 a 9 anos","10 a 14 anos","15 a 19 anos","20 a 24 anos","25 a 29 anos",
    "30 a 34 anos","35 a 39 anos","40 a 44 anos","45 a 49 anos","50 a 54 anos","55 a 59 anos",
    "60 a 64 anos","65 a 69 anos","70 a 74 anos","75 a 79 anos","80 a 84 anos","85 a 89 anos",
    "90 anos ou mais"
  ))) %>%
  mutate(Pop = ifelse(Sexo == "Homens", Pop * (-1),Pop)) %>%
  ggplot(aes(x = Grupos.de.idade, y = Pop, fill = Sexo)) + 
  geom_bar(subset = .(Sexo == "Mulheres"), stat = "identity") + 
  geom_bar(subset = .(Sexo == "Homens"), stat = "identity") + 
  scale_y_continuous(breaks = seq(-15000000, 15000000, 5000000), 
                     labels = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")) + 
  coord_flip() + 
  scale_fill_manual(values = c("Mulheres" = "red", "Homens" = "blue")) +
  labs(title = "Pirâmide Etária - Brasil 2040", y = "População (em milhões)",
       x = "Faixa Etária") +
  theme_bw()

## Dados 2050

dados_piramide %>%
  filter(Ano == 2050) %>%
  mutate(Grupos.de.idade = factor(Grupos.de.idade, levels = c(
    "0 a 4 anos","5 a 9 anos","10 a 14 anos","15 a 19 anos","20 a 24 anos","25 a 29 anos",
    "30 a 34 anos","35 a 39 anos","40 a 44 anos","45 a 49 anos","50 a 54 anos","55 a 59 anos",
    "60 a 64 anos","65 a 69 anos","70 a 74 anos","75 a 79 anos","80 a 84 anos","85 a 89 anos",
    "90 anos ou mais"
  ))) %>%
  mutate(Pop = ifelse(Sexo == "Homens", Pop * (-1),Pop)) %>%
  ggplot(aes(x = Grupos.de.idade, y = Pop, fill = Sexo)) + 
  geom_bar(subset = .(Sexo == "Mulheres"), stat = "identity") + 
  geom_bar(subset = .(Sexo == "Homens"), stat = "identity") + 
  scale_y_continuous(breaks = seq(-15000000, 15000000, 5000000), 
                     labels = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")) + 
  coord_flip() + 
  scale_fill_manual(values = c("Mulheres" = "red", "Homens" = "blue")) +
  labs(title = "Pirâmide Etária - Brasil 2050", y = "População (em milhões)",
       x = "Faixa Etária") +
  theme_bw()

## Dados 2060

dados_piramide %>%
  filter(Ano == 2060) %>%
  mutate(Grupos.de.idade = factor(Grupos.de.idade, levels = c(
    "0 a 4 anos","5 a 9 anos","10 a 14 anos","15 a 19 anos","20 a 24 anos","25 a 29 anos",
    "30 a 34 anos","35 a 39 anos","40 a 44 anos","45 a 49 anos","50 a 54 anos","55 a 59 anos",
    "60 a 64 anos","65 a 69 anos","70 a 74 anos","75 a 79 anos","80 a 84 anos","85 a 89 anos",
    "90 anos ou mais"
  ))) %>%
  mutate(Pop = ifelse(Sexo == "Homens", Pop * (-1),Pop)) %>%
  ggplot(aes(x = Grupos.de.idade, y = Pop, fill = Sexo)) + 
  geom_bar(subset = .(Sexo == "Mulheres"), stat = "identity") + 
  geom_bar(subset = .(Sexo == "Homens"), stat = "identity") + 
  scale_y_continuous(breaks = seq(-15000000, 15000000, 5000000), 
                     labels = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")) + 
  coord_flip() + 
  scale_fill_manual(values = c("Mulheres" = "red", "Homens" = "blue")) +
  labs(title = "Pirâmide Etária - Brasil 2060", y = "População (em milhões)",
       x = "Faixa Etária") +
  theme_bw()

#--------