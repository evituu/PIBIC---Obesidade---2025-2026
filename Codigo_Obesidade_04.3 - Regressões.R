#------------------------------------------------------------------------------#
###                  CÃ“DIGO OBESIDADE INTERGERACIONAL                        ###
#------------------------------------------------------------------------------#
# Aluno: Victor Eduardo
# Doscente: Adriano Firmino V. AraÃºjo

# ---------------------------------------------------------------------------
# EXTRAÃ‡ÃƒO POR readr::read_fwf + fwf_cols (POF 2017-2018 | MORADOR.txt)
# ---------------------------------------------------------------------------

getwd()
setwd("C:/Users/vitor/OneDrive/Ãrea de Trabalho/UFPB/PIBIC_2025/Base de Dados/POFF_Dados_2008_2009")

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(readxl)
  library(quantreg)
  library(tibble)
  library(tidyr)
  library(haven)
  library(anthro)
  library(childsds)
  library(survey)
})

## -------------------------------------------------------------------------- ##
##                           MODELOS ECONOMÉTRICOS                            ##
## -------------------------------------------------------------------------- ##

## -------------------------------------------------------------------------- ##
###       RegressÃ£o MQO e Quantílica: elasticidades intergeracionais        ###
## -------------------------------------------------------------------------- ##

# Amostra biparental, sem missing
amostra_reg <- base_unida_consolidada |>
  filter(
    !is.na(imc_filho),
    !is.na(imc_pai), !is.na(imc_mae),
    imc_filho > 0, imc_pai > 0, imc_mae > 0
  ) |>
  mutate(
    ln_imc_filho = log(imc_filho),
    ln_imc_pai   = log(imc_pai),
    ln_imc_mae   = log(imc_mae)
  )

# FÃ³rmula
form <- ln_imc_filho ~ ln_imc_pai + ln_imc_mae +
  idade_filho + I(idade_filho^2) +
  branco_filho + freq_escola_filho +
  n_moradores + ln_renda_total +
  dummy_norte + dummy_nordeste + dummy_sul + dummy_centro_oeste
# + dummies de instrucao_resp / instrucao_conj, se quiser

# MQO
mqo <- lm(form, data = amostra_reg)
summary(mqo)

# RegressÃ£o quantÃ­lica: mediana, q90 e q95
rq50 <- rq(form, tau = 0.5, data = amostra_reg)
rq90 <- rq(form, tau = 0.9, data = amostra_reg)
rq95 <- rq(form, tau = 0.95, data = amostra_reg)

summary(rq95, se = "boot", R = 200)
summary(rq90, se = "boot", R = 200)
summary(rq50, se = "boot", R = 200)

# Filtrar por sexo
amostra_meninOS  <- amostra_reg |> filter(sexo_filho == 1)
amostra_meninAS  <- amostra_reg |> filter(sexo_filho == 2)

# MQO para meninos
mqo_meninos <- lm(form, data = amostra_meninOS)
summary(mqo_meninos)

# MQO para meninas
mqo_meninas <- lm(form, data = amostra_meninAS)
summary(mqo_meninas)

# ---------------------------------------------------------------------------#
#          RegressÃµes quantÃ­licas por sexo do filho (Ï„ = 0.5, 0.9, 0.95)     #
# ---------------------------------------------------------------------------#

# Meninos
rq50_meninos <- rq(form, tau = 0.5, data = amostra_meninOS)
rq90_meninos <- rq(form, tau = 0.9, data = amostra_meninOS)
rq95_meninos <- rq(form, tau = 0.95, data = amostra_meninOS)

sum_rq50_meninos <- summary(rq50_meninos, se = "boot", R = 200)
sum_rq90_meninos <- summary(rq90_meninos, se = "boot", R = 200)
sum_rq95_meninos <- summary(rq95_meninos, se = "boot", R = 200)

sum_rq50_meninos
sum_rq90_meninos
sum_rq95_meninos

# Meninas
rq50_meninas <- rq(form, tau = 0.5, data = amostra_meninAS)
rq90_meninas <- rq(form, tau = 0.9, data = amostra_meninAS)
rq95_meninas <- rq(form, tau = 0.95, data = amostra_meninAS)

sum_rq50_meninas <- summary(rq50_meninas, se = "boot", R = 200)
sum_rq90_meninas <- summary(rq90_meninas, se = "boot", R = 200)
sum_rq95_meninas <- summary(rq95_meninas, se = "boot", R = 200)

sum_rq50_meninas
sum_rq90_meninas
sum_rq95_meninas

# FunÃ§Ã£o helper para pegar coeficientes de pai/mÃ£e de um modelo
extrai_betas <- function(modelo, nome_modelo, grupo) {
  coefs <- coef(modelo)
  tibble(
    grupo   = grupo,
    modelo  = nome_modelo,
    beta_pai = unname(coefs["ln_imc_pai"]),
    beta_mae = unname(coefs["ln_imc_mae"])
  )
}

# Tabelas de elasticidades
resultados_total <- bind_rows(
  extrai_betas(mqo,  "MQO_total",  "Total"),
  extrai_betas(rq50, "RQ50_total", "Total"),
  extrai_betas(rq90, "RQ90_total", "Total"),
  extrai_betas(rq95, "RQ95_total", "Total")
)

# Tabelas de elasticidades para MENINOS
resultados_meninos <- bind_rows(
  extrai_betas(mqo_meninos,  "MQO_meninos",  "Meninos"),
  extrai_betas(rq50_meninos, "RQ50_meninos", "Meninos"),
  extrai_betas(rq90_meninos, "RQ90_meninos", "Meninos"),
  extrai_betas(rq95_meninos, "RQ95_meninos", "Meninos")
)

# Tabelas de elasticidades para MENINAS
resultados_meninas <- bind_rows(
  extrai_betas(mqo_meninas,  "MQO_meninas",  "Meninas"),
  extrai_betas(rq50_meninas, "RQ50_meninas", "Meninas"),
  extrai_betas(rq90_meninas, "RQ90_meninas", "Meninas"),
  extrai_betas(rq95_meninas, "RQ95_meninas", "Meninas")
)

# Juntar tudo numa tabela sÃ³
tabela_elasticidades <- bind_rows(
  resultados_total,
  resultados_meninos,
  resultados_meninas
)

tabela_elasticidades

tabela_organizada <- tabela_elasticidades |>
  mutate(
    modelo = factor(modelo, levels = c(
      "MQO_total","RQ50_total","RQ90_total","RQ95_total",
      "MQO_meninos","RQ50_meninos","RQ90_meninos","RQ95_meninos",
      "MQO_meninas","RQ50_meninas","RQ90_meninas","RQ95_meninas"
    ))
  ) |>
  arrange(grupo, modelo)

tabela_organizada

tabela_formatada <- tabela_elasticidades |>
  mutate(
    beta_pai = round(beta_pai, 3),
    beta_mae = round(beta_mae, 3),
    modelo = recode(modelo,
                    "MQO_total" = "MQO",
                    "RQ50_total" = "RQ(50)",
                    "RQ90_total" = "RQ(90)",
                    "RQ95_total" = "RQ(95)",
                    "MQO_meninos" = "MQO",
                    "RQ50_meninos" = "RQ(50)",
                    "RQ90_meninos" = "RQ(90)",
                    "RQ95_meninos" = "RQ(95)",
                    "MQO_meninas" = "MQO",
                    "RQ50_meninas" = "RQ(50)",
                    "RQ90_meninas" = "RQ(90)",
                    "RQ95_meninas" = "RQ(95)"
    )
  ) |>
  arrange(grupo, modelo)

tabela_formatada

summary(base_unida_consolidada$idade_resp)
summary(base_unida_consolidada$idade_filho)
summary(base_unida_consolidada$idade_conj)
count(base_unida_consolidada)

# Salvar todo o resultado:
resultados <- list(
  dados_obesidade = dados_obesidade,
  dados_obesidade_trat = dados_obesidade_trat,
  base_unida_consolidada = base_unida_consolidada,
  #Matriz_Transicao = Matriz_Transicao,
  #Mobility_Index = Mobility_Index,
  #Legenda_Microrregiao = Legenda_Microrregiao,
  mqo = mqo,
  rq50 = rq50,
  rq90 = rq90,
  rq95 = rq95,
  mqo_meninos = mqo_meninos,
  mqo_meninas = mqo_meninas,
  rq50_meninos = rq50_meninos,
  rq90_meninos = rq90_meninos,
  rq95_meninos = rq95_meninos,
  rq50_meninas = rq50_meninas,
  rq90_meninas = rq90_meninas,
  rq95_meninas = rq95_meninas,
  tabela_formatada = tabela_formatada
)

# Salvar resultados:
#saveRDS(resultados, "resultados_obesidade_intergeracional.rds")

# Rodar novamente (carregar resultados)
resultados <- readRDS("resultados_obesidade_intergeracional.rds")

# Bases
dados_obesidade               <- resultados$dados_obesidade
dados_obesidade_trat          <- resultados$dados_obesidade_trat
base_unida_consolidada        <- resultados$base_unida_consolidada

# Matrizes e Ã­ndices
#Matriz_Transicao              <- resultados$Matriz_Transicao
#Mobility_Index                <- resultados$Mobility_Index
#Legenda_Microrregiao          <- resultados$Legenda_Microrregiao

# Modelos total
mqo                           <- resultados$mqo
rq50                          <- resultados$rq50
rq90                          <- resultados$rq90
rq95                          <- resultados$rq95

# Modelos por sexo
mqo_meninos                   <- resultados$mqo_meninos
mqo_meninas                   <- resultados$mqo_meninas

rq50_meninos                  <- resultados$rq50_meninos
rq90_meninos                  <- resultados$rq90_meninos
rq95_meninos                  <- resultados$rq95_meninos

rq50_meninas                  <- resultados$rq50_meninas
rq90_meninas                  <- resultados$rq90_meninas
rq95_meninas                  <- resultados$rq95_meninas

# Tabelas finais
tabela_formatada              <- resultados$tabela_formatada
