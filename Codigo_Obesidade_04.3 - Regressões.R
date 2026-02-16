#------------------------------------------------------------------------------#
###                  CÓDIGO OBESIDADE INTERGERACIONAL                        ###
#------------------------------------------------------------------------------#
# Aluno: Victor Eduardo
# Doscente: Adriano Firmino V. Araújo
##
# ---------------------------------------------------------------------------
#
getwd()
setwd("C:/Users/vitor/OneDrive/Área de Trabalho/UFPB/PIBIC_2025/Base de Dados/POFF_Dados_2008_2009")

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
  library(car)
  library(ResourceSelection)
  library(sandwich)
  library(lmtest)
  library(pscl)
  library(margins)
  library(ggeffects)
  library(pROC)
  library(splines)
  library(brglm2)
  
})

## -------------------------------------------------------------------------- ##
##                           MODELOS ECONOMÉTRICOS                            ##
## -------------------------------------------------------------------------- ##

## -------------------------------------------------------------------------- ##
###       Regressão MQO e Quantílica: elasticidades intergeracionais        ###
## -------------------------------------------------------------------------- ##

base_unida_consolidada_out <- read.csv("Base_2007_obesidade_regressão.csv")
base_unida_consolidada_out <- base_unida_consolidada_out |>
  filter(estrutura_fam == "biparental") 

colSums(is.na(base_unida_consolidada_out))

table(base_unida_consolidada_out$estrutura_fam)
sum(is.na(base_unida_consolidada_out$imc_pai))
sum(is.na(base_unida_consolidada_out$imc_mae))

# Verifica se tem valores menores que zero
any(base_unida_consolidada_out$imc_pai < 0, na.rm = TRUE)
any(base_unida_consolidada_out$imc_mae < 0, na.rm = TRUE)
any(base_unida_consolidada_out$imc_filho < 0, na.rm = TRUE)

any(base_unida_consolidada_out$imc_pai == 0, na.rm = TRUE)
any(base_unida_consolidada_out$imc_mae == 0, na.rm = TRUE)
any(base_unida_consolidada_out$imc_filho == 0, na.rm = TRUE)

# Amostra biparental
amostra_reg <- base_unida_consolidada_out |>
  mutate(
    ln_imc_filho = log(imc_filho),
    ln_imc_pai   = log(imc_pai),
    ln_imc_mae   = log(imc_mae),
    ln_renda_total = log(renda_total)
  )

# Fórmula
form <- ln_imc_filho ~ ln_imc_pai + ln_imc_mae +
  idade_filho + I(idade_filho^2) +
  branco_filho + freq_escola_filho +
  n_moradores + ln_renda_total +
  dummy_norte + dummy_nordeste + dummy_sul + dummy_centro_oeste
# + dummies de instrucao_resp / instrucao_conj, se quiser

# MQO
mqo <- lm(form, data = amostra_reg)
summary(mqo)

# Métodos da Regressão Quantílica
## "br" (default) - método simplex/Barrodale-Robert
## "fn" - Frisch–Newton recomenando para quando tiver muitas dummies
## "sfn" - versão mais eficiente do fn (Sparse Frisch-Newton)

# Regressão quantílica: mediana, q90 e q95
rq50 <- rq(form, tau = 0.5, data = amostra_reg, method = "fn")
rq90 <- rq(form, tau = 0.9, data = amostra_reg, method = "fn")
rq95 <- rq(form, tau = 0.95, data = amostra_reg, method = "fn")

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
#          Regressões quantílicas por sexo do filho (Ï„ = 0.5, 0.9, 0.95)     #
# ---------------------------------------------------------------------------#

# Meninos
rq50_meninos <- rq(form, tau = 0.5, data = amostra_meninOS, method = "fn")
rq90_meninos <- rq(form, tau = 0.9, data = amostra_meninOS, method = "fn")
rq95_meninos <- rq(form, tau = 0.95, data = amostra_meninOS, method = "fn")

sum_rq50_meninos <- summary(rq50_meninos, se = "boot", R = 200)
sum_rq90_meninos <- summary(rq90_meninos, se = "boot", R = 200)
sum_rq95_meninos <- summary(rq95_meninos, se = "boot", R = 200)

sum_rq50_meninos
sum_rq90_meninos
sum_rq95_meninos

# Meninas
rq50_meninas <- rq(form, tau = 0.5, data = amostra_meninAS, method = "fn")
rq90_meninas <- rq(form, tau = 0.9, data = amostra_meninAS, method = "fn")
rq95_meninas <- rq(form, tau = 0.95, data = amostra_meninAS, method = "fn")

sum_rq50_meninas <- summary(rq50_meninas, se = "boot", R = 200)
sum_rq90_meninas <- summary(rq90_meninas, se = "boot", R = 200)
sum_rq95_meninas <- summary(rq95_meninas, se = "boot", R = 200)

sum_rq50_meninas
sum_rq90_meninas
sum_rq95_meninas

# Função helper para pegar coeficientes de pai/mãe de um modelo
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
#saveRDS(resultados, "2008_resultados_obesidade_intergeracional_regressao.rds")

# Rodar novamente (carregar resultados)
resultados <- readRDS("resultados_obesidade_intergeracional.rds")

# Bases
base_unida_consolidada        <- resultados$base_unida_consolidada

# Modelos total
mqo   <- resultados$mqo
rq50  <- resultados$rq50
rq90  <- resultados$rq90
rq95  <- resultados$rq95

# Modelos por sexo
mqo_meninos  <- resultados$mqo_meninos
mqo_meninas  <- resultados$mqo_meninas

rq50_meninos <- resultados$rq50_meninos
rq90_meninos <- resultados$rq90_meninos
rq95_meninos <- resultados$rq95_meninos

rq50_meninas <- resultados$rq50_meninas
rq90_meninas <- resultados$rq90_meninas
rq95_meninas <- resultados$rq95_meninas

# Tabelas finais
tabela_formatada              <- resultados$tabela_formatada

# Amostras por faixa de idade (Tabela 2 do paper)

amostra_2_5  <- amostra_reg |> filter(idade_filho >= 2  & idade_filho <= 5)
amostra_6_10 <- amostra_reg |> filter(idade_filho >= 6  & idade_filho <= 10)
amostra_11_15<- amostra_reg |> filter(idade_filho >= 11 & idade_filho <= 15)
amostra_16_20<- amostra_reg |> filter(idade_filho >= 16 & idade_filho <= 20)

# De 2 a 5 anos
mqo_2_5  <- lm(form, data = amostra_2_5)
rq50_2_5 <- rq(form, tau = 0.5, data = amostra_2_5, method = "fn")
rq90_2_5 <- rq(form, tau = 0.9, data = amostra_2_5, method = "fn")
rq95_2_5 <- rq(form, tau = 0.95, data = amostra_2_5, method = "fn")

# De 6 a 10 anos
mqo_6_10  <- lm(form, data = amostra_6_10)
rq50_6_10 <- rq(form, tau = 0.5, data = amostra_6_10, method = "fn")
rq90_6_10 <- rq(form, tau = 0.9, data = amostra_6_10, method = "fn")
rq95_6_10 <- rq(form, tau = 0.95, data = amostra_6_10, method = "fn")

# De 11 a 15 anos
mqo_11_15  <- lm(form, data = amostra_11_15)
rq50_11_15 <- rq(form, tau = 0.5, data = amostra_11_15, method = "fn")
rq90_11_15 <- rq(form, tau = 0.9, data = amostra_11_15, method = "fn")
rq95_11_15 <- rq(form, tau = 0.95, data = amostra_11_15, method = "fn")

# De 16 a 20 anos
mqo_16_20  <- lm(form, data = amostra_16_20)
rq50_16_20 <- rq(form, tau = 0.5, data = amostra_16_20, method = "fn")
rq90_16_20 <- rq(form, tau = 0.9, data = amostra_16_20, method = "fn")
rq95_16_20 <- rq(form, tau = 0.95, data = amostra_16_20, method = "fn")

# Extrair as elasticidades
resultados_2_5 <- bind_rows(
  extrai_betas(mqo_2_5,  "MQO",   "2_5"),
  extrai_betas(rq50_2_5, "RQ(50)","2_5"),
  extrai_betas(rq90_2_5, "RQ(90)","2_5"),
  extrai_betas(rq95_2_5, "RQ(95)","2_5")
)

resultados_6_10 <- bind_rows(
  extrai_betas(mqo_6_10,  "MQO",   "6_10"),
  extrai_betas(rq50_6_10, "RQ(50)","6_10"),
  extrai_betas(rq90_6_10, "RQ(90)","6_10"),
  extrai_betas(rq95_6_10, "RQ(95)","6_10")
)

resultados_11_15 <- bind_rows(
  extrai_betas(mqo_11_15,  "MQO",   "11_15"),
  extrai_betas(rq50_11_15, "RQ(50)","11_15"),
  extrai_betas(rq90_11_15, "RQ(90)","11_15"),
  extrai_betas(rq95_11_15, "RQ(95)","11_15")
)

resultados_16_20 <- bind_rows(
  extrai_betas(mqo_16_20,  "MQO",   "16_20"),
  extrai_betas(rq50_16_20, "RQ(50)","16_20"),
  extrai_betas(rq90_16_20, "RQ(90)","16_20"),
  extrai_betas(rq95_16_20, "RQ(95)","16_20")
)

# Tabela de Elasciticidades por faixa de idade
tabela_elasticidades_idade <- bind_rows(
  resultados_2_5,
  resultados_6_10,
  resultados_11_15,
  resultados_16_20
)

tabela_elasticidades_idade <- tabela_elasticidades_idade |>
  mutate(
    beta_pai = round(beta_pai, 3),
    beta_mae = round(beta_mae, 3)
  ) |>
  arrange(grupo, modelo)

tabela_elasticidades_idade

## ========================================================================== ##
###                                Código Logit                              ###
## ========================================================================== ##
## (Tabela 3): persistência da obesidade
## No logit é necessário colocar a categoria da obesidade em 

table(amostra_reg$obeso_filho)
amostra_com_na <- amostra_reg |>
  filter(is.na(obeso_filho))

sum(is.na(amostra_reg$obeso_filho))
sum(is.na(amostra_reg$cat_imc_pai))
sum(is.na(amostra_reg$cat_imc_mae))
sum(is.na(amostra_reg$idade_filho))
sum(is.na(amostra_reg$branco_filho))
sum(is.na(amostra_reg$freq_escola_filho))
sum(is.na(amostra_reg$n_moradores))
sum(is.na(amostra_reg$renda_total))
sum(is.na(amostra_reg$dummy_norte))
sum(is.na(amostra_reg$dummy_nordeste))
sum(is.na(amostra_reg$dummy_sul))
sum(is.na(amostra_reg$dummy_centro_oeste))

# Base para logit (com variáveis completas)

amostra_logit <- amostra_reg 

table(amostra_logit$cat_imc_pai)
table(amostra_logit$cat_imc_mae)

# Seleção das variáveis
form_logit <- obeso_filho ~ cat_imc_mae + cat_imc_pai +
  idade_filho + I(idade_filho^2) +
  branco_filho + freq_escola_filho +
  n_moradores + ln_renda_total +
  dummy_norte + dummy_nordeste + dummy_sul + dummy_centro_oeste

# Modelo Logit
logit_total <- glm(form_logit,
                   family = binomial(link = "logit"),
                   data = amostra_logit)

summary(logit_total)

# Robusto (HC)
coeftest(logit_total, vcov. = vcovHC(logit_total, type = "HC1"))

## Ajuste global e Comparação de Modelos - LR test, AIC/BIC e pseudo-R2
# teste LR vs modelo nulo
anova(logit_total, test = "Chisq")

AIC(logit_total); BIC(logit_total); logLik(logit_total)

# pseudo-R2 (McFadden)
pR2(logit_total)

# OR, IC e Efeitos Marginais
b <- coef(logit_total)
#ci <- confint(logit_total)  # profile likelihood (mais “correto”, mas pode ser lento)
OR  <- exp(b)
OR_ci <- exp(ci)

# AME
ame_total <- margins(logit_total) |> summary()
ame_total

pred <- ggpredict(logit_total, terms = c("cat_imc_mae", "cat_imc_pai"))
plot(pred)

# Qualidade Preditiva do modelo total
p_hat <- predict(logit_total, type = "response")
roc_obj <- roc(amostra_logit$obeso_filho, p_hat)
auc(roc_obj)

auc_val <- as.numeric(auc(roc_obj))

p_roc <- ggroc(roc_obj, legacy.axes = TRUE, linewidth = 1.2) +
  geom_abline(linetype = "dashed", linewidth = 0.8, color = "grey60") +
  coord_equal() +
  labs(
    title = "Curva ROC — Modelo Logit",
    subtitle = sprintf("AUC = %.4f", auc_val),
    x = "Taxa de Falsos Positivos",
    y = "Taxa de Verdadeiros Positivos"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
  )

p_roc

# Brier Score (calibração)
mean((amostra_logit$obeso_filho - p_hat)^2, na.rm = TRUE)
hoslem.test(amostra_logit$obeso_filho, p_hat, g = 10)

## Diagnósticos
# Separação perfeita/quase-perfeita
detect_separation(form_logit, data = amostra_logit, family = binomial("logit"))

# Influência e outliers (Cook, leverage, DFBETAs)
infl <- influence.measures(logit_total)
summary(infl)

cooks <- cooks.distance(logit_total) # pontos influentes
which(cooks > 4/length(cooks))

vif(logit_total)

# Especificação Funcional: não-linearidade e interações
form_spline <- update(form_logit, . ~ . - idade_filho - I(idade_filho^2) + ns(idade_filho, df = 4))
logit_spline <- glm(form_spline, family=binomial("logit"), data=amostra_logit)

anova(logit_total, logit_spline, test="Chisq")  # compara ajuste

# Matriz de Confusão
p_hat <- predict(logit_total, type = "response")

cutoff <- 0.5
y_pred <- ifelse(p_hat >= cutoff, 1, 0)
y_true <- amostra_logit$obeso_filho

# matriz de confusão
tab <- table(Real = y_true, Predito = y_pred); tab

df_cm <- as.data.frame(cm_tab)
df_cm$perc_total <- df_cm$Freq / sum(df_cm$Freq)

p_cm <- ggplot(df_cm, aes(x = factor(Predito), y = factor(Real), fill = Freq)) +
  geom_tile(color = "grey85", linewidth = 0.7) +
  geom_text(aes(label = sprintf("%d\n(%.1f%%)", Freq, 100*perc_total)),
            fontface = "bold", size = 4) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Matriz de Confusão — Modelo Logit",
    subtitle = sprintf("Cutoff = %.2f", cutoff),
    x = "Predito",
    y = "Real",
    fill = "Contagem"
  ) +
  coord_equal() +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold")
  )

p_cm

y_true_f <- factor(y_true, levels = c(0,1))
y_pred_f <- factor(y_pred, levels = c(0,1))

confusionMatrix(data = y_pred_f, reference = y_true_f, positive = "1")

# pega o melhor cutoff (Youden)
best <- coords(roc_obj, x = "best", best.method = "youden",
               ret = c("threshold","sensitivity","specificity"),
               transpose = FALSE)

cutoff_best <- best["threshold"]

y_pred_best <- ifelse(p_hat >= cutoff_best, 1, 0)

confusionMatrix(
  data = factor(y_pred_best, levels = c(0,1)),
  reference = factor(y_true, levels = c(0,1)),
  positive = "1"
)

best

##
### Logit por Sexo
##

# Logit para meninos
logit_meninos <- glm(form_logit,
                     family = binomial(link = "logit"),
                     data = amostra_logit |> filter(sexo_filho == 1))

summary(logit_meninos)

# Logit para meninas
logit_meninas <- glm(form_logit,
                     family = binomial(link = "logit"),
                     data = amostra_logit |> filter(sexo_filho == 2))

summary(logit_meninas)

extrai_logit <- function(modelo, grupo) {
  coefs <- summary(modelo)$coefficients
  tibble(
    grupo = grupo,
    variavel = rownames(coefs),
    coef = coefs[,1],
    se   = coefs[,2],
    z    = coefs[,3],
    p    = coefs[,4]
  )
}

tabela_logit <- bind_rows(
  extrai_logit(logit_total,   "Total"),
  extrai_logit(logit_meninos, "Filho"),
  extrai_logit(logit_meninas, "Filha")
)

tabela_logit




