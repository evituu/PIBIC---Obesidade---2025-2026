#------------------------------------------------------------------------------#
###                  CÓDIGO OBESIDADE INTERGERACIONAL                        ###
#------------------------------------------------------------------------------#
# Aluno: Victor Eduardo
# Doscente: Adriano Firmino V. Araújo

# ---------------------------------------------------------------------------- #
##                    MODALGEM ECONOMÉTRICA - 2008-2009                       ##
# ---------------------------------------------------------------------------- #

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
})

library(survey)
options(survey.lonely.psu = "adjust")

dados_obesidade_geral <- read.csv("base_obesidade_geral.csv")
dados_obesidade_bi <- read.csv("base_obesidade_bi.csv")
dados_obesidade_uni <- read.csv("base_obesidade_uni.csv")

### Biparental
table(dados_obesidade_bi$sexo_filho)

# Biparental pai -> filho
dados_obesidade_geral_pai_filho <- dados_obesidade_bi |>
  filter(
  !is.na(cat_imc_pai)
  ) |>
  filter(sexo_filho == 1)

table(dados_obesidade_geral_pai_filho$cat_imc_pai)
table(dados_obesidade_geral_pai_filho$sexo_filho)
table(dados_obesidade_geral_pai_filho$estrutura_fam)

# Biparental pai -> filha
dados_obesidade_geral_pai_filha <- dados_obesidade_bi |>
  filter(
    !is.na(cat_imc_pai)
  ) |>
  filter(sexo_filho == 2)

table(dados_obesidade_geral_pai_filha$cat_imc_pai)
table(dados_obesidade_geral_pai_filha$sexo_filho)

# Biparental mae -> filho
dados_obesidade_geral_mae_filho <- dados_obesidade_bi |>
  filter(
    !is.na(cat_imc_mae)
  ) |>
  filter(sexo_filho == 1)
table(dados_obesidade_geral_mae_filho$cat_imc_mae)
table(dados_obesidade_geral_mae_filho$sexo_filho)

# Biparental mae -> filha
dados_obesidade_geral_mae_filha <- dados_obesidade_bi |>
  filter(
    !is.na(cat_imc_mae)
  ) |>
  filter(sexo_filho == 2)
table(dados_obesidade_geral_mae_filha$cat_imc_mae)
table(dados_obesidade_geral_mae_filha$sexo_filho)

### Uniparental

# Uniparental Mono mulher -> filho
dados_obesidade_uni_mae_filho <- dados_obesidade_uni |>
  filter(
    !is.na(cat_imc_mae)
  ) |>
  filter(sexo_filho == 1)
table(dados_obesidade_uni_mae_filho$cat_imc_mae)
table(dados_obesidade_uni_mae_filho$sexo_filho)

# Uniparental Mono mulher -> filha
dados_obesidade_uni_mae_filha <- dados_obesidade_uni |>
  filter(
    !is.na(cat_imc_mae)
  ) |>
  filter(sexo_filho == 2)
table(dados_obesidade_uni_mae_filha$cat_imc_mae)
table(dados_obesidade_uni_mae_filha$sexo_filho)

# Uniparental Mono pai -> filho
dados_obesidade_uni_pai_filho <- dados_obesidade_uni |>
  filter(
    !is.na(cat_imc_pai)
  ) |>
  filter(sexo_filho == 1)
table(dados_obesidade_uni_pai_filho$cat_imc_pai)
table(dados_obesidade_uni_pai_filho$sexo_filho)

# Uniparental Mono pai -> filha
dados_obesidade_uni_pai_filha <- dados_obesidade_uni |>
  filter(
    !is.na(cat_imc_pai)
  ) |>
  filter(sexo_filho == 2)
table(dados_obesidade_uni_pai_filha$cat_imc_pai)
table(dados_obesidade_uni_pai_filha$sexo_filho)

######
#########         Matriz de Transição para as Famílias Biparentais
######

###
####                    Pai -> Filho
###
table(dados_obesidade_geral_pai_filho$cat_imc_filho)
{
# 1) Filtrar faixa etária e observações válidas - PAI -> FILHO (out_filho)
base_analise_pai_filho <- out_filho |>
  filter(
    idade_filho >= 2,
    idade_filho <= 20,
    !is.na(cat_imc_child),   # categoria do filho (agora genérica)
    !is.na(cat_imc_pai),
    !is.na(estrutura_fam),
    !is.na(sexo_filho),
    !is.na(cod_upa_resp),
    !is.na(estrato_pof_resp),
    !is.na(peso_final_filho)
  ) |>
  mutate(
    cat_imc_child = factor(
      cat_imc_child,
      levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
    ),
    cat_imc_pai = factor(
      cat_imc_pai,
      levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
    ),
    # codificação numérica para o Índice beta
    imc_pai_num   = as.numeric(cat_imc_pai)   - 1,
    imc_child_num = as.numeric(cat_imc_child) - 1,
    # "controle" = combinação de estrutura familiar e sexo do filho (aqui Ã© sempre 1, mas ok)
    grupo    = interaction(estrutura_fam, sexo_filho, drop = TRUE),
    controle = dense_rank(grupo)
  )

colSums(is.na(base_analise_pai_filho))

{
  Matriz_Transicao <- tibble(
    Controle        = integer(),
    origem_imc      = character(),
    IMC_desnutrido  = double(),
    IMC_saudavel    = double(),
    IMC_sobrepeso   = double(),
    IMC_obeso       = double()
  )
  
  Mobility_Index <- NULL
  estados_imc <- c("desnutrido", "saudavel", "sobrepeso", "obeso")
  
  for (g in sort(unique(base_analise_pai_filho$controle))) {
    
    dados <- base_analise_pai_filho |>
      filter(controle == g)
    
    if (nrow(dados) < 2) next
    
    design <- tryCatch(
      svydesign(
        id      = ~cod_upa_resp,
        strata  = ~estrato_pof_resp,
        weights = ~peso_final_filho,
        data    = dados,
        nest    = TRUE
      ),
      error = function(e) {
        message("Pulando grupo ", g, " (erro no svydesign): ", e$message)
        return(NULL)
      }
    )
    if (is.null(design)) next
    
    observacao <- nrow(dados)
    populacao_estimada <- tryCatch(
      round(sum(weights(design)), 0),
      error = function(e) {
        message("NÃ£o consegui calcular svytotal para grupo ", g, ": ", e$message)
        return(NA_real_)
      }
    )
    
    # ---------- Matriz de transiÃ§Ã£o PAI -> FILHO ---------- #
    tab_IMC <- tryCatch(
      svytable(~cat_imc_pai + cat_imc_child, design),  # AQUI troca
      error = function(e) {
        message("Pulando grupo ", g, " (erro no svytable): ", e$message)
        return(NULL)
      }
    )
    if (is.null(tab_IMC)) next
    
    MTransicao <- data.frame(
      prop.table(tab_IMC, margin = 1)
    ) |>
      complete(
        cat_imc_pai   = estados_imc,
        cat_imc_child = estados_imc,
        fill = list(Freq = 0)
      ) |>
      arrange(cat_imc_pai) |>
      pivot_wider(
        names_from  = cat_imc_child,
        values_from = Freq
      ) |>
      select(-cat_imc_pai) |>
      as.matrix()
    
    rownames(MTransicao) <- estados_imc
    
    # ------- Ã­ndices (Mt, Ml, Md, Mda, Ib, IMA, IMD, IP, P00â€“P33) ------- #
    eigenvalues <- eigen(MTransicao)$values
    
    Mt  <- (nrow(MTransicao) - sum(diag(MTransicao))) / (nrow(MTransicao) - 1)
    Ml  <- 1 - abs(eigenvalues[2])
    Md  <- 1 - abs(det(MTransicao))
    Mda <- 1 - abs(det(MTransicao))^(1 / (nrow(MTransicao) - 1))
    
    Ib <- 0
    for (i in 1:nrow(MTransicao)) {
      for (j in 1:nrow(MTransicao)) {
        Ib <- Ib + (MTransicao[i, j] * abs(i - j))
      }
    }
    Ib <- Ib / (nrow(MTransicao) * (nrow(MTransicao) - 1))
    
    IMA <- 0
    for (i in 1:(nrow(MTransicao) - 1)) {
      for (j in (i + 1):nrow(MTransicao)) {
        IMA <- IMA + MTransicao[i, j]
      }
    }
    IMD <- 0
    for (i in 2:nrow(MTransicao)) {
      for (j in 1:(i - 1)) {
        IMD <- IMD + MTransicao[i, j]
      }
    }
    IP <- sum(diag(MTransicao))
    
    soma <- IMA + IMD + IP
    IMA <- IMA / soma
    IMD <- IMD / soma
    IP  <- IP  / soma
    
    P00 <- MTransicao[1, 1]
    P11 <- MTransicao[2, 2]
    P22 <- MTransicao[3, 3]
    P33 <- MTransicao[4, 4]
    
    # ---------- Índice beta (regressão) ---------- #
    regre <- tryCatch(
      svyglm(imc_child_num ~ imc_pai_num, design = design),  # AQUI troca
      error = function(e) {
        message("Erro em svyglm no grupo ", g, ": ", e$message)
        return(NULL)
      }
    )
    
    if (is.null(regre) || is.na(coef(regre)[2])) {
      beta <- NA_real_
      beta_compl <- NA_real_
    } else {
      beta <- coef(regre)[2]
      beta_compl <- 1 - beta
    }
    
    Mobility_Index_coluna <- tibble(
      Indicador = c(
        "Observacoes",
        "Populacao Estimada",
        "Indice de Prais",
        "Indice do 2o Autovalor",
        "Indice do Determinante",
        "Indice do Determinante Alternativo",
        "Indice de Bartholomew",
        "Indice de Mobilidade Ascendente",
        "Indice de Mobilidade Descendente",
        "Indice de Persistencia",
        "Persistencia em desnutrido",
        "Persistencia em saudavel",
        "Persistencia em sobrepeso",
        "Persistencia em obeso",
        "Indice Beta (Parametrico)",
        "Indice 1-Beta (Parametrico)"
      ),
      !!paste0("grupo_", g) := c(
        observacao,
        populacao_estimada,
        Mt, Ml, Md, Mda,
        Ib,
        IMA, IMD, IP,
        P00, P11, P22, P33,
        beta, beta_compl
      )
    )
    
    Matriz_linha <- as_tibble(MTransicao, .name_repair = "minimal") |>
      mutate(
        Controle   = g,
        origem_imc = estados_imc
      ) |>
      relocate(Controle, origem_imc) |>
      rename(
        IMC_desnutrido = desnutrido,
        IMC_saudavel   = saudavel,
        IMC_sobrepeso  = sobrepeso,
        IMC_obeso      = obeso
      )
    
    Matriz_Transicao <- bind_rows(Matriz_Transicao, Matriz_linha)
    
    if (is.null(Mobility_Index)) {
      Mobility_Index <- Mobility_Index_coluna
    } else {
      Mobility_Index <- full_join(Mobility_Index, Mobility_Index_coluna, by = "Indicador")
    }
  }
}

Matriz_Transicao_pai_filho <- Matriz_Transicao; Matriz_Transicao_pai_filho
Mobility_Index_pai_filho <- Mobility_Index; Mobility_Index_pai_filho
Matriz_Transicao_pai_filho
Mobility_Index_pai_filho

}

###
####                    Pai -> Filha
###
{
# 1) Filtrar faixa etÃ¡ria e observaÃ§Ãµes vÃ¡lidas - PAI -> FILHO (out_filho)
base_analise_pai_filha <- out_filha |>
  filter(
    idade_filho >= 2,
    idade_filho <= 20,
    !is.na(cat_imc_child),   # categoria do filho (agora genÃ©rica)
    !is.na(cat_imc_pai),
    !is.na(estrutura_fam),
    !is.na(sexo_filho),
    !is.na(cod_upa_resp),
    !is.na(estrato_pof_resp),
    !is.na(peso_final_filho)
  ) |>
  mutate(
    cat_imc_child = factor(
      cat_imc_child,
      levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
    ),
    cat_imc_pai = factor(
      cat_imc_pai,
      levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
    ),
    # codificaÃ§Ã£o numÃ©rica para o Ã­ndice beta
    imc_pai_num   = as.numeric(cat_imc_pai)   - 1,
    imc_child_num = as.numeric(cat_imc_child) - 1,
    # "controle" = combinaÃ§Ã£o de estrutura familiar e sexo do filho (aqui Ã© sempre 1, mas ok)
    grupo    = interaction(estrutura_fam, sexo_filho, drop = TRUE),
    controle = dense_rank(grupo)
  )

colSums(is.na(base_analise_pai_filha))

{
  Matriz_Transicao <- tibble(
    Controle        = integer(),
    origem_imc      = character(),
    IMC_desnutrido  = double(),
    IMC_saudavel    = double(),
    IMC_sobrepeso   = double(),
    IMC_obeso       = double()
  )
  
  Mobility_Index <- NULL
  estados_imc <- c("desnutrido", "saudavel", "sobrepeso", "obeso")
  
  for (g in sort(unique(base_analise_pai_filha$controle))) {
    
    dados <- base_analise_pai_filha |>
      filter(controle == g)
    
    if (nrow(dados) < 2) next
    
    design <- tryCatch(
      svydesign(
        id      = ~cod_upa_resp,
        strata  = ~estrato_pof_resp,
        weights = ~peso_final_filho,
        data    = dados,
        nest    = TRUE
      ),
      error = function(e) {
        message("Pulando grupo ", g, " (erro no svydesign): ", e$message)
        return(NULL)
      }
    )
    if (is.null(design)) next
    
    observacao <- nrow(dados)
    populacao_estimada <- tryCatch(
      round(sum(weights(design)), 0),
      error = function(e) {
        message("NÃ£o consegui calcular svytotal para grupo ", g, ": ", e$message)
        return(NA_real_)
      }
    )
    
    # ---------- Matriz de transiÃ§Ã£o PAI -> FILHO ---------- #
    tab_IMC <- tryCatch(
      svytable(~cat_imc_pai + cat_imc_child, design),  # AQUI troca
      error = function(e) {
        message("Pulando grupo ", g, " (erro no svytable): ", e$message)
        return(NULL)
      }
    )
    if (is.null(tab_IMC)) next
    
    MTransicao <- data.frame(
      prop.table(tab_IMC, margin = 1)
    ) |>
      complete(
        cat_imc_pai   = estados_imc,
        cat_imc_child = estados_imc,
        fill = list(Freq = 0)
      ) |>
      arrange(cat_imc_pai) |>
      pivot_wider(
        names_from  = cat_imc_child,
        values_from = Freq
      ) |>
      select(-cat_imc_pai) |>
      as.matrix()
    
    rownames(MTransicao) <- estados_imc
    
    # ------- Ã­ndices (Mt, Ml, Md, Mda, Ib, IMA, IMD, IP, P00â€“P33) ------- #
    eigenvalues <- eigen(MTransicao)$values
    
    Mt  <- (nrow(MTransicao) - sum(diag(MTransicao))) / (nrow(MTransicao) - 1)
    Ml  <- 1 - abs(eigenvalues[2])
    Md  <- 1 - abs(det(MTransicao))
    Mda <- 1 - abs(det(MTransicao))^(1 / (nrow(MTransicao) - 1))
    
    Ib <- 0
    for (i in 1:nrow(MTransicao)) {
      for (j in 1:nrow(MTransicao)) {
        Ib <- Ib + (MTransicao[i, j] * abs(i - j))
      }
    }
    Ib <- Ib / (nrow(MTransicao) * (nrow(MTransicao) - 1))
    
    IMA <- 0
    for (i in 1:(nrow(MTransicao) - 1)) {
      for (j in (i + 1):nrow(MTransicao)) {
        IMA <- IMA + MTransicao[i, j]
      }
    }
    IMD <- 0
    for (i in 2:nrow(MTransicao)) {
      for (j in 1:(i - 1)) {
        IMD <- IMD + MTransicao[i, j]
      }
    }
    IP <- sum(diag(MTransicao))
    
    soma <- IMA + IMD + IP
    IMA <- IMA / soma
    IMD <- IMD / soma
    IP  <- IP  / soma
    
    P00 <- MTransicao[1, 1]
    P11 <- MTransicao[2, 2]
    P22 <- MTransicao[3, 3]
    P33 <- MTransicao[4, 4]
    
    # ---------- Ãndice beta (regressÃ£o) ---------- #
    regre <- tryCatch(
      svyglm(imc_child_num ~ imc_pai_num, design = design),  # AQUI troca
      error = function(e) {
        message("Erro em svyglm no grupo ", g, ": ", e$message)
        return(NULL)
      }
    )
    
    if (is.null(regre) || is.na(coef(regre)[2])) {
      beta <- NA_real_
      beta_compl <- NA_real_
    } else {
      beta <- coef(regre)[2]
      beta_compl <- 1 - beta
    }
    
    Mobility_Index_coluna <- tibble(
      Indicador = c(
        "Observacoes",
        "Populacao Estimada",
        "Indice de Prais",
        "Indice do 2o Autovalor",
        "Indice do Determinante",
        "Indice do Determinante Alternativo",
        "Indice de Bartholomew",
        "Indice de Mobilidade Ascendente",
        "Indice de Mobilidade Descendente",
        "Indice de Persistencia",
        "Persistencia em desnutrido",
        "Persistencia em saudavel",
        "Persistencia em sobrepeso",
        "Persistencia em obeso",
        "Indice Beta (Parametrico)",
        "Indice 1-Beta (Parametrico)"
      ),
      !!paste0("grupo_", g) := c(
        observacao,
        populacao_estimada,
        Mt, Ml, Md, Mda,
        Ib,
        IMA, IMD, IP,
        P00, P11, P22, P33,
        beta, beta_compl
      )
    )
    
    Matriz_linha <- as_tibble(MTransicao, .name_repair = "minimal") |>
      mutate(
        Controle   = g,
        origem_imc = estados_imc
      ) |>
      relocate(Controle, origem_imc) |>
      rename(
        IMC_desnutrido = desnutrido,
        IMC_saudavel   = saudavel,
        IMC_sobrepeso  = sobrepeso,
        IMC_obeso      = obeso
      )
    
    Matriz_Transicao <- bind_rows(Matriz_Transicao, Matriz_linha)
    
    if (is.null(Mobility_Index)) {
      Mobility_Index <- Mobility_Index_coluna
    } else {
      Mobility_Index <- full_join(Mobility_Index, Mobility_Index_coluna, by = "Indicador")
    }
  }
}

Matriz_Transicao_pai_filha <- Matriz_Transicao; Matriz_Transicao_pai_filha
Mobility_Index_pai_filha <- Mobility_Index; Mobility_Index_pai_filha
}

###
####                    Mãe -> Filho
###
{
# 1) Filtrar faixa etÃ¡ria e observaÃ§Ãµes vÃ¡lidas - MÃƒE -> FILHO (out_filho)
base_analise_mae_filho <- out_filho |>
  filter(
    idade_filho >= 2,
    idade_filho <= 20,
    !is.na(cat_imc_child),   # categoria do filho (genÃ©rica)
    !is.na(cat_imc_mae),
    !is.na(estrutura_fam),
    !is.na(sexo_filho),
    !is.na(cod_upa_resp),
    !is.na(estrato_pof_resp),
    !is.na(peso_final_filho)
    # se quiser restringir a biparental:
    # estrutura_fam == "biparental"
  ) |>
  mutate(
    cat_imc_child = factor(
      cat_imc_child,
      levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
    ),
    cat_imc_mae = factor(
      cat_imc_mae,
      levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
    ),
    # codificaÃ§Ã£o numÃ©rica para o Ã­ndice beta
    imc_mae_num   = as.numeric(cat_imc_mae)   - 1,
    imc_child_num = as.numeric(cat_imc_child) - 1,
    # aqui ainda deixo a mesma lÃ³gica de agrupamento
    grupo    = interaction(estrutura_fam, sexo_filho, drop = TRUE),
    controle = dense_rank(grupo)
  )

colSums(is.na(base_analise_mae_filho))

{
  Matriz_Transicao <- tibble(
    Controle        = integer(),
    origem_imc      = character(),
    IMC_desnutrido  = double(),
    IMC_saudavel    = double(),
    IMC_sobrepeso   = double(),
    IMC_obeso       = double()
  )
  
  Mobility_Index <- NULL
  estados_imc <- c("desnutrido", "saudavel", "sobrepeso", "obeso")
  
  for (g in sort(unique(base_analise_mae_filho$controle))) {
    
    dados <- base_analise_mae_filho |>
      filter(controle == g)
    
    if (nrow(dados) < 2) next
    
    design <- tryCatch(
      svydesign(
        id      = ~cod_upa_resp,
        strata  = ~estrato_pof_resp,
        weights = ~peso_final_filho,
        data    = dados,
        nest    = TRUE
      ),
      error = function(e) {
        message("Pulando grupo ", g, " (erro no svydesign): ", e$message)
        return(NULL)
      }
    )
    if (is.null(design)) next
    
    observacao <- nrow(dados)
    populacao_estimada <- tryCatch(
      round(sum(weights(design)), 0),
      error = function(e) {
        message("NÃ£o consegui calcular svytotal para grupo ", g, ": ", e$message)
        return(NA_real_)
      }
    )
    
    # ---------- Matriz de transiÃ§Ã£o Mae -> FILHO ---------- #
    tab_IMC <- tryCatch(
      svytable(~cat_imc_mae + cat_imc_child, design),  # AQUI troca
      error = function(e) {
        message("Pulando grupo ", g, " (erro no svytable): ", e$message)
        return(NULL)
      }
    )
    if (is.null(tab_IMC)) next
    
    MTransicao <- data.frame(
      prop.table(tab_IMC, margin = 1)
    ) |>
      complete(
        cat_imc_mae   = estados_imc,
        cat_imc_child = estados_imc,
        fill = list(Freq = 0)
      ) |>
      arrange(cat_imc_mae) |>
      pivot_wider(
        names_from  = cat_imc_child,
        values_from = Freq
      ) |>
      select(-cat_imc_mae) |>
      as.matrix()
    
    rownames(MTransicao) <- estados_imc
    
    # ------- Ã­ndices (Mt, Ml, Md, Mda, Ib, IMA, IMD, IP, P00â€“P33) ------- #
    eigenvalues <- eigen(MTransicao)$values
    
    Mt  <- (nrow(MTransicao) - sum(diag(MTransicao))) / (nrow(MTransicao) - 1)
    Ml  <- 1 - abs(eigenvalues[2])
    Md  <- 1 - abs(det(MTransicao))
    Mda <- 1 - abs(det(MTransicao))^(1 / (nrow(MTransicao) - 1))
    
    Ib <- 0
    for (i in 1:nrow(MTransicao)) {
      for (j in 1:nrow(MTransicao)) {
        Ib <- Ib + (MTransicao[i, j] * abs(i - j))
      }
    }
    Ib <- Ib / (nrow(MTransicao) * (nrow(MTransicao) - 1))
    
    IMA <- 0
    for (i in 1:(nrow(MTransicao) - 1)) {
      for (j in (i + 1):nrow(MTransicao)) {
        IMA <- IMA + MTransicao[i, j]
      }
    }
    IMD <- 0
    for (i in 2:nrow(MTransicao)) {
      for (j in 1:(i - 1)) {
        IMD <- IMD + MTransicao[i, j]
      }
    }
    IP <- sum(diag(MTransicao))
    
    soma <- IMA + IMD + IP
    IMA <- IMA / soma
    IMD <- IMD / soma
    IP  <- IP  / soma
    
    P00 <- MTransicao[1, 1]
    P11 <- MTransicao[2, 2]
    P22 <- MTransicao[3, 3]
    P33 <- MTransicao[4, 4]
    
    # ---------- Ãndice beta (regressÃ£o) ---------- #
    regre <- tryCatch(
      svyglm(imc_child_num ~ imc_mae_num, design = design),  # AQUI troca
      error = function(e) {
        message("Erro em svyglm no grupo ", g, ": ", e$message)
        return(NULL)
      }
    )
    
    if (is.null(regre) || is.na(coef(regre)[2])) {
      beta <- NA_real_
      beta_compl <- NA_real_
    } else {
      beta <- coef(regre)[2]
      beta_compl <- 1 - beta
    }
    
    Mobility_Index_coluna <- tibble(
      Indicador = c(
        "Observacoes",
        "Populacao Estimada",
        "Indice de Prais",
        "Indice do 2o Autovalor",
        "Indice do Determinante",
        "Indice do Determinante Alternativo",
        "Indice de Bartholomew",
        "Indice de Mobilidade Ascendente",
        "Indice de Mobilidade Descendente",
        "Indice de Persistencia",
        "Persistencia em desnutrido",
        "Persistencia em saudavel",
        "Persistencia em sobrepeso",
        "Persistencia em obeso",
        "Indice Beta (Parametrico)",
        "Indice 1-Beta (Parametrico)"
      ),
      !!paste0("grupo_", g) := c(
        observacao,
        populacao_estimada,
        Mt, Ml, Md, Mda,
        Ib,
        IMA, IMD, IP,
        P00, P11, P22, P33,
        beta, beta_compl
      )
    )
    
    Matriz_linha <- as_tibble(MTransicao, .name_repair = "minimal") |>
      mutate(
        Controle   = g,
        origem_imc = estados_imc
      ) |>
      relocate(Controle, origem_imc) |>
      rename(
        IMC_desnutrido = desnutrido,
        IMC_saudavel   = saudavel,
        IMC_sobrepeso  = sobrepeso,
        IMC_obeso      = obeso
      )
    
    Matriz_Transicao <- bind_rows(Matriz_Transicao, Matriz_linha)
    
    if (is.null(Mobility_Index)) {
      Mobility_Index <- Mobility_Index_coluna
    } else {
      Mobility_Index <- full_join(Mobility_Index, Mobility_Index_coluna, by = "Indicador")
    }
  }
}

Matriz_Transicao_mae_filho <- Matriz_Transicao; Matriz_Transicao_mae_filho
Mobility_Index_mae_filho <- Mobility_Index; Mobility_Index_mae_filho
}

###
####                    Mãe -> Filha
###
{
# 1) Filtrar faixa etÃ¡ria e observaÃ§Ãµes vÃ¡lidas - MÃƒE -> FILHO (out_filho)
base_analise_mae_filha <- out_filha |>
  filter(
    idade_filho >= 2,
    idade_filho <= 20,
    !is.na(cat_imc_child),   # categoria do filho (genÃ©rica)
    !is.na(cat_imc_mae),
    !is.na(estrutura_fam),
    !is.na(sexo_filho),
    !is.na(cod_upa_resp),
    !is.na(estrato_pof_resp),
    !is.na(peso_final_filho)
    # se quiser restringir a biparental:
    # estrutura_fam == "biparental"
  ) |>
  mutate(
    cat_imc_child = factor(
      cat_imc_child,
      levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
    ),
    cat_imc_mae = factor(
      cat_imc_mae,
      levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
    ),
    # codificaÃ§Ã£o numÃ©rica para o Ã­ndice beta
    imc_mae_num   = as.numeric(cat_imc_mae)   - 1,
    imc_child_num = as.numeric(cat_imc_child) - 1,
    # aqui ainda deixo a mesma lÃ³gica de agrupamento
    grupo    = interaction(estrutura_fam, sexo_filho, drop = TRUE),
    controle = dense_rank(grupo)
  )

colSums(is.na(base_analise_mae_filha))

{
  Matriz_Transicao <- tibble(
    Controle        = integer(),
    origem_imc      = character(),
    IMC_desnutrido  = double(),
    IMC_saudavel    = double(),
    IMC_sobrepeso   = double(),
    IMC_obeso       = double()
  )
  
  Mobility_Index <- NULL
  estados_imc <- c("desnutrido", "saudavel", "sobrepeso", "obeso")
  
  for (g in sort(unique(base_analise_mae_filha$controle))) {
    
    dados <- base_analise_mae_filha |>
      filter(controle == g)
    
    if (nrow(dados) < 2) next
    
    design <- tryCatch(
      svydesign(
        id      = ~cod_upa_resp,
        strata  = ~estrato_pof_resp,
        weights = ~peso_final_filho,
        data    = dados,
        nest    = TRUE
      ),
      error = function(e) {
        message("Pulando grupo ", g, " (erro no svydesign): ", e$message)
        return(NULL)
      }
    )
    if (is.null(design)) next
    
    observacao <- nrow(dados)
    populacao_estimada <- tryCatch(
      round(sum(weights(design)), 0),
      error = function(e) {
        message("NÃ£o consegui calcular svytotal para grupo ", g, ": ", e$message)
        return(NA_real_)
      }
    )
    
    # ---------- Matriz de transiÃ§Ã£o MÃ£e -> FILHO ---------- #
    tab_IMC <- tryCatch(
      svytable(~cat_imc_mae + cat_imc_child, design),  # AQUI troca
      error = function(e) {
        message("Pulando grupo ", g, " (erro no svytable): ", e$message)
        return(NULL)
      }
    )
    if (is.null(tab_IMC)) next
    
    MTransicao <- data.frame(
      prop.table(tab_IMC, margin = 1)
    ) |>
      complete(
        cat_imc_mae   = estados_imc,
        cat_imc_child = estados_imc,
        fill = list(Freq = 0)
      ) |>
      arrange(cat_imc_mae) |>
      pivot_wider(
        names_from  = cat_imc_child,
        values_from = Freq
      ) |>
      select(-cat_imc_mae) |>
      as.matrix()
    
    rownames(MTransicao) <- estados_imc
    
    # ------- Ã­ndices (Mt, Ml, Md, Mda, Ib, IMA, IMD, IP, P00â€“P33) ------- #
    eigenvalues <- eigen(MTransicao)$values
    
    Mt  <- (nrow(MTransicao) - sum(diag(MTransicao))) / (nrow(MTransicao) - 1)
    Ml  <- 1 - abs(eigenvalues[2])
    Md  <- 1 - abs(det(MTransicao))
    Mda <- 1 - abs(det(MTransicao))^(1 / (nrow(MTransicao) - 1))
    
    Ib <- 0
    for (i in 1:nrow(MTransicao)) {
      for (j in 1:nrow(MTransicao)) {
        Ib <- Ib + (MTransicao[i, j] * abs(i - j))
      }
    }
    Ib <- Ib / (nrow(MTransicao) * (nrow(MTransicao) - 1))
    
    IMA <- 0
    for (i in 1:(nrow(MTransicao) - 1)) {
      for (j in (i + 1):nrow(MTransicao)) {
        IMA <- IMA + MTransicao[i, j]
      }
    }
    IMD <- 0
    for (i in 2:nrow(MTransicao)) {
      for (j in 1:(i - 1)) {
        IMD <- IMD + MTransicao[i, j]
      }
    }
    IP <- sum(diag(MTransicao))
    
    soma <- IMA + IMD + IP
    IMA <- IMA / soma
    IMD <- IMD / soma
    IP  <- IP  / soma
    
    P00 <- MTransicao[1, 1]
    P11 <- MTransicao[2, 2]
    P22 <- MTransicao[3, 3]
    P33 <- MTransicao[4, 4]
    
    # ---------- Ãndice beta (regressÃ£o) ---------- #
    regre <- tryCatch(
      svyglm(imc_child_num ~ imc_mae_num, design = design),  # AQUI troca
      error = function(e) {
        message("Erro em svyglm no grupo ", g, ": ", e$message)
        return(NULL)
      }
    )
    
    if (is.null(regre) || is.na(coef(regre)[2])) {
      beta <- NA_real_
      beta_compl <- NA_real_
    } else {
      beta <- coef(regre)[2]
      beta_compl <- 1 - beta
    }
    
    Mobility_Index_coluna <- tibble(
      Indicador = c(
        "Observacoes",
        "Populacao Estimada",
        "Indice de Prais",
        "Indice do 2o Autovalor",
        "Indice do Determinante",
        "Indice do Determinante Alternativo",
        "Indice de Bartholomew",
        "Indice de Mobilidade Ascendente",
        "Indice de Mobilidade Descendente",
        "Indice de Persistencia",
        "Persistencia em desnutrido",
        "Persistencia em saudavel",
        "Persistencia em sobrepeso",
        "Persistencia em obeso",
        "Indice Beta (Parametrico)",
        "Indice 1-Beta (Parametrico)"
      ),
      !!paste0("grupo_", g) := c(
        observacao,
        populacao_estimada,
        Mt, Ml, Md, Mda,
        Ib,
        IMA, IMD, IP,
        P00, P11, P22, P33,
        beta, beta_compl
      )
    )
    
    Matriz_linha <- as_tibble(MTransicao, .name_repair = "minimal") |>
      mutate(
        Controle   = g,
        origem_imc = estados_imc
      ) |>
      relocate(Controle, origem_imc) |>
      rename(
        IMC_desnutrido = desnutrido,
        IMC_saudavel   = saudavel,
        IMC_sobrepeso  = sobrepeso,
        IMC_obeso      = obeso
      )
    
    Matriz_Transicao <- bind_rows(Matriz_Transicao, Matriz_linha)
    
    if (is.null(Mobility_Index)) {
      Mobility_Index <- Mobility_Index_coluna
    } else {
      Mobility_Index <- full_join(Mobility_Index, Mobility_Index_coluna, by = "Indicador")
    }
  }
}

Matriz_Transicao_mae_filha <- Matriz_Transicao; Matriz_Transicao_mae_filha
Mobility_Index_mae_filha <- Mobility_Index; Mobility_Index_mae_filha
}

######
##########     Matriz de Transição para as Famílias Uniparentais
######

{
  # Criar IMC de Pai e de MÃ£e
  base_unida_consolidada_unix <- base_unida_consolidada_uni |>
    mutate(
      # Pai
      imc_pai = case_when(
        estrutura_fam == "mono_homem" & sexo_resp == 1 ~ imc_resp,
        estrutura_fam == "mono_homem" & sexo_conj == 1 ~ imc_conj,
        TRUE                                           ~ NA_real_
      ),
      # MÃ£e
      imc_mae = case_when(
        estrutura_fam == "mono_mulher" & sexo_resp == 2 ~ imc_resp,
        estrutura_fam == "mono_mulher" & sexo_conj == 2 ~ imc_conj,
        TRUE                                           ~ NA_real_
      )
    )
  
  # Base apenas com mono_pai
  base_unida_consolidada_unix_mono_pai <- base_unida_consolidada_unix |>
    filter(
      !is.na(imc_pai)
    ) |>
    select(-imc_mae)
  
  sum(is.na(base_unida_consolidada_unix_mono_pai$imc_pai))
  
  # Base apenas com mono_pai
  base_unida_consolidada_unix_mono_mae <- base_unida_consolidada_unix |>
    filter(
      !is.na(imc_mae)
    ) |>
    select(-imc_pai)
  
  sum(is.na(base_unida_consolidada_unix_mono_pai$imc_pai))
  
  # Puxar
  imc_index_boy_and_girl <- read_excel("imc_index_table_boys_and_girl.xlsx")
  str(imc_index_boy_and_girl)
  
  # Tratamento da tabela
  ref <- imc_index_boy_and_girl %>%
    mutate(
      
      # troca â€œâ€“â€ por "-" e converte para numÃ©rico para meninas
      age_month_girl = as.integer(str_extract(sex_and_age_girl, "^\\d+")),
      L_girl = parse_number(str_replace_all(L_girl, "â€“", "-")),
      M_girl = parse_number(str_replace_all(M_girl, "â€“", "-")),
      S_girl = parse_number(str_replace_all(S_girl, "â€“", "-")),
      `5th_girl`  = parse_number(str_replace_all(`5th_girl`,  "â€“", "-")),
      `85th_girl` = parse_number(str_replace_all(`85th_girl`, "â€“", "-")),
      `95th_girl` = parse_number(str_replace_all(`95th_girl`, "â€“", "-")),
      
      # troca â€œâ€“â€ por "-" e converte para numÃ©rico para meninos
      age_month_boy = as.integer(str_extract(sex_and_age_boy, "^\\d+")),
      L_boy = parse_number(str_replace_all(L_boy, "â€“", "-")),
      M_boy = parse_number(str_replace_all(M_boy, "â€“", "-")),
      S_boy = parse_number(str_replace_all(S_boy, "â€“", "-")),
      `5th_boy`  = parse_number(str_replace_all(`5th_boy`,  "â€“", "-")),
      `85th_boy` = parse_number(str_replace_all(`85th_boy`, "â€“", "-")),
      `95th_boy` = parse_number(str_replace_all(`95th_boy`, "â€“", "-"))
    ) %>%
    transmute(
      # Meninas
      sex_and_age_girl,
      age_month_girl,
      L_girl, M_girl, S_girl,
      p5_girl  = `5th_girl`,
      p85_girl = `85th_girl`,
      p95_girl = `95th_girl`,
      # Meninos
      sex_and_age_boy,
      age_month_boy,
      L_boy, M_boy, S_boy,
      p5_boy  = `5th_boy`,
      p85_boy = `85th_boy`,
      p95_boy = `95th_boy`
    )
  
  str(ref)
  
  # Trnasformar para o formato long
  ref_long <- bind_rows(
    ref |>
      transmute(
        sexo_filho = 2L,                  # 2 = menina
        age_month  = age_month_girl,
        L = L_girl, M = M_girl, S = S_girl,
        p5 = p5_girl, p85 = p85_girl, p95 = p95_girl
      ),
    ref |>
      transmute(
        sexo_filho = 1L,                  # 1 = menino
        age_month  = age_month_boy,
        L = L_boy, M = M_boy, S = S_boy,
        p5 = p5_boy, p85 = p85_boy, p95 = p95_boy
      )
  ) |>
    distinct(sexo_filho, age_month, .keep_all = TRUE)
  
  # FunÃ§Ã£o para classificar o IMC
  classifica_imc_adulto <- function(imc) {
    case_when(
      is.na(imc)                   ~ NA_character_,
      imc < 18.5                   ~ "desnutrido",
      imc >= 18.5 & imc < 25       ~ "saudavel",
      imc >= 25   & imc < 30       ~ "sobrepeso",
      imc >= 30                    ~ "obeso",
    )
  }
  
  # Base Mono Pai
  out_mono_pai <- base_unida_consolidada_unix_mono_pai |>
    mutate(
      age_month = as.integer(idade_meses_filho),
      bmi = as.numeric(imc_filho),
      in_range = age_month >= 24 & age_month <= 239
    ) |>
    left_join(ref_long, by = c("sexo_filho", "age_month")) %>%
    mutate(
      # z-score via LMS
      z_bmi = case_when(
        !in_range ~ NA_real_,
        is.na(L) | is.na(M) | is.na(S) ~ NA_real_,
        bmi <= 0 | M <= 0 | S <= 0 ~ NA_real_,
        abs(L) < 1e-12 ~ log(bmi / M) / S,
        TRUE ~ (((bmi / M)^L) - 1) / (L * S)
      ),
      
      # percentil contÃ­nuo (0-100)
      pct_bmi = if_else(is.na(z_bmi), NA_real_, 100 * pnorm(z_bmi)),
      
      # categoria por cortes da tabela (P5/P85/P95)
      cat_imc_filho = case_when(
        is.na(bmi) | is.na(p5) | is.na(p85) | is.na(p95) ~ NA_character_,
        bmi < p5 ~ "desnutrido",
        bmi < p85 ~ "saudavel",
        bmi < p95 ~ "sobrepeso",
        TRUE ~ "obeso"
      ),
      
      # dummy obeso (como no seu script)
      obeso_filho = if_else(cat_imc_filho == "obeso", 1L, 0L, missing = NA_integer_),
      
      # categorias dos pais (IMC adulto)
      cat_imc_pai = classifica_imc_adulto(imc_pai),
      obeso_pai = as.integer(cat_imc_pai == "obeso")
    ) |>
    filter(
      in_range == TRUE,
      !is.na(imc_pai),                 
      !is.na(cat_imc_pai)              
    )
  
  sum(is.na(out_mono_pai$cat_imc_pai))
  table(out_mono_pai$in_range)
  
  # Base Mono Mae
  out_mono_mae <- base_unida_consolidada_unix_mono_mae |>
    mutate(
      age_month = as.integer(idade_meses_filho),
      bmi = as.numeric(imc_filho),
      in_range = age_month >= 24 & age_month <= 239
    ) |>
    left_join(ref_long, by = c("sexo_filho", "age_month")) %>%
    mutate(
      # z-score via LMS
      z_bmi = case_when(
        !in_range ~ NA_real_,
        is.na(L) | is.na(M) | is.na(S) ~ NA_real_,
        bmi <= 0 | M <= 0 | S <= 0 ~ NA_real_,
        abs(L) < 1e-12 ~ log(bmi / M) / S,
        TRUE ~ (((bmi / M)^L) - 1) / (L * S)
      ),
      
      # percentil contÃ­nuo (0-100)
      pct_bmi = if_else(is.na(z_bmi), NA_real_, 100 * pnorm(z_bmi)),
      
      # categoria por cortes da tabela (P5/P85/P95)
      cat_imc_filho = case_when(
        is.na(bmi) | is.na(p5) | is.na(p85) | is.na(p95) ~ NA_character_,
        bmi < p5 ~ "desnutrido",
        bmi < p85 ~ "saudavel",
        bmi < p95 ~ "sobrepeso",
        TRUE ~ "obeso"
      ),
      
      # dummy obeso (como no seu script)
      obeso_filho = if_else(cat_imc_filho == "obeso", 1L, 0L, missing = NA_integer_),
      
      # categorias dos pais (IMC adulto)
      cat_imc_mae = classifica_imc_adulto(imc_mae),
      obeso_mae = as.integer(cat_imc_mae == "obeso")
    )  |>
    filter(
      in_range == TRUE,
      !is.na(imc_mae),                 
      !is.na(cat_imc_mae)              
    )
  
  sum(is.na(out_mono_mae$cat_imc_mae))
  table(out_mono_mae$in_range)
  table(out_mono_mae$idade_filho)
  table(base_unida_consolidada_unix_mono_mae$idade_filho)
  
  out_mono_mae_filho_filha <- out_mono_mae |>
    mutate(
      # dummy branco
      branco_filho = as.integer(cor_filho == 0),  # se 0 for branco mesmo
      
      # log renda
      ln_renda_total = log(renda_total + 1),
      
      # guarda os valores originais (antes de particionar)
      cat_imc_child = cat_imc_filho,
      imc_child     = imc_filho,
      obeso_child   = obeso_filho,
      
      # MENINAS
      cat_imc_filha = if_else(sexo_filho == 2L, cat_imc_filho, NA_character_),
      imc_filha     = if_else(sexo_filho == 2L, imc_filho, NA_real_),
      obeso_filha   = if_else(sexo_filho == 2L, obeso_filho, NA_integer_),
      
      # MENINOS
      cat_imc_filho = if_else(sexo_filho == 1L, cat_imc_filho, NA_character_),
      imc_filho     = if_else(sexo_filho == 1L, imc_filho, NA_real_),
      obeso_filho   = if_else(sexo_filho == 1L, obeso_filho, NA_integer_)
    )
  
  out_mono_pai_filho_filha <- out_mono_pai |>
    mutate(
      # dummy branco
      branco_filho = as.integer(cor_filho == 0),  # se 0 for branco mesmo
      
      # log renda
      ln_renda_total = log(renda_total + 1),
      
      # guarda os valores originais (antes de particionar)
      cat_imc_child = cat_imc_filho,
      imc_child     = imc_filho,
      obeso_child   = obeso_filho,
      
      # MENINAS
      cat_imc_filha = if_else(sexo_filho == 2L, cat_imc_filho, NA_character_),
      imc_filha     = if_else(sexo_filho == 2L, imc_filho, NA_real_),
      obeso_filha   = if_else(sexo_filho == 2L, obeso_filho, NA_integer_),
      
      # MENINOS
      cat_imc_filho = if_else(sexo_filho == 1L, cat_imc_filho, NA_character_),
      imc_filho     = if_else(sexo_filho == 1L, imc_filho, NA_real_),
      obeso_filho   = if_else(sexo_filho == 1L, obeso_filho, NA_integer_)
    )
  
  # Segregar base para as Matrizes de TransiÃ§Ã£o
  
  # Mono Pai Filho
  out_mono_pai_filho <- out_mono_pai_filho_filha |>
    filter(
      sexo_filho == 1
    )
  table(out_mono_pai_filho$sexo_filho)
  
  # Mono Pai Filha
  out_mono_pai_filha <- out_mono_pai_filho_filha |>
    filter(
      sexo_filho == 2
    )
  table(out_mono_pai_filha$sexo_filho)
  
  # Mono MÃ£e Filho
  out_mono_mae_filho <- out_mono_mae_filho_filha |>
    filter(
      sexo_filho == 1
    )
  table(out_mono_mae_filho$sexo_filho)
  
  # Mono MÃ£e Filha
  out_mono_mae_filha <- out_mono_mae_filho_filha |>
    filter(
      sexo_filho == 2
    )
  table(out_mono_mae_filha$sexo_filho)
  
  library(survey)
  options(survey.lonely.psu = "adjust")
}

###
####                        Matriz de Transição Mono pai -> filho
###

{
  # Filtrar faixa etÃ¡ria e observaÃ§Ãµes vÃ¡lidas
  base_analise_mono_pai_filho <- out_mono_pai_filho |>
    # faixa etÃ¡ria usada no artigo (ajuste se quiser)
    filter(
      !is.na(cat_imc_pai),
      !is.na(estrutura_fam),
      !is.na(sexo_filho),
      !is.na(cod_upa_resp),
      !is.na(estrato_pof_resp),
      !is.na(peso_final_filho)
    ) |>
    # pelo menos pai com categoria de IMC e filho com categoria de IMC
    filter(
      !is.na(cat_imc_pai)
    ) |>
    # garantir a mesma ordem de estados (desnutrido -> saudÃ¡vel -> sobrepeso -> obeso)
    mutate(
      cat_imc_filho = factor(
        cat_imc_filho,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      cat_imc_pai = factor(
        cat_imc_pai,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      # codificaÃ§Ã£o numÃ©rica para o Ã­ndice beta
      imc_pai_num = as.numeric(cat_imc_pai) - 1,
      imc_filho_num = as.numeric(cat_imc_filho) - 1,
      # "controle" = combinaÃ§Ã£o de estrutura familiar e sexo do filho
      grupo    = interaction(estrutura_fam, sexo_filho, drop = TRUE),
      controle = dense_rank(grupo)
    )
  
  table(base_analise_mono_pai_filho$estrutura_fam)
  colSums(is.na(base_analise_mono_pai_filho))
  
  
  {
    Matriz_Transicao <- tibble(
      Controle        = integer(),
      origem_imc      = character(),
      IMC_desnutrido  = double(),
      IMC_saudavel    = double(),
      IMC_sobrepeso   = double(),
      IMC_obeso       = double()
    )
    
    Mobility_Index <- NULL
    estados_imc <- c("desnutrido", "saudavel", "sobrepeso", "obeso")
    
    for (g in sort(unique(base_analise_mono_pai_filho$controle))) {
      
      dados <- base_analise_mono_pai_filho |>
        filter(controle == g)
      
      if (nrow(dados) < 2) next
      
      design <- tryCatch(
        svydesign(
          id      = ~cod_upa_resp,
          strata  = ~estrato_pof_resp,
          weights = ~peso_final_filho,
          data    = dados,
          nest    = TRUE
        ),
        error = function(e) {
          message("Pulando grupo ", g, " (erro no svydesign): ", e$message)
          return(NULL)
        }
      )
      if (is.null(design)) next
      
      observacao <- nrow(dados)
      populacao_estimada <- tryCatch(
        round(sum(weights(design)), 0),
        error = function(e) {
          message("NÃ£o consegui calcular svytotal para grupo ", g, ": ", e$message)
          return(NA_real_)
        }
      )
      
      # ---------- Matriz de transiÃ§Ã£o PAI -> FILHO ---------- #
      tab_IMC <- tryCatch(
        svytable(~cat_imc_pai + cat_imc_filho, design),  # AQUI troca
        error = function(e) {
          message("Pulando grupo ", g, " (erro no svytable): ", e$message)
          return(NULL)
        }
      )
      if (is.null(tab_IMC)) next
      
      MTransicao <- data.frame(
        prop.table(tab_IMC, margin = 1)
      ) |>
        complete(
          cat_imc_pai   = estados_imc,
          cat_imc_filho = estados_imc,
          fill = list(Freq = 0)
        ) |>
        arrange(cat_imc_pai) |>
        pivot_wider(
          names_from  = cat_imc_filho,
          values_from = Freq
        ) |>
        select(-cat_imc_pai) |>
        as.matrix()
      
      rownames(MTransicao) <- estados_imc
      
      # ------- Ã­ndices (Mt, Ml, Md, Mda, Ib, IMA, IMD, IP, P00â€“P33) ------- #
      eigenvalues <- eigen(MTransicao)$values
      
      Mt  <- (nrow(MTransicao) - sum(diag(MTransicao))) / (nrow(MTransicao) - 1)
      Ml  <- 1 - abs(eigenvalues[2])
      Md  <- 1 - abs(det(MTransicao))
      Mda <- 1 - abs(det(MTransicao))^(1 / (nrow(MTransicao) - 1))
      
      Ib <- 0
      for (i in 1:nrow(MTransicao)) {
        for (j in 1:nrow(MTransicao)) {
          Ib <- Ib + (MTransicao[i, j] * abs(i - j))
        }
      }
      Ib <- Ib / (nrow(MTransicao) * (nrow(MTransicao) - 1))
      
      IMA <- 0
      for (i in 1:(nrow(MTransicao) - 1)) {
        for (j in (i + 1):nrow(MTransicao)) {
          IMA <- IMA + MTransicao[i, j]
        }
      }
      IMD <- 0
      for (i in 2:nrow(MTransicao)) {
        for (j in 1:(i - 1)) {
          IMD <- IMD + MTransicao[i, j]
        }
      }
      IP <- sum(diag(MTransicao))
      
      soma <- IMA + IMD + IP
      IMA <- IMA / soma
      IMD <- IMD / soma
      IP  <- IP  / soma
      
      P00 <- MTransicao[1, 1]
      P11 <- MTransicao[2, 2]
      P22 <- MTransicao[3, 3]
      P33 <- MTransicao[4, 4]
      
      # ---------- Ãndice beta (regressÃ£o) ---------- #
      regre <- tryCatch(
        svyglm(imc_filho_num ~ imc_pai_num, design = design),  # AQUI troca
        error = function(e) {
          message("Erro em svyglm no grupo ", g, ": ", e$message)
          return(NULL)
        }
      )
      
      if (is.null(regre) || is.na(coef(regre)[2])) {
        beta <- NA_real_
        beta_compl <- NA_real_
      } else {
        beta <- coef(regre)[2]
        beta_compl <- 1 - beta
      }
      
      Mobility_Index_coluna <- tibble(
        Indicador = c(
          "Observacoes",
          "Populacao Estimada",
          "Indice de Prais",
          "Indice do 2o Autovalor",
          "Indice do Determinante",
          "Indice do Determinante Alternativo",
          "Indice de Bartholomew",
          "Indice de Mobilidade Ascendente",
          "Indice de Mobilidade Descendente",
          "Indice de Persistencia",
          "Persistencia em desnutrido",
          "Persistencia em saudavel",
          "Persistencia em sobrepeso",
          "Persistencia em obeso",
          "Indice Beta (Parametrico)",
          "Indice 1-Beta (Parametrico)"
        ),
        !!paste0("grupo_", g) := c(
          observacao,
          populacao_estimada,
          Mt, Ml, Md, Mda,
          Ib,
          IMA, IMD, IP,
          P00, P11, P22, P33,
          beta, beta_compl
        )
      )
      
      Matriz_linha <- as_tibble(MTransicao, .name_repair = "minimal") |>
        mutate(
          Controle   = g,
          origem_imc = estados_imc
        ) |>
        relocate(Controle, origem_imc) |>
        rename(
          IMC_desnutrido = desnutrido,
          IMC_saudavel   = saudavel,
          IMC_sobrepeso  = sobrepeso,
          IMC_obeso      = obeso
        )
      
      Matriz_Transicao <- bind_rows(Matriz_Transicao, Matriz_linha)
      
      if (is.null(Mobility_Index)) {
        Mobility_Index <- Mobility_Index_coluna
      } else {
        Mobility_Index <- full_join(Mobility_Index, Mobility_Index_coluna, by = "Indicador")
      }
    }
  }
  
  Matriz_Transicao_mono_pai_filho <- Matriz_Transicao; Matriz_Transicao_mono_pai_filho
  Mobility_Index_mono_pai_filho <- Mobility_Index; Mobility_Index_mono_pai_filho
  Matriz_Transicao_mono_pai_filho
  Mobility_Index_mono_pai_filho
}

###
####                        Matriz de Transição Mono pai -> filha
###

{
  # Filtrar faixa etÃ¡ria e observaÃ§Ãµes vÃ¡lidas
  base_analise_mono_pai_filha <- out_mono_pai_filha |>
    # faixa etÃ¡ria usada no artigo (ajuste se quiser)
    filter(
      !is.na(cat_imc_pai),
      !is.na(estrutura_fam),
      !is.na(sexo_filho),
      !is.na(cod_upa_resp),
      !is.na(estrato_pof_resp),
      !is.na(peso_final_filho)
    ) |>
    # pelo menos pai com categoria de IMC e filho com categoria de IMC
    filter(
      !is.na(cat_imc_pai)
    ) |>
    # garantir a mesma ordem de estados (desnutrido -> saudÃ¡vel -> sobrepeso -> obeso)
    mutate(
      cat_imc_filha = factor(
        cat_imc_filha,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      cat_imc_pai = factor(
        cat_imc_pai,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      # codificaÃ§Ã£o numÃ©rica para o Ã­ndice beta
      imc_pai_num = as.numeric(cat_imc_pai) - 1,
      imc_filha_num = as.numeric(cat_imc_filha) - 1,
      # "controle" = combinaÃ§Ã£o de estrutura familiar e sexo do filho
      grupo    = interaction(estrutura_fam, sexo_filho, drop = TRUE),
      controle = dense_rank(grupo)
    )
  
  table(base_analise_mono_pai_filha$estrutura_fam)
  colSums(is.na(base_analise_mono_pai_filha))
  
  
  {
    Matriz_Transicao <- tibble(
      Controle        = integer(),
      origem_imc      = character(),
      IMC_desnutrido  = double(),
      IMC_saudavel    = double(),
      IMC_sobrepeso   = double(),
      IMC_obeso       = double()
    )
    
    Mobility_Index <- NULL
    estados_imc <- c("desnutrido", "saudavel", "sobrepeso", "obeso")
    
    for (g in sort(unique(base_analise_mono_pai_filha$controle))) {
      
      dados <- base_analise_mono_pai_filha |>
        filter(controle == g)
      
      if (nrow(dados) < 2) next
      
      design <- tryCatch(
        svydesign(
          id      = ~cod_upa_resp,
          strata  = ~estrato_pof_resp,
          weights = ~peso_final_filho,
          data    = dados,
          nest    = TRUE
        ),
        error = function(e) {
          message("Pulando grupo ", g, " (erro no svydesign): ", e$message)
          return(NULL)
        }
      )
      if (is.null(design)) next
      
      observacao <- nrow(dados)
      populacao_estimada <- tryCatch(
        round(sum(weights(design)), 0),
        error = function(e) {
          message("NÃ£o consegui calcular svytotal para grupo ", g, ": ", e$message)
          return(NA_real_)
        }
      )
      
      # ---------- Matriz de transiÃ§Ã£o PAI -> FILHO ---------- #
      tab_IMC <- tryCatch(
        svytable(~cat_imc_pai + cat_imc_filha, design),  # AQUI troca
        error = function(e) {
          message("Pulando grupo ", g, " (erro no svytable): ", e$message)
          return(NULL)
        }
      )
      if (is.null(tab_IMC)) next
      
      MTransicao <- data.frame(
        prop.table(tab_IMC, margin = 1)
      ) |>
        complete(
          cat_imc_pai   = estados_imc,
          cat_imc_filha = estados_imc,
          fill = list(Freq = 0)
        ) |>
        arrange(cat_imc_pai) |>
        pivot_wider(
          names_from  = cat_imc_filha,
          values_from = Freq
        ) |>
        select(-cat_imc_pai) |>
        as.matrix()
      
      rownames(MTransicao) <- estados_imc
      
      # ------- Ã­ndices (Mt, Ml, Md, Mda, Ib, IMA, IMD, IP, P00â€“P33) ------- #
      eigenvalues <- eigen(MTransicao)$values
      
      Mt  <- (nrow(MTransicao) - sum(diag(MTransicao))) / (nrow(MTransicao) - 1)
      Ml  <- 1 - abs(eigenvalues[2])
      Md  <- 1 - abs(det(MTransicao))
      Mda <- 1 - abs(det(MTransicao))^(1 / (nrow(MTransicao) - 1))
      
      Ib <- 0
      for (i in 1:nrow(MTransicao)) {
        for (j in 1:nrow(MTransicao)) {
          Ib <- Ib + (MTransicao[i, j] * abs(i - j))
        }
      }
      Ib <- Ib / (nrow(MTransicao) * (nrow(MTransicao) - 1))
      
      IMA <- 0
      for (i in 1:(nrow(MTransicao) - 1)) {
        for (j in (i + 1):nrow(MTransicao)) {
          IMA <- IMA + MTransicao[i, j]
        }
      }
      IMD <- 0
      for (i in 2:nrow(MTransicao)) {
        for (j in 1:(i - 1)) {
          IMD <- IMD + MTransicao[i, j]
        }
      }
      IP <- sum(diag(MTransicao))
      
      soma <- IMA + IMD + IP
      IMA <- IMA / soma
      IMD <- IMD / soma
      IP  <- IP  / soma
      
      P00 <- MTransicao[1, 1]
      P11 <- MTransicao[2, 2]
      P22 <- MTransicao[3, 3]
      P33 <- MTransicao[4, 4]
      
      # ---------- Ãndice beta (regressÃ£o) ---------- #
      regre <- tryCatch(
        svyglm(imc_filha_num ~ imc_pai_num, design = design),  # AQUI troca
        error = function(e) {
          message("Erro em svyglm no grupo ", g, ": ", e$message)
          return(NULL)
        }
      )
      
      if (is.null(regre) || is.na(coef(regre)[2])) {
        beta <- NA_real_
        beta_compl <- NA_real_
      } else {
        beta <- coef(regre)[2]
        beta_compl <- 1 - beta
      }
      
      Mobility_Index_coluna <- tibble(
        Indicador = c(
          "Observacoes",
          "Populacao Estimada",
          "Indice de Prais",
          "Indice do 2o Autovalor",
          "Indice do Determinante",
          "Indice do Determinante Alternativo",
          "Indice de Bartholomew",
          "Indice de Mobilidade Ascendente",
          "Indice de Mobilidade Descendente",
          "Indice de Persistencia",
          "Persistencia em desnutrido",
          "Persistencia em saudavel",
          "Persistencia em sobrepeso",
          "Persistencia em obeso",
          "Indice Beta (Parametrico)",
          "Indice 1-Beta (Parametrico)"
        ),
        !!paste0("grupo_", g) := c(
          observacao,
          populacao_estimada,
          Mt, Ml, Md, Mda,
          Ib,
          IMA, IMD, IP,
          P00, P11, P22, P33,
          beta, beta_compl
        )
      )
      
      Matriz_linha <- as_tibble(MTransicao, .name_repair = "minimal") |>
        mutate(
          Controle   = g,
          origem_imc = estados_imc
        ) |>
        relocate(Controle, origem_imc) |>
        rename(
          IMC_desnutrido = desnutrido,
          IMC_saudavel   = saudavel,
          IMC_sobrepeso  = sobrepeso,
          IMC_obeso      = obeso
        )
      
      Matriz_Transicao <- bind_rows(Matriz_Transicao, Matriz_linha)
      
      if (is.null(Mobility_Index)) {
        Mobility_Index <- Mobility_Index_coluna
      } else {
        Mobility_Index <- full_join(Mobility_Index, Mobility_Index_coluna, by = "Indicador")
      }
    }
  }
  
  Matriz_Transicao_mono_pai_filha <- Matriz_Transicao; Matriz_Transicao_mono_pai_filha
  Mobility_Index_mono_pai_filha <- Mobility_Index; Mobility_Index_mono_pai_filha
  Matriz_Transicao_mono_pai_filha
  Mobility_Index_mono_pai_filha
}

###
####                        Matriz de Transição Mono Mãe -> filho
###

{
  # Filtrar faixa etÃ¡ria e observaÃ§Ãµes vÃ¡lidas
  base_analise_mono_mae_filho <- out_mono_mae_filho |>
    # faixa etÃ¡ria usada no artigo (ajuste se quiser)
    filter(
      !is.na(cat_imc_mae),
      !is.na(estrutura_fam),
      !is.na(sexo_filho),
      !is.na(cod_upa_resp),
      !is.na(estrato_pof_resp),
      !is.na(peso_final_filho)
    ) |>
    # pelo menos pai com categoria de IMC e filho com categoria de IMC
    filter(
      !is.na(cat_imc_mae)
    ) |>
    # garantir a mesma ordem de estados (desnutrido -> saudÃ¡vel -> sobrepeso -> obeso)
    mutate(
      cat_imc_filho = factor(
        cat_imc_filho,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      cat_imc_mae = factor(
        cat_imc_mae,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      # codificaÃ§Ã£o numÃ©rica para o Ã­ndice beta
      imc_mae_num = as.numeric(cat_imc_mae) - 1,
      imc_filho_num = as.numeric(cat_imc_filho) - 1,
      # "controle" = combinaÃ§Ã£o de estrutura familiar e sexo do filho
      grupo    = interaction(estrutura_fam, sexo_filho, drop = TRUE),
      controle = dense_rank(grupo)
    )
  
  table(base_analise_mono_mae_filho$estrutura_fam)
  colSums(is.na(base_analise_mono_mae_filho))
  
  
  {
    Matriz_Transicao <- tibble(
      Controle        = integer(),
      origem_imc      = character(),
      IMC_desnutrido  = double(),
      IMC_saudavel    = double(),
      IMC_sobrepeso   = double(),
      IMC_obeso       = double()
    )
    
    Mobility_Index <- NULL
    estados_imc <- c("desnutrido", "saudavel", "sobrepeso", "obeso")
    
    for (g in sort(unique(base_analise_mono_mae_filho$controle))) {
      
      dados <- base_analise_mono_mae_filho |>
        filter(controle == g)
      
      if (nrow(dados) < 2) next
      
      design <- tryCatch(
        svydesign(
          id      = ~cod_upa_resp,
          strata  = ~estrato_pof_resp,
          weights = ~peso_final_filho,
          data    = dados,
          nest    = TRUE
        ),
        error = function(e) {
          message("Pulando grupo ", g, " (erro no svydesign): ", e$message)
          return(NULL)
        }
      )
      if (is.null(design)) next
      
      observacao <- nrow(dados)
      populacao_estimada <- tryCatch(
        round(sum(weights(design)), 0),
        error = function(e) {
          message("NÃ£o consegui calcular svytotal para grupo ", g, ": ", e$message)
          return(NA_real_)
        }
      )
      
      # ---------- Matriz de transiÃ§Ã£o PAI -> FILHO ---------- #
      tab_IMC <- tryCatch(
        svytable(~cat_imc_mae + cat_imc_filho, design),  # AQUI troca
        error = function(e) {
          message("Pulando grupo ", g, " (erro no svytable): ", e$message)
          return(NULL)
        }
      )
      if (is.null(tab_IMC)) next
      
      MTransicao <- data.frame(
        prop.table(tab_IMC, margin = 1)
      ) |>
        complete(
          cat_imc_mae   = estados_imc,
          cat_imc_filho = estados_imc,
          fill = list(Freq = 0)
        ) |>
        arrange(cat_imc_mae) |>
        pivot_wider(
          names_from  = cat_imc_filho,
          values_from = Freq
        ) |>
        select(-cat_imc_mae) |>
        as.matrix()
      
      rownames(MTransicao) <- estados_imc
      
      # ------- Ã­ndices (Mt, Ml, Md, Mda, Ib, IMA, IMD, IP, P00â€“P33) ------- #
      eigenvalues <- eigen(MTransicao)$values
      
      Mt  <- (nrow(MTransicao) - sum(diag(MTransicao))) / (nrow(MTransicao) - 1)
      Ml  <- 1 - abs(eigenvalues[2])
      Md  <- 1 - abs(det(MTransicao))
      Mda <- 1 - abs(det(MTransicao))^(1 / (nrow(MTransicao) - 1))
      
      Ib <- 0
      for (i in 1:nrow(MTransicao)) {
        for (j in 1:nrow(MTransicao)) {
          Ib <- Ib + (MTransicao[i, j] * abs(i - j))
        }
      }
      Ib <- Ib / (nrow(MTransicao) * (nrow(MTransicao) - 1))
      
      IMA <- 0
      for (i in 1:(nrow(MTransicao) - 1)) {
        for (j in (i + 1):nrow(MTransicao)) {
          IMA <- IMA + MTransicao[i, j]
        }
      }
      IMD <- 0
      for (i in 2:nrow(MTransicao)) {
        for (j in 1:(i - 1)) {
          IMD <- IMD + MTransicao[i, j]
        }
      }
      IP <- sum(diag(MTransicao))
      
      soma <- IMA + IMD + IP
      IMA <- IMA / soma
      IMD <- IMD / soma
      IP  <- IP  / soma
      
      P00 <- MTransicao[1, 1]
      P11 <- MTransicao[2, 2]
      P22 <- MTransicao[3, 3]
      P33 <- MTransicao[4, 4]
      
      # ---------- Ãndice beta (regressÃ£o) ---------- #
      regre <- tryCatch(
        svyglm(imc_filho_num ~ imc_mae_num, design = design),  # AQUI troca
        error = function(e) {
          message("Erro em svyglm no grupo ", g, ": ", e$message)
          return(NULL)
        }
      )
      
      if (is.null(regre) || is.na(coef(regre)[2])) {
        beta <- NA_real_
        beta_compl <- NA_real_
      } else {
        beta <- coef(regre)[2]
        beta_compl <- 1 - beta
      }
      
      Mobility_Index_coluna <- tibble(
        Indicador = c(
          "Observacoes",
          "Populacao Estimada",
          "Indice de Prais",
          "Indice do 2o Autovalor",
          "Indice do Determinante",
          "Indice do Determinante Alternativo",
          "Indice de Bartholomew",
          "Indice de Mobilidade Ascendente",
          "Indice de Mobilidade Descendente",
          "Indice de Persistencia",
          "Persistencia em desnutrido",
          "Persistencia em saudavel",
          "Persistencia em sobrepeso",
          "Persistencia em obeso",
          "Indice Beta (Parametrico)",
          "Indice 1-Beta (Parametrico)"
        ),
        !!paste0("grupo_", g) := c(
          observacao,
          populacao_estimada,
          Mt, Ml, Md, Mda,
          Ib,
          IMA, IMD, IP,
          P00, P11, P22, P33,
          beta, beta_compl
        )
      )
      
      Matriz_linha <- as_tibble(MTransicao, .name_repair = "minimal") |>
        mutate(
          Controle   = g,
          origem_imc = estados_imc
        ) |>
        relocate(Controle, origem_imc) |>
        rename(
          IMC_desnutrido = desnutrido,
          IMC_saudavel   = saudavel,
          IMC_sobrepeso  = sobrepeso,
          IMC_obeso      = obeso
        )
      
      Matriz_Transicao <- bind_rows(Matriz_Transicao, Matriz_linha)
      
      if (is.null(Mobility_Index)) {
        Mobility_Index <- Mobility_Index_coluna
      } else {
        Mobility_Index <- full_join(Mobility_Index, Mobility_Index_coluna, by = "Indicador")
      }
    }
  }
  
  Matriz_Transicao_mono_mae_filho <- Matriz_Transicao; Matriz_Transicao_mono_mae_filho
  Mobility_Index_mono_mae_filho <- Mobility_Index; Mobility_Index_mono_mae_filho
  Matriz_Transicao_mono_mae_filho
  Mobility_Index_mono_mae_filho
}

###
####                        Matriz de Transição Mono Mãe -> filha
###

{
  # Filtrar faixa etÃ¡ria e observaÃ§Ãµes vÃ¡lidas
  base_analise_mono_mae_filha <- out_mono_mae_filha |>
    # faixa etÃ¡ria usada no artigo (ajuste se quiser)
    filter(
      !is.na(cat_imc_mae),
      !is.na(estrutura_fam),
      !is.na(sexo_filho),
      !is.na(cod_upa_resp),
      !is.na(estrato_pof_resp),
      !is.na(peso_final_filho)
    ) |>
    # pelo menos pai com categoria de IMC e filho com categoria de IMC
    filter(
      !is.na(cat_imc_mae)
    ) |>
    # garantir a mesma ordem de estados (desnutrido -> saudÃ¡vel -> sobrepeso -> obeso)
    mutate(
      cat_imc_filha = factor(
        cat_imc_filha,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      cat_imc_mae = factor(
        cat_imc_mae,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      # codificaÃ§Ã£o numÃ©rica para o Ã­ndice beta
      imc_mae_num = as.numeric(cat_imc_mae) - 1,
      imc_filha_num = as.numeric(cat_imc_filha) - 1,
      # "controle" = combinaÃ§Ã£o de estrutura familiar e sexo do filho
      grupo    = interaction(estrutura_fam, sexo_filho, drop = TRUE),
      controle = dense_rank(grupo)
    )
  
  table(base_analise_mono_pai_filha$estrutura_fam)
  colSums(is.na(base_analise_mono_pai_filha))
  
  
  {
    Matriz_Transicao <- tibble(
      Controle        = integer(),
      origem_imc      = character(),
      IMC_desnutrido  = double(),
      IMC_saudavel    = double(),
      IMC_sobrepeso   = double(),
      IMC_obeso       = double()
    )
    
    Mobility_Index <- NULL
    estados_imc <- c("desnutrido", "saudavel", "sobrepeso", "obeso")
    
    for (g in sort(unique(base_analise_mono_mae_filha$controle))) {
      
      dados <- base_analise_mono_mae_filha |>
        filter(controle == g)
      
      if (nrow(dados) < 2) next
      
      design <- tryCatch(
        svydesign(
          id      = ~cod_upa_resp,
          strata  = ~estrato_pof_resp,
          weights = ~peso_final_filho,
          data    = dados,
          nest    = TRUE
        ),
        error = function(e) {
          message("Pulando grupo ", g, " (erro no svydesign): ", e$message)
          return(NULL)
        }
      )
      if (is.null(design)) next
      
      observacao <- nrow(dados)
      populacao_estimada <- tryCatch(
        round(sum(weights(design)), 0),
        error = function(e) {
          message("NÃ£o consegui calcular svytotal para grupo ", g, ": ", e$message)
          return(NA_real_)
        }
      )
      
      # ---------- Matriz de transiÃ§Ã£o PAI -> FILHO ---------- #
      tab_IMC <- tryCatch(
        svytable(~cat_imc_mae + cat_imc_filha, design),  # AQUI troca
        error = function(e) {
          message("Pulando grupo ", g, " (erro no svytable): ", e$message)
          return(NULL)
        }
      )
      if (is.null(tab_IMC)) next
      
      MTransicao <- data.frame(
        prop.table(tab_IMC, margin = 1)
      ) |>
        complete(
          cat_imc_mae   = estados_imc,
          cat_imc_filha = estados_imc,
          fill = list(Freq = 0)
        ) |>
        arrange(cat_imc_mae) |>
        pivot_wider(
          names_from  = cat_imc_filha,
          values_from = Freq
        ) |>
        select(-cat_imc_mae) |>
        as.matrix()
      
      rownames(MTransicao) <- estados_imc
      
      # ------- Ã­ndices (Mt, Ml, Md, Mda, Ib, IMA, IMD, IP, P00â€“P33) ------- #
      eigenvalues <- eigen(MTransicao)$values
      
      Mt  <- (nrow(MTransicao) - sum(diag(MTransicao))) / (nrow(MTransicao) - 1)
      Ml  <- 1 - abs(eigenvalues[2])
      Md  <- 1 - abs(det(MTransicao))
      Mda <- 1 - abs(det(MTransicao))^(1 / (nrow(MTransicao) - 1))
      
      Ib <- 0
      for (i in 1:nrow(MTransicao)) {
        for (j in 1:nrow(MTransicao)) {
          Ib <- Ib + (MTransicao[i, j] * abs(i - j))
        }
      }
      Ib <- Ib / (nrow(MTransicao) * (nrow(MTransicao) - 1))
      
      IMA <- 0
      for (i in 1:(nrow(MTransicao) - 1)) {
        for (j in (i + 1):nrow(MTransicao)) {
          IMA <- IMA + MTransicao[i, j]
        }
      }
      IMD <- 0
      for (i in 2:nrow(MTransicao)) {
        for (j in 1:(i - 1)) {
          IMD <- IMD + MTransicao[i, j]
        }
      }
      IP <- sum(diag(MTransicao))
      
      soma <- IMA + IMD + IP
      IMA <- IMA / soma
      IMD <- IMD / soma
      IP  <- IP  / soma
      
      P00 <- MTransicao[1, 1]
      P11 <- MTransicao[2, 2]
      P22 <- MTransicao[3, 3]
      P33 <- MTransicao[4, 4]
      
      # ---------- Ãndice beta (regressÃ£o) ---------- #
      regre <- tryCatch(
        svyglm(imc_filha_num ~ imc_mae_num, design = design),  # AQUI troca
        error = function(e) {
          message("Erro em svyglm no grupo ", g, ": ", e$message)
          return(NULL)
        }
      )
      
      if (is.null(regre) || is.na(coef(regre)[2])) {
        beta <- NA_real_
        beta_compl <- NA_real_
      } else {
        beta <- coef(regre)[2]
        beta_compl <- 1 - beta
      }
      
      Mobility_Index_coluna <- tibble(
        Indicador = c(
          "Observacoes",
          "Populacao Estimada",
          "Indice de Prais",
          "Indice do 2o Autovalor",
          "Indice do Determinante",
          "Indice do Determinante Alternativo",
          "Indice de Bartholomew",
          "Indice de Mobilidade Ascendente",
          "Indice de Mobilidade Descendente",
          "Indice de Persistencia",
          "Persistencia em desnutrido",
          "Persistencia em saudavel",
          "Persistencia em sobrepeso",
          "Persistencia em obeso",
          "Indice Beta (Parametrico)",
          "Indice 1-Beta (Parametrico)"
        ),
        !!paste0("grupo_", g) := c(
          observacao,
          populacao_estimada,
          Mt, Ml, Md, Mda,
          Ib,
          IMA, IMD, IP,
          P00, P11, P22, P33,
          beta, beta_compl
        )
      )
      
      Matriz_linha <- as_tibble(MTransicao, .name_repair = "minimal") |>
        mutate(
          Controle   = g,
          origem_imc = estados_imc
        ) |>
        relocate(Controle, origem_imc) |>
        rename(
          IMC_desnutrido = desnutrido,
          IMC_saudavel   = saudavel,
          IMC_sobrepeso  = sobrepeso,
          IMC_obeso      = obeso
        )
      
      Matriz_Transicao <- bind_rows(Matriz_Transicao, Matriz_linha)
      
      if (is.null(Mobility_Index)) {
        Mobility_Index <- Mobility_Index_coluna
      } else {
        Mobility_Index <- full_join(Mobility_Index, Mobility_Index_coluna, by = "Indicador")
      }
    }
  }
  
  Matriz_Transicao_mono_mae_filha <- Matriz_Transicao; Matriz_Transicao_mono_mae_filha
  Mobility_Index_mono_mae_filha <- Mobility_Index; Mobility_Index_mono_mae_filha
  Matriz_Transicao_mono_mae_filha
  Mobility_Index_mono_mae_filha
}

# Matriz de TransiÃ§Ã£o para Família Biparental
Matriz_Transicao_pai_filho
Matriz_Transicao_mae_filho
Matriz_Transicao_pai_filha
Matriz_Transicao_mae_filha

Mobility_Index_pai_filho
Mobility_Index_mae_filho
Mobility_Index_pai_filha
Mobility_Index_mae_filha

# Matriz de TransiÃ§Ã£o para Família Uniparental - Mono Pai e Mono Mulher
Matriz_Transicao_mono_pai_filho
Matriz_Transicao_mono_mae_filho
Matriz_Transicao_mono_pai_filha
Matriz_Transicao_mono_mae_filha

Mobility_Index_mono_pai_filha
Mobility_Index_mono_mae_filha
Mobility_Index_mono_pai_filho
Mobility_Index_mono_mae_filho

