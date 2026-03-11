#------------------------------------------------------------------------------#
###                  CÓDIGO OBESIDADE INTERGERACIONAL                        ###
#------------------------------------------------------------------------------#
# Aluno: Victor Eduardo
# Doscente: Adriano Firmino V. Araújo
###
# ---------------------------------------------------------------------------- #
##                    MODELAGEM ECONOMÉTRICA - 2008-2009                       ##
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
  library(dplyr)
  library(gt)
  library(scales)
})

options(survey.lonely.psu = "adjust")

dados_obesidade_geral <- read.csv("base_obesidade_geral_2007.csv")
dados_obesidade_bi <- read.csv("base_obesidade_bi_2007.csv")
dados_obesidade_uni <- read.csv("base_obesidade_uni_2007.csv")

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
  filter(sexo_filho == 2) |>
  rename(cat_imc_filha = cat_imc_filho)

table(dados_obesidade_geral_pai_filha$cat_imc_pai)
table(dados_obesidade_geral_pai_filha$sexo_filho)
table(dados_obesidade_geral_pai_filha$estrutura_fam)

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
  filter(sexo_filho == 2) |>
  rename(cat_imc_filha = cat_imc_filho)

table(dados_obesidade_geral_mae_filha$cat_imc_mae)
table(dados_obesidade_geral_mae_filha$sexo_filho)

## ----------------------------------------------
###                   Uniparental
## ----------------------------------------------

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
  filter(sexo_filho == 2) |>
  rename(cat_imc_filha = cat_imc_filho)

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
  filter(sexo_filho == 2) |>
  rename(cat_imc_filha = cat_imc_filho)

table(dados_obesidade_uni_pai_filha$cat_imc_pai)
table(dados_obesidade_uni_pai_filha$sexo_filho)

######
#########         Matriz de Transição para as Famílias Biparentais
######

###
####                  GERAL - Responsável -> filho(a)
###
table(dados_obesidade_bi$cat_imc_resp)
table(dados_obesidade_bi$cat_imc_filho)
table(dados_obesidade_bi$sexo_filho)

# Verificar filho e filha juntos
dados_obesidade_bi_formatada <- dados_obesidade_bi |>
  mutate(sexo_filho = ifelse(sexo_filho == 2, 1, sexo_filho))

#table(dados_obesidade_bi_formatada$sexo_filho)

{
  # Filtrar faixa etária e observações válidas
  #dados_obesidade_uni_matriz <- dados_obesidade_bi |>
  dados_obesidade_uni_matriz <- dados_obesidade_bi_formatada |>  
    filter(
      !is.na(cat_imc_resp),
      !is.na(estrutura_fam),
      !is.na(sexo_filho),
      !is.na(cod_upa_resp),
      !is.na(estrato_pof_resp),
      !is.na(peso_final_filho)
    ) |>
    # pelo menos pai com categoria de IMC e filho com categoria de IMC
    filter(
      !is.na(cat_imc_resp)
    ) |>
    # garantir a mesma ordem de estados (desnutrido -> saudável -> sobrepeso -> obeso)
    mutate(
      cat_imc_filho = factor(
        cat_imc_filho,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      cat_imc_resp = factor(
        cat_imc_resp,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      # codificação numérica para o Índice beta
      imc_resp_num = as.numeric(cat_imc_resp) - 1,
      imc_filho_num = as.numeric(cat_imc_filho) - 1,
      # "controle" = combinação de estrutura familiar e sexo do filho
      grupo    = interaction(sexo_filho, drop = TRUE),
      controle = dense_rank(grupo)
    )
  
  table(dados_obesidade_uni_matriz$estrutura_fam)
  colSums(is.na(dados_obesidade_uni_matriz))
  
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
    
    for (g in sort(unique(dados_obesidade_uni_matriz$controle))) {
      
      dados <- dados_obesidade_uni_matriz |>
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
          message("Não consegui calcular svytotal para grupo ", g, ": ", e$message)
          return(NA_real_)
        }
      )
      
      # ---------- Matriz de transição PAI -> FILHO ---------- #
      tab_IMC <- tryCatch(
        svytable(~cat_imc_resp + cat_imc_filho, design),  # AQUI troca
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
          cat_imc_resp   = estados_imc,
          cat_imc_filho = estados_imc,
          fill = list(Freq = 0)
        ) |>
        arrange(cat_imc_resp) |>
        pivot_wider(
          names_from  = cat_imc_filho,
          values_from = Freq
        ) |>
        select(-cat_imc_resp) |>
        as.matrix()
      
      rownames(MTransicao) <- estados_imc
      
      # ------- Índices (Mt, Ml, Md, Mda, Ib, IMA, IMD, IP, P00â€“P33) ------- #
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
        svyglm(imc_filho_num ~ imc_resp_num, design = design),
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
  
  Matriz_Transicao_bi_total <- Matriz_Transicao; Matriz_Transicao_bi_total
  Mobility_Index_bi_total <- Mobility_Index; Mobility_Index_bi_total
  Matriz_Transicao_bi_total
  Mobility_Index_bi_total
}

Matriz_Transicao_bi_total[] <- lapply(
  Matriz_Transicao_bi_total,
  function(x) if(is.numeric(x)) round(x*100, 2) else x
)


###
####                    Pai -> Filho
###

table(dados_obesidade_geral_pai_filho$cat_imc_filho)
table(dados_obesidade_geral_pai_filho$idade_filho)

{
  # 1) Filtrar faixa etária e observações válidas - PAI -> FILHO (out_filho)
  base_analise_pai_filho <- dados_obesidade_geral_pai_filho |>
    filter(
      idade_filho >= 2,
      idade_filho <= 20,
      !is.na(cat_imc_filho),   # categoria do filho (agora genérica)
      !is.na(cat_imc_pai),
      !is.na(estrutura_fam),
      !is.na(sexo_filho),
      !is.na(cod_upa_resp),
      !is.na(estrato_pof_resp),
      !is.na(peso_final_filho)
    ) |>
    mutate(
      cat_imc_filho = factor(
        cat_imc_filho,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      cat_imc_pai = factor(
        cat_imc_pai,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      # codificação numérica para o Índice beta
      imc_pai_num   = as.numeric(cat_imc_pai)   - 1,
      imc_filho_num = as.numeric(cat_imc_filho) - 1,
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
          message("Não consegui calcular svytotal para grupo ", g, ": ", e$message)
          return(NA_real_)
        }
      )
      
      # ---------- Matriz de transição PAI -> FILHO ---------- #
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
      
      # ------- Índices (Mt, Ml, Md, Mda, Ib, IMA, IMD, IP, P00â€“P33) ------- #
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
  
  Matriz_Transicao_bi_pai_filho <- Matriz_Transicao; Matriz_Transicao_bi_pai_filho
  Mobility_Index_bi_pai_filho <- Mobility_Index; Mobility_Index_bi_pai_filho
  Matriz_Transicao_bi_pai_filho
  Mobility_Index_bi_pai_filho
  
  round(Matriz_Transicao_bi_pai_filho, 4)
  round(Mobility_Index_bi_pai_filho, 4)
  
}
Matriz_Transicao_bi_pai_filho1 <- round(Matriz_Transicao_bi_pai_filho, 6)

Matriz_Transicao_bi_pai_filho[] <- lapply(
  Matriz_Transicao_bi_pai_filho,
  function(x) if(is.numeric(x)) round(x*100, 6) else x
)

###
####                    Pai -> Filha
###
table(dados_obesidade_geral_pai_filha$cat_imc_filha)
table(dados_obesidade_geral_pai_filha$idade_filho)
table(dados_obesidade_geral_pai_filha$estrutura_fam)

{
  # 1) Filtrar faixa etária e observações válidas - PAI -> FILHA
  base_analise_pai_filha <- dados_obesidade_geral_pai_filha |>
    filter(
      idade_filho >= 2,
      idade_filho <= 20,
      !is.na(cat_imc_filha),   # categoria do filho (agora genÃ©rica)
      !is.na(cat_imc_pai),
      !is.na(estrutura_fam),
      !is.na(sexo_filho),
      !is.na(cod_upa_resp),
      !is.na(estrato_pof_resp),
      !is.na(peso_final_filho)
    ) |>
    mutate(
      cat_imc_filha = factor(
        cat_imc_filha,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      cat_imc_pai = factor(
        cat_imc_pai,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      # codificação numérica para o Índice beta
      imc_pai_num   = as.numeric(cat_imc_pai)   - 1,
      imc_filha_num = as.numeric(cat_imc_filha) - 1,
      # "controle" = combinação de estrutura familiar e sexo do filho (aqui Ã© sempre 1, mas ok)
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
          message("Não consegui calcular svytotal para grupo ", g, ": ", e$message)
          return(NA_real_)
        }
      )
      
      # ---------- Matriz de transição PAI -> FILHO ---------- #
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
      
      # ------- Índices (Mt, Ml, Md, Mda, Ib, IMA, IMD, IP, P00â€“P33) ------- #
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
  
  Matriz_Transicao_bi_pai_filha <- Matriz_Transicao; Matriz_Transicao_bi_pai_filha
  Mobility_Index_bi_pai_filha <- Mobility_Index; Mobility_Index_bi_pai_filha
}

Matriz_Transicao_bi_pai_filha[] <- lapply(
  Matriz_Transicao_bi_pai_filha,
  function(x) if(is.numeric(x)) round(x*100, 6) else x
)

###
####                    Mãe -> Filho
###

table(dados_obesidade_geral_mae_filho$cat_imc_filho)
table(dados_obesidade_geral_mae_filho$idade_filho)

{
  # 1) Filtrar faixa etária e observações válidas - Mãe -> FILHO (out_filho)
  base_analise_mae_filho <- dados_obesidade_geral_mae_filho |>
    filter(
      idade_filho >= 2,
      idade_filho <= 20,
      !is.na(cat_imc_filho),   # categoria do filho (genÃ©rica)
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
      cat_imc_filho = factor(
        cat_imc_filho,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      cat_imc_mae = factor(
        cat_imc_mae,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      # codificação numÃ©rica para o índice beta
      imc_mae_num   = as.numeric(cat_imc_mae)   - 1,
      imc_filho_num = as.numeric(cat_imc_filho) - 1,
      # aqui ainda deixo a mesma lógica de agrupamento
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
          message("Não consegui calcular svytotal para grupo ", g, ": ", e$message)
          return(NA_real_)
        }
      )
      
      # ---------- Matriz de transição Mae -> FILHO ---------- #
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
      
      # ------- Índices (Mt, Ml, Md, Mda, Ib, IMA, IMD, IP, P00â€“P33) ------- #
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
  
  Matriz_Transicao_bi_mae_filho <- Matriz_Transicao; Matriz_Transicao_bi_mae_filho
  Mobility_Index_bi_mae_filho <- Mobility_Index; Mobility_Index_bi_mae_filho
}

Matriz_Transicao_bi_mae_filho[] <- lapply(
  Matriz_Transicao_bi_mae_filho,
  function(x) if(is.numeric(x)) round(x*100, 6) else x
)

###
####                    Mãe -> Filha
###
table(dados_obesidade_geral_mae_filha$cat_imc_filha)
table(dados_obesidade_geral_mae_filha$idade_filho)
table(dados_obesidade_geral_mae_filha$sexo_filho)

{
  # 1) Filtrar faixa etária e observações válidas - Mãe -> FILHO (out_filho)
  base_analise_mae_filha <- dados_obesidade_geral_mae_filha |>
    filter(
      !is.na(cat_imc_filha),
      !is.na(cat_imc_mae),
      !is.na(estrutura_fam),
      !is.na(sexo_filho),
      !is.na(cod_upa_resp),
      !is.na(estrato_pof_resp),
      !is.na(peso_final_filho)
    ) |>
    mutate(
      cat_imc_filha = factor(
        cat_imc_filha,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      cat_imc_mae = factor(
        cat_imc_mae,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      
      imc_mae_num   = as.numeric(cat_imc_mae)   - 1,
      imc_filha_num = as.numeric(cat_imc_filha) - 1,
      
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
          message("Não consegui calcular svytotal para grupo ", g, ": ", e$message)
          return(NA_real_)
        }
      )
      
      # ---------- Matriz de transição Mãe -> FILHA ---------- #
      tab_IMC <- tryCatch(
        svytable(~cat_imc_mae + cat_imc_filha, design),
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
      
      # ------- Índices (Mt, Ml, Md, Mda, Ib, IMA, IMD, IP, P00â€“P33) ------- #
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
        svyglm(imc_filha_num ~ imc_mae_num, design = design),
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
  
  Matriz_Transicao_bi_mae_filha <- Matriz_Transicao; Matriz_Transicao_bi_mae_filha
  Mobility_Index_bi_mae_filha <- Mobility_Index; Mobility_Index_bi_mae_filha
}
Matriz_Transicao_bi_mae_filha

Matriz_Transicao_bi_mae_filha[] <- lapply(
  Matriz_Transicao_bi_mae_filha,
  function(x) if(is.numeric(x)) round(x*100, 2) else x
)

######
##########     Matriz de Transição para as Famílias Uniparentais ✅
######
table(dados_obesidade_uni$sexo_filho)
table(dados_obesidade_uni$estrutura_fam)

#dados_obesidade_uni_formatada <- dados_obesidade_uni %>%
#  mutate(sexo_filho = ifelse(sexo_filho == 2, 1, sexo_filho))

#table(dados_obesidade_uni_formatada$sexo_filho)

{
  # Filtrar faixa etária e observações válidas
  dados_obesidade_uni_matriz <- dados_obesidade_uni |>
    filter(
      !is.na(cat_imc_resp),
      !is.na(estrutura_fam),
      !is.na(sexo_filho),
      !is.na(cod_upa_resp),
      !is.na(estrato_pof_resp),
      !is.na(peso_final_filho)
    ) |>
    # pelo menos pai com categoria de IMC e filho com categoria de IMC
    filter(
      !is.na(cat_imc_resp)
    ) |>
    # garantir a mesma ordem de estados (desnutrido -> saudável -> sobrepeso -> obeso)
    mutate(
      cat_imc_filho = factor(
        cat_imc_filho,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      cat_imc_resp = factor(
        cat_imc_resp,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      # codificação numérica para o Índice beta
      imc_resp_num = as.numeric(cat_imc_resp) - 1,
      imc_filho_num = as.numeric(cat_imc_filho) - 1,
      # "controle" = combinação de estrutura familiar e sexo do filho
      grupo    = interaction(sexo_filho, drop = TRUE),
      controle = dense_rank(grupo)
    )
  
  table(dados_obesidade_uni_matriz$estrutura_fam)
  colSums(is.na(dados_obesidade_uni_matriz))
  
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
    
    for (g in sort(unique(dados_obesidade_uni_matriz$controle))) {
      
      dados <- dados_obesidade_uni_matriz |>
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
          message("Não consegui calcular svytotal para grupo ", g, ": ", e$message)
          return(NA_real_)
        }
      )
      
      # ---------- Matriz de transição PAI -> FILHO ---------- #
      tab_IMC <- tryCatch(
        svytable(~cat_imc_resp + cat_imc_filho, design),  # AQUI troca
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
          cat_imc_resp   = estados_imc,
          cat_imc_filho = estados_imc,
          fill = list(Freq = 0)
        ) |>
        arrange(cat_imc_resp) |>
        pivot_wider(
          names_from  = cat_imc_filho,
          values_from = Freq
        ) |>
        select(-cat_imc_resp) |>
        as.matrix()
      
      rownames(MTransicao) <- estados_imc
      
      # ------- Índices (Mt, Ml, Md, Mda, Ib, IMA, IMD, IP, P00â€“P33) ------- #
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
        svyglm(imc_filho_num ~ imc_resp_num, design = design),
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
  
  Matriz_Transicao_mono_total <- Matriz_Transicao; Matriz_Transicao_mono_total
  Mobility_Index_mono_total <- Mobility_Index; Mobility_Index_mono_total
  Matriz_Transicao_mono_total
  Mobility_Index_mono_total
}

Matriz_Transicao_mono_total[] <- lapply(
  Matriz_Transicao_mono_total,
  function(x) if(is.numeric(x)) round(x*100, 2) else x
)

###
####                        Matriz de Transição Mono pai -> filho
###

table(dados_obesidade_uni_pai_filho$cat_imc_pai)
table(dados_obesidade_uni_pai_filho$cat_imc_filho)

{
  # Filtrar faixa etária e observações válidas
  base_analise_mono_pai_filho <- dados_obesidade_uni_pai_filho |>
    # faixa etária usada no artigo (ajuste se quiser)
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
    # garantir a mesma ordem de estados (desnutrido -> saudável -> sobrepeso -> obeso)
    mutate(
      cat_imc_filho = factor(
        cat_imc_filho,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      cat_imc_pai = factor(
        cat_imc_pai,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      # codificação numérica para o Índice beta
      imc_pai_num = as.numeric(cat_imc_pai) - 1,
      imc_filho_num = as.numeric(cat_imc_filho) - 1,
      # "controle" = combinação de estrutura familiar e sexo do filho
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
          message("Não consegui calcular svytotal para grupo ", g, ": ", e$message)
          return(NA_real_)
        }
      )
      
      # ---------- Matriz de transição PAI -> FILHO ---------- #
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
      
      # ------- Índices (Mt, Ml, Md, Mda, Ib, IMA, IMD, IP, P00â€“P33) ------- #
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
        svyglm(imc_filho_num ~ imc_pai_num, design = design),
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

Matriz_Transicao_mono_pai_filho[] <- lapply(
  Matriz_Transicao_mono_pai_filho,
  function(x) if(is.numeric(x)) round(x*100, 2) else x
)

###
####                        Matriz de Transição Mono pai -> filha
###

table(dados_obesidade_uni_pai_filha$cat_imc_pai)
table(dados_obesidade_uni_pai_filha$cat_imc_filha)
table(dados_obesidade_uni_pai_filha$estrutura_fam)

{
  # Filtrar faixa etária e observações válidas
  base_analise_mono_pai_filha <- dados_obesidade_uni_pai_filha |>
    # faixa etária usada no artigo (ajuste se quiser)
    #filter(
    #  !is.na(cat_imc_pai),
    #  !is.na(estrutura_fam),
    #  !is.na(sexo_filho),
    #  !is.na(cod_upa_resp),
    #  !is.na(estrato_pof_resp),
    #  !is.na(peso_final_filho)
    #) |>
    # pelo menos pai com categoria de IMC e filho com categoria de IMC
    #filter(
    #  !is.na(cat_imc_pai)
    #) |>
    # garantir a mesma ordem de estados (desnutrido -> saudável -> sobrepeso -> obeso)
    mutate(
      cat_imc_filha = factor(
        cat_imc_filha,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      cat_imc_pai = factor(
        cat_imc_pai,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      # codificação numérica para o índice beta
      imc_pai_num = as.numeric(cat_imc_pai) - 1,
      imc_filha_num = as.numeric(cat_imc_filha) - 1,
      # "controle" = combinação de estrutura familiar e sexo do filho
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
          message("Não consegui calcular svytotal para grupo ", g, ": ", e$message)
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
      
      # ------- Índices (Mt, Ml, Md, Mda, Ib, IMA, IMD, IP, P00â€“P33) ------- #
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

Matriz_Transicao_mono_pai_filha[] <- lapply(
  Matriz_Transicao_mono_pai_filha,
  function(x) if(is.numeric(x)) round(x*100, 2) else x
)

###
####                        Matriz de Transição Mono Mãe -> filho ✅
###
# Conferindo base
table(dados_obesidade_uni_mae_filho$sexo_filho)   # Realmente filhos
table(dados_obesidade_uni_mae_filho$estrutura_fam)# Realmente monoparental

{
  # Filtrar faixa etária e observações válidas
  base_analise_mono_mae_filho <- dados_obesidade_uni_mae_filho |>
    # faixa etária usada no artigo (ajuste se quiser)
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
    # garantir a mesma ordem de estados (desnutrido -> saudável -> sobrepeso -> obeso)
    mutate(
      cat_imc_filho = factor(
        cat_imc_filho,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      cat_imc_mae = factor(
        cat_imc_mae,
        levels = c("desnutrido", "saudavel", "sobrepeso", "obeso")
      ),
      # codificaçãoo numérica para o índice beta
      imc_mae_num = as.numeric(cat_imc_mae) - 1,
      imc_filho_num = as.numeric(cat_imc_filho) - 1,
      # "controle" = combinação de estrutura familiar e sexo do filho
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
          message("Não consegui calcular svytotal para grupo ", g, ": ", e$message)
          return(NA_real_)
        }
      )
      
      # ---------- Matriz de transição MÃE -> FILHO ---------- #
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
      
      # ------- Índices (Mt, Ml, Md, Mda, Ib, IMA, IMD, IP, P00â€“P33) ------- #
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

Matriz_Transicao_mono_mae_filho[] <- lapply(
  Matriz_Transicao_mono_mae_filho,
  function(x) if(is.numeric(x)) round(x*100, 2) else x
)

###
####                        Matriz de Transição Mono Mãe -> filha ✅
###
table(dados_obesidade_uni_mae_filha$sexo_filho)
table(dados_obesidade_uni_mae_filha$estrutura_fam)# Realmente monoparental

{
  # Filtrar faixa etária e observaçõess válidas
  base_analise_mono_mae_filha <- dados_obesidade_uni_mae_filha |>
    # faixa etária usada no artigo (ajuste se quiser)
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
      # codificação numérica para o Índice beta
      imc_mae_num = as.numeric(cat_imc_mae) - 1,
      imc_filha_num = as.numeric(cat_imc_filha) - 1,
      # "controle" = combinaÇÃO de estrutura familiar e sexo do filho
      grupo    = interaction(estrutura_fam, sexo_filho, drop = TRUE),
      controle = dense_rank(grupo)
    )
  
  table(base_analise_mono_mae_filha$estrutura_fam)
  colSums(is.na(base_analise_mono_mae_filha))
  
  
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
          message("Não consegui calcular svytotal para grupo ", g, ": ", e$message)
          return(NA_real_)
        }
      )
      
      # ---------- Matriz de transição MÃE -> FILHA ---------- #
      tab_IMC <- tryCatch(
        svytable(~cat_imc_mae + cat_imc_filha, design),
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
      
      # ------- Índices (Mt, Ml, Md, Mda, Ib, IMA, IMD, IP, P00â€“P33) ------- #
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

Matriz_Transicao_mono_mae_filha[] <- lapply(
  Matriz_Transicao_mono_mae_filha,
  function(x) if(is.numeric(x)) round(x*100, 2) else x
)

###### ================================================================== ######
######                       RESUMO DOS RESULTADOS                        ######
###### ================================================================== ######

## -------------------------------------------------------------------------- ##
###   Matriz de Transição para Família Biparentais - Mono Pai e Mono Mulher  ###
## -------------------------------------------------------------------------- ##

# Matriz de TransiçÕ para Família Biparental
Matriz_Transicao_bi_pai_filho
Matriz_Transicao_bi_mae_filho
Matriz_Transicao_bi_pai_filha
Matriz_Transicao_bi_mae_filha

Mobility_Index_bi_pai_filho
Mobility_Index_bi_mae_filho
Mobility_Index_bi_pai_filha
Mobility_Index_bi_mae_filha

# Resumo da Matriz de Transição em Tabela
## Filhos
{
  matriz_transicao_pai_mae_filho_bi <- bind_rows(
    Matriz_Transicao_bi_pai_filho |> mutate(grupo = "Pai"),
    Matriz_Transicao_bi_mae_filho |> mutate(grupo = "Mãe"),
  ) |>
    select(-Controle) |>
    pivot_longer(starts_with("IMC_"), names_to = "destino", values_to = "prob") |>
    mutate(destino = sub("^IMC_", "", destino)) |>
    unite(col, grupo, destino, sep = " | ") |>
    pivot_wider(names_from = col, values_from = prob)
  
  matriz_transicao_pai_mae_filho_bi |>
    gt(rowname_col = "origem_imc") |>
    tab_header(
      title = md("**Matrizes de Transição (IMC) para os filhos**"),
      subtitle = md("Diagonal principal representa persistência")
    ) |>
    fmt_number(everything(), decimals = 3) |>
    
    # DIAGONAL - PAI
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "desnutrido",
        columns = "Pai | desnutrido"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "saudavel",
        columns = "Pai | saudavel"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "sobrepeso",
        columns = "Pai | sobrepeso"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "obeso",
        columns = "Pai | obeso"
      )
    ) |>
    
    # DIAGONAL - MÃE
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "desnutrido",
        columns = "Mãe | desnutrido"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "saudavel",
        columns = "Mãe | saudavel"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "sobrepeso",
        columns = "Mãe | sobrepeso"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "obeso",
        columns = "Mãe | obeso"
      )
    )
}

## Filhas
{
  matriz_transicao_pai_mae_filha_bi <- bind_rows(
    Matriz_Transicao_bi_pai_filha |> mutate(grupo = "Pai"),
    Matriz_Transicao_bi_mae_filha |> mutate(grupo = "Mãe"),
  ) |>
    select(-Controle) |>
    pivot_longer(starts_with("IMC_"), names_to = "destino", values_to = "prob") |>
    mutate(destino = sub("^IMC_", "", destino)) |>
    unite(col, grupo, destino, sep = " | ") |>
    pivot_wider(names_from = col, values_from = prob)
  
  matriz_transicao_pai_mae_filha_bi |>
    gt(rowname_col = "origem_imc") |>
    tab_header(
      title = md("**Matrizes de Transição (IMC) para os filhas**"),
      subtitle = md("Diagonal principal representa persistência")
    ) |>
    fmt_number(everything(), decimals = 3) |>
    
    # DIAGONAL - PAI
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "desnutrido",
        columns = "Pai | desnutrido"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "saudavel",
        columns = "Pai | saudavel"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "sobrepeso",
        columns = "Pai | sobrepeso"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "obeso",
        columns = "Pai | obeso"
      )
    ) |>
    
    # DIAGONAL - MÃE
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "desnutrido",
        columns = "Mãe | desnutrido"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "saudavel",
        columns = "Mãe | saudavel"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "sobrepeso",
        columns = "Mãe | sobrepeso"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "obeso",
        columns = "Mãe | obeso"
      )
    )
}

## -------------------------------------------------------------------------- ##
###   Matriz de Transição para Família Uniparental - Mono Pai e Mono Mulher  ###
## -------------------------------------------------------------------------- ##

Matriz_Transicao_mono_pai_filho
Matriz_Transicao_mono_mae_filho
Matriz_Transicao_mono_pai_filha
Matriz_Transicao_mono_mae_filha

Mobility_Index_mono_pai_filha
Mobility_Index_mono_mae_filha
Mobility_Index_mono_pai_filho
Mobility_Index_mono_mae_filho

# Resumo da Matriz de Transição em Tabela
## Filhos
{
  matriz_transicao_pai_mae_filho <- bind_rows(
    Matriz_Transicao_mono_pai_filho |> mutate(grupo = "Pai"),
    Matriz_Transicao_mono_mae_filho |> mutate(grupo = "Mãe"),
  ) |>
    select(-Controle) |>
    pivot_longer(starts_with("IMC_"), names_to = "destino", values_to = "prob") |>
    mutate(destino = sub("^IMC_", "", destino)) |>
    unite(col, grupo, destino, sep = " | ") |>
    pivot_wider(names_from = col, values_from = prob)
  
  matriz_transicao_pai_mae_filho |>
    gt(rowname_col = "origem_imc") |>
    tab_header(
      title = md("**Matrizes de Transição (IMC) para os filhos**"),
      subtitle = md("Diagonal principal representa persistência")
    ) |>
    fmt_number(everything(), decimals = 3) |>
    
    # DIAGONAL - PAI
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "desnutrido",
        columns = "Pai | desnutrido"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "saudavel",
        columns = "Pai | saudavel"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "sobrepeso",
        columns = "Pai | sobrepeso"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "obeso",
        columns = "Pai | obeso"
      )
    ) |>
    
    # DIAGONAL - MÃE
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "desnutrido",
        columns = "Mãe | desnutrido"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "saudavel",
        columns = "Mãe | saudavel"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "sobrepeso",
        columns = "Mãe | sobrepeso"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "obeso",
        columns = "Mãe | obeso"
      )
    )
}

## Filhas
{
  matriz_transicao_pai_mae_filha <- bind_rows(
    Matriz_Transicao_mono_pai_filha |> mutate(grupo = "Pai"),
    Matriz_Transicao_mono_mae_filha |> mutate(grupo = "Mãe"),
  ) |>
    select(-Controle) |>
    pivot_longer(starts_with("IMC_"), names_to = "destino", values_to = "prob") |>
    mutate(destino = sub("^IMC_", "", destino)) |>
    unite(col, grupo, destino, sep = " | ") |>
    pivot_wider(names_from = col, values_from = prob)
  
  matriz_transicao_pai_mae_filha |>
    gt(rowname_col = "origem_imc") |>
    tab_header(
      title = md("**Matrizes de Transição (IMC) para as filhas**"),
      subtitle = md("Diagonal principal representa persistência")
    ) |>
    fmt_number(everything(), decimals = 3) |>
    
    # DIAGONAL - PAI
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "desnutrido",
        columns = "Pai | desnutrido"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "saudavel",
        columns = "Pai | saudavel"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "sobrepeso",
        columns = "Pai | sobrepeso"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "obeso",
        columns = "Pai | obeso"
      )
    ) |>
    
    # DIAGONAL - MÃE
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "desnutrido",
        columns = "Mãe | desnutrido"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "saudavel",
        columns = "Mãe | saudavel"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "sobrepeso",
        columns = "Mãe | sobrepeso"
      )
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(
        rows = origem_imc == "obeso",
        columns = "Mãe | obeso"
      )
    )
}

# Resumo em tabela dos índices
{
  tabela_indices_mobilidade <- bind_rows(
    Mobility_Index_mono_pai_filha |> mutate(grupo = "Pai → Filha"),
    Mobility_Index_mono_mae_filha |> mutate(grupo = "Mãe → Filha"),
    Mobility_Index_mono_pai_filho |> mutate(grupo = "Pai → Filho"),
    Mobility_Index_mono_mae_filho |> mutate(grupo = "Mãe → Filho")
  ) |>
    rename(valor = grupo_1) |>
    pivot_wider(names_from = grupo, values_from = valor)
  
  tabela_indices_mobilidade |>
    gt(rowname_col = "Indicador") |>
    tab_header(
      title = "Índices de Mobilidade Intergeracional",
      subtitle = "Comparação por grupo parental e sexo do filho - Mono Parental"
    ) |>
    fmt_number(
      columns = c("Pai → Filha", "Mãe → Filha", "Pai → Filho", "Mãe → Filho"),
      decimals = 3
    ) |>
    fmt_number(
      columns = c("Pai → Filha", "Mãe → Filha", "Pai → Filho", "Mãe → Filho"),
      rows = Indicador %in% c("Observacoes", "Populacao Estimada"),
      decimals = 0
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_row_groups()
    )
}
