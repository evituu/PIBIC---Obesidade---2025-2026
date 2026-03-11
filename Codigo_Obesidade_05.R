#------------------------------------------------------------------------------#
###                  CÓDIGO OBESIDADE INTERGERACIONAL                        ###
#------------------------------------------------------------------------------#
# Aluno: Victor Eduardo
# Doscente: Adriano Firmino V. Araújo

# ---------------------------------------------------------------------------
# EXTRAÇÃO POR readr::read_fwf + fwf_cols (POF 2017-2018 | MORADOR.txt)
# ---------------------------------------------------------------------------

getwd()

setwd("C:/Users/vitor/OneDrive/Área de Trabalho/UFPB/PIBIC_2025/Base de Dados/POFF_Dados_2002_2003")
getwd()

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(readxl)
  library(quantreg) # Cálculo das regressões quantílicas
  library(tibble)
  library(tidyr)
  library(haven)    # Leitura de .dta
  library(anthro)   # Ferramentas para cálculos antropométricos
  library(childsds) # Cálculo de standard deviation scores
  library(survey)   # Biblioteca para desenho amostral
  library(scales)   # Controlar escalas dos gráficos
})

morador <- "T_MORADOR.txt"
dir()

# ------------------------ Leitura com fwf_cols ------------------------------

dados_obesidade <- read_fwf(
  file = morador,
  col_positions = fwf_cols(
    tipo_registro     = c(1, 2),
    UF                = c(3, 4),
    num_seq           = c(5, 7),        # Número Sequencial
    dv_seq            = c(8),           # DV do Sequencial
    ESTRATO_GEO       = c(12, 13),     # 3-4 - Estrato Geográfico
    PESO_DESENHO      = c(11, 15),      # FATOR DE EXPANSÃO 1 (SETOR) - PESO DO DESENHO
    fator_expan2      = c(25, 35),      # FATOR DE EXPANSÃO 2 - Peso Final do Domicílio
    
    NUM_DOM           = c(9, 10),
    NUM_UC            = c(11),          # Número da unidade de consumo
    NUM_INFORMANTE    = c(36, 37),      # Número do informante    
    estrato_geo       = c(12, 13),       # Estrato geográfico
    #NUM_FAMILIA       = c(46, 47),
    COD_INFORMANTE    = c(38),          # Código do informante
    V0403             = c(49, 51),      # idade
    idade_meses       = c(59, 64),      # idade em meses
    V0404             = c(40),          # sexo
    V0405             = c(72),          # cor/raça
    #V0414             = c(78, 79),     # sabe ler e escrever
    V0415             = c(65),          # frequenta escola
    ANOS_ESTUDO       = c(70, 71), 
    NIVEL_INSTRUCAO   = c(66, 67),
    RENDA_TOTAL       = c(103, 114),
    #RENDA_MONET_PC    = c(112, 127),
    #RENDA_NAO_MONET_PC = c(128, 143),
    #PESO              = c(16, 29),
    #PESO_FINAL        = c(30, 43),
    #DEDUCAO_PC         = c(177, 196),
    ALTURA_REFERIDA   = c(86, 95),
    PESO_REFERIDO     = c(76, 85)
  ),
  # Tipagem explícita (ajuste se necessário)
  col_types = cols(
    UF                = col_character(),
    num_seq           = col_character(),
    dv_seq            = col_character(),
    ESTRATO_GEO          = col_character(),
    PESO_DESENHO      = col_character(),
    NUM_DOM           = col_character(),
    NUM_UC            = col_character(),
    fator_expan2      = col_character(),
    NUM_INFORMANTE    = col_character(),
    #NUM_FAMILIA       = col_integer(),
    COD_INFORMANTE    = col_character(),
    V0403             = col_integer(),
    idade_meses       = col_integer(),
    V0404             = col_integer(),
    V0405             = col_integer(),
    #V0414             = col_integer(),
    V0415             = col_integer(),
    ANOS_ESTUDO       = col_integer(),
    NIVEL_INSTRUCAO   = col_integer(),
    RENDA_TOTAL       = col_double(),
    #RENDA_MONET_PC    = col_double(),
    #RENDA_NAO_MONET_PC = col_double(),
    #PESO              = col_double(),
    #PESO_FINAL        = col_double(),
    ALTURA_REFERIDA   = col_double(),
    PESO_REFERIDO     = col_double()
  ),
  locale = locale(encoding = "Latin1"),
  na = c("", " ", "NA"),
  show_col_types = FALSE
)

###
### ------------------------ ESCALA (decimais implícitos) ----------------------
###

scale_if_needed <- function(x, dec){
  if (all(is.na(x))) return(x)
  med <- suppressWarnings(stats::median(x, na.rm = TRUE))
  if (!is.finite(med)) return(x)
  if (med > 1e6) x <- x / (10^dec)
  x
}

dados_obesidade <- dados_obesidade |>
  mutate(
    #PESO               = scale_if_needed(PESO,               8),
    #PESO_FINAL         = scale_if_needed(PESO_FINAL,         8),
    RENDA_TOTAL        = scale_if_needed(RENDA_TOTAL,        2)
    #RENDA_MONET_PC     = scale_if_needed(RENDA_MONET_PC,     10),
    #RENDA_NAO_MONET_PC = scale_if_needed(RENDA_NAO_MONET_PC, 10)
  )

summary(dados_obesidade$RENDA_TOTAL)

# Contar quantos valores são extremamente grandes ou pequenos
dados_obesidade |>
  summarise(
    #max_peso = max(PESO_FINAL, na.rm = TRUE),
    #min_peso = min(PESO_FINAL, na.rm = TRUE),
    max_renda = max(RENDA_TOTAL, na.rm = TRUE),
    min_renda = min(RENDA_TOTAL, na.rm = TRUE)
  )

# Tamanho do banco e verificação de NAs
dim(dados_obesidade)
colSums(is.na(dados_obesidade))

# 1) Renomeiar
dados_obesidade <- dados_obesidade |>
  rename(
    uf                = UF,
    #estrato_pof       = ESTRATO_POF,
    #zona              = TIPO_SITUACAO_REG,
    peso_desenho      = PESO_DESENHO,
    peso_geo          = ESTRATO_GEO,
    num_dom           = NUM_DOM,
    num_uc            = NUM_UC,
    #num_familia       = NUM_FAMILIA,
    grau_parentesco    = COD_INFORMANTE,
    idade             = V0403,
    sexo              = V0404,
    cor               = V0405,
    #sabe_ler_escrever = V0414,
    peso_final        = fator_expan2,
    freq_escola       = V0415,
    instrucao         = NIVEL_INSTRUCAO,
    anos_estudo       = ANOS_ESTUDO,
    #peso              = PESO,
    #peso_final        = PESO_FINAL,
    renda_total       = RENDA_TOTAL,
    #renda_monet_pc    = RENDA_MONET_PC,
    #renda_nao_monet_pc = RENDA_NAO_MONET_PC,
    altura            = ALTURA_REFERIDA,
    massa              = PESO_REFERIDO
  )

# quantidade de moradores
# Dummies de região
# biparental, mono H, mono M
# Pesos

# Criar chave única para unidade de consumo e para pessoa
dados_obesidade <- dados_obesidade |>
  mutate(
    chave_uc = paste0(
      str_pad(uf, 2, pad = "0"),            # 1–2: UF
      str_pad(num_seq, 1, pad = "0"),
      str_pad(dv_seq, 1, pad = "0"),
      str_pad(num_dom, 2 , pad = "0"),
      str_pad(num_uc, 1, pad = "0"),         # 19: unidade de consumo
      str_pad(peso_final, 11, pad = "0"),     # Fator de expansão do domicílio
      str_pad(peso_desenho, 5, pad = "0"),
      str_pad(peso_geo, 2, pad = "0")
      #str_pad(estrato_pof, 4, pad = "0"),   # 3–6: Estrato
      #str_pad(cod_upa, 9, pad = "0"),       # 8–16: UPA
      #str_pad(num_familia, 2, pad = "0")    # 46-47: unidade familiar 
    )
  ) |>
  mutate(
    chave_pessoa = paste0(
      str_pad(uf, 2, pad = "0"),            # 1–2: UF
      str_pad(num_seq, 1, pad = "0"),
      str_pad(dv_seq, 1, pad = "0"),
      str_pad(num_dom, 2 , pad = "0"),
      str_pad(num_uc, 1, pad = "0"), 
      str_pad(grau_parentesco, 1, pad = "0"),
      str_pad(NUM_INFORMANTE, 2, pad = "0")
    )
  )

dados_obesidade <- dados_obesidade |>
  mutate(
    cod_upa = paste0(
      str_pad(uf,     2, pad = "0"),
      str_pad(num_seq,3, pad = "0"),
      str_pad(dv_seq, 1, pad = "0")
    )
  )

# Conferir se é única:
dados_obesidade |> 
  count(chave_uc) |>
  filter(n > 1)

# chave_uc: várias linhas por família (normal no MORADOR)
dados_obesidade |> count(num_uc) |> arrange(desc(n)) |> head()
dados_obesidade |> count(num_dom) |> arrange(desc(n)) |> head()

dados_obesidade |> count(chave_uc) |> arrange(desc(n)) |> head()
sum(duplicated(dados_obesidade$chave_uc))

# chave_pessoa: deve ser única por morador
dados_obesidade |> count(chave_pessoa) |> filter(n > 1)  # idealmente, 0 linhas
sum(duplicated(dados_obesidade$chave_pessoa))

# Nenhuma chave está repetida ou duplicada

# Nenhum NA em chaves
colSums(is.na(dados_obesidade[, c("chave_uc", "chave_pessoa")]))

# ------------------------ Checagens rápidas ---------------------------------
glimpse(dados_obesidade)
head(dados_obesidade)
# table(dados_obesidade$num_uc, useNA = "ifany")
# table(dados_obesidade$grau_parentesco, useNA = "ifany")


### ------------------------ OVERVIEW E ANÁLISE ---------------------------- ###

# Verificação dos dados da base
str(dados_obesidade)
unique(dados_obesidade$uf)
unique(dados_obesidade$num_dom)
unique(dados_obesidade$grau_parentesco)
unique(dados_obesidade$instrucao)
unique(dados_obesidade$idade)
unique(dados_obesidade$idade_meses)
unique(dados_obesidade$sexo)
unique(dados_obesidade$cor)
unique(dados_obesidade$anos_estudo)
unique(dados_obesidade$freq_escola)
unique(dados_obesidade$altura)
unique(dados_obesidade$massa)



# Verificar estatística descritiva da altura e massa
summary(dados_obesidade$massa)
summary(dados_obesidade$altura)

# Verificar quantidade de NA
sum(is.na(dados_obesidade$massa))
sum(is.na(dados_obesidade$altura))

unique(dados_obesidade$num_dom)

# Verificar estatística descritiva da altura e massa
summary(dados_obesidade$massa)
summary(dados_obesidade$altura)

# Verificar quantidade de NA
sum(is.na(dados_obesidade$massa))
sum(is.na(dados_obesidade$altura))

sum(duplicated(dados_obesidade$chave_pessoa))
sum(duplicated(dados_obesidade$chave_pessoa))

#------------------------------------------------------------------------------#
### ----------------------- TRATAMENTO DOS DADOS --------------------------- ###
#------------------------------------------------------------------------------#

# Organizar ordem das variáveis
dados_obesidade <- dados_obesidade |>
  select(
    uf, num_dom, chave_uc, num_uc,
    chave_uc, chave_pessoa, grau_parentesco, idade, sexo, cor,
    freq_escola, anos_estudo, instrucao, renda_total,
    massa, altura, imc, peso_final, peso_geo, peso_desenho
  )

quant_crianca1 <- dados_obesidade |>
  filter(grau_parentesco == 3) |>
  filter(idade < 20 & idade > 2)

table(quant_crianca1$num_uc)
table(quant_crianca1$idade)

colSums(is.na(dados_obesidade))
table(dados_obesidade$idade)

#------------------------------------------------------------------------------#
### ----------------------- TRATAMENTO DOS DADOS --------------------------- ###
#------------------------------------------------------------------------------#

dados_obesidade <- dados_obesidade |>
  mutate(sexo = case_when(
    sexo == 1 ~ 1,             # homem
    sexo %in% c(2, 3, 4) ~ 2,  # mulher
    TRUE ~ NA_real_
  ))

# Calcular IMC das observações
dados_obesidade <- dados_obesidade |>
  mutate(
    altura_m = altura / 100,
    imc = massa / (altura_m^2)
  )

summary(dados_obesidade$imc)
sum(is.na(dados_obesidade$imc))
count(dados_obesidade)

## Remover mais de uma unidade familiar

unique(dados_obesidade$num_uc)

# Transformar em numérica
dados_obesidade$num_uc <- as.numeric(dados_obesidade$num_uc)
unique(dados_obesidade$num_uc)

# Manter apenas uma unidade de consumo
dados_obesidade_trat_uma_familia_uc <- dados_obesidade |>
  filter(num_uc == 1)

unique(dados_obesidade_trat_uma_familia_uc$num_uc)
table(dados_obesidade_trat_uma_familia_uc$num_uc)

# Manter os graus de parentescos relevantes para pesquisa
sort(unique(dados_obesidade_trat_uma_familia_uc$grau_parentesco))
table(dados_obesidade_trat_uma_familia_uc$grau_parentesco)

dados_obesidade_trat <- dados_obesidade_trat_uma_familia_uc |>
  filter(grau_parentesco %in% c(1,2,3))

sort(unique(dados_obesidade_trat$grau_parentesco)) # Verificar
table(dados_obesidade_trat$grau_parentesco)        # Verificar

# Criar variável de branco e não branco
## Examinar situação:
sort(unique(dados_obesidade_trat$cor))
table(dados_obesidade_trat$cor)

# Remoção da categoria "ignorado"
dados_obesidade_trat <- dados_obesidade_trat |>
  dplyr::filter(cor != 8) |>
  dplyr::mutate(
    branco = dplyr::case_when(
      cor == 1 ~ 1L,
      cor %in% c(2,3,4,5) ~ 0L,
      TRUE ~ NA_integer_
    )
  )

sort(unique(dados_obesidade_trat$branco))
table(dados_obesidade_trat$branco)

# Não é necessário remover a renda total igual a zero
renda_zero <- dados_obesidade_trat |>
  filter(renda_total == 0)

# Contar quantos responsáveis (grau_parentesco == 1) há em cada UC
domicilios_mult_resp <- dados_obesidade_trat |>
  filter(grau_parentesco == 1) |>         
  count(chave_uc, name = "n_responsaveis") |>   # contar por família
  filter(n_responsaveis > 1)                    # mantém famílias com > 1 responsável

# Ver quantas famílias estão nessa situação
nrow(domicilios_mult_resp)
head(domicilios_mult_resp, 10)
# Os resultados apontam que existe apenas um responsável por UC

# Construir a estrutura familiar biparental e monoparental
fam_flags <- dados_obesidade_trat |>
  filter(grau_parentesco %in% c(1,2), idade >= 18, sexo %in% c(1,2)) |>
  summarise(
    n_pais = sum(sexo == 1),
    n_maes = sum(sexo == 2),
    .by = chave_uc
  ) |>
  mutate(
    estrutura_fam = case_when(
      n_pais == 1 & n_maes == 1 ~ "biparental",
      n_pais == 1 & n_maes == 0 ~ "mono_homem",
      n_pais == 0 & n_maes == 1 ~ "mono_mulher",
      TRUE ~ NA_character_
    )
  )

table(fam_flags$estrutura_fam)
table(fam_flags$n_pais)
table(fam_flags$n_maes)

# Lares homoafetivos
ucs_homo <- fam_flags |> 
  filter(n_pais >= 2 & n_maes == 0 | 
           n_maes >= 2 & n_pais == 0) |> 
  pull(chave_uc)

dados_obesidade_trat <- dados_obesidade_trat |>
  filter(!chave_uc %in% ucs_homo)

# Criar variável de região e criação da dummy regional
dados_obesidade_trat <- dados_obesidade_trat |>
  dplyr::mutate(
    regiao = case_when(
      uf %in% c(11:17) ~ "norte",
      uf %in% c(21:29) ~ "nordeste",
      uf %in% c(31:35) ~ "sudeste",
      uf %in% c(41:43) ~ "sul",
      uf %in% c(50:53) ~ "centro_oeste"
    )
  )

unique(dados_obesidade_trat$regiao)
table(dados_obesidade_trat$regiao)

# Criar dummy regional
dados_obesidade_trat <- dados_obesidade_trat |>
  mutate(
    dummy_norte        = if_else(regiao == "norte",         1L, 0L),
    dummy_nordeste     = if_else(regiao == "nordeste",      1L, 0L),
    dummy_sul          = if_else(regiao == "sul",           1L, 0L),
    dummy_centro_oeste = if_else(regiao == "centro_oeste",  1L, 0L)
    # repare: NÃO crio dummy para "sudeste" → ele é a base
  )

unique(dados_obesidade_trat$dummy_norte)
unique(dados_obesidade_trat$dummy_nordeste)
unique(dados_obesidade_trat$dummy_sul)
unique(dados_obesidade_trat$dummy_centro_oeste)

# Contar número de moradores por Unidade de Consumo - Universo de moradores
# Usar base não trata para considerar todo mundo
moradores_uc <- dados_obesidade_trat_uma_familia_uc |>
  count(chave_uc, name = "n_moradores")

table(moradores_uc$n_moradores)

# Dummies de frequência escolar
dados_obesidade_trat <- dados_obesidade_trat |>
  mutate(
    freq_escola = as.integer(freq_escola == 1)
  )

# Verificar a dummy escolar
freq_escola_verificacao <- dados_obesidade_trat |>
  mutate(
    freq_escolar = as.integer(freq_escola == 1)
  ) |>
  select(
    freq_escola, freq_escolar
  )

# Verificar dados de frequêcnia escolar
unique(dados_obesidade_trat$freq_escola)
table(dados_obesidade_trat$freq_escola)

####
#### CRIAR BASE DE DADOS
####

## 2.1 Base do responsável (V0306 == 1)
# Se houver mais de 1 “responsável” na mesma UC, fica o mais velho.
base_resp <- dados_obesidade_trat |>
  filter(grau_parentesco == 1) |>
  filter(idade >= 18) |>
  # arrange(chave_uc, desc(idade)) |>
  # distinct(chave_uc, .keep_all = TRUE) |>
  transmute(
    chave_uc,
    chave_pessoa_resp = chave_pessoa,
    idade_resp = idade,
    idade_resp2 = idade^2,              # <- idade^2
    sexo_resp  = sexo,
    branco_resp   = branco,
    instrucao_resp = instrucao,
    anos_estudo_resp = anos_estudo,
    chave_pessoa_resp = chave_pessoa,
    freq_escola_resp = freq_escola,
    peso_final_resp = peso_final,
    peso_desenho_resp = peso_desenho,
    peso_geo_resp = peso_geo,
    cod_upa_resp = cod_upa,


    altura_resp = altura,
    massa_resp = massa,
    imc_resp = imc
    
  )

# Verificar se tem mais de um cônjuge
base_conj <- dados_obesidade_trat |>
  filter(grau_parentesco %in% c(2)) |>
  count(chave_uc, name = "n_conjuges") |>
  filter(n_conjuges > 1)

nrow(base_conj); head(base_conj)

# 2.2 Base do cônjuge
base_conj <- dados_obesidade_trat |>
  filter(grau_parentesco %in% c(2)) |>
  filter(idade >= 18) |>
  # arrange(chave_uc, desc(idade)) %>%
  distinct(chave_uc, .keep_all = TRUE) |>
  transmute(
    chave_uc,
    chave_pessoa_conj = chave_pessoa,
    idade_conj = idade,
    idade_conj2 = idade^2,              # <- idade^2
    sexo_conj  = sexo,
    branco_conj   = branco,
    instrucao_conj = instrucao,
    anos_estudo_conj = anos_estudo,
    chave_pessoa_conj = chave_pessoa,
    freq_escola_conj = freq_escola,
    peso_final_conj = peso_final, 
    peso_desenho_conj = peso_desenho,
    peso_geo_conj = peso_geo,
    cod_upa_conj = cod_upa,
    
    
    
    altura_conj = altura,
    massa_conj = massa,
    imc_conj = imc
    
  )
nrow(base_conj); head(base_conj)

# 2.3 Base dos filhos
base_filho <- dados_obesidade_trat |>
  filter(grau_parentesco %in% c(3), idade >= 3, idade <= 19) |>
  filter(idade_meses >= 36 & idade_meses <= 239) |>  arrange(chave_uc) |>
  arrange(chave_uc) |>
  mutate(id_filho_uc = row_number(), .by = chave_uc) |>
  transmute(
    chave_uc,
    id_filho_uc,
    idade_filho = idade,
    idade_filho2 = idade^2,
    idade_meses_filho = idade_meses,
    sexo_filho  = sexo,
    branco_filho   = branco,
    instrucao_filho = instrucao,
    chave_pessoa_filho = chave_pessoa,
    freq_escola_filho = freq_escola,
    peso_final_filho = peso_final,
    peso_desenho_filho = peso_desenho,
    peso_geo_filho = peso_geo,
    cod_upa_filho = cod_upa,
    

    altura_filho = altura,
    massa_filho = massa,
    imc_filho = imc
  )

uc_info <- dados_obesidade_trat |>
  distinct(chave_uc, regiao, uf, dummy_norte, dummy_nordeste, dummy_sul, dummy_centro_oeste,
           renda_total)

# Criar base consolidada “filho + pais + controles”
# Fazer "inner_join" para se ter pelo menos um par de "filho - responsável"
base_unida_consolidada <- base_filho |>
  inner_join(base_resp, by = "chave_uc")

base_unida_consolidada <- base_unida_consolidada |>
  left_join(base_conj, by = "chave_uc")

base_unida_consolidada <- base_unida_consolidada |>
  left_join(fam_flags, by = "chave_uc")

base_unida_consolidada <- base_unida_consolidada |>
  left_join(moradores_uc, by = "chave_uc")

base_unida_consolidada <- base_unida_consolidada |>
  left_join(uc_info, by = "chave_uc")

str(base_unida_consolidada)

# Criar IMC de Pai e de Mãe
base_unida_consolidada <- base_unida_consolidada |>
  mutate(
    # Pai
    imc_pai = case_when(
      estrutura_fam == "biparental" & sexo_resp == 1 ~ imc_resp,
      estrutura_fam == "biparental" & sexo_conj == 1 ~ imc_conj,
      estrutura_fam == "mono_homem"                  ~ imc_resp,
      estrutura_fam == "mono_homem"                  ~ imc_conj,
      TRUE                                           ~ NA_real_
    ),
    # Mãe
    imc_mae = case_when(
      estrutura_fam == "biparental" & sexo_resp == 2 ~ imc_resp,
      estrutura_fam == "biparental" & sexo_conj == 2 ~ imc_conj,
      estrutura_fam == "mono_mulher"                 ~ imc_resp,
      estrutura_fam == "mono_mulher"                 ~ imc_conj,
      TRUE                                           ~ NA_real_
    )
  )

colSums(is.na(base_unida_consolidada))
table(base_unida_consolidada$estrutura_fam)

# Puxar o percentil de cada criança conforme o imc, idade e gênero
imc_index_boy_and_girl <- read_excel("imc_index_table_boys_and_girl.xlsx")
str(imc_index_boy_and_girl)
table(imc_index_boy_and_girl$sex_and_age_boy)
table(imc_index_boy_and_girl$sex_and_age_girl)

# Tratamento da tabela
ref <- imc_index_boy_and_girl |>
  mutate(
    
    # troca “–” por "-" e converte para numérico para meninas
    age_month_girl = as.integer(str_extract(sex_and_age_girl, "^\\d+")),
    L_girl = parse_number(str_replace_all(L_girl, "–", "-")),
    M_girl = parse_number(str_replace_all(M_girl, "–", "-")),
    S_girl = parse_number(str_replace_all(S_girl, "–", "-")),
    `5th_girl`  = parse_number(str_replace_all(`5th_girl`,  "–", "-")),
    `85th_girl` = parse_number(str_replace_all(`85th_girl`, "–", "-")),
    `95th_girl` = parse_number(str_replace_all(`95th_girl`, "–", "-")),
    
    # troca “–” por "-" e converte para numérico para meninos
    age_month_boy = as.integer(str_extract(sex_and_age_boy, "^\\d+")),
    L_boy = parse_number(str_replace_all(L_boy, "–", "-")),
    M_boy = parse_number(str_replace_all(M_boy, "–", "-")),
    S_boy = parse_number(str_replace_all(S_boy, "–", "-")),
    `5th_boy`  = parse_number(str_replace_all(`5th_boy`,  "–", "-")),
    `85th_boy` = parse_number(str_replace_all(`85th_boy`, "–", "-")),
    `95th_boy` = parse_number(str_replace_all(`95th_boy`, "–", "-"))
  ) |>
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

# Função para classificar o IMC
classifica_imc_adulto <- function(imc) {
  case_when(
    is.na(imc)                   ~ NA_character_,
    imc < 18.5                   ~ "desnutrido",
    imc >= 18.5 & imc < 25       ~ "saudavel",
    imc >= 25   & imc < 30       ~ "sobrepeso",
    imc >= 30                    ~ "obeso",
  )
}

colSums(is.na(base_unida_consolidada))
table(base_unida_consolidada$idade_filho)
table(base_unida_consolidada$idade_meses_filho)

# A base "base_unidade_consolidada_out" possui a categorização correta
base_unida_consolidada_out <- base_unida_consolidada |>
  mutate(
    age_month = as.integer(idade_meses_filho),
    bmi = as.numeric(imc_filho),                   # bmi = body mass index
    in_range = age_month >= 36 & age_month <= 239
  ) |>
  left_join(ref_long, by = c("sexo_filho", "age_month")) |>
  mutate(
    # z-score via LMS
    z_bmi = case_when(
      !in_range ~ NA_real_,
      is.na(L) | is.na(M) | is.na(S) ~ NA_real_,
      bmi <= 0 | M <= 0 | S <= 0 ~ NA_real_,
      abs(L) < 1e-12 ~ log(bmi / M) / S,
      TRUE ~ (((bmi / M)^L) - 1) / (L * S)
    ),
    
    # percentil contínuo (0-100)
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
    cat_imc_mae = classifica_imc_adulto(imc_mae),
    cat_imc_resp = classifica_imc_adulto(imc_resp),
    cat_imc_conj = classifica_imc_adulto(imc_conj),
    
    obeso_pai = as.integer(cat_imc_pai == "obeso"),
    obeso_mae = as.integer(cat_imc_mae == "obeso"),
    obeso_resp = as.integer(cat_imc_resp == "obeso"),
    obeso_conj = as.integer(cat_imc_conj == "obeso")
  ) |>
  filter(
    in_range == TRUE,
  )

table(base_unida_consolidada_out$sexo_filho)
colSums(is.na(base_unida_consolidada_out))
sum(is.na(base_unida_consolidada_out$obeso_filho))

# Base para o arquivo "Codigo_Obesidade_04.3 - Regressões.R"
write.csv(base_unida_consolidada_out, "Base_2002_obesidade_regressão.csv")






sum(is.na(base_unida_consolidada_out$cat_imc_pai))
table(base_unida_consolidada_out$in_range)
table(base_unida_consolidada_out$idade_filho)
table(base_unida_consolidada_out$estrutura_fam)

summary(base_unida_consolidada_out$imc_filho)
summary(base_unida_consolidada_out$imc_pai)
summary(base_unida_consolidada_out$imc_mae)

# Verificar
base_unida_consolidada_out_verificar <- base_unida_consolidada_out |>
  select(
    sexo_filho,
    idade_meses_filho,
    idade_filho,
    imc_filho,
    age_month,
    bmi,
    in_range,
    L,
    M,
    S,
    p5,
    p85,
    p95,
    z_bmi,
    pct_bmi,
    cat_imc_filho,
    obeso_filho
  ) |>
  filter(
    in_range == TRUE
  ); base_unida_consolidada_out_verificar

# Estatísticas da base tratada

# Quantidade de NA por coluna
na_por_coluna <- base_unida_consolidada_out |>
  summarise(across(everything(), ~sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "coluna", values_to = "n_NA") |>
  arrange(desc(n_NA))

print(na_por_coluna, n = Inf)

table(base_unida_consolidada_out$estrutura_fam)

table(base_unida_consolidada_out$idade_filho)
table(base_unida_consolidada_out$idade_meses_filho)

# Salvar Resultados
table(base_unida_consolidada_out$idade_filho)

df_resultados <- base_unida_consolidada_out |>
  filter(!is.na(cat_imc_filho), sexo_filho %in% c(1,2)) |>
  mutate(
    sexo_filho_lbl = if_else(sexo_filho == 1, "masculino", "feminino"),
    grupo = case_when(
      estrutura_fam == "biparental" ~ "biparental_geral",
      estrutura_fam == "biparental" & sexo_resp == 2 ~ "biparental_chefiado_mulher",
      estrutura_fam == "biparental" & sexo_resp == 1 ~ "biparental_chefiado_homem",
      estrutura_fam %in% c("mono_mulher", "mono_homem") ~ "uniparental_geral",
      estrutura_fam == "mono_mulher" ~ "uniparental_mono_mulher",
      estrutura_fam == "mono_homem"  ~ "uniparental_mono_homem",
      TRUE ~ NA_character_
    )
  )

tab_bip_geral <- df_resultados |>
  filter(estrutura_fam == "biparental") |>
  count(sexo_filho_lbl, cat_imc_filho) |>
  pivot_wider(names_from = cat_imc_filho, values_from = n, values_fill = 0) |>
  mutate(grupo = "biparental_geral") |>
  relocate(grupo)

tab_bip_mulher <- df_resultados |>
  filter(estrutura_fam == "biparental", sexo_resp == 2) |>
  count(sexo_filho_lbl, cat_imc_filho) |>
  pivot_wider(names_from = cat_imc_filho, values_from = n, values_fill = 0) |>
  mutate(grupo = "biparental_chefiado_mulher") |>
  relocate(grupo)

tab_bip_homem <- df_resultados |>
  filter(estrutura_fam == "biparental", sexo_resp == 1) |>
  count(sexo_filho_lbl, cat_imc_filho) |>
  pivot_wider(names_from = cat_imc_filho, values_from = n, values_fill = 0) |>
  mutate(grupo = "biparental_chefiado_homem") |>
  relocate(grupo)

tab_uni_geral <- df_resultados |>
  filter(estrutura_fam %in% c("mono_mulher", "mono_homem")) |>
  count(sexo_filho_lbl, cat_imc_filho) |>
  pivot_wider(names_from = cat_imc_filho, values_from = n, values_fill = 0) |>
  mutate(grupo = "uniparental_geral") |>
  relocate(grupo)

tab_uni_mulher <- df_resultados |>
  filter(estrutura_fam == "mono_mulher") |>
  count(sexo_filho_lbl, cat_imc_filho) |>
  pivot_wider(names_from = cat_imc_filho, values_from = n, values_fill = 0) |>
  mutate(grupo = "uniparental_mono_mulher") |>
  relocate(grupo)

tab_uni_homem <- df_resultados |>
  filter(estrutura_fam == "mono_homem") |>
  count(sexo_filho_lbl, cat_imc_filho) |>
  pivot_wider(names_from = cat_imc_filho, values_from = n, values_fill = 0) |>
  mutate(grupo = "uniparental_mono_homem") |>
  relocate(grupo)

resultado <- bind_rows(
  tab_bip_geral,
  tab_bip_mulher,
  tab_bip_homem,
  tab_uni_geral,
  tab_uni_mulher,
  tab_uni_homem
) |>
  arrange(
    grupo, sexo_filho_lbl
  ) |>
  mutate(
    total = rowSums(across(any_of(c("desnutrido","saudavel","sobrepeso","obeso"))), na.rm = TRUE)
  ) |>
  relocate(total, .after = sexo_filho_lbl)

print(resultado, n = Inf)

### ------------------------------------------------------------------------ ###
#
# Famílias biparentais e uniparenatais
table(base_unida_consolidada_out$idade_filho)
write.csv(base_unida_consolidada_out, "base_obesidade_geral_2002.csv")

# Famíliuas biparentais
base_fam_bi <- base_unida_consolidada_out |>
  filter(
    estrutura_fam == "biparental"
  )
table(base_fam_bi$estrutura_fam)
table(base_fam_bi$idade_filho)
write.csv(base_fam_bi, "base_obesidade_bi_2002.csv")

# Famílias uniparentais
base_fam_uni <- base_unida_consolidada_out |>
  filter(
    estrutura_fam %in% c("mono_mulher", "mono_homem")
  )
table(base_fam_uni$estrutura_fam)
table(base_fam_uni$idade_filho)
write.csv(base_fam_uni, "base_obesidade_uni_2002.csv")

# Análise Gráfica dos Dados com Base de Dados Completa


# Gráfico de Densidade
imc_dens <- base_unida_consolidada_out |>
  transmute(
    Filho = imc_filho,
    Pai   = imc_pai,
    Mãe   = imc_mae
  ) |>
  pivot_longer(everything(), names_to = "grupo", values_to = "imc") |>
  filter(is.finite(imc)) |>
  mutate(grupo = factor(grupo, levels = c("Filho", "Pai", "Mãe")))

# limitar eixo para não “amassar” por outliers
x_max <- quantile(imc_dens$imc, 0.99, na.rm = TRUE)
x_min <- quantile(imc_dens$imc, 0.01, na.rm = TRUE)

# Estatísticas para linhas de mediana
medianas <- imc_dens |>
  group_by(grupo) |>
  summarise(mediana = median(imc, na.rm = TRUE), .groups = "drop")

ggplot(imc_dens, aes(x = imc, colour = grupo, fill = grupo)) +
  geom_density(alpha = 0.15, linewidth = 1) +
  geom_vline(
    data = medianas,
    aes(xintercept = mediana, colour = grupo),
    linetype = "dashed",
    linewidth = 0.9,
    show.legend = FALSE
  ) +
  coord_cartesian(xlim = c(x_min, x_max)) +
  labs(
    title = "Distribuição do IMC da POFF de 2007/2008",
    subtitle = "Densidade por grupo com eixo X limitado ao P1–P99",
    x = "IMC (kg/m²)",
    y = "Densidade",
    colour = "Grupo",
    fill = "Grupo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

base_unida_consolidada_out %>%
  filter(!is.na(z_bmi), is.finite(z_bmi),
         z_bmi >= -5, z_bmi <= 5) %>%
  ggplot(aes(z_bmi)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 70,
                 fill = "steelblue",
                 alpha = 0.5,
                 color = "white") +
  geom_density(linewidth = 1) +
  geom_vline(xintercept = c(-2, -1, 0, 1, 2),
             linetype = "dashed",
             color = "red",
             alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Distribuição do Z-score IMC infantil (z_bmi)",
    subtitle = "Amostra restrita a -5 ≤ z ≤ 5",
    x = "Z-score IMC (OMS)",
    y = "Densidade"
  )

base_unida_consolidada_out |>
  group_by(idade_filho) |>
  summarise(
    prevalencia_obesidade = mean(obeso_filho == 1, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ggplot(aes(x = idade_filho, y = prevalencia_obesidade)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_line(linewidth = 1, alpha = 0.6) +                     # Liga os pontos
  geom_smooth(method = "loess", se = TRUE, linewidth = 1) + # Captura a tendência geral
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(min(base_unida_consolidada_out$idade_filho, na.rm=TRUE),
                                  max(base_unida_consolidada_out$idade_filho, na.rm=TRUE),
                                  by = 1)) +
  labs(
    title = "Prevalência de obesidade infantil por idade em 2007/2008",
    x = "Idade (anos)",
    y = "Prevalência de obesidade (%)",
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

base_unida_consolidada_out |>
  summarise(
    min_z = min(z_bmi, na.rm = TRUE),
    p001 = quantile(z_bmi, 0.001, na.rm = TRUE),
    p01  = quantile(z_bmi, 0.01, na.rm = TRUE),
    p99  = quantile(z_bmi, 0.99, na.rm = TRUE),
    p999 = quantile(z_bmi, 0.999, na.rm = TRUE),
    max_z = max(z_bmi, na.rm = TRUE)
  )

base_unida_consolidada_out %>%
  summarise(
    n = n(),
    n_z_menor_m5 = sum(z_bmi < -5, na.rm = TRUE),
    n_z_maior_5  = sum(z_bmi > 5, na.rm = TRUE),
    pct_menor_m5 = mean(z_bmi < -5, na.rm = TRUE) * 100,
    pct_maior_5  = mean(z_bmi > 5, na.rm = TRUE) * 100
  )

verificar <- base_unida_consolidada_out 
filter(z_bmi < -10) |>
  select(
    chave_uc,
    idade_filho,
    idade_meses_filho,
    sexo_filho,
    altura_m_filho,
    massa_filho,
    imc_filho,
    bmi,
    L, M, S,
    z_bmi
  ) |>
  arrange(z_bmi) |>
  head(30)

base_unida_consolidada_out |>
  filter(z_bmi < -10) |>
  select(
    idade_filho,
    idade_meses_filho,
    sexo_filho,
    altura_filho,
    massa_filho,
    imc_filho,
    z_bmi
  ) %>%
  arrange(z_bmi) |>
  print(n = 30)































