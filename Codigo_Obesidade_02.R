#------------------------------------------------------------------------------#
###                  CÓDIGO OBESIDADE INTERGERACIONAL                        ###
#------------------------------------------------------------------------------#
# Aluno: Victor Eduardo
# Doscente: Adriano Firmino V. Araújo

# ---------------------------------------------------------------------------
# EXTRAÇÃO POR readr::read_fwf + fwf_cols (POF 2017-2018 | MORADOR.txt)
# ---------------------------------------------------------------------------

getwd()
setwd("C:/Users/vitor/OneDrive/Área de Trabalho/UFPB/PIBIC_2025/Base de Dados/POFF_Dados_2017_2018")

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(ggplot2)
})

morador <- "MORADOR.txt"
caract_dieta_base <- "CARACTERISTICAS_DIETA.txt"
dir()

# ------------------------ Leitura com fwf_cols ------------------------------

dados_obesidade <- read_fwf(
  file = morador,
  col_positions = fwf_cols(
    UF                = c(1, 2),
    ESTRATO_POF       = c(3, 6),       # 3-4 
    TIPO_SITUACAO_REG = 7,             # largura 1
    COD_UPA           = c(8, 16),      # 9 posições (8..16)
    NUM_DOM           = c(17, 18),
    NUM_UC            = 19,            # largura 1
    COD_INFORMANTE    = c(20, 21),     # Código do informante
    V0306             = c(22, 23),   # condição na UC (grau de parentesco)
    V0403             = c(33, 35),     # idade
    V0404             = 36,            # sexo
    V0405             = 37,            # cor/raça
    V0414             = 48,            # sabe ler e escrever
    V0415             = 49,            # frequenta escola
    V0419             = c(60, 61),      # curso
    ANOS_ESTUDO       = c(76, 77), 
    NIVEL_INSTRUCAO   = 116,
    RENDA_TOTAL       = c(106, 115),
    RENDA_DISP_PC     = c(117, 136),
    RENDA_MONET_PC    = c(137, 156),
    RENDA_NAO_MONET_PC = c(157, 176),
    PESO              = c(78, 91),
    PESO_FINAL        = c(92, 105),
    DEDUCAO_PC         = c(177, 196)  # 20, 10 decimais
  ),
  # Tipagem explícita (ajuste se necessário)
  col_types = cols(
    UF                = col_integer(),
    ESTRATO_POF       = col_character(),
    TIPO_SITUACAO_REG = col_integer(),
    COD_UPA           = col_character(),
    NUM_DOM           = col_character(),
    NUM_UC            = col_integer(),
    COD_INFORMANTE    = col_character(),
    V0306             = col_integer(),
    V0403             = col_integer(),
    V0404             = col_integer(),
    V0405             = col_integer(),
    V0414             = col_integer(),
    V0415             = col_integer(),
    V0419             = col_integer(),
    ANOS_ESTUDO       = col_integer(),
    NIVEL_INSTRUCAO   = col_integer(),
    RENDA_TOTAL       = col_double(),
    RENDA_DISP_PC     = col_double(),
    RENDA_MONET_PC    = col_double(),
    RENDA_NAO_MONET_PC = col_double(),
    PESO              = col_double(),
    PESO_FINAL        = col_double(),
    DEDUCAO_PC         = col_double()
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
    PESO               = scale_if_needed(PESO,               8),
    PESO_FINAL         = scale_if_needed(PESO_FINAL,         8),
    RENDA_TOTAL        = scale_if_needed(RENDA_TOTAL,        2),
    RENDA_DISP_PC      = scale_if_needed(RENDA_DISP_PC,      10),
    RENDA_MONET_PC     = scale_if_needed(RENDA_MONET_PC,     10),
    RENDA_NAO_MONET_PC = scale_if_needed(RENDA_NAO_MONET_PC, 10),
    DEDUCAO_PC         = scale_if_needed(DEDUCAO_PC,         10)
  )

summary(dados_obesidade$PESO)
summary(dados_obesidade$PESO_FINAL)
summary(dados_obesidade$RENDA_TOTAL)
summary(dados_obesidade$RENDA_DISP_PC)
summary(dados_obesidade$RENDA_MONET_PC)
summary(dados_obesidade$RENDA_NAO_MONET_PC)
summary(dados_obesidade$DEDUCAO_PC)

# Contar quantos valores são extremamente grandes ou pequenos
dados_obesidade |>
  summarise(
    max_peso = max(PESO_FINAL, na.rm = TRUE),
    min_peso = min(PESO_FINAL, na.rm = TRUE),
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
    estrato_pof       = ESTRATO_POF,
    zona              = TIPO_SITUACAO_REG,
    cod_upa           = COD_UPA,
    num_dom           = NUM_DOM,
    num_uc            = NUM_UC,
    cod_informante    = COD_INFORMANTE,
    grau_parentesco   = V0306,
    idade             = V0403,
    sexo              = V0404,
    cor               = V0405,
    sabe_ler_escrever = V0414,
    freq_escola       = V0415,
    curso_freq        = V0419,
    instrucao         = NIVEL_INSTRUCAO,
    anos_estudo       = ANOS_ESTUDO,
    peso              = PESO,
    peso_final        = PESO_FINAL,
    renda_total       = RENDA_TOTAL,
    renda_monet_pc    = RENDA_MONET_PC,
    renda_nao_monet_pc = RENDA_NAO_MONET_PC,
    renda_disp_pc     = RENDA_DISP_PC,
    deducao_pc        = DEDUCAO_PC
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
      str_pad(estrato_pof, 4, pad = "0"),   # 3–6: Estrato
      str_pad(zona, 1, pad = "0"),          # 7: Tipo de situação (urb/rural)
      str_pad(cod_upa, 9, pad = "0"),       # 8–16: UPA
      str_pad(num_dom, 2, pad = "0"),       # 17–18: domicílio
      str_pad(num_uc, 1, pad = "0")         # 19: unidade de consumo
    )
  ) |>
  mutate(
    chave_pessoa = paste0(
      str_pad(uf, 2, pad = "0"),               # 1–2: UF
      str_pad(estrato_pof, 4, pad = "0"),      # 3–6: Estrato
      str_pad(zona, 1, pad = "0"),             # 7: Tipo de situação (urb/rural)
      str_pad(cod_upa, 9, pad = "0"),          # 8–16: UPA
      str_pad(num_dom, 2, pad = "0"),          # 17–18: domicílio
      str_pad(num_uc, 1, pad = "0"),            # 19: unidade de consumo
      str_pad(cod_informante, 2, pad = "0")
    )
  )

# Verificar formação da chave:
chave_verificacao <- dados_obesidade |>
  select(uf, estrato_pof, zona, cod_upa, num_dom, num_uc, cod_informante, chave_pessoa)

# Conferir se é única:
dados_obesidade |> 
  count(chave_uc) |>
  filter(n > 1)

# chave_uc: várias linhas por família (normal no MORADOR)
dados_obesidade |> count(chave_uc) |> arrange(desc(n)) |> head()

# chave_pessoa: deve ser única por morador
dados_obesidade |> count(chave_pessoa) |> filter(n > 1)  # idealmente, 0 linhas
# Nenhuma chave está repetida ou duplicada

# Nenhum NA em chaves
colSums(is.na(dados_obesidade[, c("chave_uc", "chave_pessoa")]))

# col_logical - variáveis binárias
# col_integer - códigos categóricos
# col_double - valores contínuos
# col_character - string
# col_factor - fator
# col_guess - o readr infere o tipo

# ------------------------ Checagens rápidas ---------------------------------
glimpse(dados_obesidade)
head(dados_obesidade)
# table(dados_obesidade$num_uc, useNA = "ifany")
# table(dados_obesidade$grau_parentesco, useNA = "ifany")


### ------------------------ OVERVIEW E ANÁLISE ---------------------------- ###

# Verificação dos dados da base
str(dados_obesidade)
unique(dados_obesidade$uf)
unique(dados_obesidade$zona)
unique(dados_obesidade$num_dom)
unique(dados_obesidade$grau_parentesco)
unique(dados_obesidade$idade)
unique(dados_obesidade$sexo)
unique(dados_obesidade$cor)
unique(dados_obesidade$num_uc)
unique(dados_obesidade$sabe_ler_escrever)
unique(dados_obesidade$freq_escola)
unique(dados_obesidade$curso_freq)

#------------------------------------------------------------------------------#
### ----------------------- TRATAMENTO DOS DADOS --------------------------- ###
#------------------------------------------------------------------------------#

# Excluir domicílios com mais de uma unidade familiar
length(dados_obesidade$num_uc)

dados_obesidade_trat <- dados_obesidade |>
  filter(num_uc == 1)

unique(dados_obesidade_trat$num_uc)

# Verificar a quantidade de famílias
unique(dados_obesidade_trat$num_uc)
length(dados_obesidade_trat$num_uc)

# diferença de famílias
diff_familly_antes <- length(dados_obesidade$num_uc); diff_familly_antes
diff_familly_depois <- length(dados_obesidade_trat$num_uc); diff_familly_depois
dif_famlly <- (diff_familly_antes) - (diff_familly_depois); dif_famlly

uc_antes  <- dados_obesidade      %>% distinct(chave_uc) %>% nrow(); uc_antes
uc_depois <- dados_obesidade_trat %>% distinct(chave_uc) %>% nrow(); uc_depois
dif_uc    <- uc_antes - uc_depois; dif_uc

# Manter os graus de parentescos relevantes para pesquisa
dados_obesidade_trat <- dados_obesidade_trat |>
  filter(grau_parentesco %in% c(1,2,3,4,5,6))

unique(dados_obesidade_trat$grau_parentesco) # Verificar
table(dados_obesidade_trat$grau_parentesco)

# Criar variável de branco e não branco
dados_obesidade_trat <- dados_obesidade_trat |>
  mutate(
    cor = case_when(
      cor == 1 ~ 1L,
      cor %in% c(2, 3, 4, 5, 9) ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# Verificar valores de cor/raça
unique(dados_obesidade_trat$cor)

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
  summarise(
    n_pais = sum(sexo == 1 & grau_parentesco %in% c(1,2,3)),
    n_maes = sum(sexo == 2 & grau_parentesco %in% c(1,2,3)),
    .by = chave_uc
  ) |>
  mutate(
    estrutura_fam = case_when(
      (n_pais + n_maes) >= 2 ~ "biparental",
      n_maes == 1 & n_pais == 0 ~ "mono_mulher",
      n_pais == 1 & n_maes == 0 ~ "mono_homem",
      TRUE ~ NA_character_
    )
  )

table(fam_flags$estrutura_fam)
table(fam_flags$n_pais)
table(fam_flags$n_maes)
# Total de Famílias
# 57.920
  # Gays - 
  # Lésbicos - 

# Criar variável de região
dados_obesidade_trat <- dados_obesidade_trat |>
  mutate(
    regiao = case_when(
      uf %in% c(11:17) ~ "norte",
      uf %in% c(21:29) ~ "nordeste",
      uf %in% c(31:35) ~ "sudeste",
      uf %in% c(41:43) ~ "sul",
      uf %in% c(50:53) ~ "centro_oeste"
    )
  )

# Verificar os dados de região
unique(dados_obesidade_trat$regiao)

# Dummy de região com base no sudeste
dados_obesidade_trat <- dados_obesidade_trat |>
  mutate(
    reg_norte = as.integer(regiao=="norte"),
    reg_nordeste    = as.integer(regiao=="nordeste"),
    reg_sul   = as.integer(regiao=="sul"),
    reg_centro_oeste    = as.integer(regiao=="centro_oeste")
  )

table(dados_obesidade_trat$regiao)

unique(dados_obesidade_trat$reg_norte)
unique(dados_obesidade_trat$reg_nordeste)
unique(dados_obesidade_trat$reg_sul)
unique(dados_obesidade_trat$reg_centro_oeste)

table(dados_obesidade_trat$reg_norte)
table(dados_obesidade_trat$reg_nordeste)
table(dados_obesidade_trat$reg_sul)
table(dados_obesidade_trat$reg_centro_oeste)

# Contar número de moradores por Unidade de Consumo - Universo de moradores
# Usar base não trata para considerar todo mundo
moradores_uc <- dados_obesidade |>
  filter(num_uc == 1) |>
  count(chave_uc, name = "n_moradores")

table(moradores_uc$n_moradores)

# Dummies da escola (controles X)
dados_obesidade_trat <- dados_obesidade_trat |>
  mutate(
    freq_escola = as.integer(freq_escola == 1)
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
  # arrange(chave_uc, desc(idade)) |>
  # distinct(chave_uc, .keep_all = TRUE) |>
  transmute(
    chave_uc,
    chave_pessoa_resp = chave_pessoa,
    idade_resp = idade,
    idade_resp2 = idade^2,              # <- idade^2
    sexo_resp  = sexo,
    cor_resp   = cor,
    instrucao_resp = instrucao,
    anos_estudo_resp = anos_estudo,
    chave_pessoa_resp = chave_pessoa,
    freq_escola_resp = freq_escola,
    sabe_ler_escrever_resp = sabe_ler_escrever,
    
    zona_resp = zona,
    estrato_pof_resp = estrato_pof,
    cod_upa_resp = cod_upa,
    peso_final_resp = peso_final
    
  )

# Verificar se tem mais de um cônjuge
base_conj <- dados_obesidade_trat |>
  filter(grau_parentesco %in% c(2,3)) |>
  count(chave_uc, name = "n_conjuges") |>
  filter(n_conjuges > 1)

nrow(base_conj); head(base_conj)

# 2.2 Base do cônjuge
base_conj <- dados_obesidade_trat |>
  filter(grau_parentesco %in% c(2,3)) |>
  # arrange(chave_uc, desc(idade)) %>%
  distinct(chave_uc, .keep_all = TRUE) %>%
  transmute(
    chave_uc,
    chave_pessoa_conj = chave_pessoa,
    idade_conj = idade,
    idade_conj2 = idade^2,              # <- idade^2
    sexo_conj  = sexo,
    cor_conj   = cor,
    instrucao_conj = instrucao,
    anos_estudo_conj = anos_estudo,
    chave_pessoa_conj = chave_pessoa,
    freq_escola_conj = freq_escola,
    sabe_ler_escrever_conj = sabe_ler_escrever,
    
    zona_conj = zona,
    estrato_pof_conj = estrato_pof,
    cod_upa_conj = cod_upa,
    peso_final_conj = peso_final
    
  )
nrow(base_conj); head(base_conj)

# 2.3 Base dos filhos
base_filho <- dados_obesidade_trat |>
  filter(grau_parentesco %in% c(4,5,6), idade >=2, idade <=20) |>
  arrange(chave_uc) |>
  mutate(id_filho_uc = row_number(), .by = chave_uc) |>
  transmute(
    chave_uc,
    id_filho_uc,
    idade_filho = idade,
    idade_meses_filho = as.integer(idade * 12L),  # <- para tabela OMS/CDC
    sexo_filho  = sexo,
    cor_filho   = cor,
    instrucao_filho = instrucao,
    chave_pessoa_filho = chave_pessoa,
    freq_escola_filho = freq_escola,
    sabe_ler_escrever_filho = sabe_ler_escrever,
    peso_final_filho  = peso_final
  )

uc_info <- dados_obesidade_trat |>
  distinct(chave_uc, regiao, reg_norte, reg_nordeste, reg_sul, reg_centro_oeste,
                  zona, renda_total, renda_disp_pc, renda_monet_pc, 
                  renda_nao_monet_pc)

# Criar base consolidada “filho + pais + controles”
base_analitica <- base_filho |>
  left_join(base_resp, by = "chave_uc")

# base_analitica2 <- base_filho |>
#  inner_join(base_resp, by = "chave_uc")

base_analitica <- base_analitica |>
  left_join(base_conj, by = "chave_uc")

base_analitica <- base_analitica |>
  left_join(fam_flags, by = "chave_uc")

base_analitica <- base_analitica |>
  left_join(moradores_uc, by = "chave_uc")

base_analitica <- base_analitica |>
  left_join(uc_info |> select(chave_uc, regiao, zona, renda_total, renda_disp_pc, renda_monet_pc, renda_nao_monet_pc), by = "chave_uc")

# Verificar número de moradores e moradores
moradores_nmoradores <- base_analitica |>
  select(chave_uc, n_moradores, chave_pessoa_resp, chave_pessoa_conj, chave_pessoa_filho,
         estrutura_fam); moradores_nmoradores






sum(is.na(base_analitica$chave_pessoa_filho))
sum(is.na(base_analitica$chave_pessoa_resp))

base_na_conj <- sum(is.na(base_analitica$chave_pessoa_conj))
base_conj <- length(base_analitica$chave_pessoa_conj)

x = base_conj - base_na_conj




##----------------------------------------------------------------------------##
###             Puxar variáveis de Características da dieta                  ###
##----------------------------------------------------------------------------##

# ------------------------ Leitura com fwf_cols 

caract_dieta <- read_fwf(
  file = caract_dieta_base,
  col_positions = fwf_cols(
    UF                = c(1, 2),
    ESTRATO_POF       = c(3, 6),       # 3-4 
    TIPO_SITUACAO_REG = 7,             # largura 1
    COD_UPA           = c(8, 16),      # 9 posições (8..16)
    NUM_DOM           = c(17, 18),
    NUM_UC            = 19,            # largura 1
    COD_INFORMANTE    = c(20, 21),     # Código do informante
    V7104  = 32,
    V71051 = 33, V71052 = 34, V71053 = 35, V71054 = 36, V71055 = 37, V71056 = 38,
    
    # Auto-relato de peso/altura:
    V72C01 = c(41, 43),  # peso (kg), 3 posições, 20–150
    V72C02 = c(44, 46),  # altura (cm), 3 posições, 115–204
    
    # Pesos e renda do registro:
    PESO        = c(47, 60),  # 14 posições, 8 decimais
    PESO_FINAL  = c(61, 75),  # 15 posições, 8 decimais
    RENDA_TOTAL = c(76, 85)   # 10 posições, 2 decimais
    
    ),
    # Tipagem explícita (ajuste se necessário)
  col_types = readr::cols(
    UF                 = readr::col_integer(),
    ESTRATO_POF        = readr::col_character(),
    TIPO_SITUACAO_REG  = readr::col_integer(),
    COD_UPA            = readr::col_character(),
    NUM_DOM            = readr::col_character(),
    NUM_UC             = readr::col_integer(),
    COD_INFORMANTE     = readr::col_character(),
    
    V7104  = readr::col_integer(),
    V71051 = readr::col_integer(), 
    V71052 = readr::col_integer(),
    V71053 = readr::col_integer(), 
    V71054 = readr::col_integer(),
    V71055 = readr::col_integer(), 
    V71056 = readr::col_integer(),

    V72C01 = readr::col_double(),
    V72C02 = readr::col_double(),
    
    PESO        = readr::col_double(),
    PESO_FINAL  = readr::col_double(),
    RENDA_TOTAL = readr::col_double()
  ),
  locale = readr::locale(encoding = "Latin1"),
  na = c("", " ", "NA"),
  show_col_types = FALSE
) |>
  dplyr::mutate(
    # Ajuste dos decimais implícitos
    PESO        = PESO       / 1e8,
    PESO_FINAL  = PESO_FINAL / 1e8,
    RENDA_TOTAL = RENDA_TOTAL / 1e2
  )

# Renomear variáveis
caract_dieta <- caract_dieta |>
  dplyr::rename(
    uf                 = UF,
    estrato_pof        = ESTRATO_POF,
    zona               = TIPO_SITUACAO_REG,
    cod_upa            = COD_UPA,
    num_dom            = NUM_DOM,
    num_uc             = NUM_UC,
    cod_informante     = COD_INFORMANTE,
    
    faz_dieta          = V7104,
    dieta_emagrecer    = V71051,
    dieta_pressao_alta = V71052,
    dieta_colesterol   = V71053,
    dieta_diabetes     = V71054,
    dieta_coracao      = V71055,
    dieta_outra        = V71056,
    
    peso_autorref_kg   = V72C01,   # auto-relato (kg)
    altura_autorref_cm = V72C02,   # auto-relato (cm)
    
    peso_dieta         = PESO,         # peso amostral deste registro
    peso_final_dieta   = PESO_FINAL,   # peso calibrado deste registro
    renda_total_uc     = RENDA_TOTAL   # renda da UC neste registro
  )

caract_dieta <- caract_dieta |>
  mutate(
    # ---------------------------#
    # CHAVE PARA UNIDADE DE CONSUMO
    # ---------------------------#
    chave_uc = paste0(
      stringr::str_pad(uf, 2, pad = "0"),          # 1–2: UF
      stringr::str_pad(estrato_pof, 4, pad = "0"), # 3–6: Estrato POF
      stringr::str_pad(zona, 1, pad = "0"),        # 7: Tipo de situação (1=Urbano, 2=Rural)
      stringr::str_pad(cod_upa, 9, pad = "0"),     # 8–16: UPA
      stringr::str_pad(num_dom, 2, pad = "0"),     # 17–18: Domicílio
      stringr::str_pad(num_uc, 1, pad = "0")       # 19: Unidade de Consumo
    ),
    
    # ---------------------------#
    # CHAVE PARA PESSOA
    # ---------------------------#
    chave_pessoa = paste0(
      stringr::str_pad(uf, 2, pad = "0"),
      stringr::str_pad(estrato_pof, 4, pad = "0"),
      stringr::str_pad(zona, 1, pad = "0"),
      stringr::str_pad(cod_upa, 9, pad = "0"),
      stringr::str_pad(num_dom, 2, pad = "0"),
      stringr::str_pad(num_uc, 1, pad = "0"),
      stringr::str_pad(cod_informante, 2, pad = "0") # 20–21: Informante
    )
  )

# Verificar formação da chave de pessoa
verificar_chave2 <- caract_dieta |>
  select(uf, estrato_pof, zona, cod_upa, num_dom, num_uc, cod_informante, chave_pessoa)

# Ver quantidade de observações
count(caract_dieta)

# Verificar a quantidade de NA
colSums(is.na(caract_dieta))

# Verificação dos dados da base
str(caract_dieta)
unique(caract_dieta$uf)
unique(caract_dieta$zona)
unique(caract_dieta$num_dom)
unique(caract_dieta$faz_dieta)
unique(caract_dieta$peso_autorref_kg)
unique(caract_dieta$altura_autorref_cm)
unique(caract_dieta$dieta_coracao)
unique(caract_dieta$dieta_diabetes)

# Cada pessoa deve ter chave única:
caract_dieta |> 
  count(chave_pessoa) |>
  filter(n > 1)
  # Esperado: nenhuma linha (0 duplicidades)

# Cada UC pode ter várias pessoas:
caract_dieta |> 
  count(chave_uc) |> 
  summarise(min = min(n), max = max(n))

# Verificar estatística descritiva da altura e massa
summary(caract_dieta$peso_autorref_kg)
summary(caract_dieta$altura_autorref_cm)

# Verificar quantidade de NA
sum(is.na(caract_dieta$peso_autorref_kg))
sum(is.na(caract_dieta$altura_autorref_cm))

# Calcular IMC das observações
caract_dieta <- caract_dieta |>
  mutate(
    altura_m = altura_autorref_cm / 100,
    imc = peso_autorref_kg / (altura_m^2)
  )

summary(caract_dieta$imc)
sum(is.na(caract_dieta$imc))
count(caract_dieta)

# Calcular IMC
#caract_dieta <- caract_dieta |>
#  mutate(
    # Converter altura para metros
#    altura_autorref_m = altura_autorref_cm / 100,
#    
    # Calcular IMC
#    imc_autorref = if_else(
#      is.finite(peso_autorref_kg) & is.finite(altura_autorref_m) & altura_autorref_m > 0,
      # Fórmula do IMC
#      peso_autorref_kg / (altura_autorref_m^2),
#      NA_real_
#    ),
#    
    # Aplicar filtros de plausibilidade (valores típicos da literatura)
#    imc_autorref = if_else(imc_autorref < 10 | imc_autorref > 80, NA_real_, imc_autorref)
#  )

# Visualizar graficamente o IMC geral
ggplot(caract_dieta, aes(x = imc)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 40, fill = "blue", color = "white", alpha = 0.7) +
  geom_density(color = "darkred", linewidth = 1.1) +
  labs(
    title = "Distribuição do IMC da POF 2017-2018)",
    x = "IMC (kg/m²)",
    y = "Densidade"
  ) +
  theme_minimal(base_size = 12)

# Filho

# Selecionar apenas as variáveis necessárias da base caract_dieta
dieta_filho <- caract_dieta |>
  select(
    chave_pessoa_filho1 = chave_pessoa,
    peso_filho   = peso_autorref_kg,
    altura_filho = altura_m,
    imc_filho    = imc
  )

# Unir base de dados
base_analitica_unida <- base_analitica |>
  left_join(dieta_filho, by = c("chave_pessoa_filho" = "chave_pessoa_filho1"))

base_analitica_unida_filho <- base_analitica |>
  inner_join(dieta_filho, by = c("chave_pessoa_filho" = "chave_pessoa_filho1"))

# Verificar a jução das bases
verificar_filho <- base_analitica_unida_filho |>
  select(chave_uc, chave_pessoa_filho, 
         peso_filho, altura_filho, imc_filho)

sum(is.na(base_analitica_unida$imc_filho))

# Responsável
# Selecionar apenas as variáveis necessárias da base caract_dieta
dieta_resp <- caract_dieta |>
  select(
    chave_pessoa_resp = chave_pessoa,
    peso_resp   = peso_autorref_kg,
    altura_resp = altura_m,
    imc_resp    = imc
  )

base_analitica_unida <- base_analitica_unida |>
  left_join(dieta_resp, by = c("chave_pessoa_resp" = "chave_pessoa_resp"))

base_resppp <- base_analitica |>
  inner_join(dieta_resp, by = c("chave_pessoa_resp" = "chave_pessoa_resp"))

verificar_resp <- base_analitica_unida |>
  select(chave_pessoa_resp, peso_resp, altura_resp, imc_resp); verificar_resp
sum(is.na(base_analitica_unida$imc_resp))

# Cônjuge
dieta_conj <- caract_dieta |>
  select(
    chave_pessoa_conj = chave_pessoa,
    peso_conj   = peso_autorref_kg,
    altura_conj = altura_m,
    imc_conj    = imc
  )

base_analitica_unida <- base_analitica_unida |>
  left_join(dieta_conj, by = c("chave_pessoa_conj" = "chave_pessoa_conj"))

base_conjjj <- base_analitica_unida |>
  inner_join(dieta_conj, by = c("chave_pessoa_conj" = "chave_pessoa_conj"))


sum(is.na(base_analitica_unida$imc_conj))

# Verificar variáveis de filho
base_vericacao <- base_analitica_unida |>
  select(chave_uc, imc_resp, imc_conj, imc_filho)

{
# Juntar bases de dados
#base_analitica_unida <- base_analitica_trat |>
#  left_join(dieta_slim, by = c("chave_pessoa_filho" = "chave_pessoa")) |>
#  rename(peso_filho = peso_autorref_kg, altura_filho = altura_autorref_m, imc_filho = imc_autorref) |>
#  left_join(dieta_slim, by = c("chave_pessoa_resp" = "chave_pessoa")) |>
#  rename(peso_resp_kg_auto = peso_autorref_kg, altura_resp_m_auto = altura_autorref_m, imc_resp = imc_autorref) |>
#  left_join(dieta_slim, by = c("chave_pessoa_conj" = "chave_pessoa")) |>
#  rename(peso_conj_kg_auto = peso_autorref_kg, altura_conj_m_auto = altura_autorref_m, imc_conj = imc_autorref)

#base_analitica_unida <- base_analitica_trat |>
#  left_join(dieta_filho, by = c("chave_pessoa_filho" = "chave_pessoa_filho")) |>
#  left_join(dieta_resp, by = c("chave_pessoa_resp" = "chave_pessoa_resp")) |>
#  left_join(dieta_conj, by = c("chave_pessoa_conj" = "chave_pessoa_conj"))
  }

# Remover onde ambos os pais são nulo e onde o filho também é nulo
base_limpa <- base_analitica_unida %>%
  filter(!(imc_resp == 0 & imc_conj == 0)) |>
  filter(!(imc_filho ==0))

# Verificar resultado
base_limpa_verificar <- base_limpa |>
  select(chave_uc, imc_resp, imc_conj, imc_filho)

# Checagem
summary(select(base_analitica_unida, imc_filho, imc_resp, imc_conj))

# Criar imc do pai e da mãe
base_analitica <- base_analitica_unida |>
  mutate(
    imc_pai  = dplyr::case_when(sexo_resp==1 ~ imc_resp,  sexo_conj==1 ~ imc_conj,  TRUE ~ NA_real_),
    imc_mae  = dplyr::case_when(sexo_resp==2 ~ imc_resp,  sexo_conj==2 ~ imc_conj,  TRUE ~ NA_real_)
  )

sum(is.na(base_analitica_trat))


# ---------------------------------------------------------------------------- #
##                                 MODELO OLS                                 ##
# ---------------------------------------------------------------------------- #

base_modelo <- base_analitica_trat |>
  filter(estrutura_fam == "biparental",
         !is.na(imc_filho), !is.na(imc_pai), !is.na(imc_mae))

table(base_modelo$reg_norte)
table(base_modelo$reg_nordeste)
table(base_modelo$reg_sul)
table(base_modelo$reg_centro_oeste)


  
  
  
  
  
  










  
  
  
  
  ################################################################################
  ####                                                                        ####
  ####              BAIXAR DADOS DA POFF DE 2008/2007                         ####
  ####                                                                        ####
  ################################################################################
  
  getwd()
  setwd("C:/Users/vitor/OneDrive/Área de Trabalho/UFPB/PIBIC_2025/Base de Dados/POFF_Dados_2008_2009")
  getwd()
  
  arquivo_2008 <- "T_MORADOR_S.txt"
  dir()
  
  # ------------------------ Leitura com fwf_cols ------------------------------
  
  dados_obesidade <- read_fwf(
    file = arquivo_2008,
    col_positions = fwf_cols(
      COD_UF            = c(3, 4),
      ESTRATO_POF       = c(14, 15),     # 3-4 
      TIPO_REG          = c(1, 2),       # largura 1
      COD_UPA           = c(247, 252),     # 9 posições (8..16)
      NUM_DOM           = c(9, 10),
      NUM_UC            = 11,            # largura 1
      COD_INFORMANTE    = c(12, 13),     # Código do informante
      COND_UNIDADE_CONSUMO = c(44, 45),   # condição na Unidade de Consumo (grau de parentesco)
      NUM_FAMILIA       = c(46, 47),     # Número de famílias no domicílio
      COND_FAMILIA      = c(48, 49),     #  Condição na família. Identifica o grau de parentesco ou a natureza da subordinação existente entre o morador e a pessoa de referência da sua família.
      IDADE_ANOS        = c(60, 62),     # idade
      V0405             = c(76, 77),     # sexo
      V0429             = c(98, 99),     # cor/raça
      V0418             = c(78, 79),     # sabe ler e escrever
      V0419             = c(80, 81),     # frequenta escola
      V0420             = c(82, 83),     # curso
      ANOS_ESTUDO       = c(96, 97), 
      NIVEL_INSTRUCAO_MORADOR   = 254,
      NIVEL_INSTRUCAO_PESS_REF   = 255,
      RENDA_TOTAL       = c(144, 159),
      RENDA_MONETARIA    = c(112, 127),
      RENDA_NAO_MONETARIA = c(128, 143)
    ),
    # Tipagem explícita (ajuste se necessário)
    col_types = cols(
      COD_UF               = col_integer(),
      ESTRATO_POF          = col_character(),
      TIPO_REG             = col_integer(),
      COD_UPA              = col_character(),
      NUM_DOM              = col_character(),
      NUM_UC               = col_integer(),
      COD_INFORMANTE       = col_character(),
      COND_UNIDADE_CONSUMO = col_integer(),
      NUM_FAMILIA          = col_integer(),
      COND_FAMILIA         = col_integer(),
      IDADE_ANOS           = col_integer(),
      V0405                = col_integer(),
      V0429                = col_integer(),
      V0418                = col_integer(),
      V0419                = col_integer(),
      V0420                = col_integer(),
      ANOS_ESTUDO          = col_integer(),
      NIVEL_INSTRUCAO_MORADOR  = col_integer(),
      NIVEL_INSTRUCAO_PESS_REF = col_integer(),
      RENDA_TOTAL              = col_integer(),
      RENDA_MONETARIA          = col_integer(),
      RENDA_NAO_MONETARIA      = col_integer()
    ),
    locale = locale(encoding = "Latin1"),
    na = c("", " ", "NA"),
    show_col_types = FALSE
  )
  
  # 1) Renomeiar
  dados_obesidade <- dados_obesidade |>
    rename(
      uf                = COD_UF,
      estrato_pof       = ESTRATO_POF,
      zona              = TIPO_REG,
      cod_upa           = COD_UPA,
      num_dom           = NUM_DOM,
      num_uc            = NUM_UC,
      cod_informante    = COD_INFORMANTE,
      num_familia       = NUM_FAMILIA,
      cond_familia      = COND_FAMILIA,
      idade_anos        = IDADE_ANOS,
      sexo              = V0405,
      cor               = V0429,
      freq_escola       = V0419,
      curso_freq        = V0420,
      anos_estudo       = ANOS_ESTUDO,
      nivel_instrucao_morador = NIVEL_INSTRUCAO_MORADOR,
      nivel_instrucao_pess_ref = NIVEL_INSTRUCAO_PESS_REF,
      renda_total = RENDA_TOTAL,
      renda_monetaria = RENDA_MONETARIA,
      renda_nao_monetaria = RENDA_NAO_MONETARIA
    )
  
unique(dados_obesidade$uf)
unique(dados_obesidade$cond_familia)
unique(dados_obesidade$sexo)
unique(dados_obesidade$cor)
unique(dados_obesidade$idade_anos)
unique(dados_obesidade$num_familia)
unique(dados_obesidade$grau)

  
  
  
  
  