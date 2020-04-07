

# Calcula estatísticas descritivas, cria indicador de precisao com base no CV e adequa layout para a CNAE Divisão

{
calc_to_cnae_2 <- function(x,grupo){
  var_temp %>% 
    group_by_at(vars(one_of(grupo))) %>%
    summarise(
      media = round(mean(salario_mensal, na.rm = T),2),
      desv_padr = sd(salario_mensal, na.rm = T),
      coef_var = round((desv_padr/media),2),
      maximo = max(salario_mensal),
      minimo = min(salario_mensal),
      nivel_salarial_1 = round(quantile(salario_mensal, probs = 0.25),2),
      nivel_salarial_2 = round(median(salario_mensal),2),
      nivel_salarial_3 = round(quantile(salario_mensal, probs = 0.75),2),
      n_obs=n()) %>% 
    ungroup() %>% 
    rename(
      CNAE = cnae2,
      CBO = cbo_2002_ocup) %>% 
    mutate(
      hierarquia_CNAE = "Divisão",
      hierarquia_CBO = "Ocupação",
      CNAE = as.character(CNAE),
      CBO = as.character(CBO),
      indicador_de_precisao = case_when(coef_var<=0.15 ~ "Precisão Ótima",
                                       coef_var>0.15 & coef_var <=0.50 ~ "Precisão Boa",
                                       coef_var>0.50 & coef_var <=1.50 ~ "Precisão Razoável",
                                       coef_var>1.50 & coef_var <=3.50 ~ "Pouco Precisa",
                                       coef_var>3.50 ~ "Imprecisa",
                                       coef_var==0 ~ "Precisão Exata")
      ) %>% 
        left_join(cnae2_codigos) %>% 
        left_join(cbo_ocup_df)                  
}

# Calcula estatísticas descritivas, cria indicador de precisao com base no CV e adequa layout para a CNAE Grupo
calc_to_cnae_3 <- function(x,grupo){
  var_temp %>% 
    group_by_at(vars(one_of(grupo))) %>%
    summarise(
      media = round(mean(salario_mensal, na.rm = T),2),
      desv_padr = sd(salario_mensal, na.rm = T),
      coef_var = round((desv_padr/media),2),
      maximo = max(salario_mensal),
      minimo = min(salario_mensal),
      nivel_salarial_1 = round(quantile(salario_mensal, probs = 0.25),2),
      nivel_salarial_2 = round(median(salario_mensal),2),
      nivel_salarial_3 = round(quantile(salario_mensal, probs = 0.75),2),
      n_obs=n()) %>% 
    ungroup() %>% 
    rename(
      CNAE = cnae3,
      CBO = cbo_2002_ocup) %>% 
    mutate(
      hierarquia_CNAE = "Grupo",
      hierarquia_CBO = "Ocupação",
      CNAE = as.character(CNAE),
      CBO = as.character(CBO),
      indicador_de_precisao = case_when(coef_var<=0.15 ~ "Precisão Ótima",
                                       coef_var>0.15 & coef_var <=0.50 ~ "Precisão Boa",
                                       coef_var>0.50 & coef_var <=1.50 ~ "Precisão Razoável",
                                       coef_var>1.50 & coef_var <=3.50 ~ "Pouco Precisa",
                                       coef_var>3.50 ~ "Imprecisa",
                                       coef_var==0 ~ "Precisão Exata")
    ) %>% 
    left_join(cnae3_codigos) %>% 
    left_join(cbo_ocup_df)
}
 
# Calcula estatísticas descritivas, cria indicador de precisao com base no CV e adequa layout para a CNAE Classe
calc_to_cnae_5 <- function(x,grupo){
  var_temp %>% 
    group_by_at(vars(one_of(grupo))) %>%
    summarise(
      media = round(mean(salario_mensal, na.rm = T),2),
      desv_padr = sd(salario_mensal, na.rm = T),
      coef_var = round((desv_padr/media),2),
      maximo = max(salario_mensal),
      minimo = min(salario_mensal),
      nivel_salarial_1 = round(quantile(salario_mensal, probs = 0.25),2),
      nivel_salarial_2 = round(median(salario_mensal),2),
      nivel_salarial_3 = round(quantile(salario_mensal, probs = 0.75),2),
      n_obs=n()) %>% 
    ungroup() %>% 
    rename(
      CNAE = cnae5,
      CBO = cbo_2002_ocup) %>% 
    mutate(
      hierarquia_CNAE = "Classe",
      hierarquia_CBO = "Ocupação",
      CNAE = as.character(CNAE),
      CBO = as.character(CBO),
      indicador_de_precisao = case_when(coef_var<=0.15 ~ "Precisão Ótima",
                                       coef_var>0.15 & coef_var <=0.50 ~ "Precisão Boa",
                                       coef_var>0.50 & coef_var <=1.50 ~ "Precisão Razoável",
                                       coef_var>1.50 & coef_var <=3.50 ~ "Pouco Precisa",
                                       coef_var>3.50 ~ "Imprecisa",
                                       coef_var==0 ~ "Precisão Exata")
    ) %>% 
    left_join(cnae5_codigos) %>% 
    left_join(cbo_ocup_df)
}

# Calcula estatísticas descritivas, cria indicador de precisao com base no CV e adequa layout para a CNAE Subclasse
calc_to_cnae_7 <- function(x,grupo){
  var_temp %>% 
    group_by_at(vars(one_of(grupo))) %>%
    summarise(
      media = round(mean(salario_mensal, na.rm = T),2),
      desv_padr = sd(salario_mensal, na.rm = T),
      coef_var = round((desv_padr/media),2),
      maximo = max(salario_mensal),
      minimo = min(salario_mensal),
      nivel_salarial_1 = round(quantile(salario_mensal, probs = 0.25),2),
      nivel_salarial_2 = round(median(salario_mensal),2),
      nivel_salarial_3 = round(quantile(salario_mensal, probs = 0.75),2),
      n_obs=n()) %>% 
    ungroup() %>% 
    rename(
      CNAE = cnae7,
      CBO = cbo_2002_ocup,
      indicador_de_precisao = case_when(coef_var<=0.15 ~ "Precisão Ótima",
                                       coef_var>0.15 & coef_var <=0.50 ~ "Precisão Boa",
                                       coef_var>0.50 & coef_var <=1.50 ~ "Precisão Razoável",
                                       coef_var>1.50 & coef_var <=3.50 ~ "Pouco Precisa",
                                       coef_var>3.50 ~ "Imprecisa",
                                       coef_var==0 ~ "Precisão Exata")
	  ) %>% 
    mutate(hierarquia_CNAE = "Subclasse",
           hierarquia_CBO = "Ocupação",
           CNAE = as.character(CNAE),
           CBO = as.character(CBO)) %>% 
    left_join(cnae7_codigos) %>% 
    left_join(cbo_ocup_df)
}
}
