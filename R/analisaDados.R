library(tidyverse)

load("../data/robotox.RData")

df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classe I - Extremamente Tóxico"] <- "Classe I - Extremamente Tóxico"                                  
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe I - Extremamente Tóxico."] <- "Classe I - Extremamente Tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe II - Altamente tóxico."] <- "Classe II - Altamente tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe II - Altamente tóxico."] <- "Classe II - Altamente tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe II - Altamente Tóxico."] <- "Classe II - Altamente tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe II- Produto Altamente Tóxico."] <- "Classe II - Altamente tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação: Classe II - Altamente Tóxico"] <- "Classe II - Altamente tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe III"] <- "Classe III - Moderadamente Tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe III - Extremamente Tóxico."] <- "Classe III - Moderadamente Tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe III - Medianamente Tóxico"] <- "Classe III - Moderadamente Tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe III - Medianamente Toxico."] <- "Classe III - Moderadamente Tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe III - Medianamente Tóxico."] <- "Classe III - Moderadamente Tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe III - Mediante Tóxico."] <- "Classe III - Moderadamente Tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe IV - Pouco Tóxico."] <-  "Classe IV - Pouco Tóxico"
df_join_agro_cultura$Classificacao_toxicologica[df_join_agro_cultura$Classificacao_toxicologica == "Classificação toxicológica: Classe IV - Produto Pouco Tóxico."] <-  "Classe IV - Pouco Tóxico"   

     
       
       

  
                          
   
    
   
   
       



p1 <- df_join_agro_cultura %>% 
  mutate(Aplicacao = toupper(Aplicacao)) %>%
  mutate(Aplicacao = str_replace_all(Aplicacao, "[.]",",")) %>%
  mutate(Aplicacao = strsplit(as.character(Aplicacao), ",")) %>% 
  unnest(Aplicacao) %>%
  mutate(Aplicacao = str_trim(Aplicacao)) %>%
  mutate(Aplicacao = as.factor(Aplicacao)) %>%
  mutate(Classificacao_toxicologica = toupper(Classificacao_toxicologica)) %>%
  mutate(Classificacao_toxicologica = str_replace_all(Classificacao_toxicologica, "CLASSIFICAÇÃO TOXICOLÓGICA:","")) %>%
  mutate(Classificacao_toxicologica = str_replace_all(Classificacao_toxicologica, "CLASSIFICAÇÃO:","")) %>%
  mutate(Classificacao_toxicologica = str_replace_all(Classificacao_toxicologica, "[.]","")) %>%
  mutate(Classificacao_toxicologica = str_trim(Classificacao_toxicologica)) %>%
  mutate(Classificacao_toxicologica = as.factor(Classificacao_toxicologica)) %>%
  group_by(Aplicacao) %>%
  mutate(total = n()) %>%
  ungroup()%>%
  group_by(Aplicacao, Classificacao_toxicologica, total)%>%
  summarise(parcial = n()) %>%
  arrange(total,parcial) %>%
  tail(150) %>%
  ggplot(aes(x = reorder(Aplicacao, total), y = parcial, fill = Classificacao_toxicologica)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  coord_flip() + 
  labs(x = "Número de produtos", 
       y = "Aplicaçãoo dos produtos", 
       title = "Níveis de Toxicidade dos Produtos vs. Aplicação",
       fill = "Toxicidade")

png("../figures/toxicidade_por_produto.png",width=3200,height=1800,res=300)
print(p1)
dev.off()


