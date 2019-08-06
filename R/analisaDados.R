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
  filter(cultura !="trata-se de produto técnico")%>%
  mutate(cultura = toupper(cultura)) %>%
  mutate(cultura = str_trim(cultura)) %>%
  group_by(cultura) %>% 
  mutate(total = n()) %>% 
  ungroup() %>%
  mutate(Classificacao_toxicologica = toupper(Classificacao_toxicologica)) %>%
  mutate(Classificacao_toxicologica = str_replace_all(Classificacao_toxicologica, "CLASSIFICAÇÃO TOXICOLÓGICA:","")) %>%
  mutate(Classificacao_toxicologica = str_replace_all(Classificacao_toxicologica, "CLASSIFICAÇÃO:","")) %>%
  mutate(Classificacao_toxicologica = str_replace_all(Classificacao_toxicologica, "[.]","")) %>%
  mutate(Classificacao_toxicologica = str_trim(Classificacao_toxicologica)) %>%
  mutate(Classificacao_toxicologica = as.factor(Classificacao_toxicologica)) %>%
  arrange(total) %>%
  group_by(cultura, Classificacao_toxicologica, total)%>%
  summarise(parcial = n()) %>%
  arrange(total,parcial) %>%
  filter(total > 10) %>%
  ggplot(aes(x = reorder(cultura, total), y = parcial, fill = Classificacao_toxicologica)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  coord_flip() + 
  labs(y = "Número de produtos", 
       x = "Culturas", 
       title = "Níveis de Toxicidade dos Produtos vs. Culturas",
       fill = "Toxicidade")

png("../figures/toxicidade_por_produto.png",width=3200,height=1800,res=300)
print(p1)
dev.off()


produtores <- df_join_agro_cultura %>% 
  mutate(Produtor=str_replace_all(Produtor,"S.A.","SA.")) %>%
  mutate(Produtor=str_replace_all(Produtor,"Co.","Co")) %>%
  mutate(Produtor=str_replace_all(Produtor,"S. A.","SA.")) %>%
  mutate(Produtor=str_replace_all(Produtor,"Adama SA.","Adama Brasil SA.")) %>%
  mutate(Produtor=str_replace_all(Produtor,"Agro Import","Agroimport")) %>%
  mutate(Produtor=str_replace_all(Produtor,"Biorisk Assessoria","Biorisk - Assessoria")) %>%
  mutate(Produtor = toupper(Produtor)) %>%
  select(Produtor) %>%
  group_by(Produtor) %>% summarise(total = n())
unlist(strsplit(df_join_agro_cultura$Produtor,"[.]"))


p1<-df_join_agro_cultura %>%  
  filter(cultura =="trata-se de produto técnico") %>% 
  mutate(splits = strsplit(Marca, " ")) %>%
  rowwise() %>% 
  mutate(
    agrotoxins = splits[1]
  ) %>%
  filter(!is.na(agrotoxins)) %>%
  mutate(agrotoxins = toupper(agrotoxins))%>%
  group_by(agrotoxins,Classificacao_toxicologica) %>% 
  summarise(total = n())%>%
  arrange(total)%>%
  tail(30)%>%
  ggplot(aes(x = reorder(agrotoxins, total), y = total, fill=Classificacao_toxicologica)) + 
  geom_bar(stat="identity") + 
  coord_flip()+
  labs(y = "Número de produtos", 
       x = "Agrotóxicos", 
       title = "Agrotóxicos",
       fill = "Toxicidade")
png("agrotoxicos_substancias.png",width=3200,height=1800,res=300)
print(p1)
dev.off()
