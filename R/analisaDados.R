library(tidyverse)

load("../data/robotox.RData")

p1 <- df_join_agro_cultura %>% 
  mutate(Aplicacao = str_replace_all(Aplicacao, "[.]",",")) %>%
  mutate(Aplicacao = toupper(Aplicacao)) %>%
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
  tail(100) %>%
  ggplot(aes(x = reorder(Aplicacao, total), y = parcial, fill = Classificacao_toxicologica)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  coord_flip() + 
  labs(x = "Número de produtos", 
       y = "Aplicaçãoo dos produtos", 
       title = "Níveis de Toxicidade dos Produtos vs. Aplicação",
       fill = "Toxicidade")

png("toxicidade_por_produto.png",width=3200,height=1800,res=300)
print(p1)
dev.off()
