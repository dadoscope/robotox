library(RISmed)
library(tidyverse)
library(topicmodels)
library(tokenizers)
library(tidytext)
library(wordcloud)

res1 <- EUtilsSummary("novaluron + pesticides", 
                      type = "esearch", 
                      db = "pubmed",
                      datetype = "pdat",
                      retmax = 12000,
                      mindate = 2005, 
                      maxdate = 2019)

fetch <- EUtilsGet(res1, type = "efetch", db = "pubmed")

abstracts <- data.frame(title = fetch@ArticleTitle,
                        abstract = fetch@AbstractText, 
                        journal = fetch@Title,
                        DOI = fetch@PMID, 
                        year = fetch@YearPubmed)
## ensure abstracts are character fields (not factors)
abstracts <- abstracts %>% mutate(abstract = as.character(abstract))
abstracts %>%
  head()

abstracts %>%
  group_by(year) %>%
  count() %>%
  filter(year > 2013) %>%
  ggplot(aes(year, n)) +
  geom_point() +
  geom_line() +
  labs(title = "Pubmed articles with search terms `data science` & `population health` \n2015-2016", hjust = 0.5,
       y = "Articles")
cloud <- abstracts %>%
  unnest_tokens(word, abstract) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) 

png("wordcloud.png",width=3200,height=1800,res=300)
cloud %>%
  with(wordcloud(word, n, min.freq = 10, max.words = 1000, colors = brewer.pal(8, "Dark2")), scale = c(8,.3), per.rot = 0.4)
dev.off()
