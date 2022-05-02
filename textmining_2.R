# ÅØ½ºÆ®¸¶ÀÌ´× 2Â÷½Ã ----

# ÀÚ¹Ù rJAVAÆÐÅ°Áö ¼³Ä¡ ----
install.packages("multilinguer")
library(multilinguer)

install_jdk()

# KoNLP ÆÐÅ°Áö ¼³Ä¡ ----

install.packages(c("stringr","hash","tau","Sejong","RSQLite","devtools"),
                 type = "binary")

install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP", upgrade = "never", 
                        INSTALL_opts = c("--no-multiarch"))
library(KoNLP)


#ÇüÅÂ¼Ò »çÀü¼³Á¤

useNIADic()


# ÇüÅÂ¼Ò ºÐ¼®±â¸¦ ÀÌ¿ëÇÑ ÅäÅ«È­ ----
library(dplyr)
text <- tibble(value = c("´ëÇÑ¹Î±¹Àº ¹ÎÁÖ°øÈ­±¹ÀÌ´Ù.","´ëÇÑ¹Î±¹ÀÇ ÁÖ±ÇÀº ±¹¹Î¿¡°Ô ÀÖ°í, ¸ðµç ±Ç·ÂÀº ±¹¹ÎÀ¸·ÎºÎÅÍ ³ª¿Â´Ù"))
text

extractNoun(text$value)

library(tidytext)

text %>% unnest_tokens(input = value, output = word, token = extractNoun) #¸í»ç±âÁØ
text %>% unnest_tokens(input = value, output = word, token = "words")     #¶ç¾î¾²±â ±âÁØ

# ¿¬¼³¹® ½Ç½À ----

#¸í»ç ÃßÃâÇÏ±â
raw = readLines("speech_moon.txt", encoding = "UTF-8")
library(stringr)
moon <- raw %>% str_replace_all("[^°¡-ÆR]", " ") %>% str_squish() %>% as_tibble()
moon

word_noun <- moon %>% unnest_tokens(input = value, output = word, token = extractNoun)
word_noun

#ºóµµ ±¸ÇÏ±â
word_noun <- word_noun %>% count(word, sort = T) %>% filter(str_count(word) > 1)
word_noun

#»óÀ§ 20°³ ÃßÃâ
top20 <- word_noun %>% head(20)
top20

#¸·´ë±×·¡ÇÁ ¸¸µé±â
library(ggplot2)
library(showtext)
font_add_google(name = "Gamja Flower", family = "gf")
showtext_auto()
ggplot(top20, aes(x=reorder(word, n), y=n)) + geom_col() + coord_flip() +
  geom_text(aes(label = n), hjust = -0.3) + labs(x=NULL) + 
  theme(text = element_text(family = "gf"))

#¿öµåÅ¬¶ó¿ìµå ¸¸µé±â
library(ggwordcloud)

font_add_google(name = "Black Han Sans", family = "bhs")
showtext_auto()
ggplot(word_noun, aes(label = word, size = n, col = n)) +
  geom_text_wordcloud(seed = 1234, family = "bhs") + 
  scale_radius(limits = c(3, NA), range = c(3, 15)) + 
  scale_color_gradient(low = "#00A0E2", high = "#004EA1") +
  theme_minimal()


# Æ¯Á¤´Ü¾î »ç¿ë¹®Àå È®ÀÎ ----
library(stringr)

raw_moon = readLines("speech_moon.txt", encoding = "UTF-8")
sentences_moon <- raw_moon %>% str_squish() %>% as_tibble %>% unnest_tokens(input = value, output = sentence, "sentences")
sentences_moon

sentences_moon %>% filter(str_detect(sentence, "±¹¹Î"))
sentences_moon %>% filter(str_detect(sentence, "ÀÏÀÚ¸®"))
sentences_moon %>% filter(str_detect(sentence, "ÀÏÀÚ¸®")) %>% print.data.frame(right = F)
