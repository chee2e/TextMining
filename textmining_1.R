#ÅØ½ºÆ®¸¶ÀÌ´× 1Â÷½Ã ----

raw_moon <- readLines("speech_moon.txt", encoding = "UTF-8")
head(raw_moon)

#°ø¹éÃ³¸® ----
install.packages("stringr")
library(stringr)

txt <- "Ä¡Å²Àº!! ¸ÀÀÖ´Ù. xyz Á¤¸» ¸ÀÀÖ´Ù!@#"
txt

str_replace_all(string = txt, pattern = "[^°¡-ÆR]", replacement =" ")

new = raw_moon %>% str_replace_all("[^°¡-ÆR]", replacement =" ")
head(new)


txt = "Ä¡Å²Àº   ¸ÀÀÖ´Ù    Á¤¸» ¸ÀÀÖ´Ù"
txt

str_squish(txt)

new = new %>% str_squish()
head(new)


#tibble±¸Á¶·Î ÀüÃ³¸® ----
# tibble : µ¥ÀÌÅÍ ±¸Á¶ÆÄ¾Ç ¿ëÀÌ + ÅØ½ºÆ® °ü·ÃÇÔ¼ö Àû¿ë ¿ëÀÌ
library(dplyr)

new = new %>% as_tibble()
new

#ÀüÃ³¸® ÇÑ²¨¹ø¿¡ ÇÏ±â ----
new <- raw_moon %>% str_replace_all("[^°¡-ÆR]", " ") %>% str_squish() %>% as_tibble()
new

#ÅäÅ«È­ÇÏ±â ----
install.packages("tidytext")
library(tidytext)

text <- tibble(value = "´ëÇÑ¹Î±¹Àº ¹ÎÁÖ°øÈ­±¹ÀÌ´Ù. ´ëÇÑ¹Î±¹ÀÇ ÁÖ±ÇÀº ±¹¹Î¿¡°Ô ÀÖ°í, ¸ğµç ±Ç·ÂÀº ±¹¹ÎÀ¸·ÎºÎÅÍ ³ª¿Â´Ù.")
text

text %>% unnest_tokens(input = value, output = word, token = "sentences")   #¹®Àå ±âÁØ ÅäÅ«È­
text %>% unnest_tokens(input = value, output = word, token = "words")       #¶ç¾î¾²±â ±âÁØ ÅäÅ«È­
text %>% unnest_tokens(input = value, output = word, token = "characters")  #¹®ÀÚ ±âÁØ ÅäÅ«È­

word_space <- new %>% unnest_tokens(input = value, output = word, token = "words")
word_space

#´Ü¾îºóµµ ±¸ÇÏ±â ----
word_space <- word_space %>% count(word, sort = T)
word_space 

str_count("¹è")   #¹®ÀÚ¿­ÀÇ ±ÛÀÚ¼ö¸¦ return
str_count("»ç°ú")

word_space = word_space %>% filter(str_count(word)>1) #´Ü¾îÀÇ ±æÀÌ°¡ 1ÀÌ»óÀÎ Çà¸¸ ÃßÃâ
word_space

top20 <- word_space %>% head(20) #ºóµµ ³ôÀº ´Ü¾î 20°³ ÃßÃâ
top20 

#½Ã°¢È­ÇÏ±â ----
install.packages("ggplot2")
library(ggplot2)

ggplot(top20, aes(x = reorder(word, n), y = n)) + geom_col() + coord_flip() #coord_flip : xÃà yÃàÀ» µÚÁıÀ½

ggplot(top20, aes(x = reorder(word, n), y = n)) + geom_col() + coord_flip() + 
  geom_text(aes(label = n), hjust = -0.3) + labs(title = "¿¬¼³¹® ´Ü¾î ºóµµ", x = NULL, y = NULL) +
  theme(title = element_text(size = 15))

#¿öµå Å¬¶ó¿ìµå ¸¸µé±â ----
install.packages("ggwordcloud")
library(ggwordcloud)

ggplot(word_space, aes(label = word, size = n)) +
  geom_text_wordcloud(seed = 1234) + # ·£´ıÇÏÁö ¾Ê°Ô °íÁ¤ 
  scale_radius(limits = c(3, NA), range = c(3, 30)) #limits : ÃÖ¼Ò, ÃÖ´ë ´Ü¾î ºóµµ range : ÃÖ¼Ò, ÃÖ´ë ´Ü¾îÅ©±â

#»ö»ó º¯°æ
ggplot(word_space, aes(label = word, size = n, col = n)) + geom_text_wordcloud(seed = 1234) + 
  scale_radius(limits = c(3, NA), range = c(3, 30)) +
  scale_color_gradient(low = "#66aaf2", high = "#003EA1") + theme_minimal()

#ÆùÆ® Àû¿ë
install.packages("showtext")
install.packages("jsonlite")
install.packages("curl")
library(showtext)
library(jsonlite)
library(curl)

#±¸±Û ÆùÆ® ºÒ·¯¿À±â
font_add_google(name = "Nanum Gothic", family = "ng")
showtext_auto()

ggplot(word_space, aes(label = word, size = n, col = n)) + 
  geom_text_wordcloud(seed = 1234, family = "ng") +
  scale_radius(limits = c(3, NA), range = c(3, 30)) +
  scale_color_gradient(low = "#66aaf2", high = "#00CAA1") + theme_minimal()

#ÆùÆ® º¯°æ
font_add_google(name = "Black Han Sans", family = "blackhansans")
showtext_auto()

ggplot(word_space, aes(label = word, size = n, col = n)) + 
  geom_text_wordcloud(seed = 1234, family = "blackhansans") +
  scale_radius(limits = c(3, NA), range = c(3, 30)) +
  scale_color_gradient(low = "#00A0E2", high = "#004EA1") + theme_minimal()
  