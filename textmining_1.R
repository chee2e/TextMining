#텍스트마이닝 1차시 ----

raw_moon <- readLines("speech_moon.txt", encoding = "UTF-8")
head(raw_moon)

#공백처리 ----
install.packages("stringr")
library(stringr)

txt <- "치킨은!! 맛있다. xyz 정말 맛있다!@#"
txt

str_replace_all(string = txt, pattern = "[^가-힣]", replacement =" ")

new = raw_moon %>% str_replace_all("[^가-힣]", replacement =" ")
head(new)


txt = "치킨은   맛있다    정말 맛있다"
txt

str_squish(txt)

new = new %>% str_squish()
head(new)


#tibble구조로 전처리 ----
# tibble : 데이터 구조파악 용이 + 텍스트 관련함수 적용 용이
library(dplyr)

new = new %>% as_tibble()
new

#전처리 한꺼번에 하기 ----
new <- raw_moon %>% str_replace_all("[^가-힣]", " ") %>% str_squish() %>% as_tibble()
new

#토큰화하기 ----
install.packages("tidytext")
library(tidytext)

text <- tibble(value = "대한민국은 민주공화국이다. 대한민국의 주권은 국민에게 있고, 모든 권력은 국민으로부터 나온다.")
text

text %>% unnest_tokens(input = value, output = word, token = "sentences")   #문장 기준 토큰화
text %>% unnest_tokens(input = value, output = word, token = "words")       #띄어쓰기 기준 토큰화
text %>% unnest_tokens(input = value, output = word, token = "characters")  #문자 기준 토큰화

word_space <- new %>% unnest_tokens(input = value, output = word, token = "words")
word_space

#단어빈도 구하기 ----
word_space <- word_space %>% count(word, sort = T)
word_space 

str_count("배")   #문자열의 글자수를 return
str_count("사과")

word_space = word_space %>% filter(str_count(word)>1) #단어의 길이가 1이상인 행만 추출
word_space

top20 <- word_space %>% head(20) #빈도 높은 단어 20개 추출
top20 

#시각화하기 ----
install.packages("ggplot2")
library(ggplot2)

ggplot(top20, aes(x = reorder(word, n), y = n)) + geom_col() + coord_flip() #coord_flip : x축 y축을 뒤집음

ggplot(top20, aes(x = reorder(word, n), y = n)) + geom_col() + coord_flip() + 
  geom_text(aes(label = n), hjust = -0.3) + labs(title = "연설문 단어 빈도", x = NULL, y = NULL) +
  theme(title = element_text(size = 15))

#워드 클라우드 만들기 ----
install.packages("ggwordcloud")
library(ggwordcloud)

ggplot(word_space, aes(label = word, size = n)) +
  geom_text_wordcloud(seed = 1234) + # 랜덤하지 않게 고정 
  scale_radius(limits = c(3, NA), range = c(3, 30)) #limits : 최소, 최대 단어 빈도 range : 최소, 최대 단어크기

#색상 변경
ggplot(word_space, aes(label = word, size = n, col = n)) + geom_text_wordcloud(seed = 1234) + 
  scale_radius(limits = c(3, NA), range = c(3, 30)) +
  scale_color_gradient(low = "#66aaf2", high = "#003EA1") + theme_minimal()

#폰트 적용
install.packages("showtext")
install.packages("jsonlite")
install.packages("curl")
library(showtext)
library(jsonlite)
library(curl)

#구글 폰트 불러오기
font_add_google(name = "Nanum Gothic", family = "ng")
showtext_auto()

ggplot(word_space, aes(label = word, size = n, col = n)) + 
  geom_text_wordcloud(seed = 1234, family = "ng") +
  scale_radius(limits = c(3, NA), range = c(3, 30)) +
  scale_color_gradient(low = "#66aaf2", high = "#00CAA1") + theme_minimal()

#폰트 변경
font_add_google(name = "Black Han Sans", family = "blackhansans")
showtext_auto()

ggplot(word_space, aes(label = word, size = n, col = n)) + 
  geom_text_wordcloud(seed = 1234, family = "blackhansans") +
  scale_radius(limits = c(3, NA), range = c(3, 30)) +
  scale_color_gradient(low = "#00A0E2", high = "#004EA1") + theme_minimal()
  