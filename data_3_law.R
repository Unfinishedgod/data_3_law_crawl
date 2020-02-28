library(wordcloud)
library(wordcloud2)
library(KoNLP)
library(rvest)
library(tidyverse)
library(glue)

keyword <- "공팔리터"

# news_url <-"https://search.naver.com/search.naver?&where=news&query=%EB%8D%B0%EC%9D%B4%ED%84%B0%203%EB%B2%95&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=0&ds=&de=&docid=&nso=so:r,p:all,a:all&mynews=0&cluster_rank=65&start=1&refresh_start=0"

news_url <- glue("https://search.naver.com/search.naver?&where=news&query={keyword}&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=0&ds=&de=&docid=&nso=so:r,p:all,a:all&mynews=0&cluster_rank=22&start=11&refresh_start=0")

news_html <- read_html(news_url) 

news_urls <- news_html %>% 
  html_nodes(".news.mynews.section._prs_nws") %>% 
  html_nodes("._sp_each_url") %>% 
  html_attr("href")

news_urls <- news_urls[news_urls %>% str_detect("news.naver")]

news_text <- map_chr(news_urls, function(news_page){
  news_page %>% 
    read_html() %>% 
    html_nodes("#articleBodyContents._article_body_contents") %>% 
    html_text()
})

cleanging_text <- news_text %>% 
  str_remove_all("flash 오류를 우회하기 위한 함수 추가") %>% 
  str_remove_all("function _flash_removeCallback") %>% 
  str_remove_all("[a-zA-Z]") %>% 
  str_replace_all("\\W"," ") %>% 
  str_replace_all("  ","")
  

# cleanging_text <- paste0(cleanging_text, collapse = "")

nouns <- KoNLP::extractNoun(cleanging_text)

# table 형태로 변환
wordcount <- table(unlist(nouns))

df.word  <- as.data.frame(wordcount, stringsAsFactors = FALSE)
df.word <- rename(df.word, word = Var1, freq = Freq)

word.freq  <- df.word %>% 
  filter(nchar(word) >=2 & freq >= 2) %>% 
  arrange(desc(freq)) 


wordcloud2(word.freq,
           fontFamily="Malgun Gothic",
           size = 0.5,
           minRotation=0,
           maxRotation=0,
)


ggplot(head(word.freq,30), aes(x=reorder(word,freq),y=freq,fill=word),colour=gradient) +
  geom_col() +
  theme_bw() + 
  labs(x="",y="단어 빈도",title = "데이터 3법 네이버 뉴스 단어 빈도") + 
  coord_flip() + 
  theme(legend.position = "none") +
  geom_text(aes(label=freq), vjust=0.5, hjust = 3,color="black", size=4) +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=90, hjust=1)) 

