# install.packages("wordcloud2")

library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(KoNLP)
library(ggplot2)
library(dplyr)
library(rvest)
library(stringr)
library(reshape2)
library(lubridate)
library(DT)
library(tidyverse)

news_url <-"https://search.naver.com/search.naver?&where=news&query=%EB%8D%B0%EC%9D%B4%ED%84%B0%203%EB%B2%95&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=0&ds=&de=&docid=&nso=so:r,p:all,a:all&mynews=0&cluster_rank=65&start=1&refresh_start=0"

# news_url <- "https://news.naver.com/main/home.nhn"

news_html <- read_html(news_url) 

# news_html %>% 
#   html_nodes(".list_body.newsflash_body")

news_urls <- news_html %>% 
  html_nodes(".news.mynews.section._prs_nws") %>% 
  html_nodes("._sp_each_url") %>% 
  html_attr("href")


news_text <- map_dfr(news_urls[1:4], function(news_page){
  news_page %>% 
    read_html() %>% 
    html_nodes("#articleBodyContents._article_body_contents") %>% 
    html_text()
}) 

news_text

##########################################################################################
news_urls
news_urls[1] %>% 
  read_html() %>% 
  html_nodes(xpath = '//*[@id="main_content"]/div[2]') %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  unique() 

news_text <- lapply(hdline_news_url, function(news_page){
  news_page %>% 
    read_html() %>% 
    html_nodes("#articleBodyContents._article_body_contents") %>% 
    html_text()
}) 

news_text <- news_text %>% 
  unlist() %>% 
  str_replace_all("flash 오류를 우회하기 위한 함수 추가","") %>% 
  str_replace_all("function _flash_removeCallback","") %>% 
  str_replace_all("동영상 뉴스","") %>% 
  str_replace_all("\\W"," ") %>% 
  str_replace_all("   "," ")
}




















hdline_urls <- paste0("https://news.naver.com",hdline_urls)

hdline_new_text_sum <- lapply(hdline_urls, function(page) {
  hdline_news_url <- page %>% 
    read_html() %>% 
    html_nodes(xpath = '//*[@id="main_content"]/div[2]') %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    unique() 
  
  news_text <- lapply(hdline_news_url, function(news_page){
    news_page %>% 
      read_html() %>% 
      html_nodes("#articleBodyContents._article_body_contents") %>% 
      html_text()
  }) 
  
  news_text <- news_text %>% 
    unlist() %>% 
    str_replace_all("flash 오류를 우회하기 위한 함수 추가","") %>% 
    str_replace_all("function _flash_removeCallback","") %>% 
    str_replace_all("동영상 뉴스","") %>% 
    str_replace_all("\\W"," ") %>% 
    str_replace_all("   "," ")
}
)

hdline_new_text_sum

asdf <- hdline_new_text_sum[[5]]

nouns <- KoNLP::extractNoun(asdf)
# table 형태로 변환
wordcount <- table(unlist(nouns))

df.word  <- as.data.frame(wordcount, stringsAsFactors = FALSE)
df.word <- rename(df.word, word = Var1, freq = Freq)

word.freq  <- df.word %>% 
  filter(nchar(word) >=2 & freq >= 2) %>% 
  arrange(desc(freq)) 



wordcloud::wordcloud(words = word.freq$word, freq = word.freq$freq,
                     min.freq = 2, max.words = 200,
                     random.order = FALSE, rot.per = 0.1,
                     scale= c(5,0.5),
                     colors = brewer.pal(8, "Dark2"))


wordcloud2(word.freq,
           fontFamily="Malgun Gothic",
           size = 0.5,
           minRotation=0,
           maxRotation=0,
)


ggplot(head(word.freq,20), aes(x=reorder(word,freq),y=freq),colour=gradient) +
  # geom_bar(stat="identity") +
  geom_bar(stat="identity",aes(fill=word)) +
  # geom_text(aes(label=rownames(word))) +
  # geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  # geom_text() +
  # scale_fill_brewer() +
  theme_bw() + 
  # scale_fill_brewer(palette = "Greens") +
  coord_flip()

