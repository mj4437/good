
---
title: "로또 당첨 번호 분석"
author: "Minjae Seo"
format: html
---

```{r}
#| echo: false


library(httr)
library(rvest)
library(stringr)

get_lotto_numbers <- function(draw_number) {
  url = 'https://www.dhlottery.co.kr/gameResult.do?method=byWin'
  
  data_lotto = POST(
    url,
    body = list(
      drwNo = draw_number,
      dwrNoList = draw_number
    )
  )
  
  data_lotto_html = data_lotto %>% read_html()
  
  numbers = data_lotto_html %>% 
    html_nodes('.num.win') %>%
    html_text() %>%
    str_extract_all('\\d+') %>%
    unlist() %>%
    as.numeric()
  
  return(numbers)
}
```


```{r}
#| echo: false

library(dplyr)

start_draw <- 1124
total_draws <- 5

all_numbers <- data.frame(Number = numeric())

for (i in 0:(total_draws-1)) {
  draw_number <- start_draw - i
  cat("크롤링 중:", draw_number, "회차\n")
  
  numbers <- get_lotto_numbers(draw_number)
  
    all_numbers <- bind_rows(all_numbers, data.frame(Number = numbers))
  
  Sys.sleep(2)
}

number_counts <- all_numbers %>%
  group_by(Number) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))
```


```{r}
#| echo: false

library(ggplot2)

ggplot(number_counts, aes(x = as.factor(Number), y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  labs(title = "로또 번호별 당첨 횟수",
       x = "번호",
       y = "당첨 횟수") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


```
