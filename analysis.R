library(tidyverse)
library(tidytext)

d <- read_csv("2020-2021-critical-race-posts-schools-districts.csv")

nrc <- get_sentiments(lexicon = "nrc")

dd <- d %>% 
    unnest_tokens(word, Message) %>% 
    left_join(nrc)

dd %>% 
    count(sentiment) %>% 
    arrange(desc(n)) %>% 
    filter(!is.na(sentiment)) %>% 
    mutate(prop = n / nrow(d))

has_any <- function(x) {
    ifelse(!is.na(x), 1, 0)
}

dd$`Post Created` <- lubridate::ymd_hms(dd$`Post Created`)

dd %>% 
    count(URL, sentiment, `Post Created`) %>% 
    mutate(sentiment_binary = has_any(sentiment)) %>% 
    filter(!is.na(sentiment)) %>% 
    mutate(day = lubridate::round_date(`Post Created`, "week")) %>% 
    count(day, sentiment, sentiment_binary) %>% 
    ggplot(aes(x = day, y = n)) +
    geom_point() +
    geom_line() +
    facet_wrap(~sentiment) +
    theme_bw()

dd %>% 
    count(URL, sentiment) %>% 
    spread(sentiment, n) %>% 
    mutate_at(2:12, has_any) %>% 
    summarize_at(2:12, .funs = list(sum)) %>% 
    gather(key, val) %>% 
    filter(key != "<NA>") %>% 
    mutate(prop = val / nrow(d)) %>% 
    arrange(desc(prop))
