# Get data from Wikipedia "Notre Dame Purdue Football Rivalry" page, and plot scores & wins over time

library(rvest)
library(stringr)
library(tidyr)

page <- "https://en.wikipedia.org/wiki/Notre_Dame%E2%80%93Purdue_football_rivalry"

get.purdue.nd <- function(selector, page){
     page.html <- read_html(page)
     
     df <- page.html %>% html_nodes(selector) %>% 
          html_table(header = FALSE, fill = TRUE) %>% as.data.frame()
     
     df <- df[2:length(df$X1),]
     
     names(df) <- c("date", "site", "winning.team", "winning.score", 
                    "losing.team", "losing.score", "series")
     
     df %>% mutate(date = mdy(date), site = as.factor(site),
                                   winning.team = as.factor(winning.team),
                                   losing.team = as.factor(losing.team),
                                   winning.score = str_extract(winning.score, "\\d+") %>% as.numeric(),
                                   losing.score = str_extract(losing.score, "\\d+") %>% as.numeric(),
                                   series = NULL)
}

purdue.nd <- 
     rbind(get.purdue.nd("#mw-content-text > table:nth-child(43)", page),
      get.purdue.nd("#mw-content-text > table:nth-child(45)", page))

purdue.nd <-
     purdue.nd %>% mutate(winning.team = str_replace(winning.team, "\\#[12] ", ""),
                          losing.team = str_replace(losing.team, "\\#[12] ", ""))

purdue.nd <- purdue.nd %>% select(date, site, winning.team, losing.team, winning.score, losing.score) %>% 
     gather(win.lose, team, winning.team:losing.team) %>% rowwise() %>% 
     mutate(score = ifelse(win.lose == "winning.team", winning.score, losing.score)) %>% 
     select(-winning.score, -losing.score) %>% 
     mutate(win.lose = ifelse(win.lose == "winning.team", "win", "lose"))

library(ggplot2)

purdue.nd <- purdue.nd %>% rowwise() %>% 
     mutate(decade = paste0(substr(year(date), 1, 2), substr(year(date), 3, 3), "0's"))

ggplot(purdue.nd, aes(x = team, y = score)) + 
     geom_bar(stat="identity", aes(fill = team)) + ggtitle("Total Score: Purdue vs. Notre Dame (by decade)") + 
     facet_wrap(~decade) +
     scale_fill_manual(limits = c("Notre Dame", "Purdue"), values = c("#0C2340", "#B1810B"))

ggplot(purdue.nd %>% filter(win.lose == "win") %>% 
            group_by(decade, team) %>% summarise(num.wins = n()), aes(x = team, y = num.wins)) + 
     geom_bar(stat="identity", aes(fill = team)) + ggtitle("Total Wins: Purdue vs. Notre Dame (by decade)") +
     facet_wrap(~decade) +
     scale_fill_manual(limits = c("Notre Dame", "Purdue"), values = c("#0C2340", "#B1810B")) + 
     geom_label(aes(label = num.wins), color = NA) + geom_text(aes(label = num.wins))

ggplot(purdue.nd %>% arrange(date) %>% group_by(team) %>% 
            mutate(cumulative.score = cumsum(score)) %>% ungroup(), 
       aes(date, cumulative.score, color = team, group = team)) + geom_line() +
     scale_color_manual(limits = c("Notre Dame", "Purdue"), values = c("#0C2340", "#B1810B")) + 
     ggtitle("Cumulative Score: Purdue vs. Notre Dame")

ggplot(purdue.nd %>% arrange(date) %>% group_by(decade, team) %>% 
            mutate(cumulative.score = cumsum(score)) %>% ungroup(), 
       aes(x = substr(year(date), 4, 4), y = cumulative.score, color = team, group = team)) + 
     geom_line() +
     scale_color_manual(limits = c("Notre Dame", "Purdue"), values = c("#0C2340", "#B1810B")) + 
     ggtitle("Cumulative Score: Purdue vs. Notre Dame (by decade)") + facet_wrap(~decade) +
     xlab("Year in Decade")

ggplot(purdue.nd, aes(x = substr(year(date), 4, 4), y = score, color = team, group = team)) + 
     geom_line() + geom_point() +
     scale_color_manual(limits = c("Notre Dame", "Purdue"), values = c("#0C2340", "#B1810B")) + 
     ggtitle("Individual Game Score: Purdue vs. Notre Dame (by decade)") + facet_wrap(~decade) +
     xlab("Year in Decade")