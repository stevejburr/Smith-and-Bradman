# Get to get data from ESPN Cricinfo

library(tidyverse)
library(here)
library(XML)
library(RcppRoll)

# donwload match by match data for each player



get_rolling_averages <- function(id) {
  
  url <- paste0("http://stats.espncricinfo.com/ci/engine/player/",id,".html?class=1;template=results;type=batting;view=innings")
  
  tables <-readHTMLTable(url, stringsAsFactors = F)
  
  table <- tables$`Innings by innings list`
  
  #some of the columns are blank, don't care about these so put in dummy values
  colnames(table)[10] <- "A"
  colnames(table)[14] <- "B"
  
  #remove DNB/absent etc from records
  table %>%
    mutate(
      Runs=str_replace(Runs,"\\*",""),
      Runs=as.numeric(Runs)) %>%
    filter(!is.na(Runs)) %>%
    mutate(out=if_else(Dismissal %in% c("retired notout","not out"),0,1)) -> table
  
  # check can match official average:  
  table %>%
    summarise(Runs=sum(Runs),
              outs=sum(out)) %>%
    mutate(Average=Runs/outs)
  
  # calculate cumulative averages up to this point in career
  # calculate rolling averages for latest 10, and 20 innings
  
  table %>%
    mutate(CumRuns=cumsum(Runs),
           CumOuts=cumsum(out),
           CumAverage=CumRuns/CumOuts,
           CumInnings=1:n(),
           CumRuns_10=roll_sum(Runs,10,align="right",fill=NA),
           CumOuts_10=roll_sum(out,10,align="right",fill=NA),
           CumAverage_10=CumRuns_10/CumOuts_10,
           CumRuns_20=roll_sum(Runs,20,align="right",fill=NA),
           CumOuts_20=roll_sum(out,20,align="right",fill=NA),
           CumAverage_20=CumRuns_20/CumOuts_20
           ) -> table
  
  table$player <- id
  
  return(table)
}
  

bradman <- get_rolling_averages(4188)

steve_smith <- get_rolling_averages(267192)



url <- "http://stats.espncricinfo.com/ci/content/records/282910.html"
tables <-readHTMLTable(url, stringsAsFactors = F)
tables

# want a list of top averaging players with cricinfo ids
doc <- htmlParse(url)
links <- xpathSApply(doc, "//a",saveXML)
free(doc)

links %>% 
  as.data.frame() %>% 
  mutate(player_data=str_match(`.`,"\\/player\\/[0-9]+"),
         player_id=str_remove(player_data,"\\/player\\/"),
         player_name=str_match(`.`,">[a-zA-Z \\-\\.]+<\\/a>"),
         player_name=str_remove(player_name,"<\\/a>"),
         player_name=str_remove(player_name,">")) %>%
  filter(!is.na(player_data)) %>%
  select(player_id,player_name) -> top_average_players

top_average_players 

top_average_players %>%
  pull(player_id) -> ids

all_records <- map_dfr(ids, get_rolling_averages)

saveRDS(all_records,"all_records.rds")

all_records %>%  
ggplot() +
  geom_line(aes(group=1,x=CumInnings,y=CumAverage_10))

steve_smith %>%  
  ggplot() +
  geom_line(aes(group=1,x=CumInnings,y=CumAverage_20))

steve_smith %>%
  ggplot() +
  geom_line(aes(group=1,x=CumInnings,y=CumAverage))


all_records%>%
  ggplot() +
  geom_line(aes(group=player,colour=player,x=CumInnings,y=CumAverage)) +
  geom_hline(aes(yintercept=100))


all_records %>%
  ggplot() +
  geom_line(aes(group=player,colour=player,x=CumInnings,y=CumAverage_20)) +
  geom_hline(aes(yintercept=100))


all_records %>%
  ggplot() +
  geom_line(aes(group=player,colour=player,x=CumInnings,y=CumAverage_10)) +
  geom_hline(aes(yintercept=100))


#get best 20 innings average for all top average players:

all_records %>%
  group_by(player) %>%
  filter(CumAverage_20==max(CumAverage_20,na.rm=T)) %>%
  ungroup() %>%
  select(player,CumAverage_20) %>%
  arrange(-CumAverage_20) %>%
  left_join(top_average_players,
            by=c("player"="player_id"))
