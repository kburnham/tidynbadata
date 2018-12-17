



library(mysportsfeedsR)
library(tidyverse)
library(glue)
library(lubridate)
library(directlabels)

authenticate_v2_x(apikey = 'abd52306-deb4-4204-8605-e877fc')


walk(list.files('R', pattern = '.R$', full.names = TRUE), function(x) {
  message(glue('loading {basename(x)} . . .'))
  source(x)
})


sched <- get_team_schedule()
# plot cumulative +/- (by game, not play)

theme_set(theme_bw())



bs_list <- sched %>% filter(status == 'complete') %>%
  pull(msf_game_id) %>%
  map(get_player_game_data, 'Knicks') %>%
  map(summarize_raw_box_scores)

## how do I get a box score full of 0s so that everyone starts at 0?


bs_list <- map2(bs_list, seq(bs_list), function(x, y) x %>% mutate(game_num = y))
bs_list %>% map(~.$game_num)




## need a list of all player.ids

all_ids <- bs_list %>% map(function(x) {
  x %>% pull(player.id)
}) %>% flatten_chr %>% unique

last_names = bs_list %>% map(function(x) {
  x %>% pull(last_name)
}) %>% flatten_chr %>% unique

dummy_df <- data.frame(player.id = as.integer(all_ids), last_name = last_names,  plus_minus = 0, game_num = 0)


bs_list2 <- c(list(dummy_df), bs_list)


bsdf <- do.call('bind_rows', bs_list2)





bsdf %>% count(game_num)
#bs %>% filter(min_played > 100) %>% pull(last_name) %>% dput

p <- bsdf %>% group_by(last_name) %>% filter(last_name %in% c("Hardaway Jr.", "Thomas", "Kanter", "Hezonja", "Vonleh", "Burke",
                                                         "Ntilikina", "Dotson", "Trier", "Robinson", "Mudiay")) %>%
  mutate(`+/-` = cumsum(plus_minus)) %>%
  ggplot(aes(x = game_num, y = `+/-`, color = last_name)) + geom_line() + ggtitle('Knicks 2018-2019 Cumulative Plus/Minus, select players')






p + geom_dl(aes(label = last_name), method = list('last.points', cex = .8)) + guides(color = FALSE) + scale_x_discrete(expand=c(0, 1)) + theme(axis.text.x = element_text('game'))

