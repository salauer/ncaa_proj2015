library(dplyr)
library(tidyr)

####################### rvest doesn't appear to work in deployed Shiny environment
# library(rvest)
# 
# bracket_link <- html("https://tournament.fantasysports.yahoo.com/t1/group/123754")
# bracket_names <- as.data.frame(html_table(bracket_link))
# brackets <- bracket_link %>% html_nodes("a") %>% html_attr("href")
# bracket_list <- paste0("https://tournament.fantasysports.yahoo.com",
#                        brackets[which(substring(brackets, 2,3) == "t1")])
# bracket_list <- bracket_list[-1]
# all_picks <- c()
# for(i in 1:length(bracket_list)){
#         bracket_name <- bracket_names$Bracket[i]
#         bracket_html <- html(bracket_list[i])
#         picks <- html_nodes(bracket_html, ".has-value-1 strong") %>% html_text()
#         picks <- picks[-grep("\n", picks)]
#         picks <- ifelse(substr(picks, 2, 2) == " ", 
#                         substring(picks, 3),
#                         substring(picks, 4))
#         one_picks <- data_frame(Bracket = rep(bracket_name, length(picks)),
#                                 Picks = picks,
#                                 Round = c(rep(c(rep(32,8), rep(16,4), 
#                                                 rep(8,2)),2),
#                                           rep(4,4), rep(2, 2), 1,
#                                           rep(c(rep(32,8), rep(16,4),
#                                                 rep(8,2)),2)))
#         one_picks <- arrange(one_picks, desc(Round))
#         one_picks$Game <- paste0("G",seq(1,length(picks)))
#         all_picks <- bind_rows(all_picks, one_picks)
# }
# 
# schedule <- data_frame(Game = paste0("G",seq(1,63)),
#                        team_a = paste0("G", seq(1,63), "TeamA"),
#                        team_b = paste0("G", seq(1,63), "TeamB"),
#                        odds_a = paste0("G", seq(1,63), "OddsA"),
#                        odds_b = paste0("G", seq(1,63), "OddsB"),
#                        Round = c(rep(32,32), rep(16,16), rep(8,8), 
#                                  rep(4,4), rep(2,2), rep(1,1)),
#                        Points = c(rep(1,32), rep(2,16), rep(4,8), 
#                                   rep(8,4), rep(16,2), rep(32,1)),
#                        Next = c(paste0("G", rep(seq(33, 63), each = 2), "Team", c("A", "B")[seq(1,2)]), "Champ"))
# 
# all_teams <- html_nodes(bracket_html, ".realpick") %>% html_text()
# first_round_teams <- sapply(strsplit(all_teams, "\n"), "[[",1)
# teams_no_ranks <- ifelse(substr(first_round_teams, 2, 2) == " ", 
#                          substring(first_round_teams, 3),
#                          substring(first_round_teams, 4))
# schedule$team_a[1:32] <- teams_no_ranks[seq(1,63,2)]
# schedule$team_b[1:32] <- teams_no_ranks[seq(2,64,2)]
# 
# current_bracket <- html("http://sports.yahoo.com/college-basketball/bracket/")
# whole_bracket <- html_nodes(current_bracket, "b") %>% html_text()
# current_winners <- table(whole_bracket)[which(table(whole_bracket)>1)]
# names(current_winners) <- ifelse(substr(names(current_winners), 2, 2) == " ", 
#                                  substring(names(current_winners), 3),
#                                  substring(names(current_winners), 4))
# current_winners <- current_winners-1
# 
# for(i in 1:length(current_winners)){
#         for(j in 1:current_winners[i]){
#                 if(names(current_winners[i]) %in% schedule$team_a){
#                         game_index <- max(which(names(current_winners[i]) == schedule$team_a))
#                         if(substring(schedule$Next[game_index], 4) == "TeamA")
#                                 schedule$team_a[which(schedule$team_a == schedule$Next[game_index])] <- names(current_winners[i])
#                         if(substring(schedule$Next[game_index], 4) == "TeamB")
#                                 schedule$team_b[which(schedule$team_b == schedule$Next[game_index])] <- names(current_winners[i])
#                         schedule$Next[game_index] <- names(current_winners[i])
#                         schedule$odds_a[game_index] <- 1
#                         schedule$odds_b[game_index] <- 0
#                         schedule$team_a[game_index] <- "Winner"
#                         next
#                 }
#                 if(names(current_winners[i]) %in% schedule$team_b){
#                         game_index <- max(which(names(current_winners[i]) == schedule$team_b))
#                         if(substring(schedule$Next[game_index], 4) == "TeamA")
#                                 schedule$team_a[which(schedule$team_a == schedule$Next[game_index])] <- names(current_winners[i])
#                         if(substring(schedule$Next[game_index], 4) == "TeamB")
#                                 schedule$team_b[which(schedule$team_b == schedule$Next[game_index])] <- names(current_winners[i])
#                         schedule$Next[game_index] <- names(current_winners[i])
#                         schedule$odds_b[game_index] <- 1
#                         schedule$odds_a[game_index] <- 0
#                         schedule$team_b[game_index] <- "Winner"
#                         next
#                 }
#         }
# }
#################################### End of rvest area


schedule <- read.csv("schedule.csv", stringsAsFactors=FALSE)
all_picks <- read.csv("allpicks.csv", stringsAsFactors=FALSE)

simulate_game <- function(team_a, team_b, odds_a, odds_b) {
        winner <- sample(c(rep(team_a, odds_a), rep(team_b, odds_b)), 1)
        return(winner)
}
# simulate_game(schedule$team_a, schedule$team_b, schedule$odds_a, schedule$odds_b)

simulate_tourny <- function(odds, odd_type, fte_odds){
        for(i in 1:dim(odds)[1]){
                if(!"Winner" %in% c(odds$team_a[i], odds$team_b[i])){
                        winner <- simulate_game(odds$team_a[i],
                                                odds$team_b[i],
                                                odds$odds_a[i],
                                                odds$odds_b[i])
                } else winner <- odds$Next[i]
                if(odds$Next[i] %in% odds$team_a){
                        next_idx <- max(which(odds$team_a == odds$Next[i]))
                        odds$team_a[next_idx] <- winner
                        odds$Next[i] <- winner
                        if(odd_type != "even"){
                                odds$odds_a[next_idx] <-
                                        ifelse(nchar(odds$odds_a[next_idx])>1,
                                               fte_odds$Odds[which(as.character(fte_odds$Team) == odds$team_a[next_idx] & fte_odds$Round == odds$Round[next_idx])], 
                                               as.numeric(odds$odds_a[next_idx]))
                        }
                        next
                }
                if(odds$Next[i] %in% odds$team_b){
                        next_idx <- max(which(odds$team_b == odds$Next[i]))
                        odds$team_b[next_idx] <- winner
                        odds$Next[i] <- winner
                        if(odd_type != "even"){
                                odds$odds_b[next_idx] <-
                                        ifelse(nchar(odds$odds_b[next_idx])>1,
                                               fte_odds$Odds[which(as.character(fte_odds$Team) == odds$team_b[next_idx] & fte_odds$Round == odds$Round[next_idx])], 
                                               as.numeric(odds$odds_b[next_idx]))
                        }
                        next
                }
        }
        odds$Next[63] <- winner
        return(odds)
}
# tail(simulate_tourny(schedule))


