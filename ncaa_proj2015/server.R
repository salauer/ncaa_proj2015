library(dplyr)
library(shiny)

shinyServer(function(input, output) {
        fte_odds <- reactive({
#                 browser()
                if(input$odd_type == "fivethirtyeight"){
                        load_odds <- read.csv("adj_538.csv")
                }
                if(input$odd_type == "fte2"){
                        load_odds <- read.csv("R32-538.csv")
                }
                
                 fte_odds <- load_odds %>%
                        gather("Round", "Odds", 2:7) %>%
                        mutate(Odds = ifelse(is.na(as.numeric(Odds)), 0,
                                             1000*as.numeric(Odds)),
                               Round = as.numeric(substring(Round, 2)))
                fte_odds$Team <- gsub("^\\s+|\\s+$", "", fte_odds$Team)
                return(fte_odds)
        })
        
        odds <- reactive({
#                 browser()
                odds <- schedule
                if(input$odd_type == "even"){
                        
                        odds$odds_a <- ifelse(odds$odds_a == 0, 
                                              0, 1)
                        odds$odds_b <- ifelse(odds$odds_b == 0, 
                                              0, 1)
                }
                if(input$odd_type != "even"){
                        fte_odds <- fte_odds()
                        first_round <- filter(odds, Round == 32)
                        fte_first <- filter(fte_odds, Round == 32)
                        first_round$odds_a <-
                                ifelse(nchar(first_round$odds_a)>1,
                                       fte_first$Odds[match(first_round$team_a, 
                                                            as.character(fte_first$Team))], 
                                       as.numeric(first_round$odds_a))
                        first_round$odds_b <-
                                ifelse(nchar(first_round$odds_b)>1,
                                       fte_first$Odds[match(first_round$team_b, 
                                                            as.character(fte_first$Team))], 
                                       as.numeric(first_round$odds_b))
                        odds[which(odds$Round == 32),] <- first_round
                }
                return(odds)
        })
        
        output$best <- renderDataTable({
#                 browser()
                odds <- odds()
                all_sims <- c()
                input$submit
                
                for(i in 1:isolate(input$sims)){
                        if(input$odd_type != "even")
                                fte_odds <- fte_odds() else fte_odds <- odds()
                        one_sim <- select(simulate_tourny(odds, "fte2", fte_odds), Game,
                                          Round, Winner = Next, 
                                          Pot_Points = Points)
                        one_bracket <- left_join(all_picks, one_sim,
                                                 by = c("Round", "Game")) %>%
                                mutate(Points = ifelse(Picks == Winner, 
                                                       Pot_Points,
                                                       0)) %>%
                                group_by(Bracket) %>%
                                summarise(All_Points = sum(Points)) %>%
                                arrange(desc(All_Points))
                        one_bracket$Dollars <- c(.7*19*20, .2*19*20, 
                                                 .1*19*20, rep(0, 16))
                        one_bracket$sim <- i
                        all_sims <- bind_rows(all_sims, one_bracket)
                }
                final_df <- all_sims %>%
                        group_by(Bracket) %>%
                        summarise(Mean_Points = mean(All_Points),
                                  Mean_Dollars = round(mean(Dollars),2),
                                  Winning_Pct = 100*mean(Dollars==266),
                                  Money_Pct = 100*mean(Dollars > 0)) %>%
                        arrange(desc(Mean_Dollars))
                return(final_df)
        })
        
})
