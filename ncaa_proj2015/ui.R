library(shiny)
remaining_winners <- schedule[grep("Team", schedule$Next),]
loser_choices <- unique(c(as.vector(remaining_winners$team_a),
                          as.vector(remaining_winners$team_b)))
loser_choices <- c("None", sort(loser_choices[-grep("Team", loser_choices)]))
if("Kentucky" %in% loser_choices)
        loser_choices <- loser_choices[-which(loser_choices=="Kentucky")]

shinyUI(fluidPage(
        
        # Application title
        titlePanel("Real Life is for March Simulations (updated 3/23 7:00AM)"),
        
        # Sidebar with a slider input for number of bins
        sidebarLayout(
                sidebarPanel(
                        sliderInput("sims",
                                    "Number of simulations:",
                                    min = 100,
                                    max = 1000,
                                    value = 100, step = 100),
                        
                        selectInput(inputId = "odd_type",
                                    label = "Game prediction method:",
                                    choices = c("Fivethirtyeight S16 Projections" = "fte3",
                                                "Fivethirtyeight R32 Projections" = "fte2",
                                                "Fivethirtyeight Pre-64 Projections" = "fte",
                                                "All teams equal chances" = "even")),
                        
                        checkboxInput(inputId = "uk_lose",
                                      label = "Make Kentucky lose next game"),
                        
                        selectInput(inputId = "loser",
                                    label = "Or make another team lose (click or type):",
                                    choices = loser_choices),
                        
                        actionButton("submit", "Submit"),
                        
                        helpText("If machine breaks, hit Submit again"),
                        
                        helpText(a("Fivethirtyeight Predictions", href="http://fivethirtyeight.com/interactives/march-madness-predictions-2015/#mens", target="_blank")),
                        
                        helpText(a("View App Code on Github", href="https://github.com/salauer/ncaa_proj2015", target="_blank")),
                        
                        helpText("Built by Stephen Lauer")
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                        tabsetPanel(
                                tabPanel("Brackets",
                                         dataTableOutput("best")),
                                tabPanel("Schools",
                                         dataTableOutput("champ"))
                        )
                )
        )
))
