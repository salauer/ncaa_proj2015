
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
        
        # Application title
        titlePanel("Real Life is for March Simulations (updated 3/21 5:35PM)"),
        
        # Sidebar with a slider input for number of bins
        sidebarLayout(
                sidebarPanel(
                        sliderInput("sims",
                                    "Number of simulations:",
                                    min = 100,
                                    max = 1000,
                                    value = 100, step = 100),
                        
                        selectInput(inputId = "odd_type",
                                    label = "Game Prediction Method:",
                                    choices = c("All teams equal chances" = "even",
                                                "Fivethirtyeight Pre-64 Preds" = "fivethirtyeight",
                                                "Fivethirtyeight R32 Preds" = "fte2"),
                                    selected = "even"),
                        
                        checkboxInput(inputId = "uk_lose",
                                      label = "Make Kentucky Lose Next Game"),
                        
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
