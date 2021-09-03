library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(dplyr)

ui <- navbarPage(title = "Pokemon Research",
                 tabPanel(title = "Types and Stats",
                          sidebarPanel(
                            selectInput(inputId = "diff_stat", 
                                        label = "Different Types and their Base Statistics",
                                        choices = c("hp", "attack", "defense", "special_attack", 
                                                    "special_defense", "speed", "total"))
                          ),
                          mainPanel(plotlyOutput("type"))),
                 tabPanel(title = "Comparison",
                          sidebarPanel(
                            selectInput(inputId = "indv1",
                                        label = "Pokemon 1",
                                        choices = df$name
                          ),
                          selectInput(inputId = "indv2",
                                      label = "Pokemon 2",
                                      choices = df$name)),
                          mainPanel(plotlyOutput("radar"))),
                 tabPanel(title = "Strongest Generation",
                          sidebarPanel(
                            sliderInput(inputId = "gen",
                                        label = "Generation",
                                        value = 1, min = 1, max = 7),
                            
                          ),
                          mainPanel(plotlyOutput("gen_density"))
                          ))

