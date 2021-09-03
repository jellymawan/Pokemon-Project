library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(dplyr)

df <- read.csv("../data/pokemon.csv", stringsAsFactors = FALSE)
df$is_legendary <- ifelse(df$is_legendary == 1, "Legendary", 
                            "Non-Legendary")

type_data <- df %>% 
  select(name, type1, hp, defense, attack, sp_attack, sp_defense, speed) %>% 
  group_by(type1) %>% 
  summarise(hp = mean(hp),
            attack = mean(attack),
            defense = mean(defense),
            special_attack = mean(sp_attack),
            special_defense = mean(sp_defense),
            speed = mean(speed),
            total = mean(attack + defense + hp + sp_attack + sp_defense + 
                           speed))

server <- function(input, output, session) {
  output$type <- renderPlotly({
    ggplot(data = type_data, mapping = aes_string(x = "type1", 
                                                  y = input$diff_stat)) +
      geom_line(group = 1) +
      geom_point() +
      labs(x = "Types",
           y = "Base Stat (avg)")
  })
  
  output$radar <- renderPlotly({
    pokemon1 <- df %>% 
      filter(name == input$indv1) %>% 
      select(hp, attack, defense, sp_attack, sp_defense, speed)
    
    r1 <- map_dbl(pokemon1[, 1:6], ~.x)
    
    pokemon2 <- df %>% 
      filter(name == input$indv2) %>% 
      select(hp, attack, defense, sp_attack, sp_defense, speed)
    r2 <- map_dbl(pokemon2[, 1:6], ~.x)
    nms <- c("HP", "Attack", "Defense", "Special Attack", "Special Defense", 
             "Speed")
    
    #code to plot the radar
    fig <- plot_ly(
      type = 'scatterpolar',
      fill = 'toself',
      mode = 'markers',
      width = 700, height = 700
      
     ) %>% 
     add_trace(
        r = r1,
        theta = nms,
        name = input$indv1,
        hoverinfo = 'text',
        text = ~paste(" HP: ", r1[1], "\n",
                      "Attack: ", r1[2], "\n",
                      "Defense: ", r1[3], "\n",
                      "Special Attack: ", r1[4], "\n",
                      "Special Defense: ", r1[5], "\n",
                      "Speed: ", r1[6])
      ) %>% 
      add_trace(
        r = r2,
        theta = nms,
        name = input$indv2,
        hoverinfo = 'text',
        text = ~paste(" HP: ", r2[1], "\n",
                      "Attack: ", r2[2], "\n",
                      "Defense: ", r2[3], "\n",
                      "Special Attack: ", r2[4], "\n",
                      "Special Defense: ", r2[5], "\n",
                      "Speed: ", r2[6])
      )
    m <- list(
      l = 70,
      r = 70,
      b = 140,
      t = 140,
      pad = 4
    )
    fig <- fig %>%
      layout(margin = m,
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,200)
          )
        )
      )
  })
  

  output$gen_density <- renderPlotly({
    gen_data <- df %>% 
      filter(generation == input$gen) %>% 
      mutate(overall = hp + attack + defense + sp_attack + sp_defense + speed)
    
    ggplot(data = gen_data, mapping = aes_string(x = "overall", 
                                                 group = "is_legendary", 
                                                 fill = "is_legendary")) +
      geom_density(adjust=1.5, alpha = 0.4)
    
  })
  
  
}


