library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
data <- read.csv("../data/pokemon.csv", stringsAsFactors = FALSE)

# DATA CLEANING

clean_data <- subset(data, select = -c(abilities, japanese_name, base_happiness, classfication,
                                       percentage_male, generation))
str(clean_data)
clean_data$is_legendary <- ifelse(clean_data$is_legendary == 1, "Legendary", "Non-Legendary")
clean_data$capture_rate <- strtoi(clean_data$capture_rate)
legendaries <- clean_data %>% 
  filter(is_legendary == "Legendary")


#  - Capture Rate - Is there a correlation between legendary pokemon and capture rate?
max_cr <- max(clean_data$capture_rate, na.rm = T) 
max_cr_pok <- clean_data %>% 
  filter(capture_rate == max_cr,
         is_legendary == "Legendary")
min_cr <- min(clean_data$capture_rate, na.rm = T) 

all_cr <- clean_data %>% 
  group_by(capture_rate) %>% 
  summarise(count = n())

legendaries_cc <- clean_data %>% 
  filter(is_legendary == "Legendary") %>% 
  group_by(capture_rate) %>% 
  summarise(count = n())

all_bg <- all_cr %>% 
  ggplot(aes(x = capture_rate, y = count)) +
  geom_bar(stat = "identity") +
  labs(title = "All Pokemon and their Capture Rates",
       x = "Capture Rate", y = "Number of Pokemon")
#ggplotly(all_bg)

bar_graph <- legendaries_cc %>% 
  ggplot(aes(x=capture_rate, y=count, text = paste(" Capture Rate: ", capture_rate, "\n", "Count: ", count))) +
  geom_bar(stat = "identity") +
  labs(title = "All Legendary Pokemons and their Capture Rates",
       x = "Capture Rate",
       y = "Number of Legendaries")

#ggplotly(bar_graph, tooltip = "text")


#   Which type is the most likely to be a legendary Pokemon?
legendaries_type1 <- legendaries %>% 
  group_by(type1) %>% 
  summarise(type1_count = n())

legendaries_type2 <- legendaries %>% 
  group_by(type2) %>% 
  summarise(type2_count = n())

legendaries_types <- merge(legendaries_type1, legendaries_type2, by.x = c("type1"), by.y = c("type2")) %>% 
  mutate(total = type1_count + type2_count)

bar_stacked <- legendaries_types %>% 
  ggplot(aes(x = type1, y = type1_count+ type2_count, fill = type1,text = paste(" Count:", type1_count+type2_count))) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Legendaries and their Types",
       x = "Type",
       y = "Number of Pokemon")


most_type <- legendaries_types %>% 
  filter(total == max(total))


ggplotly(bar_stacked, tooltip = "text")


#  - Is there a correlation between a pokemon's overall stats that makes them a legendary?
overall <- clean_data %>% 
  mutate(overall = hp + attack + defense + sp_attack + sp_defense + speed)

density <- overall %>% 
  ggplot(aes(x = overall, group = is_legendary, fill = is_legendary, text = paste(""))) +
  geom_density(adjust=1.5, alpha = 0.4)

ggplotly(density)


#   Which type is the strongest overall? Which is the weakest?
type_avg <- overall %>% 
  group_by(type1) %>% 
  summarise(mean = mean(overall)) 

type1 <- type_avg %>% 
  ggplot(aes(x = type1, y = mean)) +
  geom_line(aes(group = 1)) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  geom_point() +
  labs(title = "Overall Statistics and Type",
       x = "Types", y = "Overall Statistics (mean)")

#ggplotly(type1)


updated_data <- read.csv("../data/pokemon_updated.csv", stringsAsFactors = FALSE)
clean_data <- subset(updated_data, select = -c(abilities, japanese_name, base_happiness, classfication,
                                       percentage_male))
str(clean_data)
clean_data$is_legendary <- ifelse(clean_data$is_legendary == 1, "Legendary", "Non-Legendary")
clean_data$capture_rate <- strtoi(clean_data$capture_rate)

overall <- clean_data %>% 
  mutate(overall = hp + attack + defense + sp_attack + sp_defense + speed)

#Are starter pokemon better than other non-legendaries?
starters <- overall %>% 
  filter(is_legendary == "Non-Legendary") %>% 
  group_by(generation) %>% 
  filter(row_number() < 10) %>% 
  mutate(is_starter = "starter")

f <- function(evol, df){
  x <- df %>% 
    filter(evolution_stage == evol)
  a <- overall %>% 
    filter(is_legendary == "Non-Legendary" & evolution_stage == evol) %>% 
    anti_join(x)
  g <- ggplot(a, aes(x = overall)) +
    geom_histogram(fill = "pink") +
    geom_vline(xintercept = median(a$overall), color = "blue", alpha = 0.5) +
    geom_vline(xintercept = x$overall, color = "red", alpha = 0.5) +
    geom_vline(xintercept = mean(a$overall), color = "black", alpha = 0.5) +
    labs(title = paste("Evolution ", evol, " Pokemon"),
         x = "Overall Statistics",
         y = "Number of Pokemon")
  
  g
}

#ggplotly(f(1, starters))
#ggplotly(f(2, starters))
#ggplotly(f(3, starters))


#Why are starter pokemon always typed fire, grass, water? Does it have anything to do with typing?

library(reshape2)

types <- overall %>% 
  filter(type2 == "") %>% 
  group_by(type1) %>% 
  summarise(against_bug = median(against_bug), 
            against_dark = median(against_dark), 
            against_dragon = median(against_dragon),
            against_electric = median(against_electric),
            against_fairy = median(against_fairy),
            against_fight = median(against_fight),
            against_fire = median(against_fire),
            against_flying = median(against_flying),
            against_ghost = median(against_ghost),
            against_grass = median(against_grass),
            against_ground = median(against_ground),
            against_ice = median(against_ice), 
            against_normal = median(against_normal),
            against_poison  = median(against_poison),
            against_psychic = median(against_psychic),
            against_rock = median(against_rock),
            against_steel = median(against_steel),
            against_water = median(against_water))
types <- melt(types)

type_weakness <- types %>% 
  ggplot(aes(x = type1, y = variable, fill = value)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#ggplotly(type_weakness)
