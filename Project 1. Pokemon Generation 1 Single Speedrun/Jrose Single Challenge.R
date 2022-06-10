
## Load Packages

 library(tidyverse)
 library(lubridate)
 library(readxl)
 
 ## Import Data
 
SingleChallenge <- read_xlsx("Jrose Single Challenge.xlsx")
 
## Cleaning Data Types
str(SingleChallenge)
SingleChallenge <- SingleChallenge %>% 
select(Pokemon, seconds,  
       DEX,  "Level Completed", 
       lowerthanstarter = "<Starter 65", "Evolution", "Video Order", 
       Run_Finished = "Run Finished", TYPE_1:Average)

 SingleChallenge$Run_Finished <- factor(SingleChallenge$Run_Finished, 
                                        levels = c("Finished", "Unfinished", 
                                                   "Impossible"))

 
SingleChallenge <- SingleChallenge %>% 
  mutate(minutes =(minute(minutes(seconds/60))), seconds = seconds(seconds)) 

SingleChallenge$Evolution <- factor(SingleChallenge$Evolution)
# Create type Vectpr

type <- c("Normal", "Fighting", "Flying", "Poison", "Ground", "Rock", "Bug", 
          "Ghost", "Steel", "Fire", "Water", "Grass", "Electric", "Psychic",
          "Ice", "Dragon", "Dark", "Fairy")
palette <- c(Normal = "#a8a878", Fighting = "#c03028", Flying = "#a890f0", Poison = "#a040a0", Ground = "#e0c068", Rock = "#b8a038", Bug = "#a8b820", 
             Ghost = "#705898", "Steel" = "#b8b8d0", "Fire" = "#f08030", "Water" = "#6890f0", "Grass" = "#78c850", "Electric" = "#f8d030", "Psychic" = "#f85888",
             "Ice" = "#98d8d8", "Dragon" = "#7038f8", "Dark" = "#705848", "Fairy" = "#ff9be1")
SingleChallenge$TYPE_1 <- factor(SingleChallenge$TYPE_1, 
                                 levels = c("Normal", "Fighting", "Flying", "Poison", "Ground", "Rock", "Bug", 
                                            "Ghost", "Steel", "Fire", "Water", "Grass", "Electric", "Psychic",
                                            "Ice", "Dragon", "Dark", "Fairy"))

SingleChallenge$TYPE_2 <- factor(SingleChallenge$TYPE_2, 
                                 levels = c("Normal", "Fighting", "Flying", "Poison", "Ground", "Rock", "Bug", 
                                            "Ghost", "Steel", "Fire", "Water", "Grass", "Electric", "Psychic",
                                            "Ice", "Dragon", "Dark", "Fairy", "None"))
## Cleaning Data Types


SingleChallenge %>% ggplot(aes(Run_Finished, fill = Run_Finished))+
     geom_bar() +
    labs(x = "Runs Completed") 
     
     


 


 SingleChallenge %>% 
    filter(is.na(minutes) == FALSE) %>% 
     ggplot(aes(Pokemon, minutes)) +
   geom_col() + 
   theme(axis.text.x=element_text(angle=90,hjust=1))  +
    # Create facets, wrapping by year, using vars()
    facet_wrap(vars(Evolution),scales = "free_y")+
    coord_flip()
 
 SingleChallenge %>% 
     filter(is.na(minutes) == FALSE) %>% 
     ggplot(aes(Pokemon, minutes, fill = TYPE_1)) +
     geom_col() + 
     theme(axis.text.x=element_text(angle=90,hjust=1))  +
     scale_fill_manual("Transmission", values = palette)  +
     coord_flip()

 
 geom_tile()
 
 completedruns <- SingleChallenge %>% 
   filter(Run_Finished == "Finished")
 uncompletedruns <- SingleChallenge %>% 
   filter(Run_Finished != "Finished")
 ??pokemon
 # model time
 aa <- lm(minutes~TYPE_1, completedruns)
 uncompletedruns$predictedminutes <- predict(aa, uncompletedruns)