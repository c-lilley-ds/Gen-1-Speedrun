 library(tidyverse)
 library(lubridate)
 library(readxl)
SingleChallenge <- read_xlsx("Jrose Single Challenge.xlsx")
 str(SingleChallenge)
 SingleChallenge <- SingleChallenge %>% 
select(Pokemon, seconds,  
       DEX,  "Level Completed", 
       lowerthanstarter = "<Starter 65", "Evolution", "Video Order", Run_Finished = "Run Finished", "TYPE_1")

 SingleChallenge$Run_Finished <- factor(SingleChallenge$Run_Finished, 
                                        levels = c("Finished", "Unfinished", "Impossible"))
RunsCompleted <- SingleChallenge %>% 
 select(Run_Finished) %>% 
     filter(is.na(Run_Finished) ==FALSE) %>% 
     group_by(Run_Finished) %>% 
 count()
 
Runs_Completed_Bar<- SingleChallenge %>% ggplot(aes(Run_Finished, fill = Run_Finished))+
     geom_bar(position = "fill")
    #   labs(x = "Number of Cylinders") +
     
     

 SingleChallenge <- SingleChallenge %>% 
    mutate(minutes =(minute(minutes(seconds/60))), seconds = seconds(seconds)) 
 
 SingleChallenge$Evolution <- factor(SingleChallenge$Evolution)
args(filter)
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
 # type Vectpr
 
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
 
 geom_tile()
 