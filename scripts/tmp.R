
programs %>% 
  filter(word == "the" & epoch == 20)

programs %>% 
  filter(epoch == 20) %>% 
  summarise(sum(f))

