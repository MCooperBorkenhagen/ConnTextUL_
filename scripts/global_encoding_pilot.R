

# read the prediction data, generate encodings and write
# the encoding data to file
utility("distances")

# backup if utility() doesn't work:
source(url('https://raw.githubusercontent.com/MCooperBorkenhagen/utilities/master/distances.R'))


#data = 'data/o2p_predictions.csv'

data = 'data/wj3&traindata__predictions.xlsx'


global_encodings = read_excel(data) %>% 
  rename(word = word_raw) %>% 
  group_by(word) %>% 
  summarise(global_encoding = first(global_encoding)) %>% 
  rownames_to_column(var = "index") %>% 
  mutate(index = as.numeric(index)) %>% 
  separate_wider_delim(global_encoding, delim = ":", names = str_c("d", 1:128), cols_remove = FALSE) %>% 
  mutate(across(starts_with("d"), as.numeric))


dists = global_encodings %>% 
  select(-word, -index) %>% 
  dist()

PCA = global_encodings %>% 
  select(-word, -index, -global_encoding) %>% 
  prcomp()


PCS = PCA$x %>% 
  as_tibble() %>% 
  mutate(word = global_encodings$word) %>% 
  select(word, everything())



distance_matrix_to_long(dists, words = unique(global_encodings$word)) %>% 
  write.csv('data/global_encodings_distances.csv', row.names = F)

write.csv(global_encodings, 'data/global_encodings.csv', row.names = F)
write.csv(PCS, 'data/global_encodings_principal_components.csv', row.names = F)
write_rds(PCA, 'data/global_encodings_principal_components_analysis.rds')

rm(list = ls())

