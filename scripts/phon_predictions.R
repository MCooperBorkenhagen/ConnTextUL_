



# get phonreps and their names
#phonreps = read_csv(RCurl::getURL('https://raw.githubusercontent.com/emelex-ai/ConnTextUL/main/raw/phonreps.csv?token=GHSAT0AAAAAACEJC2ADNZI7V6ZMZCPXALFUZE3TOTA'))

# if that doesn't work, try the local version:
phonreps = read_csv('data/phonreps.csv')

cols_ = phonreps %>% 
  select(-phone) %>% 
  names()


cols_ = gsub( "^#", "", cols_)
cols_ = c(cols_, "sos", "eos")

phonemewise_measures = list()


for (i in seq(nrow(preds))){
  
  prediction = as_tibble(vector_from_string(preds$phon_prediction_features[i], flatten = F)) 
  colnames(prediction) = cols_
  
  prediction = prediction %>% 
    mutate(phoneme = str_split(preds$phon_prediction[i], ":")[[1]],
           phoneme_index = seq_len(n()),
           condition = "predicted")
  
  target = as_tibble(vector_from_string(preds$phon_target_features[i], flatten = F))
  colnames(target) = cols_
  
  target = target %>% 
    mutate(phoneme = str_split(preds$phon_target[i], ":")[[1]],
           phoneme_index = seq_len(n()),
           condition = "target")
  
  out = rbind(prediction, target) %>% 
    mutate(word = preds$word[i]) %>% 
    select(word, phoneme, phoneme_index, condition, everything())
  
  phonemewise_measures[i][[1]] <- out
  
}




preds_by_phoneme = bind_rows(phonemewise_measures) %>% 
  left_join(preds, by = "word")






write_csv(preds_by_phoneme, 'data/preds_by_phoneme.csv')
