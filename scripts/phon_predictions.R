# calculate a range of metrics from predictions and their probabilities

preds$phon_prediction_probabilities_mean = NA
preds$phon_prediction_distance_mean = NA
#preds$phon_prediction_probabilities_total = NA
#preds$phon_prediction_units_total = NA


for (i in seq(nrow(preds))){

  #preds$phon_prediction_probabilities_total[i] = length(vector_from_string(preds$phon_prediction_features[i]))
  #preds$phon_prediction_units_total[i] = length(vector_from_string(preds$phon_prediction_probabilities[i]))
  preds$phon_prediction_probabilities_mean[i] = mean(vector_from_string(preds$phon_prediction_probabilities[i]))
  
  phon_prediction_probability_corrected = correct_unitwise_probabilities(vector_from_string(preds$phon_prediction_probabilities[i]), vector_from_string(preds$phon_target_features[i], flatten = T), strip_final = T)
  preds$phon_prediction_probabilities_mean[i] = mean(phon_prediction_probability_corrected)
  preds$phon_prediction_distance_mean[i] = L2(vector_from_string(preds$phon_prediction_features[i]), vector_from_string(preds$phon_target_features[i]))
  
}



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
