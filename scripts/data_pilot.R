
source('scripts/utilities.R')

# this script generates the preds_by_word and preds_by_phoneme 
# objects and writes to file, which are then loaded by load.R
# for analysis in scripts elsewhere

data = 'data/o2p_predictions.csv'


preds_by_word_pilot = read_csv(data) %>% 
  rename(word = word_raw) %>% 
  group_by(word) %>% 
  summarise(phon_target = first(phon_target),
            phon_prediction = first(phon_prediction),
            correct = first(correct),
            in_validation_set = mean(in_validation_set),
            phon_target_features = first(phon_target_features),
            phon_prediction_features = first(phon_prediction_features),
            phon_prediction_probabilities = first(phon_prediction_probabilities),
            frequency = n()) %>% 
  mutate(phonlength = phonlength(phon_target_features),
         orthlength = str_length(word)) %>% 
  glimpse()

# calculate a range of metrics from predictions and their probabilities
#preds_by_word$phon_prediction_probabilities_mean = NA
#preds_by_word$phon_prediction_distance_mean = NA
preds_by_word_pilot$stress_target = NA
preds_by_word_pilot$stress_prediction = NA


for (i in seq(nrow(preds_by_word_pilot))){
  
  
  preds_by_word_pilot$stress_prediction[i] = stress_pattern(preds_by_word$phon_prediction[i])
  preds_by_word_pilot$stress_target[i] = stress_pattern(preds_by_word$phon_target[i])
  #preds_by_word$phon_prediction_probabilities_mean[i] = mean(vector_from_string(preds_by_word$phon_prediction_probabilities[i]))
  
  #phon_prediction_probability_corrected = correct_unitwise_probabilities(vector_from_string(preds_by_word$phon_prediction_probabilities[i]), vector_from_string(preds_by_word$phon_target_features[i], flatten = T), strip_final = T)
  #preds_by_word$phon_prediction_probabilities_mean[i] = mean(phon_prediction_probability_corrected)
  #preds_by_word$phon_prediction_distance_mean[i] = L2(vector_from_string(preds_by_word$phon_prediction_features[i]), vector_from_string(preds_by_word$phon_target_features[i]))
  
}

# warnings result above if the prediction and the target are different lengths



# get phonreps and their names
#phonreps = read_csv(RCurl::getURL('https://raw.githubusercontent.com/emelex-ai/ConnTextUL/main/raw/phonreps.csv?token=GHSAT0AAAAAACEJC2ADNZI7V6ZMZCPXALFUZE3TOTA'))

# if that doesn't work, try the local version:
phonreps = read_csv('data/phonreps.csv')

cols_ = phonreps %>% 
  select(-phone) %>% 
  names()


cols_ = gsub( "^#", "", cols_)
cols_ = c(cols_, "sos", "eos")
cols_ = gsub("-", "_", cols_)
cols_ = str_c(cols_, "_feature")

phonemewise_measures_pilot = list()


for (i in seq(nrow(preds_by_word))){
  
  # process the predicted features
  prediction_pilot = as_tibble(vector_from_string(preds_by_word_pilot$phon_prediction_features[i], flatten = F)) 
  colnames(prediction_pilot) = cols_
  
  prediction_pilot = prediction_pilot %>% 
    mutate(phoneme = str_split(preds_by_word_pilot$phon_prediction[i], ":")[[1]],
           phoneme_index = seq_len(n()),
           condition = "predicted")
  
  # process the target features
  target_pilot = as_tibble(vector_from_string(preds_by_word_pilot$phon_target_features[i], flatten = F))
  colnames(target_pilot) = cols_
  
  target_pilot = target_pilot %>% 
    mutate(phoneme = str_split(preds_by_word_pilot$phon_target[i], ":")[[1]],
           phoneme_index = seq_len(n()),
           condition = "target")
  
  # process the raw activations (probabilities)
  raw_pilot = as_tibble(vector_from_string(preds_by_word_pilot$phon_prediction_probabilities[i], flatten = F))
  colnames(raw_pilot) = cols_
  
  raw_pilot = raw_pilot %>% 
    filter(row_number() <= n()-1) %>% 
    mutate(phoneme = str_split(preds_by_word_pilot$phon_prediction[i], ":")[[1]],
           phoneme_index = seq_len(n()),
           condition = "raw")
  
  out_pilot = rbind(prediction_pilot, raw_pilot, target_pilot) %>% 
    mutate(word = preds_by_word_pilot$word[i]) %>% 
    select(word, phoneme, phoneme_index, condition, everything())
  
  phonemewise_measures_pilot[i][[1]] <- out_pilot
  
}



preds_by_phoneme_pilot = bind_rows(phonemewise_measures_pilot) %>% 
  left_join(preds_by_word_pilot %>% 
              select(word, correct, in_validation_set, frequency), by = "word") %>% 
  select(word, correct, phoneme, phoneme_index, condition, in_validation_set, frequency, everything())



write_csv(preds_by_word_pilot, 'data/preds_by_word_pilot.csv')
write_csv(preds_by_phoneme_pilot, 'data/preds_by_phoneme_pilot.csv')
