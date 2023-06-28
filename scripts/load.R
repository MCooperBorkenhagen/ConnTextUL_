

preds = read_csv('data/o2p_predictions.csv') %>% 
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
preds$phon_prediction_probabilities_mean = NA
preds$phon_prediction_distance_mean = NA

for (i in seq(nrow(preds))){
  
  #preds$phon_prediction_probabilities_total[i] = length(vector_from_string(preds$phon_prediction_features[i]))
  #preds$phon_prediction_units_total[i] = length(vector_from_string(preds$phon_prediction_probabilities[i]))
  preds$phon_prediction_probabilities_mean[i] = mean(vector_from_string(preds$phon_prediction_probabilities[i]))
  
  phon_prediction_probability_corrected = correct_unitwise_probabilities(vector_from_string(preds$phon_prediction_probabilities[i]), vector_from_string(preds$phon_target_features[i], flatten = T), strip_final = T)
  preds$phon_prediction_probabilities_mean[i] = mean(phon_prediction_probability_corrected)
  preds$phon_prediction_distance_mean[i] = L2(vector_from_string(preds$phon_prediction_features[i]), vector_from_string(preds$phon_target_features[i]))
  
}


preds_by_phoneme = read_csv('data/preds_by_phoneme.csv')
