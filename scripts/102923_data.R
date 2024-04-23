
source('scripts/utilities.R')
require(tidyverse)
conflicted::conflicts_prefer(dplyr::filter)

lapply(read.csv('scripts/requirements.txt', stringsAsFactors = F)[[1]], require, ch = T)

# this script generates the preds_by_word and preds_by_phoneme 
# objects and writes to file for the proof of concept ("poc") dataset
# produced in October 2023. In turn, these can be loaded by load.R
# for analysis in scripts elsewhere
path = 'data/102923_outputs.xlsx'
sheets = excel_sheets(path)


# the main dataframe
d = read_excel(path, sheet = sheets[1]) %>% 
  mutate(epoch = str_replace(sheets[1], "epoch 0", ""))

for (sheet in sheets[-1]) {
  
  tmp = read_excel(path, sheet = sheet) %>% 
    mutate(epoch = str_replace(sheet, "epoch 0", ""))
  d = rbind(d, tmp)
  
}


d = d %>% 
  mutate(epoch = as.numeric(epoch),
         correct = as.numeric(correct)) %>% 
  rename(word = word_raw)

inputs = read_csv('data/102923_data.csv') %>% 
  mutate(word = tolower(word_raw)) %>% 
  select(word, word_index, text_index, program_type)



# calculate a range of metrics from predictions and their probabilities
d$stress_target = NA
d$stress_prediction = NA


for (i in seq(nrow(d))){
  
  d$stress_prediction[i] = stress_pattern(d$phon_prediction[i])
  d$stress_target[i] = stress_pattern(d$phon_target[i])
  
}

# generate the distance measure of the prediction from the target
# note that the WJ3 words not in the training set have NA values
# for phon_target_features, which means we have to pass
d$phon_distance_from_target = NA

for (row in seq(nrow(d))){
  if (is.na(d$phon_target_features[row])){
    d$phon_distance_from_target[row] = NA
  }
  else {
    d$phon_distance_from_target[row] = padded_distance(d$phon_prediction_features[row], d$phon_target_features[row])
  }
  
  
}


preds_by_word = d %>% 
  group_by(epoch, word) %>% 
  summarise(phon_target = first(phon_target),
            phon_prediction = first(phon_prediction),
            correct = first(correct),
            in_validation_set = first(in_wj3),
            in_training_set = first(in_traindata),
            phon_target_features = first(phon_target_features),
            phon_prediction_features = first(phon_prediction_features),
            phon_prediction_probabilities = first(phon_prediction_probabilities),
            phon_distance_from_target = first(phon_distance_from_target)) %>% 
  mutate(phonlength = phonlength(phon_target_features),
         orthlength = str_length(word),
         epoch = as.numeric(epoch)) %>% 
  left_join(inputs %>% 
              group_by(word) %>% 
              summarise(f = n())) %>% 
  #filter(phon_target_features != 'None') %>% # remove these "None" values - need to get those from NC
  glimpse()


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

phonemewise_measures = list()

# note that this routine differs slightly from the corresponding routine
# for the pilot data. The targets and predictions are slightly different
# between the pilot (6XX23.csv) and proof of concept (102923.xlsx) data
for (i in seq(nrow(preds_by_word))){
  
  # process the predicted features and convert to tibble
  # the number of columns will equal the total number of features (likely 33)
  # the number of rows will be the number of segments/ phonemes
  # the end of word segment has already been stripped off prior to the rendering of the data
  # therefore the presence of the end of word sement is incidental
  prediction = as_tibble(vector_from_string(preds_by_word$phon_prediction_features[i], flatten = F)) 
  colnames(prediction) = cols_
  
  prediction = prediction %>% 
    mutate(phoneme = str_split(preds_by_word$phon_prediction[i], ":")[[1]],
           phoneme_index = seq_len(n()),
           condition = "predicted")
  
  # process the target features
  # the dimensions are the same as predictions however (importantly)
  # the end of word segment is STILL PRESENT on the targets
  # as a result the last row of the tibble here should always be the end of word segment
  target = as_tibble(vector_from_string(preds_by_word$phon_target_features[i], flatten = F))
  colnames(target) = cols_
  
  # so, remove the last row (to get rid of the end of word segment)
  end_of_word = nrow(target)
  
  
  # stuck here:str_split(preds_by_word$phon_target[i], ":")[[1]]
  target = target %>% 
    slice(-end_of_word) %>%
    mutate(phoneme = str_split(preds_by_word$phon_target[i], ":")[[1]],
           phoneme_index = seq_len(n()),
           condition = "target")
  
  # process the raw activations (probabilities) of predictions (across features for each segment)
  # in these data the send of word segment should have been stripped off
  # and this prediction should match the dimensions of the `prediction` object above automatically 
  # note that it should also match the ith item in preds_by_word$phon_prediction (which has EOW segment stripped off already)
  raw = as_tibble(vector_from_string(preds_by_word$phon_prediction_probabilities[i], flatten = F))
  colnames(raw) = cols_
  
  raw = raw %>% 
    mutate(phoneme = str_split(preds_by_word$phon_prediction[i], ":")[[1]],
           phoneme_index = seq_len(n()),
           condition = "raw")
  
  out = rbind(prediction, raw, target) %>% 
    mutate(word = preds_by_word$word[i]) %>%
    select(epoch, word, phoneme, phoneme_index, condition, everything())
  
  phonemewise_measures[i][[1]] <- out
  
}




j = jsonlite::toJSON(phonemewise_measures, pretty = T)
writeLines(j, file('data/102923_phonemewise_measures.json'))

write_csv(preds_by_word, 'data/102923_preds_by_word.csv')




tmp = bind_rows(phonemewise_measures)

preds_by_phoneme = bind_rows(phonemewise_measures) %>%
  left_join(preds_by_word %>% 
              select(epoch, word, correct, in_validation_set, frequency = f), by = c("epoch", "word")) %>%
  select(epoch, word, correct, phoneme, phoneme_index, condition, in_validation_set, frequency, everything())


write_csv(preds_by_phoneme, 'data/102923_preds_by_phoneme.csv')
