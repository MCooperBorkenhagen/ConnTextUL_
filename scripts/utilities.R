options(scipen = 200)

phonlength = function(x, sep = ";"){
  
  return(str_count(x, sep))
  
}


vector_from_string = function(x, segment_separator = ";", unit_separator = ":", flatten = T, n_features = 33){
  
  a = str_split(x, segment_separator)
  b =  unlist(a)
  c = str_split(paste(b, collapse = unit_separator), unit_separator)[[1]]
  
  if (flatten){
    
    return(as.numeric(c)) 
    
  }
  
  if (!flatten){
    n_segments = length(b)
    n_features = str_count(b[1], unit_separator) + 1
    d = matrix(as.numeric(c), nrow = n_features, ncol = n_segments)
    return(t(d))
  }
}




strip_final_segment = function(x, units_per_segment = 33){
  return(x[1:(length(x)-units_per_segment)])}



L2 = function(x, y){
 return(sqrt(sum((x - y)^2)))
}


L2_sum = function(...){

  print(paste("Length:", length(c(...))))
  return(sqrt(sum((c(...))^2)))
  
}


L2x = function(...){
  
  #' This is a more forceful version of L2() which coerces NA values to 0
  
  args_ = c(...)
  x = args_[1]
  y = args_[2]
  
  
  if (length(args_) > 2){
    warning("More than two input arguments provided. Only the first two taken.\n")
  }
  if (is.na(args_[1])){
    x = 0
    warning("First input argument provided was NA - coreced to Zero.\n")
  }
  if (is.na(args_[2])){
    y = 0
    warning("Second input argument provided was missing/NA - coreced to Zero.\n")
  }
  
  return(sqrt(sum((x - y)^2)))
  
}



correct_unitwise_probabilities = function(predicted, target, strip_final = T, units_per_segment = 33){
  
  #' Make a correction on the unitwise probability vector with respect to the corresponding target.
  #' This function is useful when the probability values provided include probabilities for the terminal 
  #' segment at the end, and you need to strip that off so that you can compare it to its target.
  #' @param predicted This is the vector of probabilities from a prediction.
  #' This parameter may also be the prediction itself, in which case you'll likely adjust the strip_final parameter
  #' @param target The target to be used for the correction, such that element i in target corresponds to
  #' element i in predicted.
  #' @param strip_final Specify whether or not to strip the final set of elements off of the predicted
  #' vector. The number of elements to be stripped off is specified in units_per_segment (default is TRUE)
  #' @param units_per_segment How many units comprise a segment (i.e., phoneme). This defines the number
  #' to be stripped of the end of predicted if strip_final is set to TRUE.
  #' @returns A vector representing the unitwise accuracy of the predicted vector.
  
  if (strip_final){
    predicted = strip_final_segment(predicted, units_per_segment = units_per_segment)
  }
  
  unitwise_accuracy = c()
  for (e in seq(length(target))){
    if (target[e] == 0){
      unitwise_accuracy =  c(unitwise_accuracy, 1-predicted[e])
      
    }  else {unitwise_accuracy =  c(unitwise_accuracy, predicted[e])}
  }
  
  return(unitwise_accuracy)
  
}


L2c = function(...){
  
  #' An elementwise application of L2 (rather than vector-wise)
  #' @param ... Numeric elements over which to calculated Euclidean distance
  #' @returns The square root of the sum of the squared elements provided in ...
  x = c(...)
  return(sqrt(sum((x)^2)))
  
}

summarise_across_ends_with <- function(data, suffix, fun_) {
  
  grouping_vars <- data %>% groups()
  
  data %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(across(ends_with(suffix), fun_))
  
  # data %>%
  #   group_by(across(.vars = everything())) %>%
  #   summarise(across(ends_with(suffix), fun_))
  
  }



difference = function(...){
  
  args_ = c(...)
  x = args_[1]
  y = args_[2]
  
  if (length(args_) > 2){
    warning("More than two input arguments provided. Only the first two taken.\n")
  }
  if (is.na(args_[1])){
    x = 0
    warning("First input argument provided was NA - coreced to Zero.\n")
  }
  if (is.na(args_[2])){
    y = 0
    warning("Second input argument provided was missing/NA - coreced to Zero.\n")
  }
  
  return(x-y)
  }


stress_pattern = function(x){
  
  return(paste(str_extract_all(x, "\\d+")[[1]], collapse = ""))
  
}


phon_features = c("labial_feature", "dental_feature", "alveolar_feature", "palatal_feature", "velar_feature",
                  "glottal_feature", "stop_feature", "fricative_feature", "affricate_feature", "nasal_feature",
                  "liquid_feature", "glide_feature", "rhotic_feature", "tap_feature", "voice_feature", "front_feature",
                  "center_feature", "back_feature", "close_feature", "close_mid_feature", "mid_feature", 
                  "open_mid_feature", "near_open_feature", "open_feature", "tense_feature", "retroflex_feature",
                  "round_feature", "post_y_feature", "post_w_feature", "primary_feature", "secondary_feature",
                  "sos_feature", "eos_feature")

padded_distance = function(predicted, target, value = 0, strip_target = T, units_per_segment = 33){
  
  predicted = vector_from_string(predicted)
  target = vector_from_string(target)
  
  #' Pad two vectors and calculate the distance between them using L2
  #' @param predicted The vector representing the prediction
  #' @param target The vector representing the target
  #' @param value The value to be used for the pad (default is 0)
  #' @param strip_target Should the final segment be stripped off the target (the end-of-word segment; default is TRUE)
  #' @param units_per_segment The number of units expected per phoneme segment (default is 33)
  
  #' @returns The square root of the sum of the squared elements provided in ...
  
  if (strip_target){
    target = strip_final_segment(target, units_per_segment = units_per_segment)
  }
  
  max_ = max(length(predicted), length(target))
  predicted_pad = c(predicted, rep(value, max_ - length(predicted)))
  target_pad = c(target, rep(value, max_ - length(target)))
 
  return(L2(predicted_pad, target_pad))
  
}



