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


difference = function(x, y){
  
  return(x-y)
  
}

