'%nin%' <- function(x,y) { !('%in%'(x,y)) }

### mutate values in PKO and Activity based on inputs (lists)

input_aggregate_m <- function(var, input_list) {
  l <- length(input_list)
  var[var %nin% unlist(input_list)] <- NA
  for (i in 1:l) {
    var[var %in% input_list[[i]]] <- paste("Mission group", i)
  }
  return(var)
}

input_aggregate_a <- function(var, input_list) {
  l <- length(input_list)
  var[var %nin% unlist(input_list)] <- NA
  for (i in 1:l) {
    var[var %in% input_list[[i]]] <- paste("Activities", i)
  }
  return(var)
}
