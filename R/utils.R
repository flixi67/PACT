'%nin%' <- function(x,y) { !('%in%'(x,y)) }

### mutate values in PKO and Activity based on inputs (lists)

input_aggregate <- function(var, input_list) {
  l <- length(input_list)
  var[var %nin% unlist(input_list)] <- NA
  for (i in 1:l) {
    var[var %in% input_list[[i]]] <- names(input_list)[i]
  }
  return(var)
}
