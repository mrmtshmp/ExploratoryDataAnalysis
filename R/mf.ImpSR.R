#' Interpolation.
#' @import  magrittr
#'
#' @param data A data.frame-class object.
#'
#' @export

mf.SRIntporbyWideData <-
  function(
    data
    ){
    result <-
      apply(
        data,
        MARGIN = 1,
        function(vec){
          pos_miss <- which(is.na(vec))
          if(length(pos_miss) > 0){
            if(
              min(match(names(pos_miss),col.visit)) > 1 &
              max(match(names(pos_miss),col.visit)) < length(col.visit)
              ){
              vec[pos_miss] <-
                (as.numeric(vec[pos_miss-1]) +
                   as.numeric(vec[pos_miss+1])
                 )/2
              }
            }
          return(vec)
          }
        ) %>%
      t()
    for(i in 1:length(data)){
      class_i <- class(data[,i])
      print(class_i)
      eval(
        sprintf("result[,i] <- as.%s(result[,i])", class(data[,i]))
        )
      print(class(result[,i]))
      }
    return(result)
  }
