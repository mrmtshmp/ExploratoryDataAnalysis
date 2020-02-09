#' Extract rows with rowSum >0, then extract cols with colSum>0.
#'
#' @param data A data.frame-class object in which phenotype data were input.
#'
#' @export


mf.cross_posiSum <- function(data){
  for(i in 1:2){
    print(dim(data)[i])
    print(i)
    vec.sumPosi <- apply(
      X = data, MARGIN = i,
      FUN = function(x){
        return(sum(x)>0)
      }
    )
    if(i==1) data <- data[vec.sumPosi,]
    if(i==2) data <- data[,vec.sumPosi]
  }
  return(data)
}
