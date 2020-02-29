#' Make many boxplots from tidy ordering sheet
#'
#' @import dplyr
#' @import coin
#'
#' @param df.ADS <object; input> A data frame
#' @param df.input_for_cmp_2levels <object; input> A data frame with variables (var.x, var.y, str)
#'
#' @export


mf.rsamp.wilcox_test.statistic <-
  function(
    var.x="Disease",
    data = pData.obj.aggTaxa.ADS,
    count.table = MRcounts(obj.aggTaxa.ADS),
    ori.count.table = MRcounts(obj.aggTaxa),
    itt.rsamp = itt.rsamp.wilcox,
    func.stat = vegan::diversity,
    ...
  ){

    if(ncol(ori.count.table) > ncol(count.table)){
      ori.count.table <-
        ori.count.table[,colnames(count.table)]
      }


    data[,"cmp.group"] <- data[,var.x]

    df.itt <-
      data.frame(
        'dummy'=1, 'itt' = seq(1:itt.rsamp)
      )


    vec.rowSumPosi <- apply(
      count.table,
      1,
      FUN = function(x){
        return(sum(x)>0)
      }
    )

    res.rsamp.wilcox_test..statistic..standardizedlinearstatistic <-
      ddply(
        df.itt,
        .(itt),
        .progress = "text",

        function(itt){
          if(itt$itt==1){
            rsamp.MRcounts.obj.aggTaxa <- count.table
          }else{
            rsamp.MRcounts.obj.aggTaxa <-
              ori.count.table[
                sample(
                  x = 1:nrow(ori.count.table),
                  size = sum(vec.rowSumPosi)
                ),
                ]
          }

          data$alpha_div.rsamp_i <-
            apply(
              rsamp.MRcounts.obj.aggTaxa, MARGIN = 2,
              func.stat, ...
            )

          res.wilcox_test <- coin::wilcox_test(
            alpha_div.rsamp_i ~ factor(cmp.group), data=data, distribution="exact"
          )

          statistics <- res.wilcox_test@statistic@standardizedlinearstatistic

          return(statistics)
        }
      )
    return( res.rsamp.wilcox_test..statistic..standardizedlinearstatistic)
  }
