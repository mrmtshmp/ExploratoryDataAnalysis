#' Resampling test for significance of mutual information between alpha-diversity index and some feature selection method.
#' This function uses MIPermute internally without permutation (this is controlled by setting *n.sim=1*)
#'
#'
#' @import dplyr
#' @import coin
#'
#' @param var.x Column name of phenotype data.
#' @param data A data.frame-class object in which phenotype data were input.
#' @param count.table A data.frame-class object of selected features.
#' @param ori.count.table  A data.frame-class object of features without selection.
#' @param itt.rsamp = itt.rsamp.wilcox,
#' @param func.stat = vegan::diversity,
#' @param ... Arguments to be passed to a function in func.stat and MIPermute function.
#'
#' @export


mf.rsamp.mutualinfo_test <-
  function(
    var.x="BVAS",
    data = pData.obj.aggTaxa.ADS,
    count.table = MRcounts(obj.aggTaxa.ADS),
    ori.count.table = MRcounts(obj.aggTaxa),
    itt.rsamp = itt.rsamp.wilcox,
    func.stat = vegan::diversity,
    MIPermute.method='emp',
    MIPermute.discretize.X="equalfreq",
    MIPermute.discretize.Y="equalfreq",
    MIPermute.alpha = 0.6,
    ...
  ){

    data[,"cmp.group"] <- data[,var.x]

    inc.sample.ID <-
      colnames(count.table)[
        colnames(count.table) %in% rownames(data)
        ]

    count.table <- count.table[,inc.sample.ID]
    ori.count.table <- ori.count.table[,inc.sample.ID]

    print(sprintf('IDs of analyzed samples:%s', paste(inc.sample.ID,collapse = ' ')))

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

    res.rsamp.mutualinfo_test <-
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

          res.mutualinfo <- ExploratoryDataAnalysis::MIPermute(
            n.sim = 1,
            Y=data[,'alpha_div.rsamp_i'],
            X=data[,'cmp.group'],
            S = 1,
            data = data,
            method = MIPermute.method,
            disc.X = MIPermute.discretize.X,
            disc.Y = MIPermute.discretize.Y,
            alpha = MIPermute.alpha
            )

          return(res.mutualinfo)
        }
      )
    return(res.rsamp.mutualinfo_test)
  }
