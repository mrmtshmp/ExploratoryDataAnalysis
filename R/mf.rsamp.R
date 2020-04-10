#' Resampling method for significance evaluation of some feature selection method.
#'
#'
#' @import tidyr
#' @import magrittr
#' @import dplyr
#' @import tibble
#' @import robustbase
#' @import boot
#'
#' @param var.x A character string to be RHS of formula in func.im.
#' @param data A data.frame-class object in which phenotype data were input.
#' @param count.table A data.frame-class object of selected features.
#' @param ori.count.table  A data.frame-class object of features without selection.
#' @param itt.rsamp The number of itteration.
#' @param func.stat A function object to compute summary of features (DEFAULT = vegan::diversity)
#' @param func.lm  A function object to
#' @param lm.summary.statistics = vegan::diversity,
#' @param list.do.call.func.stat list of arguments to be input func.stat.
#' @param list.do.call.func.lm list of arguments to be input func.stat.
#' @param ... so far ineffective.
#'
#' @export


mf.rsamp.lm_test <-
  function(
    var.x= "BVAS",
    data = pData.obj.aggTaxa.ADS,
    count.table = MRcounts(obj.aggTaxa.ADS),
    ori.count.table = MRcounts(obj.aggTaxa),
    itt.rsamp = itt.rsamp.wilcox,
    func.stat = vegan::diversity,
    func.lm = robustbase::lmrob,
    lm.summary.statistics="coefficients",
    boot.estimate = TRUE,
    boot.method   = "ordinary",
    var.estimate = "Estimate",
    confint = NA,
    nR      = 500,
    list.do.call.func.stat = list(index="simpson"),
    list.do.call.func.lm = list(method="MM"),
    ...
  ){

    if(!(var.x %in% names(data))) stop("var.x is not in data.")

    fml=as.formula(sprintf("alpha_div.rsamp_i ~ %s", var.x))


    data[,"cmp.group"] <- data[,var.x]

    inc.sample.ID <-
      colnames(count.table)[
        colnames(count.table) %in% rownames(data)
        ]


    vec.rowSumPosi <- apply(
      count.table,
      1,
      FUN = function(x){
        return(sum(x)>0)
      }
    )


    fun.statistic <- function(data, ind){
      res.lm <-
        try(
          do.call(
            func.lm,
            args = c(
              list(fml, data[ind,]),
              list.do.call.func.lm
            )
          )
        )

      if(class(res.lm)[1]!="try-error"){
        statistics <-
          summary(res.lm)[[lm.summary.statistics]] %>%
          data.frame() %>%
          rownames_to_column("terms")

        return(statistics[,var.estimate])
      }
    }

    count.table <- count.table[,inc.sample.ID]
    ori.count.table <- ori.count.table[,inc.sample.ID]

    ori.count.table <- ori.count.table[vec.rowSumPosi,]


    print(sprintf('IDs of analyzed samples:%s', paste(inc.sample.ID,collapse = ' ')))

    df.itt <-
      data.frame(
        'dummy'=1, 'itt' = seq(1:itt.rsamp)
      )


    res.rsamp.summary.lm <-
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
              function(mat)
                do.call( func.stat, args = c(list(x = mat), list.do.call.func.stat))
            )

          res.lm <-
            try(
              do.call(func.lm, args = c(list(fml, data), list.do.call.func.lm))
            )

          if(class(res.lm)[1]!="try-error"){

            statistics <-
              summary(res.lm)[[lm.summary.statistics]] %>%
              data.frame() %>%
              rownames_to_column("terms")

            if(itt$itt==1){
              if(boot.estimate){
                bsRegr <- boot(
                  data,
                  statistic = fun.statistic,
                  R=nR,
                  sim  = boot.method,
                  stype = "i"
                )
                df.bsRegr <- data.frame(
                  Original = bsRegr$t0,
                  Bias = apply(bsRegr$t, 2, mean) - bsRegr$t0,
                  Std.Error = apply(bsRegr$t, 2, sd)
                )
                print(df.bsRegr)

                statistics[,"var.estimate"] <- statistics[,var.estimate]

                statistics <- full_join(statistics,df.bsRegr, by=c("var.estimate"="Original"))
              }
            }
            return(statistics)
          }
        }
      )
    return(res.rsamp.summary.lm)
  }
