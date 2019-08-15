#' Conduct fitZIG analysis (metagenomeSeq paclage) with selected subpopulation
#'
#' @import metagenomeSeq
#' @import tibble
#'
#' @param obj <object; input> A metagenomeSeq-class object.
#' @param var.select  <object> A variable name for selection of subpopulation.
#' @param condition   Levels in var.select variable of to be selected sub population.
#' @param var.analyze <character> A variable name for independent variable in analysis model.
#' @param rareTrimming <numeric> Taxa with the lesser number of subjects in which the taxa were detected are trimmed.
#' @param fit.zig.settings <numeric> Pass to fitZig function.
#'
#'
#' @export


fitzig.select <- function(
  obj,
  var.select  = Type,
  condition   = c(3,4),
  var.analyze = "ACPA",
  rareTrimming,
  fit.zig.settings
  ){
  cumNorm.obj.aggTaxon <- obj

  pData(cumNorm.obj.aggTaxon)$.Type <-
    pData(cumNorm.obj.aggTaxon)[,var.select]

  .condition <-
    condition

  cumNorm.obj.aggTaxon <- cumNorm.obj.aggTaxon[
    ,
    which(
      pData(
        cumNorm.obj.aggTaxon
        )$.Type %in%
        .condition
      )
    ]


  obj.aggTaxon.ACPA_posi   <-  factor(
    pData(
      cumNorm.obj.aggTaxon)[,var.analyze]
    )

  mod.ACPA_posi <-  model.matrix(
    ~ obj.aggTaxon.ACPA_posi
    )

  rareFeatures <- which(
    rowSums(
      MRcounts(cumNorm.obj.aggTaxon) > 0 # TRUE=1; FALSE=0
      ) < rareTrimming
    )

  cat(
    sprintf("Rare features are excluded from analysis :%s \n",rareFeatures)
    )


  obj.fitzig = cumNorm.obj.aggTaxon[
    which(
      rowSums(
        MRcounts(
          cumNorm.obj.aggTaxon) >0
        ) > rareTrimming
      ) ,
    ]

  fit.obj.aggTaxon.ACPA_posi = fitZig(
    obj = obj.fitzig,
    mod = mod.ACPA_posi,
    useCSSoffset = FALSE, # explicitly written in the model specification ("+ normFactor")
    control = fit.zig.settings
    )

  print(colnames(MRcounts(cumNorm.obj.aggTaxon)))


  res.fitZig.obj.aggTaxon_full.ACPA_posi <- MRfulltable(
    fit.obj.aggTaxon.ACPA_posi,
    taxa = rownames(
      fData(obj.fitzig)
      ),
    coef   = 2,
    number = length(
      rownames(
        fData(obj.fitzig)
        )
    ),
    eff       = effFilt,
    numberEff = TRUE
  ) %>%
    rownames_to_column("OTU") %>%
    left_join(
      fData(obj.fitzig) %>%
        rownames_to_column("OTU")
    )

  return(
    list(
      fit.obj.aggTaxon.ACPA_posi,
      res.fitZig.obj.aggTaxon_full.ACPA_posi
      )
    )
  }

