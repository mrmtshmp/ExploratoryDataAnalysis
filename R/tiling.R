#' Make heatmap-like tile plot. (wrapper of gplots::heatmap2)

#'
#' @importFrom gplots heatmap.2
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom grDevices dev.off
#' @importFrom grDevices quartz
#' @importFrom graphics axis
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom graphics rect
#' @importFrom stats cmdscale
#' @importFrom stats as.dendrogram
#' @importFrom stats dist hclust
#' @importFrom stats hclust
#'
#' @import dplyr
#' @import ggplot2
#' @param fn.output.pdf <character; output> File name of PDF output.
#' @param surfix.maintitle <character; output> Surfix of Main title in each page.
#' @param bin <numeric; processing>
#' @param n.breaks <numeric; processing; prefix =20>
#' @param col.bias <numeric; processing; prefix =2> Colour brewer setting
#' @param breaks  <numeric>
#' @param row.adjust <numeric>
#' @param count_table <object; input data; mandatory> A data.frame with features as column and subjects as rows.
#' @param physi_table <object; input data> A data.frame with 2 column of a feature and subject ID. This ID must be identical with of count_table.
#' @param filt.Features <character strings; input data> Select column of count_table. If NULL, all features are selected.
#' @param unitHierarchy <for future update...> c('Kingdom', 'Phylum', 'Class', 'Order',  'Family', 'Genus', 'Species', "NoAgg"),
#' @param method.dist.Row   = c('manhattan','euclidean'),
#' @param method.hclust.Row = c('ward.D2'),
#' @param method.dist.Col   = c('manhattan','euclidean'),
#' @param method.hclust.Col = c('ward.D2')
#'
#' @export

# func.tilemap ------------------------------------------------------------
#
#  modified from func.data_for_heatmap_v4 (crea. 180719)
#

func.tilemap <- function(
  fn.output.pdf='Tile',
  surfix.maintitle = 'Group: ',
  bin           = FALSE,
  n.breaks      = 20,
  col.bias      = 2,
  breaks        = NULL,
  row.adjust    = 0,
  count_table,
  physi_table,
  filt.Features = NULL,
  unitHierarchy = c('Kingdom', 'Phylum', 'Class', 'Order',  'Family', 'Genus', 'Species', "NoAgg"),
  method.dist.Row   = c('manhattan','euclidean'),
  method.hclust.Row = c('ward.D2'),
  method.dist.Col   = c('manhattan','euclidean'),
  method.hclust.Col = c('ward.D2')
  #  MRexperiment-class object
  #  aggregated data
  #  (obtained from "aggregateByTaxonomy")
  ){

  # preproccessing input data

  .physi_table <- physi_table
  colnames(.physi_table) <- c('ID', 'subj.group')
  if(
    !is.factor(.physi_table[,'subj.group'])
    ) .physi_table <-
    .physi_table %>%
    mutate(
      subj.group = factor(subj.group, exclude=NULL)
      )

  if(
    is.null(filt.Features)
    ) .filt.Features <- colnames(count_table)

  for(
    i in 1: length(levels( .physi_table$subj.group))
    ){
    print(.physi_table[
      as.numeric(.physi_table$subj.group) == i,
      'ID'
      ]
      )
    if(
      length(
        .physi_table[
          as.numeric(.physi_table$subj.group) == i,
          'ID'
          ]
        ) < 2
    ) {
      stop(
        sprintf(
          "%s is a Level of n < 2.",
          levels(.physi_table$subj.group)[i]
          )
        )
      }
    }


  unitHieral <- unitHierarchy


  #  Count table of each group of subjects
  #  and dendrogram (for clustering by column)

  for(
    i in 1: length(
      levels( .physi_table$subj.group)
      )
    ){
    .filt.ID <- .physi_table[
      as.numeric(.physi_table$subj.group) == i,
      'ID'
      ]

    OxUdata <- count_table %>%

      dplyr::filter(
        rownames(count_table) %in%
          .filt.ID
        ) %>%

      t() %>%
      as.data.frame() %>%
      rownames_to_column(
        "Features"
        ) %>%
      filter(
        Features %in% .filt.Features
        ) %>%
      column_to_rownames(
        "Features"
        )

    colv <- hclust(
      dist(
        t(OxUdata),
        method=method.dist.Col
        ),
      method = method.hclust.Col
      ) %>%
      as.dendrogram()

    assign(sprintf('%s_%s', 'OxUdata', i), OxUdata)
    assign(sprintf('%s_%s', 'colv', i), colv)
    assign(
      sprintf('group_%s', i),
      levels(.physi_table$subj.group)[i]
      )
  }
  print(group_1)

  #  Count table of all subjects
  #  and dendrogram (for clustering by row)

  OxUdata_all <- count_table %>%

    # dplyr::filter(
    #   rownames(count_table) %in%
    #     .filt.ID
    # ) %>%

    t() %>%
    as.data.frame() %>%
    rownames_to_column(
      "Features"
    ) %>%
    filter(
      Features %in% .filt.Features
    ) %>%
    column_to_rownames(
      "Features"
    )

  rowv <- hclust(
    dist(
      as.matrix(OxUdata_all),
      method = method.dist.Row
      ),
    method = method.hclust.Row
    ) %>%
    as.dendrogram()

  # Brew colors
  # 1. Define breaks of colors
  # 2.

  if(is.null(breaks)){
    all_num <- unique(OxUdata_all)
      # unlist(c(OTUdata_AAV,OTUdata_SC))
      # )

    max_brk <- max(all_num)

    breaks  <-
      c(0,
        seq(
          min(
            all_num[all_num > 0]
          )-0.2,
          max_brk,
          length.out = n.breaks
        )
      )
  }
  print(breaks)

  col_1 = grDevices::colorRampPalette(
    c(
      paste0(
        "gray",
        seq(0,90,n.breaks)[
          order(
            seq(0,90,n.breaks),
            decreasing=TRUE)
          ]
      )
    ),
    space = "rgb",
    interpolate =
      c("linear"),
    bias=col.bias
    )

  # Output

  quartz(
    type="pdf",
    file=sprintf(
      fmt = "%s.pdf",
      fn.output.pdf
      ),
    width=15,
    height=15)

  par(family="Arial Black")

  nr <- nrow(OxUdata_1) + row.adjust

  for(
    i in 1: length(
      levels( .physi_table$subj.group)
      )
    ){
    OxUdata    <- get(sprintf('%s_%s', 'OxUdata', i))
    colv       <- get(sprintf('%s_%s', 'colv', i))
    group_name <- get(
      sprintf(
        'group_%s', i
        )
      )

    plot.tiles <-
      heatmap.2(
        as.matrix(OxUdata),
        Rowv = rowv,
        Colv = colv,
        col = c("white",col_1(n.breaks-1)),
        scale = c("none"),
        trace=c("none"),# level trace
        cexRow = 5/log10(nr*10),
        density.info=c("none"),
        key = F,
        #      cexRow=1,
        breaks = breaks,
        main=sprintf('%s: %s', surfix.maintitle,group_name),
        margins = c(5,25),
        srtRow=0,
        sepwidth=c(0.005,0.005),
        sepcolor="gray50",
        colsep=0:ncol(OxUdata),
        rowsep=0:nrow(OxUdata)
      )
    }

  options(scipen = TRUE)

  # Colour Key

  ori.par    <- par()
  par(omi = c(0,5,0,0))
  plot(NULL, xlim=c(0,0.1), ylim=c(0,(n.breaks+1)), axes=FALSE, xlab="", ylab="")
  rect( 0, 1:n.breaks, 0.1, 2:(n.breaks+1), col = c("white",col_1(n.breaks)), border=NA)
  axis(
    side=2,
    at = 1:(n.breaks+1),
    las = 1, # always horizontal,
    labels=round(breaks,0)
  )
  dev.off()
}


