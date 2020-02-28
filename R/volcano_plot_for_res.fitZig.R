
#' Calculate distance and extract subjects
#'
#' @importFrom GUniFrac GUniFrac
#' @importFrom vegan vegdist
#' @import tibble
#' @import plyr
#' @import dplyr
#'
#' @param data A matrix-class object
#' @param aggTaxonLvl <character; input>
#' @param plot.out FALSE
#' @param x.margin default:0.25
#' @param y.margin default: 0.25
#' @param ggplot.theme default: theme_bw(),
#' @param cbPalette
#'
#' @export


mf.volcano_plot <- function(
  data,
  aggTaxonLvl= "Genus",
  plot.out=FALSE,
  x.margin=0.25,
  y.margin=0.25,
  ggplot.theme=theme_bw(),
  cbPalette = c("#56B4E9","#D55E00", "#999999",  "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#00AFBB", "#E7B800", "#FC4E07")
){
  colnames(data)[2] <- "Effect"
  colnames(data)[3] <- "pvalue"
  # colnames(data)[2] <- "posi_gr_0"
  # colnames(data)[3] <- "posi_gr_1"

  data[order(data$pvalue),"SeqID"]   <- c(1:nrow(data))

  data$aggTaxonLvl   <- aggTaxonLvl

  data <- data %>%
    # mutate(
    #   NoAgg = ifelse(
    #     aggTaxonLvl=="NoAgg",
    #     sprintf("%s,%s",Genus, Species),
    #     ""),
    #   colTaxon = sprintf("%s, %s, %s", Kingdom, Phylum, Class)
    # ) %>%
    mutate(
      Epaulet= #ifelse(
        # pvalues< 0.05, # | OTU %in% colTaxon_opt,
        SeqID,
      # eval(
      #   parse(
      #     text=aggTaxonLvl
      #     )
      #   ),
      # ""
      # ),
      PointSize= #if_else(
        # posi_gr_0 < 3 | posi_gr_0 >= 3,
        # 0,
        1
      # )
    )

  # if(
  #   !is.null(colTaxon_opt)
  # ){
  #   res.fitZig.obj.aggTaxon_full <- res.fitZig.obj.aggTaxon_full %>%
  #     mutate(
  #       colTaxon = ifelse(
  #         OTU %in% colTaxon_opt,
  #         3,
  #         1
  #       )
  #     )
  # }

  gg.data <- data %>%
    ggplot(
      aes(
        x = Effect, y = -log2(pvalue),
        # group=OTU,
        label=Epaulet
      )
    )

  geom <- geom_point(
    col="black",size=3#,
    # aes(col=factor(colTaxon))#,size=PointSize)
  )

  limit_x <- scale_x_continuous(
    limits = c(
      min(
        data$Effect
      ),
      max(
        data$Effect
      ) + x.margin
    )
  )
  limit_y <- scale_y_continuous(
    limits = c(
      min(
        -log2(
          data$pvalue
        )
      ),
      max(
        -log2(
          data$pvalue
        )
      ) + y.margin
    )
  )

  summ_hist_theme.Volcano <- ggplot.theme

  # summ_hist_theme.Volcano$legend.text$family <- NULL
  # summ_hist_theme.Volcano$legend.text$face <- NULL
  # coord_cartesian(
  #   xlim = c(0, 100), ylim = c(10, 20)
  #   )

  # summ_hist_theme.Volcano$legend.text$angle <- 0
  # summ_hist_theme.Volcano$strip.text.x$size <- 45

  text_overlay <-  geom_text_repel(size=8, angle=0, segment.size = 0, direction="y",force = 7) #, nudge_x=0.5) #geom_text(
  #   aes(
  #     label=Epaulet,
  #     hjust=-0.1,
  #     vjust=-0.1,
  # #    size=8,
  #     angle= 45
  #     ),
  #   size= 3,
  #   position=position_jitter(width=0.01,height=0.01)
  #   )

  geom_significance <- geom_hline(yintercept = -log2(0.05))

  g_legend <- function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    legend
  }


  plot.body <- plot(
    gg.data +
      geom +
      summ_hist_theme.Volcano +
      text_overlay +
      limit_x + limit_y +
      geom_significance +
      #    scale_color_viridis(option="A") +
      scale_color_manual(values=cbPalette) +
      theme(
        axis.title = element_text(size = 28),
        axis.title.x =  element_text(family = "Arial Black", size = 20),#"Log2 FC"),
        axis.title.y =  element_text(family = "Arial Black", size = 20),#"log2 p-value (FDR adjusted)"),
        axis.text.x = element_text(family = "Arial Black", size = 20),
        axis.text.y = element_text(family = "Arial Black", size = 20)
      )
  )

  #  legend <- g_legend(plot.body)

  return(
    list(
      plot.body#,
      #      g_legend(plot.body)
    )
  )

  if(plot.out){
    quartz(
      type="pdf",
      file=sprintf(
        fmt = "./%s_aggTaxon_%s_Trimmed_%s.pdf",
        "Volcano_plot",
        aggTaxonLvl,
        rareTrimming
      ),
      pointsize = 3,
      width=12,
      height=10
    )

    par(family="Arial Black")

    # grid.arrange(
    plot(
      plot.body + theme(legend.position = 'none')
    )
    #   ,
    #   legend,
    #   ncol=2,
    #   nrow=1 ,
    #   widths=c(1/2, 1/2)
    # )
    dev.off()
  }
}

#' Calculate distance and extract subjects
#'
#' @importFrom GUniFrac GUniFrac
#' @importFrom vegan vegdist
#' @import tibble
#' @import plyr
#' @import dplyr
#'
#' @param res.fitZig.obj.aggTaxon_full A matrix to calculate distance
#' @param aggTaxonLvl <character; input>
#'
#' @export

volcano_plot_for_res.fitZig <- function(
  res.fitZig.obj.aggTaxon_full= res.fitZig.obj.aggTaxon_full,
  aggTaxonLvl="Genus"
  ){
  cbPalette <- c("#56B4E9","#D55E00", "#999999",  "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#00AFBB", "#E7B800", "#FC4E07")

  colnames(res.fitZig.obj.aggTaxon_full)[11] <- "Effect"
  colnames(res.fitZig.obj.aggTaxon_full)[2] <- "posi_gr_0"
  colnames(res.fitZig.obj.aggTaxon_full)[3] <- "posi_gr_1"

  res.fitZig.obj.aggTaxon_full[order(res.fitZig.obj.aggTaxon_full$pvalues),"SeqID"]   <- c(1:nrow(res.fitZig.obj.aggTaxon_full))

  res.fitZig.obj.aggTaxon_full$aggTaxonLvl   <- aggTaxonLvl

  res.fitZig.obj.aggTaxon_full <- res.fitZig.obj.aggTaxon_full %>%
    mutate(
      NoAgg = ifelse(
        aggTaxonLvl=="NoAgg",
        sprintf("%s,%s",Genus, Species),
        ""),
      colTaxon = sprintf("%s, %s, %s", Kingdom, Phylum, Class)

      # ifelse(
      # aggTaxonLvl=="Family",
      # sprintf("%s, %s, %s, %s", Kingdom, Phylum, Class, Order),
      # ifelse(
      #   aggTaxonLvl=="Genus",
      #   sprintf("%s, %s, %s, %s, %s", Kingdom, Phylum, Class, Order, Family),
      #   ifelse(
      #     aggTaxonLvl=="NoAgg",
      #     sprintf("%s, %s, %s, %s, %s, %s, %s", Kingdom, Phylum, Class, Order, Family, Genus, Species),
      #     "")
      #  )
      #)

    ) %>%
    mutate(
      Epaulet=ifelse(
        pvalues< 0.05 | OTU %in% colTaxon_opt,
        SeqID,
        # eval(
        #   parse(
        #     text=aggTaxonLvl
        #     )
        #   ),
        ""
      ),
      PointSize=if_else(
        posi_gr_0 < 3 | posi_gr_0 >= 3,
        0,
        1
      )
    )

  if(
    !is.null(colTaxon_opt)
  ){
    res.fitZig.obj.aggTaxon_full <- res.fitZig.obj.aggTaxon_full %>%
      mutate(
        colTaxon = ifelse(
          OTU %in% colTaxon_opt,
          3,
          1
        )
      )
  }

  gg.res.fitZig.obj.aggTaxon_full <- res.fitZig.obj.aggTaxon_full %>%
    ggplot(
      aes(
        x = Effect, y = -log10(pvalues),
        group=OTU,
        label=Epaulet
      )
    )

  geom <- geom_point(
    col="black",size=3,
    aes(col=factor(colTaxon))#,size=PointSize)
  )

  limit_x <- scale_x_continuous(
    limits = c(
      min(
        res.fitZig.obj.aggTaxon_full$Effect
      ),
      max(
        res.fitZig.obj.aggTaxon_full$Effect
      ) + 1
    )
  )
  limit_y <- scale_y_continuous(
    limits = c(
      min(
        -log10(
          res.fitZig.obj.aggTaxon_full$pvalues
        )
      ),
      max(
        -log10(
          res.fitZig.obj.aggTaxon_full$pvalues
        )
      ) + 0.5
    )
  )

  summ_hist_theme.Volcano <- summ_hist_theme

  # summ_hist_theme.Volcano$legend.text$family <- NULL
  # summ_hist_theme.Volcano$legend.text$face <- NULL
  # coord_cartesian(
  #   xlim = c(0, 100), ylim = c(10, 20)
  #   )

  summ_hist_theme.Volcano$legend.text$angle <- 0
  summ_hist_theme.Volcano$strip.text.x$size <- 45

  text_overlay <-  geom_text_repel(size=8, angle=0, segment.size = 0, direction="y",force = 7) #, nudge_x=0.5) #geom_text(
  #   aes(
  #     label=Epaulet,
  #     hjust=-0.1,
  #     vjust=-0.1,
  # #    size=8,
  #     angle= 45
  #     ),
  #   size= 3,
  #   position=position_jitter(width=0.01,height=0.01)
  #   )

  geom_significance <- geom_hline(yintercept = -log10(0.05))

  g_legend <- function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    legend
  }

  plot.body <- plot(
    gg.res.fitZig.obj.aggTaxon_full +
      geom +
      summ_hist_theme.Volcano +
      text_overlay +
      limit_x + limit_y +
      geom_significance +
      #    scale_color_viridis(option="A") +
      scale_color_manual(values=cbPalette) +
      theme(
        axis.title = element_text(size = 28),
        axis.title.x =  element_text(family = "Arial Black", size = 20),#"Log2 FC"),
        axis.title.y =  element_text(family = "Arial Black", size = 20),#"Log10 p-value (FDR adjusted)"),
        axis.text.x = element_text(family = "Arial Black", size = 20),
        axis.text.y = element_text(family = "Arial Black", size = 20)
      )
  )

  #legend <- g_legend(plot.body)

  return(
    list(
      plot.body#,
      #g_legend(plot.body)
      )
    )
}

# quartz(
#   type="pdf",
#   file=sprintf(
#     fmt = "./%s_aggTaxon_%s_Trimmed_%s.pdf",
#     "Volcano_plot",
#     aggTaxonLvl,
#     rareTrimming
#   ),
#   pointsize = 3,
#   width=12,
#   height=10
# )
#
# par(family="Arial Black")
#
# grid.arrange(
#   plot.body + theme(legend.position = 'none'),
#   # legend,
#   ncol=2,
#   nrow=1 ,
#   widths=c(1/2, 1/2)
# )
#
# dev.off()
