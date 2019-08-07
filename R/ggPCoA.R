#' PCoA plot via cmdscale
#'
#' @import ggplot2
#'
#' @param distmat <object; input data; mandatory> An object with dist-class.
#' @param groups <character; output> A varible used forcolor coding.
#' @param ggplot_theme <object; proccessing>
#' @param axes <character strings; output; prefix = c("CS11","CS2")>  Labels for x and y axes.
#' @param overlay <logical; output; prefix =FALSE> Print subject IDs on respective plots?
#' @param labels <; processing; prefix =2> Colour brewer setting
#' @param title <object; input data; mandatory> A data.frame with features as column and subjects as rows.
#' @param legend.name <object; input data> A data.frame with 2 column of a feature and subject ID. This ID must be identical with of count_table.
#' @param jitter.v <numeric; proccessing; prefix=0.3>
#' @param jitter.h <numeric; proccessing; prefix=0.3>
#' @param size <numeric; proccessing; prefix=2>
#' @param alpha <numeric; proccessing; prefix=0.7>
#'
#' @export


gg_PCoA <- function(
  distmat,
  groups,
  ggplot_theme = summ_plot_theme,
  axes = c("CS1", "CS2"),
  overlay = FALSE,
  labels  = "sample_ID",
  title="PCoA plot using by MDS",
  legend.name = "Cluster",
  jitter.v = 0.3,
  jitter.h = 0.3,
  size  = 2,
  alpha = 0.7
){

  .size  = size
  .alpha = alpha
  map <- cmdscale(distmat) %>% #, k=100) %>%
    data.frame()

  colnames(map) <- axes

  map <- cbind(map,groups)

  if(overlay){
    if(labels=="sample_ID"){
      map$sample <- rownames(map)
    }else{
      map$sample <- labels
    }
  }

  p       <- ggplot(map, aes_string(x=axes[1], y=axes[2]))
  geom    <- geom_jitter(
    aes(
      color= factor(groups)#,
#      shape= groups
      ),
    alpha=.alpha,
    size =.size,
#    position = 'jitter',
    width = jitter.h,
    height = jitter.v
  )
  if(overlay){
    text_overlay <-  geom_text(
      aes(label=sample),
      hjust=-0.1
    )
  }
  hline      <- geom_hline(yintercept=0,linetype=2)
  vline      <- geom_vline(xintercept=0,linetype=2)
  legend     <- scale_color_discrete(name=legend.name)
  theme      <- ggplot_theme
  main_title <- labs(title=title)

  if(overlay){
    p + geom + text_overlay + hline + vline + legend + theme + main_title # + stat_density2d(aes(color=groups), h=0.15)
  }else{
    p + geom + hline + vline + legend + theme + main_title# + stat_density2d(aes(colour=groups), h = 0.15)
  }
}
