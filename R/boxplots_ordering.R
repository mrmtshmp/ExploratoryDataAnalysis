#' Make many boxplots from tydy ordering sheet
#'
#' @import ggplot2
#'
#' @param data <object; input> A data frame with variables (ind, var.x, var.y, trans.y, trans.x, var.col, str, dn.surfix)
#' @param ggdata <object; input> A object with ggplot-class
#' @param var.x <character; proccessing>
#' @param var.y <character; proccessing>
#' @param size <numeric; proccessing>
#' @param var.col <character; proccessing>
#' @param plot.col <character; proccessing>
#' @param box.col <character; proccessing>
#' @param str <character; proccessing>
#' @param dn.surfix <character; output>
#'
#' @export

# Box plot -----------

mf.boxplot <- function(

  data,
  ggdata,
  var.x,
  var.y,
  size = 0.5,
  var.col=NA,
  plot.col="black",
  box.col="gray",
  str,
  dn.surfix
  ){

  formula.facet <- sprintf("%s ~ %s", ".", str)

  n.str <- length(
    t(
      unique(data[,str])
    )
  )

  pdf(
    sprintf(
      "%s/%s.pdf",
      dir.output,
      dn.surfix
    ),
    width = 5 * n.str
  )

  if(
    !is.na(match(plot.col, "_"))
    ) {
    plot.color <-
      scale_color_gradient(
        low = strsplit(plot.col, "_")[[1]][1],
        high = strsplit(plot.col, "_")[[1]][2]
        )

    jitter <- geom_jitter(
      aes(
        y   = get(var.y),
        x   = get(var.x),
        color=get(var.col)
        ),
      size = size,
      width = 0.3
      )
    }else{
      plot.color <-
        scale_color_gradient(
          low = plot.col,
          high = plot.col
          )

      jitter <- geom_jitter(
        aes(
          y   = get(var.y),
          x   = get(var.x)
          ),
        size = size,
        width = 0.3,
        col=plot.col
        )
      }

  plot(
    plot.box_plot <-
      ggdata +
      geom_boxplot(
        aes(
          y   = get(var.y),
          x   = get(var.x)
        ),
        color=box.col,
        outlier.alpha = 0
      ) +

      jitter +


      plot.color +
      scale_y_log10() +
      facet_grid(
        as.formula(formula.facet)
        ) +
      theme_bw()
  )

  dev.off()
}


#' Make many boxplots from tydy ordering sheet
#'
#' @import ggplot2
#'
#'
#' @param D <object; input> A data frame with variables (ind, var.x, var.y, trans.y, trans.x, var.col, str, dn.surfix)
#' @param data <object;> A data.frame-class object
#' @param ggdata <object; input> A object with ggplot-class
#'
#' @export

mf.wrap.boxplot <- function(D, data, ggdata, ...){
  .var.x     = D$var.x
  .var.y     = D$var.y
  .size      = D$size
  .var.col   = D$var.col
  .plot.col   = D$plot.col
  .box.col   = D$box.col
  .str       = D$str
  .dn.surfix = D$dn.surfix
  mf.boxplot(
    data, ggdata,
    var.x=.var.x, var.y=.var.y, size = .size,
    var.col=.var.col, plot.col = .plot.col, box.col = .box.col,
    str=.str, dn.surfix = .dn.surfix, ...)
  }


#' Make many scatterplots from tydy ordering sheet
#'
#' @import ggplot2
#'
#' @param data <object; input> A data frame with variables (ind, var.x, var.y, trans.y, trans.x, var.col, str, dn.surfix)
#' @param ggdata <object; input> A object with ggplot-class
#' @param var.x <character; proccessing>
#' @param var.y <character; proccessing>
#' @param trans.y <character> To be passed to scale_y_continuous(trans= )
#' @param trans.x <character> To be passed to scale_x_continuous(trans= )
#' @param size <numeric; proccessing>
#' @param var.col <character; proccessing>
#' @param plot.col <character> "color_color"(to be passed to scale_color_gradient) or "color"
#' @param str <character; proccessing>
#' @param dn.surfix <character; output>
#' @param beta <character; proccessing> A numeric vector for coefficients of regression line
#'
#' @export

# Scatter plot -------

mf.scatterplot <- function(
  data, ggdata, var.x, var.y,
  trans.y  = c("log10", "identity"),
  trans.x  = c("log10", "identity"),
  size     = 0.5,
  var.col  = NA,
  plot.col = "black",
  line.col = "gray",
  cont.col = FALSE,
  str,
  dn.surfix,
  betas
){

  formula.facet <- sprintf(
    "%s ~ %s", ".",
    str
    )

  if(
    !is.na(match(plot.col, "_"))
    ) {
    plot.color <-
      scale_color_gradient(
        low = strsplit(plot.col, "_")[[1]][1],
        high = strsplit(plot.col, "_")[[1]][2]
        )

    points <- geom_point(
      aes(
        y   = get(var.y),
        x   = get(var.x),
        color=get(var.col)
        ),
      size = size,
      width = 0.3
      )
    }else{
      plot.color <-
        scale_color_gradient(
          low = plot.col,
          high = plot.col
          )

      point <- geom_point(
        aes(
          y   = get(var.y),
          x   = get(var.x)
          ),
        size = size,
        width = 0.3,
        col=plot.col
        )
      }

  trans.y <- trans.y
  trans.x <- trans.x

  n.str <- length(
    t(
      unique(data[,str])
      )
    )
  print(sprintf("strata=%s", n.str))

  pdf(
    sprintf(
      "%s/%s.pdf",
      dir.output,
      dn.surfix
      ),
    width = 7 * n.str
    )

  p =
    ggdata +
    point +
    facet_grid(
      as.formula(formula.facet)
    ) +

    plot.color +
    theme_bw()

  if(
    (trans.x=="NoScale") &
    (trans.y=="NoScale")
  ){
    print("No scaled")
    plot(p)
  }

  if(
    (trans.x=="NoScale") &
    (trans.y!="NoScale")
  ){
    print(" Y scaled")
    plot(
      p +
        scale_y_continuous(trans=trans.y)
    )
  }

  if(
    (trans.x!="NoScale") &
    (trans.y=="NoScale")
  ){
    print(" X scaled")
    plot(
      p +
        scale_x_continuous(trans=trans.x)
    )
  }

  if(
    (trans.x!="NoScale") &
    (trans.y!="NoScale")
  ){
    print("Both scaled")
    plot(
      p +
        scale_y_continuous(trans=trans.x) +
        scale_x_continuous(trans=trans.x)
    )
  }

  dev.off()
}

#' Make many boxplots from tydy ordering sheet
#'
#' @import ggplot2
#'
#'
#' @param D <object; input> A data frame with variables (ind, var.x, var.y, trans.y, trans.x, var.col, str, dn.surfix)
#' @param data <object;> A data.frame-class object
#' @param ggdata <object; input> A object with ggplot-class
#'
#' @export

mf.wrap.scatterplot <- function(D, data, ggdata, ...){
  .var.x     = D$var.x
  .var.y     = D$var.y
  .trans.y   = D$trans.y
  .trans.x   = D$trans.x
  .size      = D$size
  .var.col   = D$var.col
  .plot.col  = D$plot.col
  .line.col  = D$line.col
  .cont.col  = D$cont.col
  .str       = D$str
  .dn.surfix = D$dn.surfix
  .cont.col  = 1 - (is.factor(.var.col) + is.character(.var.col))
#  .betas     = D[, c('b0','b1','b2','b3')]

  mf.scatterplot(
    data , ggdata,
    var.x =.var.x,
    var.y =.var.y,
    trans.y =.trans.y,
    trans.x =.trans.x,
    size = .size,
    var.col  = .var.col,
    plot.col = .plot.col,
    line.col = .line.col,
    cont.col = .cont.col,
    str      = .str,
    dn.surfix= .dn.surfix,
    ...
    # beatas=.betas
    )
  }


