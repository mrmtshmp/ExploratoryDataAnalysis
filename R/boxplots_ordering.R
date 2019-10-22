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
  scale.var.y,
  var.caption,
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
      "%s/%s_Panels_%s_var.X_%s_var.Y_%s.pdf",
      dir.output,
      dn.surfix,
      str, var.x, var.y
    ),
    width = 7 * n.str
  )

  if(
    !is.na(match(plot.col, "_"))
    ) {
    plot.color <-
      scale_color_gradient(
        low = strsplit(plot.col, "_")[[1]][1],
        high = strsplit(plot.col, "_")[[1]][2]
        )

    jitter <- geom_beeswarm(
      aes(
        y=get(var.y), x=get(var.x), color=get(var.col)
        ),
      size = size,
      width = 0.3
      )

    # jitter <- geom_jitter(
    #   aes(
    #     y   = get(var.y),
    #     x   = get(var.x),
    #     color=get(var.col)
    #     ),
    #   size = size,
    #   width = 0.3
    #   )
    }else{
      plot.color <-
        scale_color_gradient(
          low = plot.col,
          high = plot.col
          )

      jitter <- geom_beeswarm(
        aes(
          y=get(var.y), x=get(var.x), shape=get(var.x)
          ),
        size = size,
        width = 0.3,
        col=plot.col
      )

      # jitter <- geom_jitter(
      #   aes(
      #     y   = get(var.y),
      #     x   = get(var.x)
      #     ),
      #   size = size,
      #   width = 0.3,
      #   col=plot.col
      #   )
    }

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
#      scale_y_log10() +
      scale_x_discrete() +
      facet_grid(
        as.formula(formula.facet)
      ) +
      theme_bw() +
      xlab(var.x) +
      ylab(var.y) +
      labs(
        title = str,
        caption = var.caption
      )

  scale.y <- unique(scale.var.y)

  if(scale.y=="log10"){
    plot(
      plot.box_plot + scale_y_log10()
      )
    }

  if(scale.y=="not_scale"){
    plot(
      plot.box_plot
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

mf.wrap.boxplot <- function(D, data, ggdata, ...){
  .var.x     = D$var.x
  .var.y     = D$var.y
  .scale.var.y=D$scale.var.y
  .var.caption=D$var.caption
  .size      = D$size
  .var.col   = D$var.col
  .plot.col   = D$plot.col
  .box.col   = D$box.col
  .str       = D$str
  .dn.surfix = D$dn.surfix
  mf.boxplot(
    data, ggdata,
    var.x=.var.x, var.y=.var.y, scale.var.y = .scale.var.y, var.caption = .var.caption,
    size = .size,var.col=.var.col, plot.col = .plot.col, box.col = .box.col,
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
#' @param betas <character; proccessing> A numeric vector for coefficients of regression line
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

  line_0 <- geom_abline(
    intercept = betas[,"b0"], #+ betas[,"b2"],
    slope = betas[,"b1"] #+ betas[,"b3"]
    )

  line_1 <- geom_abline(
    intercept = betas[,"b0"] + betas[,"b2"],
    slope = betas[,"b1"] + betas[,"b3"]
    )

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
    line_0 + line_1 +
    geom_density_2d(
      aes(
        y   = get(var.y),
        x   = get(var.x)
        ),
      color='gray', alpha=0.6, stat =
      ) + scale_colour_gradient(low="green",high="red") +

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
#' @param betas <character; proccessing> A numeric vector for coefficients of regression line
#'
#' @export

# Scatter plot with miss box -------

mf.scatterplot_with_missbox <- function(
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

  data$miss <- is.na(data[,var.y])

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
    geom_density_2d(
      aes(
        y   = get(var.y),
        x   = get(var.x)
      ),
      color='gray', alpha=0.6, h = c(width.SJ(get(var.x)), width.SJ(get(var.y)))
    ) + scale_colour_gradient(low="green",high="red") +

    plot.color +
    theme_bw()


  if(
    is.null(betas)
    ){
    line_0 <- geom_abline(
      intercept = betas[,"b0"], #+ betas[,"b2"],
      slope = betas[,"b1"] #+ betas[,"b3"]
      )
    line_1 <- geom_abline(
      intercept = betas[,"b0"] + betas[,"b2"],
      slope = betas[,"b1"] + betas[,"b3"]
      )

    p <- p + line_0 + line_1
    }

  if(
    (trans.x=="NoScale") &
    (trans.y=="NoScale")
  ){
    print("No scaled")
    p1 <- p
  }

  if(
    (trans.x=="NoScale") &
    (trans.y!="NoScale")
  ){
    print(" Y scaled")
    p1 <- p +
        scale_y_continuous(trans=trans.y)
  }

  if(
    (trans.x!="NoScale") &
    (trans.y=="NoScale")
  ){
    print(" X scaled")
    p1 <- p +
        scale_x_continuous(trans=trans.x)
  }

  if(
    (trans.x!="NoScale") &
    (trans.y!="NoScale")
  ){
    print("Both scaled")

    p1 <- p +
        scale_y_continuous(trans=trans.x) +
        scale_x_continuous(trans=trans.x)
  }
  p2 <- mf.boxplot(
    data,
    ggdata,
    var.x="miss",
    var.y,
    scale.var.y=trans.y,
    var.caption=NULL,
    size = size,
    var.col=NA,
    plot.col=plot.col,
    box.col="gray",
    str,
    dn.surfix
    )
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  g <- cbind(g1, g2, size = "first")
  print(class(g))
  g$widths = grid::unit.pmax(g1$widths, g2$widths)
  plot(g)
#  gridExtra::grid.arrange(p1, p2, nrow = 1)
  dev.off()
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
#' @param betas <character; proccessing> A numeric vector for coefficients of regression line
#'
#' @export


mf.wrap.scatterplot <- function(D, data, ggdata, df.beta, ...){
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
  .betas     = df.beta[
    df.beta$.id==D$str ,
    c('b0','b1','b2','b3')
    ]

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
    betas = .betas,
    ...
    )
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
#' @param betas <character; proccessing> A numeric vector for coefficients of regression line
#'
#' @export


mf.wrap.scatterplot.with_missbox <- function(D, data, ggdata, df.beta, ...){
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
  .betas     = df.beta[
    df.beta$.id==D$str ,
    c('b0','b1','b2','b3')
    ]

  mf.scatterplot_with_missbox(
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
    betas = .betas,
    ...
  )
}

