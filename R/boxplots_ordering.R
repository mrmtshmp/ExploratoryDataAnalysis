#' Make many boxplots from tidy ordering sheet
#'
#' @import ggplot2
#' @import ggbeeswarm
#'
#' @param data <object; input> A data frame with variables (ind, var.x, var.y, trans.y, trans.x, var.col, str, dn.surfix)
#' @param output.plot <logical>
#' @param var.x <character; proccessing>
#' @param var.y <character; proccessing>
#' @param scale.var.y <character; proccessing>
#' @param size <numeric; proccessing>
#' @param var.col <character; proccessing>
#' @param plot.col <character; proccessing>
#' @param box.col <character; proccessing>
#' @param str <character; proccessing>
#' @param theme.input ggplot2::theme-class object
#' @param dn.surfix <character; output>
#'
#' @export

# Box plot -----------

mf.boxplot <- function(

  data,
  ggdata=NULL,
  output.plot=TRUE,
  var.x,
  var.y,
  scale.var.y='not_scale',
  var.caption ='',
  ax.lab.x ="X",
  ax.lab.y ="Y",
  size = 0.5,
  var.col=NA,
  plot.col="black",
  plot.alpha=1,
  plot.y_intcpt.alpha=0,
  plot.y_intcpt=1,
  box.col="gray",
  beeswarm = FALSE,
  width.box = NA,
  theme.input = theme_bw(),
  str,
  str.x=NULL,
  str.y=NULL,
  dn.surfix,
  .dir.output=dir.output
){
  print(var.x)
  print(var.y)

  if(is.null(ggdata)){
    ggdata <- data %>%
      ggplot(aes(x=as.factor(get(var.x)), y=get(var.y)))
    }


  if(!is.null(str.x) & !is.null(str.y)){
    formula.facet <-
      sprintf("%s ~ %s", str.y, str.x)
    str <- str.x

  }else{
    if(is.null(str)){stop("argument 'str' is NULL.")}
    formula.facet <-
      sprintf("%s ~ %s", ".", str)
  }


  nx.str <- length(
    unique(as.character(data[,str]))
    )

  nx.var <- length(
    unique(as.character(data[,var.x]))
    )

  if(!is.null(str.y)){
    ny.str <- length(
      unique(as.character(data[,str.y]))
    )
  }else{
    ny.str <- 1
  }


  if(is.na(width.box)){width.box <- 1}

  if (length(grep("_", plot.col))) {
    plot.color <-
      scale_color_gradient(
        low = strsplit(plot.col, "_")[[1]][1],
        high = strsplit(plot.col, "_")[[1]][2]
      )

    if(beeswarm){
      jitter <- geom_beeswarm(
        aes(
          y   = get(var.y),
          x   = as.factor(get(var.x)),
          color=get(var.col)
          ),
        size = size, alpha=plot.alpha
        )
      boxplot <- geom_boxplot(
        aes(
          y   = get(var.y),
          x   = as.factor(get(var.x))
          ),
        width = width.box,
        color=box.col,
        outlier.alpha = 0
        )
    }else{
    jitter <- geom_point(
      aes(
        y   = get(var.y),
        x   = as.factor(get(var.x)),
        color=get(var.col)
      ),
      position = position_quasirandom(groupOnX = TRUE),
      size = size, alpha=plot.alpha
    )
    boxplot <- geom_boxplot(
      aes(
        y   = get(var.y),
        x   = as.factor(get(var.x))
      ),
      color=box.col,
      outlier.alpha = 0
      )
    }

    # jitter <- geom_beeswarm(
    #   aes(
    #     y=get(var.y), x=get(var.x), color=get(var.col)
    #     ),
    #   size = size,
    #   groupOnX = TRUE,na.rm = TRUE
    #   )

  }else{
    plot.color <-
      scale_color_gradient(
        low = plot.col,
        high = plot.col
      )

    #       jitter <- geom_beeswarm(
    #         aes(
    #           y=get(var.y), x=get(var.x)
    #           ),
    #         groupOnX = TRUE,na.rm = TRUE,
    #         size = size,
    # #        width = 0.3,
    #         col=plot.col
    #       )

    if(beeswarm){
      jitter <- geom_beeswarm(
        aes(
          y   = get(var.y),
          x   = as.factor(get(var.x))
        ),
        size = size, alpha=plot.alpha
      )
      boxplot <- geom_boxplot(
        aes(
          y   = get(var.y),
          x   = as.factor(get(var.x))
        ),
        width = width.box,
        color=box.col,
        outlier.alpha = 0
      )
    }else{
    jitter <- geom_point(
      aes(
        y   = get(var.y),
        x   = as.factor(get(var.x))
      ),
      position = position_quasirandom(groupOnX = TRUE),
      size = size,
      col=plot.col,
      alpha=plot.alpha
      )
    boxplot <- geom_boxplot(
      aes(
        y   = get(var.y),
        x   = as.factor(get(var.x))
      ),
      color=box.col,
      outlier.alpha = 0
      )
    }
  }

  plot.box_plot <-
    ggdata +
    boxplot +

    jitter +

    geom_hline(
      yintercept = plot.y_intcpt,
      alpha = plot.y_intcpt.alpha,
      size=0.5, col="black"
    ) +

    plot.color +
    #      scale_y_log10() +
    scale_x_discrete() +
    facet_grid(
      as.formula(formula.facet)
    ) +
    theme_bw() +
    theme.input +
    xlab(ax.lab.x) +
    ylab(ax.lab.y) +
    labs(
      title = str,
      caption = var.caption
    )

  scale.y <- unique(scale.var.y)


  if(scale.y=="log10"){
    plot.result <-
      plot.box_plot + scale_y_log10()
  }

  if(scale.y=="not_scale"){

    plot.result <-
      plot.box_plot
  }

  if(output.plot){
    pdf(
      sprintf(
        "%s/%s_Panels_%s_var.X_%s_var.Y_%s.pdf",
        .dir.output,
        dn.surfix,
        str, var.x, var.y
      ),
      width = 2.5 * nx.str *nx.var *width.box,
      height = 5 * ny.str
    )

    plot(plot.box_plot)

    dev.off()
  }else{
    return(plot.box_plot)
  }
}

#' Make many boxplots on line plot from tidy ordering sheet
#'
#' @import ggplot2
#' @import ggbeeswarm
#'
#' @param data <object; input> A data frame with variables (ind, var.x, var.y, trans.y, trans.x, var.col, str, dn.surfix)
#' @param ggdata <object; input> A object with ggplot-class
#' @param var.x <character; proccessing>
#' @param var.y <character; proccessing>
#' @param var.group <character; proccessing>
#' @param size <numeric; proccessing>
#' @param var.col <character; proccessing>
#' @param plot.col <character; proccessing>
#' @param box.col <character; proccessing>
#' @param str <character; proccessing>
#' @param dn.surfix <character; output>
#'
#' @export


mf.lineplot <- function(

  data,
  ggdata,
  var.x,
  var.y,
  var.group,
  scale.var.y,
  ax.breaks.x,
  ax.lab.x=NULL,
  ax.lab.y=NULL,
  var.caption,
  size = 0.5,
  var.col=NA,
  plot.col="black",
  box.col="gray",
  str,
  str.x=NULL,
  str.y=NULL,
  .dir.output = dir.output,
  dn.surfix
){

  # Settings for facetting (size of PDF is included).

  if(!is.null(str.x) & !is.null(str.y)){
    formula.facet <-
      sprintf("%s ~ %s", str.y, str.x)
    str <- str.x

  }else{
    if(is.null(str)){stop("argument 'str' is NULL.")}
    formula.facet <-
      sprintf("%s ~ %s", ".", str)
  }


  nx.str <- length(
    unique(as.character(data[,str]))
  )

  nx.var <- length(
    unique(as.character(data[,var.x]))
  )

  if(!is.null(str.y)){
    ny.str <- length(
      unique(as.character(data[,str.y]))
    )
  }else{
    ny.str <- 1
  }


  # Brewing colors (IF plot.col includes "_").

  if(
    !is.na(match(plot.col, "_"))
  ) {

    plot.color <-
      scale_color_gradient(
        low = strsplit(plot.col, "_")[[1]][1],
        high = strsplit(plot.col, "_")[[1]][2]
      )

    jitter <- geom_point(
      aes(
        y=get(var.y),
        x=get(var.x),
        color=get(var.col)
      ),
      size = size,
      width = 0.1,
      position =
        position_jitter(width = 0.1, height = 0)
    )

    ggline <- geom_line(
      aes(
        y=get(var.y), x=get(var.x),
        group=get(var.group),
        color=get(var.col)
      ),
      position =
        position_jitter(
          width = 0.1, height = 0
        )
    )


    # ELSE{...} of IF(plot.col includes "_"){...}.--------------

  }else{
    plot.color <-
      scale_color_gradient(
        low = plot.col,
        high = plot.col
      )

    jitter <- geom_point(
      aes(
        y=get(var.y),
        x=as.numeric(get(var.x))
      ),
      size = size,
      width = 0.1,
      position =
        position_jitter(width = 0.1, height = 0)
    )
    # print("jitter ok")

    ggline <- geom_line(
      aes(
        y=get(var.y),
        x=get(var.x),
        group=get(var.group)
      ),
      position =
        position_jitter(
          width = 0.1, height = 0
        )
    )
  }
  # Make ggplot object (ggdata + geom_boxplot() + geom_point() + geom_line() + scale_color_gradient() + ).--------------

  plot.box_plot <-
    ggdata +
    jitter +
    ggline +
    scale_x_continuous(breaks = ax.breaks.x) +
    facet_grid(
      as.formula(formula.facet)
    ) +
    theme_bw() +
    xlab(ax.lab.x) +
    ylab(ax.lab.y) +
    labs(
      title = str,
      caption = var.caption
    ) #+
  #    plot.color


  scale.y <- unique(scale.var.y)



  # Open and close PDF device. ----------------------------------------------

  pdf(
    sprintf(
      "%s/%s_Panels_%s_var.X_%s_var.Y_%s.pdf",
      .dir.output,
      dn.surfix,
      str,
      var.x,
      var.y
    ),
    width = 2.5 * nx.str * nx.var,
    height = 5 * ny.str
  )

  if(scale.y=="log10"){
    plot(
      plot.box_plot + scale_y_log10()
    )
  }

  if(scale.y=="not_scale"){
    #    return(plot.box_plot)
    plot(
      plot.box_plot
    )
  }
  dev.off()
}


#' Make many boxplots from tidy ordering sheet
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


#' Make many scatterplots from tidy ordering sheet
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
  data,
  output.plot=TRUE,
  var.x,
  var.y,
  trans.y  = c("log10", "identity"),
  trans.x  = c("log10", "identity"),
  size     = 0.5,
  var.col  = NA,
  plot.col = "black",
  line.col = "gray",
  cont.col = FALSE,
  contour.col='gray',
  contour.alpha=0.6,
  ax.lab.x = NULL,
  ax.lab.y = NULL,
  var.caption = NULL,
  str,
  dn.surfix,
  betas=NULL,
  ggdata=NULL
){

  data <-
    data[
      !is.na(data[,var.x]) & !is.na(data[,var.y])
      ,
      ]

  if(is.null(ggdata)){ggdata <- ggplot(data)}

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

  if(!is.null(betas)){
    line_0 <- geom_abline(
      intercept = betas[,"b0"], #+ betas[,"b2"],
      slope = betas[,"b1"] #+ betas[,"b3"]
    )

    line_1 <- geom_abline(
      intercept = betas[,"b0"] + betas[,"b2"],
      slope = betas[,"b1"] + betas[,"b3"]
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


  if(!is.null(betas)){
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
        color=contour.col, alpha=contour.alpha, stat =
      ) + scale_colour_gradient(low="green",high="red") +

      plot.color +
      xlab(ax.lab.x) +
      ylab(ax.lab.y) +
      labs(
        title = str,
        caption = var.caption
      ) +
      theme_bw()
  }else{
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
        color=contour.col, alpha=contour.alpha, stat =
      ) + scale_colour_gradient(low="green",high="red") +

      plot.color +
      xlab(ax.lab.x) +
      ylab(ax.lab.y) +
      labs(
        title = str,
        caption = var.caption
      ) +
      theme_bw()
  }


  if(
    (trans.x=="NoScale") &
    (trans.y=="NoScale")
  ){
    print("No scaled")
    plot.output <-
      p
  }

  if(
    (trans.x=="NoScale") &
    (trans.y!="NoScale")
  ){
    print(" Y scaled")
    plot.output <-
      p +
        scale_y_continuous(trans=trans.y)
  }

  if(
    (trans.x!="NoScale") &
    (trans.y=="NoScale")
  ){
    print(" X scaled")
    plot.output <-
      p +
        scale_x_continuous(trans=trans.x)
  }

  if(
    (trans.x!="NoScale") &
    (trans.y!="NoScale")
  ){
    print("Both scaled")
    plot.output <-
      p +
        scale_y_continuous(trans=trans.x) +
        scale_x_continuous(trans=trans.x)
  }
  if(output.plot){
    pdf(
      sprintf(
        "%s/%s.pdf",
        dir.output,
        dn.surfix
      ),
      width = 7 * n.str
    )
    plot(plot.output)
    dev.off()
  }else{
      return(plot.output)
    }

}



#' Make many scatterplots from tidy ordering sheet
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
  output.plot=TRUE,
  trans.y  = c("log10", "identity"),
  trans.x  = c("log10", "identity"),
  size     = 0.5,
  var.col  = NA,
  plot.col = "black",
  line.col = "gray",
  cont.col = FALSE,
  ax.lab.x = NULL,
  ax.lab.y = NULL,
  var.caption = NULL,
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
    xlab(ax.lab.x) +
    ylab(ax.lab.y) +
    labs(
      title = str,
      caption = var.caption) +
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

  if(output.plot){
    pdf(
      sprintf(
        "%s/%s.pdf",
        dir.output,
        dn.surfix
      ),
      width = 7 * n.str
    )
    plot(g)
    #  gridExtra::grid.arrange(p1, p2, nrow = 1)
    dev.off()
  }else{
      return(g)
    }
  }

#' Make many scatterplots from tidy ordering sheet
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


#' Make many scatterplots from tidy ordering sheet
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

