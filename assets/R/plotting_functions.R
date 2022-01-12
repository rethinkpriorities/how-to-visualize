#NOTE: THESE plotting functions were mainly geared for EA survey work

### Histograms and vertical lines ####

# function for histogram with specified breakpoints
hist_plot <- function(d,x){
  ggplot(d, aes(x = {{x}})) +
    geom_histogram(fill = "white", color = "black")   +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) }

# function for log scaled histogram with specified breakpoints
hist_plot_lscale <- function(d,x,breaks,prefix=""){
  ggplot(d, aes(x = x)) +
    geom_histogram(fill = "white", color = "black") +
    theme_classic() +
    scale_x_continuous(trans = pseudo_log_trans(base=10), breaks=breaks, labels=label_number_si(prefix=prefix)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) }

# function for adding a mean line and label to a histogram
# Todo: this code is crappy -- make it simply take function arguments .. so we don't need 3+ versions of this
geom_vline_mean <- function( x, tgap=1.3, label="Mean"){ list(geom_vline(xintercept = base::mean(x, na.rm = TRUE),
                                                                         colour = "blue"),
                                                              geom_text( aes( x = base::mean(x + tgap, na.rm = TRUE),
                                                                              label = label, y = 100 ),
                                                                         colour = "blue", angle = 45, size = 3.5 )) }

# function for adding a median line and label to a histogram
geom_vline_med <- function(x, tgap = 1.3, label = "Median") {
  list(
    geom_vline(
      xintercept = median(x, na.rm = TRUE),
      colour = "forestgreen",
      linetype = "dashed"
    ),
    geom_text(
      aes(
        x = median(x + tgap, na.rm = TRUE),
        label = label,
        y = 120,
      ),
      colour = "forestgreen",
      angle = 45,
      size = 3.5
    )
  )
}

geom_hline_med <- function(y, tgap = 1.3, label = "Median") {
  list(
    geom_hline(
      yintercept = median(y, na.rm = TRUE),
      colour = "forestgreen",
      linetype = "dashed"
    ),
    geom_text(
      aes(
        y = median(y, na.rm = TRUE),
        label = label
      ),
      colour = "forestgreen",
      angle = 45,
      size = 3.5    )
  )
}

# function for adding a 90th percentile line and label to a histogram
geom_vline_90 <- function(x, tgap = 1.3, label = "90th pct") {
  list(
    geom_vline(
      xintercept = quantile(x + tgap, 0.9, na.rm=TRUE),
      colour = "red",
      linetype = "dashed"
    ),
    geom_text(
      aes(
        x = quantile(x + tgap, 0.9, na.rm=TRUE),
        label = label,
        y = 200,
      ),
      colour = "red",
      angle = 45,
      size = 3.5
    )
  )
}

### Forest plot (of coefficients and confidence intervals) ####

# takes a particular tidy dataframe coming out of a model, with particular labels, as the argument

forest_plot <- function(coefs, ordered = TRUE, intercept=FALSE, ylab="", xlab = "", vline = 0){

    create_plot <- function(coefs){
        plot <- coefs %>% ggplot2::ggplot(aes(x = estimate, y = term)) +
        ggplot2::geom_pointrange(aes(xmin = conf.low, xmax = conf.high))
        return(plot)
    }

  if (intercept == FALSE){
    coefs <- coefs %>% dplyr::filter(term != "(Intercept)")
  }

  if (ordered != TRUE){
    forest <- create_plot(coefs)
  }
  else {
    forest <- coefs %>% dplyr::arrange(estimate) %>%
      dplyr::mutate(term = factor(term, levels = unique(term))) %>%
      create_plot()
  }

  forest <- forest +
    ylab(ylab) +
    xlab(xlab) +
    geom_vline(xintercept={{vline}}, colour="grey")


  return(forest)

}

grouped_forest_plot <- function(coefs, groups, ordered = TRUE,
                                intercept=FALSE, ylab="", xlab = "", vline = 0){

  create_plot <- function(coefs){
    plot <- coefs %>% ggplot2::ggplot(aes(x = estimate, y = term, colour = {{groups}})) +
      ggplot2::geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                               position = position_dodge(width = 0.8))
    return(plot)
  }

  if (intercept == FALSE){
    coefs <- coefs %>% dplyr::filter(term != "(Intercept)")
  }

  if (ordered != TRUE){
    forest <- create_plot(coefs)
  }
  else {
    forest <- coefs %>% dplyr::arrange(estimate) %>%
      dplyr::mutate(term = factor(term, levels = unique(term))) %>%
      create_plot()
  }

  forest <- forest +
    ylab(ylab) +
    xlab(xlab) +
    geom_vline(xintercept={{vline}}, colour="grey")


  return(forest)

}


### Additional plotting functions in functions.R, not used in EAS work



# VISUALISATION functions: ####

#TODO -- add ggrepel to these? geom_label_repel() and geom_text_repel

plot_histogram <- function(df, feature) {
  chart_title <- substitute(paste("Histogram of ", feature,
    sep = ""))
  plt <- ggplot(df, aes(x = eval(parse(text = feature)))) +
    geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#33AADE",
      color = "black") + geom_density(alpha = 0.3, fill = "red") +
    geom_vline(aes(xintercept = mean(eval(parse(text = feature)))),
      color = "black", linetype = "dashed", size = 1) +
    labs(x = feature, y = "Density")
  print(plt)
}

# ... Multiple histogram: ####

plot_multi_histogram <- function(df, feature, label_column) {
  chart_title <- substitute(paste("Histograms of ", feature,
    " by ", label_column, sep = ""))
  plt <- ggplot(df, aes(x = eval(parse(text = feature)), fill = eval(parse(text = label_column)))) +
    geom_histogram(alpha = 0.3, position = "identity", aes(y = ..density..),
      color = "black") + geom_density(alpha = 0.3) + geom_vline(aes(xintercept = mean(eval(parse(text = feature)))),
    color = "black", linetype = "dashed", size = 1) + labs(title = chart_title,
    subtitle = "dashed line = mean", x = feature, y = "Density")
  plt + guides(fill = guide_legend(title = label_column))
}


dotplot_func <- function(df = ADSX, yvar = donation, xvar = Stage,
  treatvar = Treatment, title = "") {
  yvar <- enquo(yvar)
  xvar <- enquo(xvar)
  treatvar <- enquo(treatvar)
  df %>% ungroup() %>% # mutate(xvar = as.factor(!!xvar)) %>%
  dplyr::group_by(!!xvar, !!treatvar) %>% # drop_na(!!yvar, !!treatvar) %>%
  dplyr::select(!!yvar, !!treatvar, !!xvar) %>% dplyr::summarise(meanyvar = mean(!!yvar,
    na.rm = TRUE)) %>% ggplot(aes(y = meanyvar, x = !!xvar,
    color = !!treatvar, group = !!treatvar, shape = !!treatvar)) +
    geom_point(size = 6) + geom_line() + expand_limits(y = 0) +
    scale_x_continuous(breaks = c(1, 2)) + scale_y_continuous(yvar) +
    theme(panel.grid.major.y = element_line(color = "white",
      size = 0.3)) + theme(panel.grid.minor.y = element_line(color = "white",
    size = 0.1)) + theme(panel.grid.major.x = element_blank()) +
    theme(panel.grid.minor.x = element_blank()) + theme(axis.ticks.x = element_blank()) +
    theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12)) +
    theme(legend.text = element_text(size = 12), legend.title = element_text(size = 15,
      face = "bold")) + ggtitle(title)
}

geom_mean <- function() {
  list(stat_summary(fun.y = "mean", geom = "point", fill = "red"),
    stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",
      width = 0.4))
}

boxplot_func <- function(df = ADSX, yvar = donation, treatvar = Treatment, facetfunc = 1,
  comparisons = list(c("No ask-Domestic", "Domestic-Domestic"))) {
  yv <- enquo(yvar)
  tv <- enquo(treatvar)
  ungroup(df) %>%
      group_by({{treatvar}}) %>%
      drop_na({{yvar}}, {{treatvar}}) %>%
    ggplot(aes({{treatvar}}, {{yvar}})) +
    geom_boxplot() +
    facet_grid({{facetfunc}}) +
    theme(axis.title = element_text(size = 14),
    axis.text = element_text(size = 14)) +
    theme(axis.text.x = element_text(size = 12)) +
    labs(title = tv, y = yv, caption = "p-values of Wilcox-(below) and  t-test (above brackets)") +
    geom_signif(comparisons = comparisons, step_increase = c(0.4),
      vjust = 1.7, margin_top = 0.5, textsize = 5) +
    geom_signif(comparisons = comparisons,
    step_increase = c(0.4), vjust = 0, margin_top = 0.5,
    textsize = 5, test = "t.test") +
     stat_summary(
    fun.y = mean, geom = "point", shape = 18,
      size = 3, color = "red")
}

# https://stackoverflow.com/questions/13407236/remove-a-layer-from-a-ggplot2-chart
# Delete layers from ggplot
remove_geom <- function(ggplot2_object, geom_type) {
  # Delete layers that match the requested type.
  layers <- lapply(ggplot2_object$layers, function(x) {
    if (class(x$geom)[1] == geom_type) {
      NULL
    } else {
      x
    }
  })
  # Delete the unwanted layers.
  layers <- layers[!sapply(layers, is.null)]
  ggplot2_object$layers <- layers
  ggplot2_object
}
