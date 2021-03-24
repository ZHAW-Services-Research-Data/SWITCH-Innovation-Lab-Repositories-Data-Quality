## Theme

theme_zhaw <- function (base_size = 10, base_family = "Arial", panel.grid.major.x.blank = FALSE, panel.grid.minor.x.blank = FALSE, 
                        panel.grid.major.y.blank = FALSE, panel.grid.minor.y.blank = FALSE, keep_ticks = FALSE, axis.text.x.diagonal = FALSE) 
{
  theme_replacement <- ggplot2::theme(
    plot.title = ggplot2::element_text(size = 18, hjust = 0, vjust = 3), 
    plot.caption = ggplot2::element_text(hjust = 0), 
    plot.margin = unit(c(15,5,5,5), "pt"),
    strip.text.x = ggplot2::element_text(size = 15, hjust = 0), complete = TRUE)
  res <- ggplot2::theme_minimal(base_size = base_size, base_family = base_family) %+replace% 
    theme_replacement
  
  if (panel.grid.major.x.blank) {
    res <- res %+replace% ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(), 
                                         complete = TRUE)
  }
  if (panel.grid.minor.x.blank) {
    res <- res %+replace% ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(), 
                                         complete = TRUE)
  }
  if (panel.grid.major.y.blank) {
    res <- res %+replace% ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), 
                                         complete = TRUE)
  }
  if (panel.grid.minor.y.blank) {
    res <- res %+replace% ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank(), 
                                         complete = TRUE)
  }
  
  if (axis.text.x.diagonal) {
    res <- res %+replace% ggplot2::theme(axis.text.x = element_text(hjust=1.5))
  }
  
  if(keep_ticks) {
    res <- res %+replace% ggplot2::theme(axis.ticks = element_line(), complete = TRUE)
  }
  return(res)
}

## Save Plots

PlotFileName <- function(filename_parts, sep = "_", rootdir="plots/", suffix="png") {
  fname <- paste(filename_parts, collapse = sep)
  fname <- paste0(rootdir, fname, ".", suffix)
  return(fname)
}

SavePlot <- function(g, size, filename_parts, sep="_", rootdir="plots/", suffix="png", dpi=300, width=NULL, height=NULL, units="cm") {
  fname <- PlotFileName(filename_parts, sep, rootdir="plots/", suffix="png")
  
  switch(size,
         'ppt.lan'= {ggplot2::ggsave(fname, g, dpi=dpi, width=13, height=6.5, units="in", device=suffix)},
         'doc.por'= {ggplot2::ggsave(fname, g, dpi=dpi, width=16, height=10, units="cm", device=suffix)},
         'a4'= {ggplot2::ggsave(fname, g, dpi=dpi, width=19, height=28, units="cm", device=suffix)},
         'a4.lan'= {ggplot2::ggsave(fname, g, dpi=dpi, width=28, height=19, units="cm", device=suffix)},
         'a4.lan.half'= {ggplot2::ggsave(fname, g, dpi=dpi, width=16, height=18, units="cm", device=suffix)},
         'a4.lan.quadratic' = {ggplot2::ggsave(fname, g, dpi=dpi, width=19, height=19, units="cm", device=suffix)},
         'a5.third'= {ggplot2::ggsave(fname, g, dpi=dpi, width=10.8 *(14/5), height=5*(14/5), units="cm", device=suffix)},
         'a5.third2'= {ggplot2::ggsave(fname, g, dpi=dpi, width=10.8 *(14/5), height=10*(14/5), units="cm", device=suffix)},
         'a5.quad'= {ggplot2::ggsave(fname, g, dpi=dpi, width=7 *(14/5), height=5*(14/5), units="cm", device=suffix)},
         'a5.quad2'= {ggplot2::ggsave(fname, g, dpi=dpi, width=5 *(14/5), height=5*(14/5), units="cm", device=suffix)},
         'a5.quad3'= {ggplot2::ggsave(fname, g, dpi=dpi, width=7.5 *(14/5), height=5*(14/5), units="cm", device=suffix)},
         'a5.por'= {ggplot2::ggsave(fname, g, dpi=dpi, width=10.8 *(14/5), height=14*(14/5), units="cm", device=suffix)},
         'a5.lan'= {ggplot2::ggsave(fname, g, dpi=dpi, width=14 *(14/5), height=10.8*(14/5), units="cm", device=suffix)},
         'a3'= {ggplot2::ggsave(fname, g, dpi=dpi, width=29, height=42, units="cm", device=suffix)},
         'a3.lan'= {ggplot2::ggsave(fname, g, dpi=dpi, width=42, height=29, units="cm", device=suffix)},
         'a2.lan'= {ggplot2::ggsave(fname, g, dpi=dpi, width=59, height=42, units="cm", device=suffix)},
         'jmir.lan' = {ggplot2::ggsave(fname, g, dpi=dpi, width=15, height=7, units="cm", device=suffix)},
         'jmir.lan.high' = {ggplot2::ggsave(fname, g, dpi=dpi, width=15, height=10, units="cm", device=suffix)},
         'jmir.lan.integrated' = {ggplot2::ggsave(fname, g, dpi=dpi, width=15, height=20, units="cm", device=suffix)},
         {ggplot2::ggsave(fname, g, dpi=dpi, width=width, height=height, units=units, device=suffix)}
  )
  return(fname)
}

plot_distribution <- function(df_var_long, var, scale_count = TRUE, order_freq = TRUE, annotate_labels = TRUE, x_lab = NULL, y_lab = NULL, gg_title = NULL, add_title = TRUE, adjust_plot_margin = NULL, x_label_angle = NULL) {
  if (order_freq & scale_count){
    g_res <- df_var_long %>% ggplot(aes(x=fct_infreq(!!sym(var)))) 
  } else if (order_freq & !scale_count) {
    g_res <- df_var_long %>% ggplot(aes(x=fct_infreq(!!sym(var)), y=(..count..)/sum(..count..)))
  } else if (!order_freq & scale_count) {
    g_res <- df_var_long %>% ggplot(aes(x=!!sym(var)))
  } else {
    g_res <- df_var_long %>% ggplot(aes(x=!!sym(var), y=(..count..)/sum(..count..)))
  }
  
  g_res <- g_res + 
    geom_bar(stat = "count") 
  
  if (annotate_labels) {
    if (scale_count) {
      g_res <- g_res +
        geom_text(stat='count', aes(y = after_stat(count) + max(after_stat(count))/10, 
                                    label=after_stat(count)), vjust=2.5, size=2.5) +
        scale_y_continuous(expand=c(0,0))
    } else {
      g_res <- g_res +
        geom_text(stat='count', aes(y = after_stat((..count..)/sum(..count..)) + max(after_stat((..count..)/sum(..count..)))/10, 
                                    label=scales::percent(after_stat((..count..)/sum(..count..)))), vjust=2.5, size=2.5) +
        scale_y_continuous(expand=c(0,0), labels = scales::percent_format(accuracy = 1)) 
    }
  }
  
  
  if (!is.null(x_label_angle)) {
    g_res <- g_res +
      scale_x_discrete(guide = guide_axis(angle = x_label_angle))
  } 
  
  x_lab <- ifelse(is.null(x_lab), str_to_sentence(var), x_lab)
  if (is.null(y_lab)) {
    y_lab <- ifelse(scale_count, "Count", "Percentage")
  }
  g_res <- g_res + xlab(x_lab) + ylab(y_lab)
  
  gg_title <- ifelse(is.null(gg_title), ifelse(scale_count, "Distribution of " %+% var, "Relative distribution of " %+% var), gg_title)
  
  if (add_title) {
    g_res <- g_res + ggtitle(gg_title) 
  } 
  
  g_res <- g_res +
    theme_zhaw(panel.grid.major.x.blank = TRUE, keep_ticks = TRUE)
  
  if (!is.null(adjust_plot_margin)) {
    g_res <- g_res +
      theme(plot.margin = adjust_plot_margin)
  }
  
  return (g_res)
}