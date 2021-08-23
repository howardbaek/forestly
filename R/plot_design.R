#' Define auxiliary functions for the interactive plot
#'
#' @param t_display a data frame necessarily contains the following columns:
#'                  ae: the ae term, for example, Amnesia, Abdominal Pain
#'                  ae_label: a ae label to classify ae, its value is either ALL/AESER/AEREL...
#'                  n1: the number of ae events in the treatment arm
#'                  n2: the number of ae events in the control arm
#'                  N1: the total sample size in the treatment arm
#'                  N2: the total sample size in the control arm
#'                  pct_1: the ratio between n1 and N1
#'                  pct_2: the ratio between n2 and N2
#'                  fig_diff: the difference between pct_1 and pct_2, i.e., pct_1 - pct_2
#'                  lower: the lower value of the error bar
#'                  upper: the upper value of the error bar
#' @param fig_prop_range a numerical vector of length 2 to indicate 
#'                      the range to plot the two ae proportion, ex., c(-0.05, 0.65)
#' @param fig_prop_label a character vector of length 2, 
#'                       with the first entry indicating the label of the treatment arm, 
#'                       and the second entry indicating the label of the control arm.
#'                       The default value is c("treatment", "control")
#' @param fig_prop_color a character vector of length 2, 
#'                       with the first entry indicating the color of ae proportion in the treatment arm, 
#'                       and the second entry indicating the color of ae proportion in the control arm.
#'                       The default value is c("gold", "purple")
#' @param fig_prop_colwidth a numerical value to specify the column width of the two proportion plot
#'                          the default value is 300
#' @param fig_diff_range a numerical vector of length 2 to indicate 
#'                      the range to plot the difference between two ae proportion, ex., c(-0.5, 0.5)
#' @param fig_diff_label a string to indicate the x-label of the error.
#'                       The default value is "treatment <- Favor -> control"
#' @param fig_diff_color a string to indicate the color the error.
#'                       The default value is "black"
#' @param fig_diff_colwidth a numerical value to specify the column width of the two proportion plot
#'                          the default value is 300
#'                                             
#' @return a list contain the plotting details, including
#'         design_prop_cell: the Java Script to plot the two ae proportions;
#'         design_diff_cell: the Jave Script to plot the difference of the two ae proportions;
#'         design_prop_footer: a function to design the footer of the two ae proportions;
#'         design_diff_footer: a function to design the footer of the difference between two ae proportions.
#' @export
#'
#' @examples 
#' tb = data.frame(ae = c("headache", "pain", "fever", "running nose", "fever", "headache", "running nose"),
#'                 ae_label = c("ALL", "ALL", "ALL", "ALL", "AESER", "AEREL", "AEREL"),
#'                 n_1 = c(2, 3, 1, 5, 1, 1, 3),
#'                 n_2 = c(4, 5, 3, 7, 3, 3, 5),
#'                 N_1 = rep(50, 7),
#'                 N_2 = rep(50, 7))
#' tb = tb %>% dplyr::mutate(pct_1 = n_1/N_1,
#'                           pct_2 = n_2/N_2,
#'                           fig_diff = pct_1 - pct_2,
#'                           lower = fig_diff - 0.05,
#'                           upper = fig_diff + 0.05)
#' 
#' a <- plot_design(tb,
#'                  fig_prop_range = c(0, 0.8),
#'                  fig_prop_color = c("blue", "green"),
#'                  fig_prop_colwidth = 300,
#'                  fig_diff_range = c(-0.5, 0.9),
#'                  fig_diff_label = "treatment <- Favor -> control",
#'                  fig_diff_color = "blue",
#'                  fig_diff_colwidth = 300)

plot_design <- function(t_display, 
                        ## setting of the two proportion plots
                        fig_prop_range, 
                        fig_prop_label = c("treatment", "control"),
                        fig_prop_color = c("#00857C", "#66203A"),
                        fig_prop_colwidth = 300,
                        ## setting of the error bar plots
                        fig_diff_range, 
                        fig_diff_label = "treatment <- Favor -> control",
                        fig_diff_color = "black",
                        fig_diff_colwidth = 300){
  
  # Function to create proportion of subjects figure
  js_fig_prop_cell <- sparkline_point_js4(tbl = t_display,
                                          type = "cell",
                                          x = c("pct_1", "pct_2"),
                                          y = c(1, 1),
                                          xlim = fig_prop_range,
                                          color = fig_prop_color,
                                          width = fig_prop_colwidth, 
                                          height = 30,
                                          text = c("x[0]", "x[1]"))
  # Function to create Axis 
  fig_prop_footer <- sparkline_point_js4(tbl = data.frame(x = 1), 
                                         x = c("x", "x"),
                                         y = -1,
                                         xlab = "",
                                         type = "footer",
                                         xlim = fig_prop_range,
                                         height = 90,
                                         width = fig_prop_colwidth,
                                         color = fig_prop_color,
                                         legend = TRUE,
                                         legend_label = fig_prop_label,
                                         legend_title = "",
                                         legend_position = -0.8,
                                         legend_type = "point",
                                         margin = c(70, rep(0,4)))

  # Function to create proportion difference figure
  js_fig_diff_cell <- sparkline_point_js4(tbl = t_display,
                                          type = "cell",
                                          x = "fig_diff",
                                          x_lower = "lower",
                                          x_upper = "upper",
                                          xlim = fig_diff_range,
                                          width = fig_diff_colwidth,
                                          text = "'range:' + x + '(' + x_lower + ',' + x_upper + ')'",
                                          color = fig_diff_color)

  
  # Function to create Axis 
  fig_diff_footer <- sparkline_point_js4(tbl = data.frame(x = 1), 
                                         type = "footer",
                                         x = "x",
                                         y = -1,
                                         xlab = fig_diff_label,
                                         xlim = fig_diff_range,
                                         height = 90,
                                         width = fig_diff_colwidth,
                                         legend = FALSE,
                                         margin = c(70, rep(0,4)))
  
  list(design_prop_cell = js_fig_prop_cell,
       design_diff_cell = js_fig_diff_cell,
       design_prop_footer = fig_prop_footer,
       design_diff_footer = fig_diff_footer)
}
