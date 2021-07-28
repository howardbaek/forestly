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
#' @param fig_diff_range a numerical vector of length 2 to indicate 
#'                      the range to plot the difference between two ae proportion, ex., c(-0.5, 0.5)
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
#' tb = tb %>% mutate(pct_1 = n_1/N_1,
#'                    pct_2 = n_2/N_2,
#'                    fig_diff = pct_1 - pct_2,
#'                    lower = fig_diff - 0.05,
#'                    upper = fig_diff + 0.05)
#' 
#' plot_design(tb, color = c("blue", "green"), 
#'             fig_prop_range = c(0, 0.8), fig_diff_range = c(-0.5, 0.9))

plot_design <- function(t_display, 
                        ## setting of the two proportion plots
                        fig_prop_range, 
                        fig_prop_label = c("treatment", "control"),
                        fig_prop_color = c("gold", "purple"),
                        ## setting of the error bar plots
                        fig_diff_range, 
                        fig_diff_label = "treatment <- Favor -> control",
                        fig_diff_color = "black"){
  
  # Function to create proportion of subjects figure
  js_fig_prop_cell <- sparkline_point_js2(tbl = t_display,
                                          x = c("pct_1","pct_2"),
                                          y = c(1, 1),
                                          xlim =  fig_prop_range,
                                          color = fig_prop_color,
                                          height = 30,
                                          text = c("x[0]", "x[1]"))
  # Function to create Axis 
  fig_prop_footer <- function(value){
    #tmp <- tb[1, ] %>% pivot_longer(cols = starts_with("pct"))
    #xlim <- round(range(c(tb$pct_1, tb$pct_2)) + c(-0.51, 0.51) )
    sparkline_draw_axis(color = fig_prop_color, 
                        label = fig_prop_label,
                        #label = c("MK9999", "Placebo"),
                        #label = levels(db$listing$treatment), 
                        xlim = fig_prop_range,
                        showlegend = TRUE)
  }
  
  # Function to create proportion difference figure
  js_fig_diff_cell <- sparkline_point_js2(tbl = t_display, 
                                          x = "fig_diff", 
                                          x_lower = "lower", 
                                          x_upper = "upper",
                                          xlim = fig_diff_range,
                                          text = "'range:' + x + '(' + x_lower + ',' + x_upper + ')'",
                                          color = fig_diff_color)
  
  # Function to create Axis 
  fig_diff_footer <- function(value){
    #tmp <- tb[1, ] %>% pivot_longer(cols = starts_with("pct"))
    #xlim = round(range(c(tb$lower, tb$upper)) + c(-0.51, 0.51))
    sparkline_draw_axis(showlegend = FALSE, 
                        xlab = fig_diff_label,
                        #xlab = "MK9999 <- Favor -> Placebo", 
                        color = "black",
                        xlim = fig_diff_range, 
                        height = 58, margin_bottom = 50)
  }
  
  
  list(design_prop_cell = js_fig_prop_cell,
       design_diff_cell = js_fig_diff_cell,
       design_prop_footer = fig_prop_footer,
       design_diff_footer = fig_diff_footer)
}
