#' Generate interactive forest plot with a select list as filter
#'
#' @param db a list of two components:
#'           (1) db$table: ae data frame rbinded by interested ae labels
#'           (2) db$listing: a standard adverse event data frame with select columns
#' @param fig_prop_color a character vector of length two to specify the colors to plot the two proportions.
#'                       The first entry refers to the color of the AE proportion in the treatment arm.
#'                       The second entry refers to the color of the AE proportion in the control arm.
#'                       The default value is c("gold", "purple").
#' @param fig_prop_label a character vector of length two to specify the x-axis label of the two propositions plots.
#'                       The first entry refers to the x-lab legend of the treatment arm.
#'                       The second entry refers to the x-lab legend of the control arm.
#'                       The default value is  c("treatment", "control")
#' @param fig_diff_color a string to specify the color to plot the error bar.
#'                       The default value is "black".
#' @param fig_diff_label a string to specify the x-axis legend of the error bar.
#'                       The default value is "treatment <- Favor -> control".
#' @param small_sample a integral vector of length 2. The first element is for treatment group and 
#'                     the second element is for control group. The default value is c(4, 4).
#' @return a reactable with a select list
#' @export
#'
#' @examples
#' library(dplyr)
#' tb <- data.frame(ae = c("headache", "pain", "fever", "running nose", "fever", "headache", "running nose"),
#'                  ae_label = c("ALL", "ALL", "ALL", "ALL", "AESER", "AEREL", "AEREL"),
#'                  n_1 = c(40, 50, 30, 50, 30, 30, 50),
#'                  n_2 = c(20, 30, 10, 50, 10, 10, 30),
#'                  N_1 = rep(60, 7),
#'                  N_2 = rep(60, 7),
#'                  stratum = rep("NULL", 7))
#' tb <- tb %>% dplyr::mutate(pct_1 = n_1/N_1 * 100, pct_2 = n_2/N_2 * 100)
#' db <- list(table = tb, listing = tb %>% rename(AE = ae),
#'            sample_size = tibble::tibble(treatment = c("treatment", "control"), N = c(60, 60)),
#'            treatment_order = c("MK9999" = "Xanomeline", "Placebo" = "Placebo"))
#' 
#' plot_forest(db)
#' plot_forest(db, fig_prop_color = c("red", "green"), fig_diff_color = "blue")


plot_forest <- function(db, 
                        fig_prop_color = c("#00857C", "#66203A"), 
                        fig_prop_label = NULL,
                        fig_diff_color = "black", 
                        fig_diff_label = NULL,
                        small_sample = NULL){
  
  # Set the default arguments
  if(is.null(fig_prop_label)){
    fig_prop_label = names(db$treatment_order)
  }
  
  if(is.null(fig_diff_label)){
    fig_diff_label = paste0(names(db$treatment_order)[1],
                            "<- Favor ->",
                            names(db$treatment_order)[2])
  }
  
  if(is.null(small_sample)){
    small_sample = c(4, 4)
  }
  
  # Listing of subjects details
  t_detail <- db$listing
  
  # Calculate test using M&N method 
  tb <- cbind(db$table,
              with(db$table, prop_test_mn(x0 = n_2, n0 = N_2, x1 = n_1, n1 = N_1))) 
  
  # Round the digits
  tb$pct_1 = round(tb$pct_1, 2)
  tb$pct_2 = round(tb$pct_2, 2)
  tb$lower = round(tb$lower, 2)
  tb$upper = round(tb$upper, 2)
  
  # Calculate the range of the forest plot
  fig_prop_range = round(range(c(tb$pct_1, tb$pct_2)) + c(-2, 2)) #c(-0.51, 0.51))
  fig_diff_range = round(range(c(tb$lower, tb$upper)) + c(-2, 2)) #c(-0.51, 0.51))
  
  # Sort the data frame to display in the reactable
  t_display <- tb %>% dplyr::mutate(fig_prop = NA, 
                                    fig_diff = round(est, 2), 
                                    ae = tools::toTitleCase(tolower(ae))) %>% 
                      dplyr::select(ae, ae_label, stratum, 
                                    fig_prop, fig_diff, 
                                    lower, upper, 
                                    n_1, pct_1, n_2, pct_2)
  
  # Make the error bar = 0 if the sample size is smaller than 4
  t_display$fig_diff[(t_display$n_1 <= small_sample[1] & t_display$n_2 <= small_sample[2])] = 0
  t_display$lower[(t_display$n_1 <= small_sample[1] & t_display$n_2 <= small_sample[2])] = 0
  t_display$upper[(t_display$n_1 <= small_sample[1] & t_display$n_2 <= small_sample[2])] = 0
  
  # Make the data frame eligible for check box design
  t_display1 <- crosstalk::SharedData$new(t_display)
  
  # Store the design details in a object
  design_details = plot_design(t_display, 
                               fig_prop_range, 
                               fig_prop_label = fig_prop_label,
                               fig_prop_color = fig_prop_color,
                               fig_diff_range, 
                               fig_diff_label = fig_diff_label,
                               fig_diff_color = fig_diff_color)
  
  # Make a reactable with a select list
  p <- bscols(
    # Width of the select list and reactable
    widths = c(1.5, 10.5),
    # Make a select list
    crosstalk::filter_select("filter_AEcategory", "AE filters", t_display1, ~ae_label, multiple = FALSE),
    # Make a reactable 
    mk_reactable(  #mk_reactable saved in the R/ folder: define default behavior of reactable.
      
      # data frame to plot
      t_display1,
      
      # reactable configuration https://glin.github.io/reactable/reference/reactable.html
      resizable = TRUE,
      filterable = TRUE,
      searchable = TRUE,
      defaultPageSize = 10,
      borderless = TRUE,
      striped = TRUE,
      highlight = TRUE,
      fullWidth = TRUE,
      width = 1200,
      theme = reactableTheme(cellPadding = "0px 8px"), # No padding between two cells 
                                                       # (ensure no break line between reference line)
      
      # Default sort variable
      defaultSorted = c("ae_label", "fig_diff"),
      defaultSortOrder = c("desc", "asc"),
      
      # Define listing of subjects
      details = function(index){
        t_row <- t_display[index, ]
        subset(t_detail, toupper(AE) %in% toupper(t_row$ae)) %>% mk_reactable
      },
      
      # Customize cell feature
      columnGroups = list(
        colGroup(name = paste0(fig_prop_label[1], "(N=", as.numeric(db$sample_size[1, "N"]), ")"),  columns = c("n_1", "pct_1")),
        colGroup(name = paste0(fig_prop_label[2], "(N=", as.numeric(db$sample_size[2, "N"]), ")"), columns = c("n_2", "pct_2"))
      ),
      
      # List of column definitions
      columns = list(
        ae = colDef(header = "Adverse Events", minWidth = 150, align = "right"),
        
        stratum = colDef(header = "Stratum", show = FALSE),
        
        ae_label = colDef(header = "Label", show = FALSE),
        
        fig_prop = colDef(header = "AE Proportion (%)",
                          width = 300, align = "center",
                          cell = JS(design_details$design_prop_cell),
                          footer = JS(design_details$design_prop_footer),
                          html = TRUE,
                          style = "font-size: 0px; padding: 0px; margin: 0px;",
                          footerStyle = "font-size: 0px; padding: 0px; margin: 0px;"),
        
        fig_diff = colDef(header = "Risk Diff (%) + 95% CI",
                          defaultSortOrder = "desc",
                          width = 300, align = "center",
                          cell = JS(design_details$design_diff_cell),
                          footer = JS(design_details$design_diff_footer),
                          html = TRUE,
                          style = "font-size: 0px; padding: 0px; margin: 0px;"),
        
        n_1 = colDef(header = "n", defaultSortOrder = "desc", width = 60, align = "center"),
        n_2 = colDef(header = "n", defaultSortOrder = "desc", width = 60, align = "center"),
        
        pct_1 = colDef(header = "(%)", defaultSortOrder = "desc", width = 80,
                       align = "center", format = colFormat(digits = 2) ),
        pct_2 = colDef(header = "(%)", defaultSortOrder = "desc", width = 80,
                       align = "center", format = colFormat(digits = 2) ),
        
        lower = colDef(header = "lower CI", show = FALSE),
        upper = colDef(header = "upper CI", show = FALSE)
      )
    ),
    crosstool(t_display1,
              # transceiver widgets are more like normal crosstalk widgets.
              class = "transceiver",
              # set the initial value
              init = which(t_display$ae_label == "All"),
              # channel set to "filter" to use the crosstalk filter handle
              channel = "filter",
              # reset optional vector of crosstalk group keys;
              # use with init when data == relay (one crosstalk group) to reset the initial filter/select handle.
              reset = rownames(t_display))
  )
  
  
  plotly_js <- "https://unpkg.com/react-plotly.js@1.0.2/dist/create-plotly-component.js"
  browsable(tagList(
    reactR::html_dependency_react(),
    htmltools::tags$script(src = "https://cdn.plot.ly/plotly-latest.min.js"),
    htmltools::tags$script(src = plotly_js),
    p
  ))
  
}

