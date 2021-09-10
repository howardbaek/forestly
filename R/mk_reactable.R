#' mk_reactable
#'
#' mk_reactable
#'
#' This function is used to create a data table from tabular data with sorting and pagination by default.
#' The reactable configuration follows https://glin.github.io/reactable/reference/reactable.html
#' The data table is an HTML widget that can be used in R Markdown documents and Shiny applications, or viewed from an R console.
#'
#' @param data A data frame or matrix to obtain variables.such as 'iris' data set.
#' @param resizable A character string to define the criteria to enable columns resizing.
#' @param filterable A character string to define the criteria to enable columns filtering.
#' @param searchable A character string to define the criteria to enable global table searching.
#' @param defaultPageSize A character string to define the Page size options for the table.
#' @param borderless  A character string to remove inner borders from table.
#' @param striped A character string to add zebra-striping to table rows.
#' @param highlight A character string to highlight table rows on hover.
#' @param fullWidth Stretch the table to fill the full width of its container? Defaults to TRUE.
#' @param width Width of the table in pixels. Defaults to "auto" for automatic sizing.
#' @param theme Theme options for the table. 
#'              The default value is \code{reactableTheme(cellPadding = "0px 8px")}: 
#'              No padding between two cells (ensure no break line between reference line)
#' @param ... Additional arguments transferred to reactable.
#' 
#' @return an interactive data table.
#' 
#' @examples
#' \dontrun{
#' mk_reactable(iris[1:5, ], resizable = TRUE, filterable = TRUE, 
#'              searchable = TRUE, defaultPageSize = 10,
#'              borderless = TRUE, striped = TRUE, highlight = TRUE)
#' }              
#'
#' @export
mk_reactable <- function(data,
                         resizable = TRUE,
                         filterable = TRUE,
                         searchable = TRUE,
                         defaultPageSize = 10,
                         borderless = TRUE,
                         striped = TRUE,
                         highlight = TRUE,
                         fullWidth = TRUE,
                         width = 1200,
                         theme = reactableTheme(cellPadding = "0px 8px"), 
                         ...){

  reactable::reactable(data = data,
            resizable  = resizable,
            filterable = filterable,
            searchable = searchable,
            defaultPageSize = defaultPageSize,
            borderless = borderless,
            striped = striped,
            highlight = highlight,
            fullWidth = fullWidth,
            width = width,
            theme = theme,
            ...)

}

