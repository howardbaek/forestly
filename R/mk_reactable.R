#' mk_reactable
#'
#' mk_reactable
#'
#' This function is used to create a data table from tabular data with sorting and pagination by default.
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
#' @return an interactive data table.
#' @export
#' @examples
#' library(reactable)
#' mk_reactable(iris[1:5, ], resizable = TRUE, filterable = TRUE, searchable = TRUE, defaultPageSize = 10,
#'              borderless = TRUE, striped = TRUE, highlight = TRUE)
mk_reactable <- function(data,
                         resizable = TRUE,
                         filterable = TRUE,
                         searchable = TRUE,
                         defaultPageSize = 10,
                         borderless = TRUE,
                         striped = TRUE,
                         highlight = TRUE,
                         ...){

  reactable(data = data,
            resizable  = resizable,
            filterable = filterable,
            searchable = searchable,
            defaultPageSize = defaultPageSize,
            borderless = borderless,
            striped = striped,
            highlight = highlight,
            ...)

}

