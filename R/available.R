#' List supported file format conversions
#'
#' @return A dataframe with two columns: `Input` and `Output`, describing the source and target file formats for each conversion.
#' @export
#'
#' @examples
#' supported_conversions()
supported_conversions <- function(){
  read.csv(system.file("supported.tsv",package = "interchange"), check.names = FALSE, header = TRUE, sep = "\t")
}
