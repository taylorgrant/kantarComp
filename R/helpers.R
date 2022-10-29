#' Summarise Quarterly Spend
#' @description Function to summarise quarterly ad spend. Can then group and summarise further as necessary.
#'
#' @param tbl Kantar data pull used in the `pull_kantar` function
#' @param row The row number where the data starts; determined in the `pull_kantar` function
#'
#' @return Tibble of summarized quarterly spend
#' @export
#'
#' @examples
#' \dontrun{
#' summarise_spend("path-to-kantar.xlsx", row = 7)
#' }
summarise_spend <- function(tbl, row) {
  tbl[-c(1:row - 1, nrow(tbl)),] %>%
    janitor::row_to_names(row_number = 1) %>%
    janitor::clean_names() %>%
    tidyr::pivot_longer(-c(parent:creative),
                 names_to = "quarter",
                 values_to = "spend") %>%
    dplyr::mutate(quarter = stringr::str_replace_all(quarter, "_000", ""),
           quarter = stringr::str_replace_all(quarter, "qtr_", ""),
           quarter = zoo::as.yearqtr(quarter, format = "%q_%Y"),
           quarter = factor(quarter),
           spend = as.numeric(spend)*1000) %>%
    tidyr::separate(product, into = c("brand", "product"), sep = " : ") %>%
    dplyr::group_by(advertiser, brand, product, media, quarter) %>%
    dplyr::summarise(spend = sum(spend, na.rm = TRUE))
}

#' Add Image
#' @description When appending images, scale them to cut down on file size
#'
#' @param imgurl url of the image being read in by `magick`
#' @param size Size of the scaling
#'
#' @return Scaled image
#' @export
#'
#' @examples
#' \dontrun{
#' see usage in `save_gifs()`
#' }
add_image <- function(imgurl, size){
  magick::image_read(imgurl) %>%
    magick::image_scale(stringr::str_extract(size, "[0-9]+"))
}
