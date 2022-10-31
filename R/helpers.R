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
           spend = as.numeric(spend)) %>%
    tidyr::separate(product, into = c("brand", "product"), sep = " : ") %>%
    dplyr::group_by(advertiser, brand, product, media, quarter) %>%
    dplyr::summarise(spend = round(sum(spend, na.rm = TRUE)))
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

#' Plot Highchart
#' @description Function to parse spend data and plot with `highcharter`
#' @param tbl Spend data
#' @param group What to aggregate by - advertiser, brand, product
#'
#' @return highcharter graph
#' @export
#'
#' @examples
#' \dontrun{
#' plot_highchart(spend, advertiser)
#' }
plot_highchart <- function(tbl, group) {
  group <- dplyr::enexpr(group)
  data <- tbl %>%
    dplyr::group_by(!!group, quarter) %>%
    dplyr::summarise(spend = sum(spend)) %>%
    dplyr::filter(spend > 0)

  # highcharter
  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- ","
  options(highcharter.lang = hcoptslang)

  pal <- c('#2f7ed8', '#0d233a', '#8bbc21', '#910000', '#1aadce',
           '#492970', '#f28f43', '#77a1e5', '#c42525', '#a6c96a')

  hc <- highcharter::hchart(data, 'column',
                            highcharter::hcaes(x = quarter, y = spend,
                                     group = !!group, marker = FALSE)) %>%
    highcharter::hc_xAxis(title = list(text = glue::glue("Quarterly {stringr::str_to_title(group)} Spend")),
             type = "category") %>%
    highcharter::hc_yAxis(title = list(text = "Spend ($1000's)"),
                          labels = list(format = "${value}")) %>%
    highcharter::hc_colors(pal) %>%
    highcharter::hc_plotOptions(line = list(marker = list(enabled = FALSE))) %>%
    highcharter::hc_title(text = paste0("Spend by Quarter")) %>%
    highcharter::hc_credits(enabled = TRUE,
               text = "Source: Kantar",
               style = list(fontSize = "10px"))

  return(hc)
}

#' Plot Highchart (multgroup)
#' @description Passing multiple arguments to group by and hardcoding the plot
#' @param tbl Spend data in tibble
#' @param ... Multiple groups, either brand & product or brand & media
#'
#' @return Highchart object
#' @export
#'
#' @examples
#' \dontrun{
#' plot_multichart(spend, brand, product)
#' }
plot_multichart <- function(tbl, ...) {
  groupCol <- dplyr::quos(...)

  # highcharter
  hcoptslang <- getOption("highcharter.lang")
  hcoptslang$thousandsSep <- ","
  options(highcharter.lang = hcoptslang)
  pal <- c('#2f7ed8', '#0d233a', '#8bbc21', '#910000', '#1aadce',
           '#492970', '#f28f43', '#77a1e5', '#c42525', '#a6c96a')

  if (as.list(match.call())[4] == "product") {
    data <- tbl %>%
      dplyr::group_by(!!!groupCol, quarter) %>%
      dplyr::summarise(spend = sum(spend)) %>%
      dplyr::filter(spend > 0) %>%
      tidyr::unite("product", brand:product, sep = " - ")

    hc <- highcharter::hchart(data, 'column',
                              highcharter::hcaes(x = quarter, y = spend,
                                                 group = product, marker = FALSE)) %>%
      highcharter::hc_xAxis(title = list(text = glue::glue("Quarterly Product Spend")),
                            type = "category")
  } else {
    data <- tbl %>%
      dplyr::group_by(!!!groupCol, quarter) %>%
      dplyr::summarise(spend = sum(spend)) %>%
      dplyr::filter(spend > 0) %>%
      tidyr::unite("product", brand:media, sep = " - ")

    hc <- highcharter::hchart(data, 'column',
                              highcharter::hcaes(x = quarter, y = spend,
                                                 group = product, marker = FALSE)) %>%
      highcharter::hc_xAxis(title = list(text = glue::glue("Quarterly Media Spend")),
                            type = "category")
  }
  hc <- hc %>%
    highcharter::hc_yAxis(title = list(text = "Spend")) %>%
    highcharter::hc_colors(pal) %>%
    highcharter::hc_plotOptions(line = list(marker = list(enabled = FALSE))) %>%
    highcharter::hc_title(text = paste0("Spend by Quarter")) %>%
    highcharter::hc_credits(enabled = TRUE,
                            text = "Source: Kantar",
                            style = list(fontSize = "10px"))
  return(hc)
}

#' Spend Tables
#'
#' @description Simple function to create DT spend tables
#' @param tbl Tibble spend data from `kantar_pull` function
#' @param choice Type of spend data
#'
#' @return DT datatable rendered in shiny
#' @export
#'
#' @examples
#' \dontrun{
#' spend_table(spend, brand)
#' }
spend_table <- function(tbl, choice) {

  if (choice == "advertiser") {
    # advertiser
    out <- tbl %>%
      dplyr::group_by(advertiser, quarter) %>%
      dplyr::summarise(spend = sum(spend)) %>%
      tidyr::pivot_wider(id_cols = advertiser,
                         names_from = quarter,
                         values_from = spend) %>%
      dplyr::rename(Advertiser = advertiser)
  } else if (choice == "brand") {
    # brand
    out <- tbl %>%
      dplyr::group_by(brand, quarter) %>%
      dplyr::summarise(spend = sum(spend)) %>%
      tidyr::pivot_wider(id_cols = brand,
                  names_from = quarter,
                  values_from = spend) %>%
      dplyr::rename(Brand = brand)
  } else if (choice == 'media') {
    out <- tbl %>%
      dplyr::group_by(brand, media, quarter) %>%
      dplyr::summarise(spend = sum(spend)) %>%
      tidyr::unite("media", brand:media, sep = "-") %>%
      tidyr::pivot_wider(id_cols = media,
                         names_from = quarter,
                         values_from = spend) %>%
      tidyr::separate(media, into = c("Brand", "Media"), sep = "-")
  } else {
    out <- tbl %>%
      dplyr::group_by(brand, product, quarter) %>%
      dplyr::summarise(spend = sum(spend)) %>%
      tidyr::unite("product", brand:product, sep = "-") %>%
      tidyr::pivot_wider(id_cols = product,
                  names_from = quarter,
                  values_from = spend) %>%
      tidyr::separate(product, into = c("Brand", "Product"), sep = "-")
  }
  dt <- out %>%
    DT::datatable(extensions = "Buttons",
                  rownames = FALSE,
                  options = list(dom = "Blrtip",
                                 buttons = c('excel'),
                  columnDefs = list(list(width = '150px', targets = c(0,1)))),
                  caption = "Quarterly Spend ($1000's)")

  if (choice %in% c("advertiser", "brand")) {
    dt %>%
      DT::formatCurrency(columns = 2:ncol(out), currency = "$",
                         interval = 3, mark = ",",digits = 0)
  } else {
    dt %>%
      DT::formatCurrency(columns = 3:ncol(out), currency = "$",
                         interval = 3, mark = ",",digits = 0)
  }

}


