#' Extract Kantar Data
#'
#' @description Given a specific data pull from Kantar, extract and collate spend data at various levels and also process digital imagery with the \href{https://cran.r-project.org/package=magick/vignettes/intro.html}{magick} package.
#'
#' @param filename The path and file name of the Kantar data pull.
#' @param asset_count Number of assets per advertiser; selected via slider input in app
#'
#' @return A list with two tibbles of data; "image_out" contains the `magick` information for each image or gif used by an advertiser; "spend" is summarized spend data for each quarter
#' @export
#'
#' @examples
#' \dontrun{
#' filename <- "~/Desktop/kantar_data_pull.xlsx"
#' dat <- pull_kantar(filename)
#' }
pull_kantar <- function(filename, asset_count) {

  dat <- readxl::read_excel(filename, sheet = 1)
  dat <- dat[-nrow(dat),]
  row_start <- which(dat == "PARENT", arr.ind=TRUE)[1]

  dat <- dat |>
    dplyr::slice(-c(1:row_start - 1)) |>
    janitor::row_to_names(row_number = 1) |>
    janitor::clean_names() |>
    dplyr::ungroup()

  # only want imagery for previous 2 quarters
  # qtrs <- stringr::str_replace_all(glue::glue("qtr {seq(lubridate::quarter(Sys.Date()) - 1,
  #                                    lubridate::quarter(Sys.Date()) - 2)} {lubridate::year(Sys.Date())}_000"), " ", "_")

  # grab the top n (spend based) from the last quarter
  qtrs <- glue::glue("qtr_{lubridate::quarter(Sys.Date())-1}_{lubridate::year(Sys.Date())}_000")

  cat("Reading image info...\n")
  tmpdat <- dat |>
    tidyr::pivot_longer(-c(parent:creative),
                        names_to = "quarter",
                        values_to = "spend") |>
    dplyr::filter(quarter %in% qtrs) |>
    dplyr::filter(media == "Internet - Display" &
                    !is.na(spend)) |>
    dplyr::distinct(creative, .keep_all = TRUE) |>
    dplyr::mutate(creative = gsub(".*\\#", "", creative),
                  link = paste0("http://www.evaliant.net/creatives/displayadimg.aspx?id=",creative,"&rc=us"),
                  spend = as.numeric(spend)) |>
    dplyr::group_by(advertiser) |>
    dplyr::slice_max(spend, n = asset_count)

  # function to try to read imagery and get info from server
  try_read <- purrr::possibly(function(x) {
    magick::image_info(magick::image_read(x))
  }, otherwise = NA)

  # function to download imagery if necessary
  try_dl <- purrr::possibly(function(url) {
    fig_folder <- file.path(here::here(), "kantar_jpgs")
    dir_create <- function(x) ifelse(!dir.exists(x), dir.create(x), FALSE)
    dir_create(fig_folder)

    tmp <- tempfile()
    utils::download.file(url, destfile = tmp)
    utils::unzip(tmp, exdir = "tmpdata/unzipped/.")
    jpg_files <- dir(file.path(here::here("tmpdata", "unzipped")), pattern = "*.jpeg", full.names = TRUE)
    file.copy(from = jpg_files[1], to = here::here('kantar_jpgs'))
    f <- list.files(file.path(here::here("tmpdata", "unzipped")), full.names = T)
    file.remove(f)
    return(stringr::str_replace(jpg_files[1], "tmpdata/unzipped", "kantar_jpgs"))
  }, otherwise = NA)

  # 1st round - what's on server
  image_out <- tmpdat |>
    dplyr::mutate(info = purrr::map(link, try_read)) |>
    tidyr::unnest(cols = info)

  # 2nd round - downloads
  image_out_dl <- image_out |>
    dplyr::filter(is.na(format)) |>
    dplyr::select(-c(format:density)) |>
    dplyr::mutate(link = purrr::map(link, try_dl)) |>
    dplyr::mutate(info = purrr::map(link, try_read)) |>
    tidyr::unnest(cols = c(link, info)) |>
    dplyr::filter(!is.na(format))

  image_out <- image_out |>
    dplyr::bind_rows(image_out_dl) |>
    dplyr::filter(!is.na(format)) |>
    dplyr::arrange(advertiser, dplyr::desc(spend))

  cat("Saving gifs to folder...\n")
    if (any(unique(image_out$format) == "GIF")) {
      # now find, build, and save the gifs
      list_to_output <- split(image_out, image_out$advertiser)
      for (i in 1:length(list_to_output)) {
        save_gifs(list_to_output[[i]])
      }
    }

    # summarise the spend data while here
    spend <- summarise_spend(dat)

    output <- list(image_out = image_out,
                   spend = spend)
}
