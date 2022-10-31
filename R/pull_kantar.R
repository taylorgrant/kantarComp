#' Extract Kantar Data
#'
#' @description Given a specific data pull from Kantar, extract and collate spend data at various levels and also process digital imagery with the \href{https://cran.r-project.org/package=magick/vignettes/intro.html}{magick} package.
#'
#' @param filename The path and file name of the Kantar data pull.
#'
#' @return A list with two tibbles of data; "image_out" contains the `magick` information for each image or gif used by an advertiser; "spend" is summarized spend data for each quarter
#' @export
#'
#' @examples
#' \dontrun{
#' filename <- "~/Desktop/kantar_data_pull.xlsx"
#' dat <- pull_kantar(filename)
#' }
pull_kantar <- function(filename) {

  dat <- readxl::read_excel(filename, sheet = 1)
  row_start <- which(dat == "PARENT", arr.ind=TRUE)[1]

  x <- tidyxl::xlsx_cells(filename)
  skip <- row_start + 1

  build_df <- function(rw){
    tmp <- x[x$row == rw, c("character")]
    tibble::tibble(parent = tmp$character[1],
           advertiser =  tmp$character[2],
           brand =  tmp$character[3],
           product =  tmp$character[4],
           media =  tmp$character[5],
           creative =  tmp$character[6])
  }

  # build the dataframe
  nrow <- 1:(nrow(dat) - skip) + skip
  test <- nrow %>%
    purrr::map_dfr(build_df)

    # function to try to read imagery and get info
    try_read <- purrr::possibly(function(x) {
      magick::image_info(magick::image_read(x))
    }, otherwise = NA)

    cat("Reading image info...\n")
    image_out <- test %>%
      dplyr::filter(media == "Internet - Display") %>%
      dplyr::mutate(creative = gsub(".*\\#", "", creative),
             link = paste0("http://www.evaliant.net/creatives/displayadimg.aspx?id=",creative,"&rc=us")) %>%
      dplyr::mutate(info = purrr::map(link, try_read)) %>%
      tidyr::unnest(cols = info) %>%
      dplyr::filter(!is.na(format))

    cat("Saving gifs to folder...\n")
    if (any(unique(image_out$format) == "GIF")) {
      # now find, build, and save the gifs
      list_to_output <- split(image_out, image_out$advertiser)
      for (i in 1:length(list_to_output)) {
        save_gifs(list_to_output[[i]])
      }
    }

    # summarise the spend data while here
    spend <- summarise_spend(dat, row_start)

    output <- list(image_out = image_out,
                   spend = spend)
}
