#' Save GIFs found in Kantar data
#' @description The `pull_kantar` uses the \link{magick} package to initially process each image. This function then looks for any GIFs in the data, reads in all images and appends them together to recreate the GIF. Because `magick` will run into cache issues depending on working memory, for any GIFs with more than 20 frames, the function samples every other frame to cut down the file size. Multiple gifs are appended together, and the stacking depends on the width of the gif, so that a single saved gif will contain multiple gifs that will run simultaneously.
#' @param tbl Tibble of data returned by the `pull_kantar` function. Each row is a piece of creative
#'
#' @return This function saves the gifs in a new folder - "kantar_gifs" - in the current working directory. These gifs will eventually be
#' @export
#'
#' @examples
#' \dontrun{
#' save_gifs(tbl)
#' }
save_gifs <- function(tbl) {

  out <- tbl %>%
    dplyr::filter(format == "GIF") %>%
    dplyr::group_by(creative) %>%
    dplyr::mutate(nrow = dplyr::n()) %>%
    dplyr::filter(nrow > 1) %>%
    dplyr::mutate(id = dplyr::row_number(),
           selector = ifelse(nrow > 20 & id %% 2 != 1, "drop", "keep")) %>%
    dplyr::filter(selector == "keep") %>%
    dplyr::select(-c(id, selector)) %>%
    dplyr::arrange(product, width, height, nrow) %>%
    dplyr::mutate(group = ifelse(width < 600, "a", "b"),
                  group = ifelse(width > 1000, "c", group))

  bygroup <- function(data, grp, size) {
    # split on groups
    tmp <- data[data$group == grp,]
    n <- ifelse(grp == "a", 4,
                ifelse(grp == "b", 2, 1))
    ids <- rep(1:ceiling(nrow(tmp)/n), each = n)[1:nrow(tmp)] # define columns

    tmp <- tmp %>%
      dplyr::ungroup() %>%
      dplyr::mutate(id = ids)

    advert_list <- split(tmp, list(tmp$id, tmp$height, tmp$nrow))
    # drop empties from list
    advert_list <- Filter(function(x) dim(x)[1] > 0, advert_list)

    if (length(advert_list) == 0) {

    } else {
      # create folder if not here
      gif_folder <- file.path(here::here(), "kantar_gifs")
      dir_create <- function(x) ifelse(!dir.exists(x), dir.create(x), FALSE)
      dir_create(gif_folder)

      # function to build gifs
      append_to_gif <- function(tbl, size) {
        ffs <- unique(tbl$nrow)
        out <- add_image(tbl$link, size) %>%
          magick::image_border("white", "5x5")
        starts <- seq(1, length(out), ffs)
        for (i in 1:1) {
          new_gif <- magick::image_append(c(out[seq(i, length(out), ffs)]), stack = FALSE)
          for (j in 1:(ffs-1)) {
            tmp <- magick::image_append(c(out[starts+j]), stack = FALSE)
            new_gif <- c(new_gif, tmp)
          }
        }
        magick::image_write(new_gif, glue::glue(here::here("kantar_gifs/{unique(tbl$brand)}-gif-{unique(tbl$group)}{unique(tbl$id)}.gif")))
      }
      purrr::map2(advert_list, size, append_to_gif)
    }
  }
  out <- out %>%
    dplyr::mutate(size = dplyr::case_when(group == "a" ~ 300,
                            group == "b" ~ 500,
                            TRUE ~ 700))
  purrr::walk2(out$group, out$size, bygroup, data = out)
}
