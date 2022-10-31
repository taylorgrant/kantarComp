#' Render Images
#' @description Sort, resize and render images in order of size. If smaller width, stack in columns, otherwise stack in rows.
#'
#' @param tbl Tibble output from from `pull_kantar` function
#' @param size Size for scaling
#' @param group Group either a, b, or c depending on width of image
#' @param n Number of columns
#' @param stack_yn Stack images or not
#'
#' @return Images are to be renedered in a .Rmd file
#' @export
#'
#' @examples
#' \dontrun{
#' list_to_output <- split(tbl, tbl$advertiser)
#' render_images(list_to_output[[1]], size = 175, group = "a", n = 4, stack_yn = TRUE)
#' }
render_images <- function(tbl, size, group, n, stack_yn) {

  # if null results for a group, make a white blank placeholder
  make_blanks <- function() {
    magick::image_blank(width=1020, height=10, color = "white") %>%
      magick::image_border('white', '5x5')
  }

  # function to contain filesize
  add_image <- function(filepath, size){
    magick::image_read(filepath) %>%
      magick::image_scale(stringr::str_extract(size, "[0-9]+"))
  }

  # function for appending
  append_img <- function(tbl, size) {
    add_image(tbl$link, size) %>%
      magick::image_border("white", "5x5") %>%
      magick::image_append(stack = FALSE)
  }

  # add size groupings
  out <- tbl %>%
    dplyr::group_by(creative) %>%
    dplyr::mutate(nrow = dplyr::n()) %>%
    dplyr::filter(!is.na(format)) %>%
    dplyr::arrange(product, format, nrow, width, height) %>%
    tidyr::nest(info = colorspace:density) %>%
    dplyr::filter(format %in% c("JPEG", "PNG")) %>%
    dplyr::mutate(group = ifelse(width < 600, "a", "b"),
                  group = ifelse(width > 1000, "c", group))

  # split on groups
  tmp <- out[out$group == group,]
  ids <- rep(1:ceiling(nrow(tmp)/n), each = n)[1:nrow(tmp)] # defines columns

  tmp <- tmp %>%
    dplyr::ungroup() %>%
    dplyr::mutate(id = ids)

  advert_list <- split(tmp, tmp$id)

  # appending into n columns
  testfig <- purrr::map2(advert_list, size, append_img)

  if (length(testfig) > 1) {
    fig <- testfig[[1]]
    for (i in 2:length(testfig)) {
      out <- testfig[[i]]
      fig <- c(fig, out)
    }
  } else {
    fig <- testfig[1]
  }

  if (is.null(fig[[1]])) {
    make_blanks()
  } else if (length(fig) == 1) {
    magick::image_append(append_img(tmp, size), stack = stack_yn)
  } else {
    magick::image_append(fig, stack = stack_yn)
  }
}
