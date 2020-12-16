##### Test DF -----
test_df <- data.frame(
  device = c("samsung smart tv", "google home", "record player", "radio", "lamp"),
  height_a = c(11.4, 1, .7, 1.3, 12),
  height_b = c(13, 1.3, .95, 1.3, 12.01),
  x = c(1, NA, 1, NA, 1),
  y = c(NA, 1, 1, 1, NA),
  z = c(1, 1, 1, 1, 1)
)


### Map Example
# purrr is a tidyverse lib is big on the map function.
# this of map as the same thing as distribution, where you can distribute a change to every cell in a column fast
test_df %>%
  dplyr::mutate(
    x_edit = purrr::map_dbl(.x = x, .f = function(num) { # map_dbl expects a number output value in the given cell
      num_good <- ifelse(is.na(num), 0, num)
      sum(num_good)
    }),
    x_bool = purrr::map_lgl(.x = x, .f = function(num) { # map_lgl expects a logical T/F output value in the given cell
      ifelse(is.na(num), FALSE, TRUE)
    })
  )

# But lets say I need all the values from column x to do a calculation
test_df %>%
  tidyr::nest(data = -c(device)) %>%
  dplyr::mutate(
    calc_for_a = purrr::map_dbl(.x = data, .f = function(df) {
      n <- df %>%
        dplyr::select(x) %>%
        dplyr::mutate(x = ifelse(is.na(x), 0, x)) %>%
        tibble::deframe() %>%
        sum()

      height_a <- df %>%
        dplyr::select(height_a) %>%
        dplyr::mutate(height_a = height_a / n) %>%
        tibble::deframe()
    }),
    calc_for_b = purrr::map_dbl(.x = data, .f = function(df) {
      n <- df %>%
        dplyr::select(x, y, z) %>%
        dplyr::mutate(x = ifelse(is.na(x), 0, x),
                      y = ifelse(is.na(y), 0, y),
                      z = ifelse(is.na(z), 0, z),
                      total = x + y + z) %>%
        dplyr::select(total) %>%
        tibble::deframe() %>%
        sum()

      height_b <- df %>%
        dplyr::select(height_b) %>%
        dplyr::mutate(height_b = height_b / n) %>%
        tibble::deframe()
    })
  )

### Function Example
# This actually looks very similar to purrr::map() when you write your own function
# I used to use this pattern below, but as I was 'forced' to use purrr moreso it turned out to be great
# Also given purrr's back end code, it's actually pretty efficient
temp_f <- function(df) {
  df %>%
    dplyr::mutate(x = ifelse(is.na(x), 0, x),
                  y = ifelse(is.na(y), 0, y),
                  z = ifelse(is.na(z), 0, z),
                  total = x + y + z,
                  x_fraction = x / total,
                  y_fraction = y / total,
                  z_fraction = z / total) %>%
    dplyr::select(device, total, x_fraction, y_fraction, z_fraction)
}

temp_f(test_df)
