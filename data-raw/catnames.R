## Loads cat name data

cat_names <-
  readr::read_csv("data-raw/train.csv") %>%
  dplyr::left_join(readr::read_csv("data-raw/test.csv")) %>%
  dplyr::filter(animal_type == "Cat" & !is.na(name)) %>%
  dplyr::mutate(name = gsub(pattern = "\\*",
                            replacement = "",
                            x = name),
                breed = gsub(pattern = " Mix",
                             replacement = "",
                             x = breed)) %>%
  dplyr::mutate(color = ifelse(test = color == "Black/White" |
                                 color == "White/Black",
                               yes = "Tuxedo",
                               no = color)) %>%
  dplyr::mutate(color = strsplit(x = color,
                                 split = "/")) %>%
  tidyr::unnest(color) %>%
  dplyr::arrange(breed, color)

cat_breeds <- as.data.frame(table(catnames$breed))
cat_colors <- as.data.frame(table(catnames$color))

usethis::use_data(cat_names, overwrite = TRUE)
