
library(sf)
library(ggplot2)
library(raster)
library(dplyr, warn.conflicts = FALSE)
library(tidygeocoder)
library(showtext) 

font_add_google(name = "Roboto Mono", family = "Roboto Mono")
font_add_google(name = "Archivo", family = "Archivo")
showtext_auto()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# data from: https://grambank.clld.org/parameters/GB031#2/21.0/151.9
dual_dat <- read_sf('https://grambank.clld.org/parameters/GB031.geojson')

dual_dat$longitude <- st_coordinates(dual_dat$geometry)[, 1]
dual_dat$latitude <- st_coordinates(dual_dat$geometry)[, 2]

dual_dat_dual <- filter(dual_dat, label == 1)
dual_dat_singular <- filter(dual_dat, label == 0)

reverse_dual <- dual_dat %>%
  reverse_geocode(lat = latitude, long = longitude, method = 'osm',
                  address = address_found, full_results = TRUE)

library(countrycode)

reverse_dual$country_names <- countrycode(reverse_dual$country_code, "iso2c", "country.name")

reverse_sum <- reverse_dual |>
  tidyr::drop_na(country_names) |>
  group_by(country_names, country_code, label) |>
  summarise(count = n()) |>
  group_by(label) |>
  mutate(total_sum = sum(count)) |>
  filter(label != '?')

write.csv(reverse_sum, 'countries_dual.csv')

# read in processed data #### 
island_dual_dat <- read.csv('countries_dual.csv')

island_sum <- island_dual_dat |>
  group_by(label, island) |>
  summarise(count_dual = sum(count)) |>
  group_by(label) |>
  mutate(sum_total = sum(count_dual), prop_isl = count_dual / sum_total)

data_long <- tidyr::uncount(island_sum, count_dual)

## add generative noise & plot ####
# separate out yes islands & no islands
islands_yes <- filter(data_long, island == 'y')
islands_no <- filter(data_long, island == 'n')

random_swap_noise <- function(data, categories, reference_point, distance_threshold, noise_prob) {
  swapped_coord_data <- data
  
  x_coords <- swapped_coord_data$x
  y_coords <- swapped_coord_data$y
  
  # calculate distance in the y direction for every point
  y_distances <- abs(y_coords - reference_point[2])
  
  points_within_threshold <- which(y_distances <= distance_threshold)
  
  # randomly select points to swap coordinates x noise probability
  points_to_swap <- sample(points_within_threshold, size = floor(length(points_within_threshold) * noise_prob))
  
  for (i in points_to_swap) {
    j <- sample(points_within_threshold[-which(points_within_threshold == i)], size = 1)
    
    # swap coords
    temp_x <- swapped_coord_data[i, "x"]
    swapped_coord_data[i, "x"] <- swapped_coord_data[j, "x"]
    swapped_coord_data[j, "x"] <- temp_x
    
    temp_y <- swapped_coord_data[i, "y"]
    swapped_coord_data[i, "y"] <- swapped_coord_data[j, "y"]
    swapped_coord_data[j, "y"] <- temp_y
  }
  
  # Return the data with swapped coordinates
  return(swapped_coord_data)
}


# define possible categories
possible_categories <- c("0", "1")

# set noise probability
noise_probability <- 0.4

# set distance threshold for jitter
distance_threshold <- 2

circle_coords_list <- list()
i <- 1
for (df in list(islands_no, islands_yes)){
  areas <- c(rep(1, nrow(df))) 
  packing <- packcircles::circleProgressiveLayout(areas) 
  # shuffle packing coordinates?
  #packing <- packing[sample(1:nrow(packing)), ]
  # reorder so that the points follow top to bottom?
  packing <- packing[order(packing$y),]
  
  df <- cbind(df, packing)
  df$label <- as.factor(df$label)

  # identify point at which the categories shift
  breakpoint_index <- max(which(df$label == '0'))
  reference_point <- as.numeric(df[breakpoint_index, c("x", "y")])
  
  # run noise function on each df
  print('adding noise')
  df_with_noise <- random_swap_noise(df, possible_categories, reference_point, distance_threshold, noise_probability)

  # rotate coordinates
  print('rotating coordinates')
  x_coords <- df_with_noise$x
  y_coords <- df_with_noise$y
  
  angle <- 20 * (pi / 180)
  
  rotation_matrix <- matrix(c(cos(angle), -sin(angle),
                              sin(angle), cos(angle)), nrow = 2)
  
  rotated_coords <- cbind(x_coords, y_coords) %*% rotation_matrix
  
  df_with_noise$x <- rotated_coords[, 1]
  df_with_noise$y <- rotated_coords[, 2]
  
  circle_coords_list[[i]] <- df_with_noise
  i <- i+1
}


# combine list elements into full df so we can facet it
circle_coords_df <- do.call(rbind, circle_coords_list)

annot_df <- data.frame(island  = c("n", "y"),
                  plot_annot = c("non-island nations", "island nations"))

ggplot() +
  geom_point(data = circle_coords_df, 
             aes(x = x, y = y, color = label),
             size = 1.8) +
  facet_wrap(~island) +
  geom_text(data = annot_df, aes(x = 0, y = 22, label = plot_annot),
            color = 'grey20', family = 'Roboto Mono', size = 10) +
  scale_color_manual(values = c('#102542', '#F8551A')) +
  labs(title = "Singular, <span style = 'color:#F8551A;'>dual</span>, plural",
        subtitle = "Almost 40% of island nation languages have a dual form. <br>
       Only about 22% of non-island nation languages do.",
       caption = 'Details: Grambank data were reverse geocoded to 2023 country boundaries, after which each country was categorized as island vs. non-island. <br> 
       Approximately 100 languages with coordinates were not successfully reverse-coded.') +
  xlim(c(-22,22)) + ylim(c(-22, 23)) +
  coord_equal() +
  theme_void() +
  theme(plot.background = element_rect(color = NA, fill = '#D0EBE3'),
        strip.text = element_blank(),
        legend.position = 'none',
        plot.title = element_markdown(family = 'Archivo', size = 100,
                                      padding = margin(5.5, 5.5, 5.5, 5.5),
                                      hjust = .1),
        plot.subtitle = element_markdown(family = 'Roboto Mono', size = 30,
                                         padding = margin(5.5, 5.5, 5.5, 5.5),
                                         lineheight = .5,
                                         hjust = .1),
        plot.caption = element_markdown(family = 'Roboto Mono', size = 20,
                                        padding = margin(5.5, 5.5, 5.5, 5.5),
                                        lineheight = .5))

ggsave('dual_viz.png', height = 6, width = 8, dpi = 300)

