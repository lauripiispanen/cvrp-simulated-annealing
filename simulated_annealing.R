library(maps)
library(ggplot2)
library(dplyr)
library(geosphere)
library(gtable)
library(grid)

capitals <- world.cities %>%
  filter(capital == 1) %>%
  mutate(weight = log(pop)) %>%
  select(lat, long, weight)

set.seed(12345)

origin_lat = 68.073611
origin_long = 29.315278
initial_temperature <- 1.0
max_leg_weight <- 200

refine_route <- function(route, temp) {
  new_route <- as_tibble(route)
  row_order <- 1:nrow(new_route)
  num_to_swap <- as.integer(nrow(new_route) * runif(1, 0, temp))
  for (idx in 1:num_to_swap) {
    entries <- as.integer(runif(c(1, 1), 0, nrow(new_route)))
    row_order[entries] = row_order[rev(entries)]
  }
  new_route[row_order, ]
}

add_home_base_visits <- function(route) {
  new_route <- tibble(lat = origin_lat, long = origin_long)
  current_weight <- 0
  for (idx in 1:nrow(route)) {
    next_dest <- route[idx, ]
    if (current_weight + next_dest$weight >= max_leg_weight) {
      new_route <- add_row(new_route, lat = origin_lat, long = origin_long)
      current_weight <- 0
    }
    new_route <- add_row(new_route, lat = next_dest$lat, long = next_dest$long)
    current_weight <- current_weight + next_dest$weight
  }
  add_row(new_route, lat = origin_lat, long = origin_long)
}

calculate_route_total_cost <- function(route) {
  route %>%
    add_home_base_visits(.) %>%
    mutate(next_long = lead(long), next_lat = lead(lat)) %>%
    rowwise() %>%
    mutate(dist = distm(c(long, lat), c(next_long, next_lat), fun = distHaversine)) %>%
    ungroup() %>%
    na.omit %>%
    summarise(dist = sum(dist)) %>%
    pull(dist)
}

initial_route <- capitals %>% sample()

route_to_segments <- function(route) {
  points <- route %>%
    add_home_base_visits(.) %>%
    mutate(
      latend = lead(lat),
      longend = lead(long)
    ) %>%
    select(
      lat, long, latend, longend
    ) %>%
    na.omit

  (gcIntermediate(
       points %>% select(long, lat),
       points %>% select(longend, latend),
       breakAtDateLine = TRUE,
       addStartEnd = TRUE
    ) %>%
      purrr::map(function(x) { if (!is.list(x)) { list(x) } else { x }}) %>%
      purrr::flatten() %>%
      purrr::map(as.data.frame)) %>%
      purrr::map(function(x) {
        x %>%
          mutate(
            latend = lead(lat),
            long = lon,
            longend = lead(long)
          ) %>%
          select(long, lat, longend, latend) %>%
          filter(complete.cases(.))
      }) %>%
      bind_rows()
}

current_route <- initial_route
temperature <- initial_temperature
cost_history <- calculate_route_total_cost(current_route)

select_best_route <- function(..., temp = NULL) {
  if (is.null(temp)) {
    stop("Temperature cannot be null")
  }
  routes <- list(...)
  routes[[routes %>% purrr::map(calculate_route_total_cost) %>% which.min]]
}

plot_route <- function(route, cost_history) {
  cost_hist <- data.frame(cost = cost_history)
  g_map <- ggplot(route_to_segments(route)) +
    borders("world", colour="gray50", fill="gray50") +
    geom_point(data = capitals,
               aes(x = long, y = lat, colour = "red"),
               size = 2.5,
               show.legend = FALSE) +
    geom_segment(aes(x = long, y = lat, xend = longend, yend = latend),
                 size = 0.5) +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank()) +
    coord_equal(expand = FALSE,
                xlim = c(-180, 180),
                ylim = c(-84, 84),
                ratio = 1.2)
  
  g_cost <- ggplot(cost_hist) +
    geom_line(aes(x=1:nrow(cost_hist), y=cost), size = 1) +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          plot.background = element_rect(
            fill = "white",
            colour = "grey75",
            size = 1
          ),
          plot.caption = element_text(vjust = 2.5, hjust = 0.5, size=rel(2.5)),
          plot.margin = margin(t = 0.1, l = 0, b = 0, r = 0.1, "cm")) +
    labs(caption="Cost") +
    geom_text(data = data.frame(x = c(1.5), y = c(1), text = c(last(cost_history))),
              aes(x, y, label = text),
              size = 25,
              vjust = "bottom")
  
  p1 <- ggplotGrob(g_map)
  p2 <- ggplotGrob(g_cost)
  
  gt <- gtable(widths = unit(c(1, .5, .9, .1), "null"), 
               heights = unit(c(.2, 1, 1, .75, .1), "null"))
  gt <- gtable_add_grob(gt, p1, t = 1, b = 5, l = 1, r = 4)
  gt <- gtable_add_grob(gt, p2, t = 4, b = 4, l = 3, r = 3)
}


rounds <- 1
prev_cost <- NULL

for (i in 1:rounds) {
  current_route <- select_best_route(
    refine_route(current_route, temperature),
    current_route,
    temp = temperature
  )
  current_cost <- calculate_route_total_cost(current_route)
  if (is.null(prev_cost) || current_cost < prev_cost) {
    cost_history <- c(cost_history, current_cost)
    temperature <- temperature * (1 - (i / rounds))
    g <- plot_route(current_route, cost_history)
    out_filename <- paste0("route", length(cost_history), ".png")
    width_px <- 3840
    height_px <- 2160
    out_dpi <- 72
    ggsave(out_filename,
           plot = g,
           device = "png",
           dpi = out_dpi,
           width = width_px / out_dpi,
           height = height_px / out_dpi,
           units = "in",
           limitsize = FALSE)
    graphics.off()
    prev_cost <- current_cost
  }
}