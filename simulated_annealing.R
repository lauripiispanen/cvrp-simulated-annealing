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
  num_to_swap <- (runif(1, 0, temp) * 5) + 1
  for (idx in 1:num_to_swap) {
    entries <- as.integer(runif(c(1, 1), 0, nrow(new_route)))
    row_order[entries] = row_order[rev(entries)]
  }
  new_route[row_order, ]
}

add_home_base_visits <- function(route) {
  new_route <- tibble(lat = origin_lat, long = origin_long)
  lats <- c(origin_lat)
  longs <- c(origin_long)

  current_weight <- 0
  for (idx in 1:nrow(route)) {
    next_dest <- route[idx, ]
    if (current_weight + next_dest$weight >= max_leg_weight) {
      lats <- c(lats, origin_lat)
      longs <- c(longs, origin_long)
      current_weight <- 0
    }
    lats <- c(lats, next_dest$lat)
    longs <- c(longs, next_dest$long)
    current_weight <- current_weight + next_dest$weight
  }
  lats <- c(lats, origin_lat)
  longs <- c(longs, origin_long)

  tibble(lat = lats, long = longs)
}

distances_for_cities <- list()

m_dist <- function(long, lat, long2, lat2) {
  key <- paste(
    round(long, digits = 2),
    round(lat, digits = 2),
    round(long2, digits = 2),
    round(lat2, digits = 2),
    sep = "+")
  if (is.null(distances_for_cities[[key]])) {
    distances_for_cities[[key]] <<- distm(c(long, lat), c(long2, lat2), fun = distHaversine)
  }
  distances_for_cities[[key]]
}

calculate_route_total_cost <- function(route) {
  sum(apply(route %>%
    add_home_base_visits(.) %>%
    mutate(
      next_long = lead(long),
      next_lat = lead(lat)
    ) %>% filter(complete.cases(.)), 1, function(x) {
      m_dist(x[[2]], x[[1]], x[[3]], x[[4]])
    }))
}

initial_route <- as_tibble(capitals %>% sample())

route_to_segments <- function(route) {
  points <- route %>%
    add_home_base_visits(.) %>%
    mutate(
      latend = lead(lat),
      longend = lead(long)
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
          filter(complete.cases(.))
      }) %>%
      bind_rows()
}

current_route <- initial_route
temperature <- initial_temperature
prob_choose_worse <- 0.01
cost_history <- calculate_route_total_cost(current_route)

select_better_route <- function(current_route,
                                current_cost,
                                suggested_route,
                                temp = NULL) {
  if (is.null(temp)) {
    stop("Temperature cannot be null")
  }
  
  new_cost <- calculate_route_total_cost(suggested_route)
  should_choose_worse <- (runif(1) < prob_choose_worse * temp)
  if (is.null(current_cost) || new_cost < current_cost || should_choose_worse) {
    list(
      cost = new_cost,
      route = suggested_route
    )
  } else {
    list(
      cost = current_cost,
      route = current_route
    )
  }
}

g_map <- ggplot() +
  borders("world", colour="gray50", fill="gray50") +
  geom_point(data = capitals,
             aes(x = long, y = lat, colour = "red"),
             size = 2.5,
             show.legend = FALSE) +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  coord_equal(expand = FALSE,
              xlim = c(-180, 180),
              ylim = c(-84, 84),
              ratio = 1.2)

gt <- gtable(widths = unit(c(1, .5, .9, .1), "null"), 
             heights = unit(c(.2, 1, 1, .75, .1), "null"))

plot_route <- function(route, cost_history) {
  cost_hist <- data.frame(cost = cost_history)
  
  g_cost <- ggplot() +
    geom_line(data = cost_hist, aes(x=1:nrow(cost_hist), y=cost), size = 1) +
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
    geom_text(data = 
                data.frame(x = c((nrow(cost_hist) + 1) / 2),
                           y = c(1), 
                           text = c(last(cost_history))),
              aes(x, y, label = as.integer(text)),
              size = 25,
              vjust = "bottom")
  
  p1 <- ggplotGrob(g_map + geom_segment(data = route_to_segments(route), aes(x = long, y = lat, xend = longend, yend = latend),
                                        size = 0.5))
  gt <- gtable_add_grob(gt, p1, t = 1, b = 5, l = 1, r = 4)
  
  p2 <- ggplotGrob(g_cost)
  gt <- gtable_add_grob(gt, p2, t = 4, b = 4, l = 3, r = 3)
}


rounds <- 250000
prev_cost <- NULL

run_cvrp_simulation <- function() {
  pb <- txtProgressBar(min = 0, max = rounds, initial = 0, char = "=", style = 3, file = "")
  
  for (i in 1:rounds) {
    route_comparison <- select_better_route(
      current_route,
      prev_cost,
      refine_route(current_route, temperature),
      temp = temperature
    )
    
    new_cost <- route_comparison[['cost']]
    current_route <- route_comparison[['route']]
    if (is.null(prev_cost) || new_cost != prev_cost) {
      cost_history <- c(cost_history, new_cost)
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
      prev_cost <- new_cost
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
}