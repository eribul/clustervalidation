set.seed(123)

library(mvtnorm)
library(tidyverse)
library(NbClust)
library(gridExtra)

indices <- c(
  "kl", "ch", "ccc", "marriot", "tracew", "rubin", "cindex", "db",
  "silhouette", "duda", "beale", "ratkowsky", "ptbiserial", "mcclain",
  "dunn", "sdindex", "sdbw"
)

# Random data -------------------------------------------------------------

# Randomly assign x and y cpoordinate for center point of cluster
xy_cords <- function() c(sample.int(20, 1), sample.int(20, 1))

# Gaussion dist data centered around given coordinates
rdata <- function(center = xy_cords(), n = 100) {
  data <- rmvnorm(n, mean = center)
  data %>%
    as_tibble() %>%
    rename(x = V1, y = V2)
}

# Data set with n random clusters
rdata_n <- function(n) {
  tibble(class = factor(seq_len(n))) %>%
  mutate(data  = map(class, ~rdata())) %>%
  unnest(data)
}


# Optimal number of clusters ----------------------------------------------

optim_nclust <- function(data, nb) {

  winners <-
    nb$Best.nc %>%
    t() %>%
    as_tibble(rownames = "index") %>%
    rename(n = Number_clusters, value = Value_Index) %>%
    mutate(index = tolower(index))

  all <-
    nb$All.index %>%
    as_tibble(rownames = "n") %>%
    gather("index", "value", -n) %>%
    mutate(
      n = as.numeric(n),
      index = tolower(index)
    )

  all <- bind_rows(all = all, winners = winners, .id = "type") %>%
    filter(index %in% indices)

  # Plot original data
  p_orig <-
    data %>%
    ggplot(aes(x, y, color = class)) +
    geom_point() +
    coord_fixed(xlim = c(0, 22), ylim = c(0, 22)) +
    theme_minimal() +
    theme(legend.position = "none") +
    ggtitle("Simulated data")

  p_best <-
    data %>%
    mutate(class = as.factor(nb$Best.partition)) %>%
    ggplot(aes(x, y, color = class)) +
    geom_point() +
    coord_fixed(xlim = c(0, 22), ylim = c(0, 22)) +
    theme_minimal() +
    theme(legend.position = "none") +
    ggtitle("Clustered data")

  # Plot validation measures
  p_validation <-
    filter(all, type == "all") %>%
      ggplot(aes(n, value, group = index)) +
      geom_line() +
      geom_point(color = "red", data = filter(all, type == "winners")) +
      facet_wrap(~ index, scales = "free_y") +
      theme_minimal() +
      ggtitle(sprintf(
        "Actual = %d clusters, found = %d",
        length(levels(data$class)),
        max(nb$Best.partition))
      ) +
      labs(x = "number of clusters", y = "index value")

  gridExtra::grid.arrange(
    arrangeGrob(p_orig, p_best, ncol = 1),
    p_validation,
    ncol = 2
  )
}



# Increasing number of clusters -------------------------------------------

find_n_clust <-
  tibble(n = 1:15) %>%
  mutate(
    data = map(n, rdata_n),
    datam = map(data, ~as.matrix(select(., -class))),
    nb   = map(datam, NbClust, method = "kmeans", index = "all"),
    gg   = map2(data, nb, optim_nclust)
  )
saveRDS(find_n_clust, "find_n_clust.rds")

# Save
walk2(
  find_n_clust$n,
  find_n_clust$gg,
  ~ ggsave(paste0(.x, ".png"), .y, height = 20, width = 30, units = "cm")
)
