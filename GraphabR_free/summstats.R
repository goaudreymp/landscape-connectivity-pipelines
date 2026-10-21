patch <- rast("./hf_ma1/patches.tif")
links <- vect("./hf_ma1/MAX_ma1-links.shp")

plot(ifel(patch > 0, 1, NA),
     col = "forestgreen",
     legend = FALSE)
plot(links, add = T)

patch2 <- rast("./hf2_ma1/patches.tif")
links2 <- vect("./hf2_ma1/MAX_ma1-links.shp")

plot(ifel(patch2 > 0, 1, NA),
     col = "forestgreen",
     legend = FALSE)
plot(links2, add = T, col = "red")

patch3 <- rast("./hf3_ma1/patches.tif")
links3 <- vect("./hf3_ma1/MAX_ma1-links.shp")

plot(ifel(patch3 > 0, 1, NA),
     col = "forestgreen",
     legend = FALSE)
plot(links3, add = T, col = "green")

patch4 <- rast("./hf4_ma1/patches.tif")
links4 <- vect("./hf4_ma1/MAX_ma1-links.shp")

plot(ifel(patch4 > 0, 1, NA),
     col = "forestgreen",
     legend = FALSE)
plot(links4, add = T, col = "blue")


results_df <- read.csv("testthis.csv")
results_df <- read.csv("graphab_results_summary.csv")
## PLOTTING ####
ggplot(results_df,
       aes(x = thr, y = minarea, color = IIC)) +
  geom_point(size = 4) +
  facet_wrap(~ cost_file, nrow = 2) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_viridis_c(name = "IIC") +
  labs(x = "Interpatch distance (m)", y = "Minimum habitat patch size (ha)") +
  theme_bw() +
  theme(legend.position = "right")

# Normalise IIC
results_norm <- results_df %>%
  group_by(cost_file) %>%
  mutate(IIC_norm = (IIC - min(IIC))/(max(IIC) - min(IIC))) %>% 
  ungroup()

ggplot(results_norm,
       aes(x = thr, y = minarea, color = IIC_norm)) +
  geom_point(size = 4) +
  facet_wrap(~ cost_file, nrow = 2) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_viridis_c(name = "Normalized") +
  labs(x = "Interpatch distance (m)", y = "Minimum habitat patch size (ha)") +
  theme_bw() +
  theme(legend.position = "right")

# Breadth
breadth <- results_norm %>%
  group_by(cost_file) %>%
  summarise(breadth50 = mean(IIC_norm >= 0.5),
            breadth80 = mean(IIC_norm >= 0.8),
            breadth90 = mean(IIC_norm >= 0.9),
            .groups = "drop")

# Centroid
centroid <- results_norm %>%
  group_by(cost_file) %>%
  summarise(centroid_thr = 10^weighted.mean(log10(thr), IIC_norm),
            centroid_patch = 10^weighted.mean(log10(minarea), IIC_norm),
            .groups = "drop")

# Median normalized IIC
median_iic <- results_norm %>%
  group_by(cost_file) %>%
  summarise(median_IIC = median(IIC_norm), .groups = "drop")

# Flag lowest 20%
results_norm <- results_norm %>%
  group_by(cost_file) %>%
  mutate(low20 = IIC_norm <= quantile(IIC_norm, 0.20)) %>%
  ungroup()

# Lowest 20% of connectivity
low_conn <- results_norm %>%
  filter(low20) %>%
  group_by(cost_file) %>%
  summarise(low_thr = median(thr), 
            low_patch = median(minarea),
            .groups = "drop")

# Sensitivity to patch size
patch_sensitivity <- results_norm %>%
  group_by(cost_file, thr) %>%
  summarise(sensitivity = max(IIC_norm) - min(IIC_norm), 
            .groups = "drop") %>%
  group_by(cost_file) %>%
  summarise(patch_sensitivity = mean(sensitivity),
            .groups = "drop")

# Sensitivity to interpatch distance
distance_sensitivity <- results_norm %>%
  group_by(cost_file, minarea) %>%
  summarise(sensitivity = max(IIC_norm) - min(IIC_norm),
            .groups = "drop") %>%
  group_by(cost_file) %>%
  summarise(distance_sensitivity = mean(sensitivity),
    .groups = "drop")

# ===========================================================
# Final summary table
# ===========================================================

summary_table <- median_iic %>%
  left_join(breadth, by = "cost_file") %>%
  left_join(centroid, by = "cost_file") %>%
  left_join(low_conn, by = "cost_file") %>%
  left_join(patch_sensitivity, by = "cost_file") %>%
  left_join(distance_sensitivity, by = "cost_file") %>%
  mutate(across(where(is.numeric), round, 3))

summary_table
