sapply(c("dplyr", "tidyr", "ggplot2", "ggrepel", "scales", "purrr"), function(x) {
  if (!require(x, character.only = TRUE)) install.packages(x)
})

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(scales)
library(purrr)

Sys.setlocale("LC_TIME", "English")

customers <- read.csv("customers_20.csv")
products <- read.csv("products_20.csv")
sales <- read.csv("sales_20.csv")

names(customers) <- gsub("[ .]", "_", names(customers))
names(products) <- gsub("[ .]", "_", names(products))
names(sales) <- gsub("[ .]", "_", names(sales))

sales_tidy <- sales %>%
  separate_rows(Product_id_list, sep = ",")

sales_products <- merge(sales_tidy, products, 
                        by.x = "Product_id_list", by.y = "Product_id")
full_data <- merge(sales_products, customers, by = "Customer_id")

full_data$Invoice_date <- as.Date(full_data$Invoice_date, format = "%d/%m/%Y")

#RFM
analysis_date <- max(full_data$Invoice_date) + 1

rfm_summary <- full_data %>%
  group_by(Customer_id) %>%
  summarise(
    Recency = as.numeric(analysis_date - max(Invoice_date)),
    Frequency = n_distinct(Invoice_no),
    Monetary = sum(Price)
  ) %>%
  ungroup()

rfm_scaled <- scale(rfm_summary[, c("Recency", "Frequency", "Monetary")])

#Elbow Method
set.seed(123)

wss <- function(k) {
  kmeans(rfm_scaled, k, nstart = 25)$tot.withinss
}

k_values <- 1:10
wss_values <- map_dbl(k_values, wss)
elbow_data <- data.frame(k = k_values, wss = wss_values)

p11 <- ggplot(elbow_data, aes(x = k, y = wss)) +
  geom_line(color = "navy", linewidth = 1) +
  geom_point(color = "navy", size = 3) +
  scale_x_continuous(breaks = k_values) +
  labs(
    title = "Elbow Method for Optimal 'k'",
    x = "Number of Clusters (k)",
    y = "Within-Cluster Sum of Squares"
  ) +
  theme_minimal()
print(p11)

#K-Means Clustering
set.seed(123)
optimal_k <- 3
kmeans_clusters <- kmeans(rfm_scaled, centers = optimal_k, nstart = 25)
rfm_summary$Cluster <- as.factor(kmeans_clusters$cluster)

rfm_summary <- rfm_summary %>%
  mutate(Persona = case_when(
    Cluster == 1 ~ "High-Value Customer",
    Cluster == 2 ~ "Potential Loyalists",
    Cluster == 3 ~ "At-Risk Customers"
  )) %>%
  mutate(Persona = factor(Persona, 
                          levels = c("High-Value Customer", 
                                     "Potential Loyalists", 
                                     "At-Risk Customers")))

centroids_initial <- rfm_summary %>%
  group_by(Persona) %>%
  summarise(
    Monetary = mean(Monetary),      # Average spending
    Recency = mean(Recency),        # Average days since purchase
    .groups = 'drop'
  )

rfm_filtered <- rfm_summary %>%
  left_join(centroids_initial, by = "Persona", suffix = c("", "_center")) %>%
  mutate(
    Distance = sqrt((Monetary - Monetary_center)^2 + (Recency - Recency_center)^2)
  ) %>%
  group_by(Persona) %>%
  mutate(
    Distance_Percentile = percent_rank(Distance)
  ) %>%
  ungroup() %>%
  filter(Distance_Percentile <= 0.90) %>%
  select(-ends_with("_center"), -Distance, -Distance_Percentile)

centroids <- rfm_filtered %>%
  group_by(Persona) %>%
  summarise(
    Monetary = mean(Monetary),
    Recency = mean(Recency), 
    .groups = 'drop'
  )

avg_monetary <- mean(rfm_filtered$Monetary)
avg_recency <- mean(rfm_filtered$Recency)

persona_colors <- c(
  "High-Value Customer" = "#D55E00",
  "Potential Loyalists" = "#009E73",
  "At-Risk Customers" = "#0072B2"
)

set.seed(123)
sample_percentage <- 0.4
rfm_sample <- rfm_filtered %>% sample_frac(sample_percentage)

p12 <- ggplot(rfm_filtered, aes(x = Monetary, y = Recency, color = Persona)) +
  geom_hline(yintercept = avg_recency, linetype = "dashed", 
             color = "grey40", alpha = 0.8, linewidth = 0.8) +
  geom_vline(xintercept = avg_monetary, linetype = "dashed", 
             color = "grey40", alpha = 0.8, linewidth = 0.8) +
  
  annotate("text", 
           x = max(rfm_filtered$Monetary) * 0.85, 
           y = avg_recency, 
           label = paste0("Avg Recency: ", round(avg_recency, 0), " days"),
           vjust = -0.5, 
           color = "grey20", 
           size = 3.5, 
           fontface = "bold") +
  
  annotate("text", 
           x = avg_monetary, 
           y = max(rfm_filtered$Recency) * 0.95, 
           label = paste0("Avg Spending: ", dollar(round(avg_monetary, 0))),
           hjust = -0.1, 
           color = "grey20", 
           size = 3.5, 
           fontface = "bold") +
  
  geom_point(data = rfm_sample, alpha = 0.7, size = 2.5) +
  
  # Add centroids
  geom_point(data = centroids, shape = 23, fill = "white", 
             color = "black", size = 7, stroke = 1.5) +
  geom_text_repel(data = centroids, aes(label = Persona), 
                  fontface = "bold", color = "black", 
                  box.padding = 1.2, size = 4.5,
                  min.segment.length = 0) +
  
  scale_color_manual(values = persona_colors) +
  scale_x_continuous(labels = dollar_format()) +
  labs(
    title = "Actionable Customer Segments by RFM",
    subtitle = paste0("Sample visualization (", sample_percentage * 100, 
                      "% shown). White diamonds = average customer in each segment."),
    x = "Total Customer Spending",
    y = "Days Since Last Purchase",
    color = "Customer Persona"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 9, color = "grey20")
  )
print(p12)

removed_count <- nrow(rfm_summary) - nrow(rfm_filtered)
removed_pct <- round(removed_count / nrow(rfm_summary) * 100, 1)
cat(sprintf("\n=== Filtering Statistics ===\n"))
cat(sprintf("Removed: %d outliers (%s%%)\n\n", removed_count, removed_pct))

summary_stats <- rfm_summary %>%
  group_by(Persona) %>%
  summarise(
    Avg_Recency = round(mean(Recency), 1),
    Avg_Frequency = round(mean(Frequency), 1),
    Avg_Monetary = round(mean(Monetary), 0),
    Customer_Count = n(),
    .groups = 'drop'
  )

#Profile Comparison Bar Chart
summary_long <- summary_stats %>%
  pivot_longer(
    cols = -Persona,
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(Metric = dplyr::recode(Metric,
                                "Avg_Recency" = "Avg. Recency (Days)",
                                "Avg_Frequency" = "Avg. Frequency (Purchases)",
                                "Avg_Monetary" = "Avg. Spending ($)",
                                "Customer_Count" = "Number of Customers")) %>%
  mutate(Metric = factor(Metric, 
                         levels = c("Number of Customers", 
                                    "Avg. Spending ($)", 
                                    "Avg. Frequency (Purchases)", 
                                    "Avg. Recency (Days)")))

p13 <- ggplot(summary_long, aes(x = Persona, y = Value, fill = Persona)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(Value, 1)), vjust = -0.4, 
            size = 3.5, color = "black", fontface = "bold") +
  facet_wrap(~Metric, scales = "free_y") +
  scale_fill_manual(values = persona_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Profile Comparison of Customer Personas",
    subtitle = "Key characteristics of each segment based on RFM analysis",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank()
  )
print(p13)

output_dir <- "graph"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  message(paste("Created new directory:", output_dir))
}

plot_list <- list(
  "11_Elbow_Method" = p11,
  "12_RFM_Segments_Scatter" = p12,
  "13_RFM_Profile_Comparison" = p13
)

cat("\nStarting export for RFM plots to 'graph' folder...\n")
for (plot_name in names(plot_list)) {
  file_path <- file.path(output_dir, paste0(plot_name, ".png"))
  
  ggsave(filename = file_path, plot = plot_list[[plot_name]], 
         width = 10, height = 6, dpi = 300, bg = "white")
  
  cat(paste("Saved:", file_path, "\n"))
}
cat("Export complete!\n")