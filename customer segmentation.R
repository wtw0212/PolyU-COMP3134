# ============================================
# COMPLETE CUSTOMER SEGMENTATION PIPELINE
# ============================================

# --- 1. Load Libraries ---
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(scales)
library(purrr)

# --- 2. Load and Preprocess Data ---
customers <- read.csv("COMP3134/customers_20.csv")
products <- read.csv("COMP3134/products_20.csv")
sales <- read.csv("COMP3134/sales_20.csv")

# Clean column names
names(customers) <- gsub("[ .]", "_", names(customers))
names(products) <- gsub("[ .]", "_", names(products))
names(sales) <- gsub("[ .]", "_", names(sales))

# Merge datasets
sales_tidy <- sales %>%
  separate_rows(Product_id_list, sep = ",")

sales_products <- merge(sales_tidy, products, by.x = "Product_id_list", by.y = "Product_id")
full_data <- merge(sales_products, customers, by = "Customer_id")

full_data$Invoice_date <- as.Date(full_data$Invoice_date, format = "%d/%m/%Y")

# --- 3. Create RFM Features ---
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

# --- 4. Elbow Method ---
set.seed(123)

wss <- function(k) {
  kmeans(rfm_scaled, k, nstart = 25)$tot.withinss
}

k_values <- 1:10
wss_values <- map_dbl(k_values, wss)
elbow_data <- data.frame(k = k_values, wss = wss_values)

print(
  ggplot(elbow_data, aes(x = k, y = wss)) +
    geom_line(color = "navy", linewidth = 1) +
    geom_point(color = "navy", size = 3) +
    scale_x_continuous(breaks = k_values) +
    labs(title = "Elbow Method for Optimal 'k'",
         x = "Number of Clusters (k)",
         y = "Within-Cluster Sum of Squares") +
    theme_minimal()
)

# --- 5. K-Means Clustering ---
set.seed(123)
optimal_k <- 3
kmeans_clusters <- kmeans(rfm_scaled, centers = optimal_k, nstart = 25)
rfm_summary$Cluster <- as.factor(kmeans_clusters$cluster)

# --- 6. Add Personas ---
rfm_summary <- rfm_summary %>%
  mutate(Persona = case_when(
    Cluster == 1 ~ "High-Value Customer",
    Cluster == 2 ~ "Potential Loyalists",
    Cluster == 3 ~ "At-Risk Customers"
  )) %>%
  mutate(Persona = factor(Persona, 
                          levels = c("High-Value Customer", "Potential Loyalists", "At-Risk Customers")))

# --- 7. Prepare Centroids ---
centroids <- rfm_summary %>%
  group_by(Persona) %>%
  summarise(
    Monetary = mean(Monetary),
    Recency = mean(Recency),
    .groups = 'drop'
  )

avg_monetary <- mean(rfm_summary$Monetary)
avg_recency <- mean(rfm_summary$Recency)

persona_colors <- c(
  "High-Value Customer" = "#D55E00",
  "Potential Loyalists" = "#009E73",
  "At-Risk Customers" = "#0072B2"
)

print(
  ggplot(rfm_summary, aes(x = Monetary, y = Recency, color = Persona)) +
    # Add lines for average spending and recency
    geom_hline(yintercept = avg_recency, linetype = "dashed", color = "grey40", alpha = 0.8) +
    geom_vline(xintercept = avg_monetary, linetype = "dashed", color = "grey40", alpha = 0.8) +
    
    geom_point(alpha = 0.7, size = 2) +
    
    stat_ellipse(aes(group = Persona), 
                 type = "t",
                 level = 0.75,
                 linewidth = 1.1,
                 linetype = "dashed",
                 color = "black") + 
    
    # Add and label the cluster centroids
    geom_point(data = centroids, shape = 23, fill = "white", color = "black", size = 6, stroke = 1.5) +
    geom_text_repel(data = centroids, aes(label = Persona), 
                    fontface = "bold", color = "black", box.padding = 0.9, size = 4) +
    
    # Apply custom colors and formatting
    scale_color_manual(values = persona_colors) +
    scale_x_continuous(labels = dollar_format()) +
    labs(title = "Actionable Customer Segments by RFM",
         subtitle = "Personas defined by spending and purchase timing to guide marketing strategy.",
         x = "Total Customer Spending",
         y = "Days Since Last Purchase",
         color = "Customer Persona") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold", size = 16),
          plot.subtitle = element_text(size = 10, color = "grey20"))
)


# --- 9. Summary Statistics ---
summary_stats <- rfm_summary %>%
  group_by(Persona) %>%
  summarise(
    Avg_Recency = round(mean(Recency), 1),
    Avg_Frequency = round(mean(Frequency), 1),
    Avg_Monetary = round(mean(Monetary), 0),
    Customer_Count = n(),
    .groups = 'drop'
  )

print(summary_stats)

