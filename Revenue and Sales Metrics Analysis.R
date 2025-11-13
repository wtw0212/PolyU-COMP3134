sapply(c("tidyverse", "arules", "arulesViz", "rpart", "rpart.plot", 
         "ggplot2", "dplyr", "scales", "zoo"), 
       function(x) if (!require(x, character.only = TRUE)) install.packages(x))

library(tidyverse)
library(arules)
library(arulesViz)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(dplyr)
library(scales)
library(zoo)

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

category_colors <- c(
  "Electronics" = "#1f77b4",
  "Clothing" = "#ff7f0e",
  "Groceries" = "#2ca02c",
  "Books" = "#d62728",
  "Toys" = "#9467bd"
)

#Basic Revenue Metrics
total_revenue <- sum(full_data$Price)
print(paste("Total Revenue: $", round(total_revenue, 2)))
avg_order_value <- total_revenue / length(unique(full_data$Invoice_no))

#Sales by Category
sales_by_category <- full_data %>%
  group_by(Category) %>%
  summarise(Total_Revenue = sum(Price)) %>%
  arrange(desc(Total_Revenue))

p1 <- ggplot(sales_by_category, aes(x = reorder(Category, -Total_Revenue), 
                                    y = Total_Revenue, fill = Category)) +
  geom_col() +
  scale_fill_manual(values = category_colors) +
  scale_y_continuous(labels = comma) + 
  labs(
    title = "Total Revenue by Category",
    x = "Category",
    y = "Total Revenue"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
print(p1)

#Top 10 Products by Sales
sales_by_product <- full_data %>%
  group_by(Product_id_list) %>%
  summarise(Total_Revenue = sum(Price))

product_to_category <- full_data %>%
  select(Product_id_list, Category) %>%
  distinct(Product_id_list, .keep_all = TRUE)

top_10_products <- sales_by_product %>%
  left_join(product_to_category, by = "Product_id_list") %>%
  arrange(desc(Total_Revenue)) %>%
  head(10) %>%
  mutate(Product_Label = paste0(Product_id_list, " (", Category, ")"))

p2 <- ggplot(top_10_products, aes(x = reorder(Product_Label, -Total_Revenue), 
                                  y = Total_Revenue, fill = Category)) +
  geom_col() +
  scale_fill_manual(values = category_colors) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Top 10 Products by Sales",
    x = "Product (Category)",
    y = "Total Revenue",
    fill = "Category" 
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top" 
  )
print(p2)

#Sales by Shopping Mall
sales_by_mall <- full_data %>%
  group_by(Shopping_mall) %>%
  summarise(Total_Revenue = sum(Price)) %>%
  arrange(desc(Total_Revenue))

p3 <- ggplot(sales_by_mall, aes(x = reorder(Shopping_mall, -Total_Revenue), 
                                y = Total_Revenue)) +
  geom_col(fill = "coral") + 
  scale_y_continuous(labels = comma) + 
  labs(
    title = "Total Revenue by Shopping Mall",
    x = "Shopping Mall",
    y = "Total Revenue"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p3)

#Detailed Mall Analysis
mall_performance_summary <- full_data %>%
  group_by(Shopping_mall) %>%
  summarise(
    Total_Revenue = sum(Price),
    Total_Transactions = n_distinct(Invoice_no),
    .groups = 'drop'
  ) %>%
  mutate(Avg_Transaction_Value = Total_Revenue / Total_Transactions) %>%
  arrange(desc(Total_Revenue))

top_category_by_mall <- full_data %>%
  group_by(Shopping_mall, Category) %>%
  summarise(Category_Revenue = sum(Price), .groups = 'drop') %>%
  group_by(Shopping_mall) %>%
  slice_max(order_by = Category_Revenue, n = 1) %>%
  rename(Top_Category = Category, Top_Category_Revenue = Category_Revenue)

best_product_by_mall <- full_data %>%
  group_by(Shopping_mall, Product_id_list) %>%
  summarise(Product_Revenue = sum(Price), .groups = 'drop') %>%
  group_by(Shopping_mall) %>%
  slice_max(order_by = Product_Revenue, n = 1) %>%
  rename(Best_Product_ID = Product_id_list, Best_Product_Revenue = Product_Revenue)

# Category Sales by Mall
category_sales_by_mall <- full_data %>%
  group_by(Shopping_mall, Category) %>%
  summarise(Total_Revenue = sum(Price), .groups = 'drop')

p4 <- ggplot(category_sales_by_mall, aes(x = reorder(Category, -Total_Revenue), 
                                         y = Total_Revenue, fill = Category)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = category_colors) +
  facet_wrap(~Shopping_mall, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Category Sales Performance by Shopping Mall",
    subtitle = "Each mall's revenue breakdown by product category",
    x = "Product Category",
    y = "Total Revenue ($)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(face = "bold", size = 10)
  )
print(p4)

#Sales Trends Over Time
sales_trends <- full_data %>%
  group_by(Invoice_date) %>%
  summarise(Daily_Revenue = sum(Price)) %>%
  arrange(Invoice_date)

sales_trends$Weekly_Avg_Revenue <- rollmean(sales_trends$Daily_Revenue, 
                                            k = 7, fill = NA, align = "right")

p5 <- ggplot(sales_trends, aes(x = Invoice_date)) +
  geom_col(aes(y = Daily_Revenue), fill = "lightgrey", alpha = 0.7) +
  geom_line(aes(y = Weekly_Avg_Revenue), color = "navy", linewidth = 1) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Sales Trends with 7-Day Rolling Average",
    subtitle = "Daily revenue shown in grey, with the smoothed weekly trend in blue.",
    x = "Date",
    y = "Revenue"
  ) +
  theme_minimal()
print(p5)

#Sales by Payment Method
sales_by_payment <- full_data %>%
  group_by(Payment_method) %>%
  summarise(Total_Revenue = sum(Price)) %>%
  arrange(desc(Total_Revenue))

p6 <- ggplot(sales_by_payment, aes(x = reorder(Payment_method, -Total_Revenue), 
                                   y = Total_Revenue)) +
  geom_col(fill = "coral") + 
  scale_y_continuous(labels = comma) + 
  labs(
    title = "Total Revenue by Payment Method",
    x = "Payment Method",
    y = "Total Revenue"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p6)

#SEASONAL SALES ANALYSIS
full_data <- full_data %>%
  mutate(
    Month = month(Invoice_date, label = TRUE),
    Month_Num = month(Invoice_date),
    Year = year(Invoice_date),
    Season = case_when(
      Month_Num %in% c(12, 1, 2) ~ "Winter",
      Month_Num %in% c(3, 4, 5) ~ "Spring",
      Month_Num %in% c(6, 7, 8) ~ "Summer",
      Month_Num %in% c(9, 10, 11) ~ "Fall"
    )
  )

# Seasonal Sales by Category (Monthly)
seasonal_sales_by_category <- full_data %>%
  group_by(Month, Month_Num, Category) %>%
  summarise(
    Total_Revenue = sum(Price),
    Transaction_Count = n(),
    .groups = 'drop'
  ) %>%
  arrange(Month_Num, Category)

# Line Chart by Category
p7 <- ggplot(seasonal_sales_by_category, aes(x = Month, y = Total_Revenue, 
                                             color = Category, group = Category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = category_colors) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Seasonal Sales Trends by Product Category",
    subtitle = "Monthly revenue performance across different categories",
    x = "Month",
    y = "Total Revenue ($)",
    color = "Category"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )
print(p7)

# Faceted Bar Chart
p8 <- ggplot(seasonal_sales_by_category, aes(x = Month, y = Total_Revenue, 
                                             fill = Category)) +
  geom_col(position = "dodge", show.legend = FALSE) +
  scale_fill_manual(values = category_colors) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~Category, scales = "free_y") +
  labs(
    title = "Monthly Revenue by Category",
    subtitle = "Each category's y-axis is scaled independently",
    x = "Month",
    y = "Total Revenue ($)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 12)
  )
print(p8)

# Seasonal Summary by Quarter/Season
seasonal_summary <- full_data %>%
  group_by(Season, Category) %>%
  summarise(
    Total_Revenue = sum(Price),
    Transaction_Count = n(),
    Avg_Transaction_Value = mean(Price),
    .groups = 'drop'
  ) %>%
  arrange(factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")), Category)

# Seasonal Summary Heatmap
p9 <- ggplot(seasonal_summary, aes(x = Season, y = Category, fill = Total_Revenue)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", labels = comma) +
  geom_text(aes(label = paste0("$", round(Total_Revenue/1000, 1), "K")), 
            color = "white", size = 3, fontface = "bold") +
  labs(
    title = "Seasonal Sales Heatmap by Category",
    subtitle = "Revenue distribution across seasons and product categories",
    x = "Season",
    y = "Category",
    fill = "Total Revenue ($)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )
print(p9)

# --- 12. ANNUAL REVENUE ANALYSIS ---
annual_category_revenue <- full_data %>%
  mutate(Year = year(Invoice_date)) %>%
  group_by(Year, Category) %>%
  summarise(Revenue = sum(Price), .groups = 'drop') %>%
  group_by(Year) %>%
  mutate(
    Total_Year = sum(Revenue),
    Percentage = Revenue / Total_Year * 100,
    Label = paste0("$", scales::comma(round(Revenue, 0)), " (", 
                   round(Percentage, 1), "%", ")")
  ) %>%
  ungroup()

# Annual Revenue with Category Details
p10 <- ggplot(annual_category_revenue, aes(x = factor(Year), y = Revenue, 
                                           fill = Category)) +
  geom_col(position = "stack") +
  geom_text(aes(label = Label), 
            position = position_stack(vjust = 0.5),  
            color = "black",                           
            fontface = "bold",
            size = 3) +
  scale_fill_manual(values = category_colors) +
  scale_y_continuous(labels = comma_format(prefix = "$")) +
  labs(
    title = "Annual Revenue Analysis with Category Details",
    subtitle = "Each segment shows revenue amount and percentage contribution",
    x = "Year",
    y = "Total Revenue ($)",
    fill = "Product Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 10),
    legend.position = "right",
    axis.text.x = element_text(size = 11, face = "bold")
  )
print(p10)



# Create 'graph' folder if it doesn't exist
output_dir <- "graph"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  message(paste("Created new directory:", output_dir))
}

# List of all plots to export
plot_list <- list(
  "01_Total_Revenue_by_Category" = p1,
  "02_Top_10_Products" = p2,
  "03_Revenue_by_Mall" = p3,
  "04_Category_Sales_by_Mall" = p4,
  "05_Sales_Trends_Rolling_Avg" = p5,
  "06_Revenue_by_Payment" = p6,
  "07_Seasonal_Trends_Line" = p7,
  "08_Seasonal_Revenue_Bar" = p8,
  "09_Seasonal_Heatmap" = p9,
  "10_Annual_Revenue_Breakdown" = p10
)

cat("\nStarting export to 'graph' folder...\n")
for (plot_name in names(plot_list)) {
  file_path <- file.path(output_dir, paste0(plot_name, ".png"))
    ggsave(filename = file_path, plot = plot_list[[plot_name]], 
         width = 10, height = 6, dpi = 300, bg = "white")
  
  cat(paste("Saved:", file_path, "\n"))
}
cat("Export complete!\n")