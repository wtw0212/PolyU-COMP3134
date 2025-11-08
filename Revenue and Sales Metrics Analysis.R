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

customers <- read.csv("COMP3134/customers_20.csv")
products <- read.csv("COMP3134/products_20.csv")
sales <- read.csv("COMP3134/sales_20.csv")
names(customers) <- gsub("[ .]", "_", names(customers))
names(products) <- gsub("[ .]", "_", names(products))
names(sales) <- gsub("[ .]", "_", names(sales))

sales_tidy <- sales %>%
  separate_rows(Product_id_list, sep = ",")

sales_products <- merge(sales_tidy, products, by.x = "Product_id_list", by.y = "Product_id")
full_data <- merge(sales_products, customers, by = "Customer_id")

full_data$Invoice_date <- as.Date(full_data$Invoice_date, format = "%d/%m/%Y")

# Calculate Total Revenue
total_revenue <- sum(full_data$Price)
print(paste("Total Revenue: $", round(total_revenue, 2)))

# Calculate Average Order Value
avg_order_value <- total_revenue / length(unique(full_data$Invoice_no))
print(paste("Average Order Value: $", round(avg_order_value, 2)))

# Sales by Category
sales_by_category <- full_data %>%
  group_by(Category) %>%
  summarise(Total_Revenue = sum(Price)) %>%
  arrange(desc(Total_Revenue))

ggplot(sales_by_category, aes(x = reorder(Category, -Total_Revenue), y = Total_Revenue)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = comma) + 
  labs(
    title = "Total Revenue by Category",
    x = "Category",
    y = "Total Revenue"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Sales by Product
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

ggplot(top_10_products, aes(x = reorder(Product_Label, -Total_Revenue), y = Total_Revenue, fill = Category)) +
  geom_col() +
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

# Sales by Shopping Mall
sales_by_mall <- full_data %>%
  group_by(Shopping_mall) %>%
  summarise(Total_Revenue = sum(Price)) %>%
  arrange(desc(Total_Revenue))

ggplot(sales_by_mall, aes(x = reorder(Shopping_mall, -Total_Revenue), y = Total_Revenue)) +
  geom_col(fill = "purple") + 
  scale_y_continuous(labels = comma) + 
  labs(
    title = "Total Revenue by Shopping Mall",
    x = "Shopping Mall",
    y = "Total Revenue"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

mall_performance_summary <- full_data %>%
  group_by(Shopping_mall) %>%
  summarise(
    Total_Revenue = sum(Price),
    Total_Transactions = n_distinct(Invoice_no),
    .groups = 'drop'
  ) %>%
  mutate(
    Avg_Transaction_Value = Total_Revenue / Total_Transactions
  ) %>%
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

detailed_mall_analysis <- mall_performance_summary %>%
  left_join(top_category_by_mall, by = "Shopping_mall") %>%
  left_join(best_product_by_mall, by = "Shopping_mall")

print("--- Detailed Performance Analysis by Shopping Mall ---")
print(detailed_mall_analysis)


category_sales_by_mall <- full_data %>%
  group_by(Shopping_mall, Category) %>%
  summarise(Total_Revenue = sum(Price), .groups = 'drop')

print(
  ggplot(category_sales_by_mall, aes(x = reorder(Category, -Total_Revenue), y = Total_Revenue, fill = Category)) +
    geom_col(show.legend = FALSE) +
    # Use independent y-scales to make comparisons within each mall clearer
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
)

# Sales Trends Over Time
sales_trends <- full_data %>%
  group_by(Invoice_date) %>%
  summarise(Daily_Revenue = sum(Price)) %>%
  arrange(Invoice_date)

sales_trends$Weekly_Avg_Revenue <- rollmean(sales_trends$Daily_Revenue, k = 7, fill = NA, align = "right")

ggplot(sales_trends, aes(x = Invoice_date)) +
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

# Sales by Payment Method
sales_by_payment <- full_data %>%
  group_by(Payment_method) %>%
  summarise(Total_Revenue = sum(Price)) %>%
  arrange(desc(Total_Revenue))

ggplot(sales_by_payment, aes(x = reorder(Payment_method, -Total_Revenue), y = Total_Revenue)) +
  geom_col(fill = "coral") + 
  scale_y_continuous(labels = comma) + 
  labs(
    title = "Total Revenue by Payment Method",
    x = "Payment Method",
    y = "Total Revenue"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#SEASONAL SALES ANALYSIS BY CATEGORY
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

#Line Chart by Category
ggplot(seasonal_sales_by_category, aes(x = Month, y = Total_Revenue, color = Category, group = Category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
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

#Faceted Bar Chart

ggplot(seasonal_sales_by_category, aes(x = Month, y = Total_Revenue, fill = Category)) +
  geom_col(position = "dodge", show.legend = FALSE) +
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

print("--- Seasonal Sales Summary by Category ---")
print(seasonal_summary)

# Seasonal Summary Heatmap
ggplot(seasonal_summary, aes(x = Season, y = Category, fill = Total_Revenue)) +
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

# Calculate Seasonality Index
monthly_category_avg <- full_data %>%
  group_by(Category, Month, Month_Num) %>%
  summarise(Monthly_Revenue = sum(Price), .groups = 'drop')

category_avg <- full_data %>%
  group_by(Category) %>%
  summarise(Annual_Avg_Monthly = sum(Price) / 12, .groups = 'drop')

seasonality_index <- monthly_category_avg %>%
  left_join(category_avg, by = "Category") %>%
  mutate(Seasonality_Index = (Monthly_Revenue / Annual_Avg_Monthly) * 100)

#Seasonality Index
ggplot(seasonality_index, aes(x = Month, y = Seasonality_Index, color = Category, group = Category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "gray", alpha = 0.7) +
  labs(
    title = "Seasonality Index by Category",
    subtitle = "Index of 100 = average monthly performance. Values above 100 indicate above-average months.",
    x = "Month",
    y = "Seasonality Index",
    color = "Category"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

peak_low_seasons <- seasonal_summary %>%
  group_by(Category) %>%
  slice_max(order_by = Total_Revenue, n = 1) %>%
  rename(Peak_Season = Season, Peak_Revenue = Total_Revenue) %>%
  select(Category, Peak_Season, Peak_Revenue)

low_seasons <- seasonal_summary %>%
  group_by(Category) %>%
  slice_min(order_by = Total_Revenue, n = 1) %>%
  rename(Low_Season = Season, Low_Revenue = Total_Revenue) %>%
  select(Category, Low_Season, Low_Revenue)

season_performance <- peak_low_seasons %>%
  left_join(low_seasons, by = "Category") %>%
  mutate(Variance = Peak_Revenue - Low_Revenue,
         Variance_Percentage = round((Variance / Low_Revenue) * 100, 2))

print("--- Peak and Low Season Performance by Category ---")
print(season_performance)
