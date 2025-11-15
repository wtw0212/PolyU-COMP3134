sapply(c("tidyverse", "arules", "arulesViz", "ggplot2", "scales", "gridExtra"), 
       function(x) if (!require(x, character.only = TRUE)) install.packages(x))

library(tidyverse)
library(arules)
library(arulesViz)
library(ggplot2)
library(scales)
library(gridExtra)

Sys.setlocale("LC_TIME", "English")

# Note: customers.csv is loaded in other scripts but not used in this analysis.
# customers <- read.csv("customers_20.csv") 
products <- read.csv("products_20.csv")
sales <- read.csv("sales_20.csv")

# names(customers) <- gsub("[ .]", "_", names(customers))
names(products) <- gsub("[ .]", "_", names(products))
names(sales) <- gsub("[ .]", "_", names(sales))

sales$Product_id_list <- gsub(" ", "", sales$Product_id_list)

# Create a long-form dataset for category analysis
sales_with_categories <- sales %>%
  mutate(
    Invoice_date = as.Date(Invoice_date, format = "%d/%m/%Y"),
    Product_ids = strsplit(Product_id_list, ",")
  ) %>%
  unnest(Product_ids) %>%
  left_join(products, by = c("Product_ids" = "Product_id"))

trans_by_product <- sales %>%
  select(Invoice_no, Product_id_list) %>%
  mutate(items = strsplit(Product_id_list, ","))
trans_product <- as(lapply(trans_by_product$items, function(x) unique(x)), "transactions")
trans_by_category <- sales_with_categories %>%
  group_by(Invoice_no) %>%
  summarise(Categories = list(unique(Category)), .groups = 'drop')
trans_category <- as(trans_by_category$Categories, "transactions")
trans_sizes <- size(trans_product)

category_colors <- c(
  "Electronics" = "#1f77b4",
  "Clothing" = "#ff7f0e",
  "Groceries" = "#2ca02c",
  "Books" = "#d62728",
  "Toys" = "#9467bd"
)

#Top 20 Products Plot
item_freq_product <- itemFrequency(trans_product, type = "absolute")
top_20_products <- sort(item_freq_product, decreasing = TRUE)[1:20]

top_product_df <- data.frame(
  Product_id = names(top_20_products),
  Frequency = as.numeric(top_20_products)
) %>%
  left_join(products, by = c("Product_id" = "Product_id")) %>%
  mutate(Label = paste0(Product_id, "\n(", Category, ")"))

p14 <- ggplot(top_product_df, aes(x = reorder(Label, -Frequency), 
                                  y = Frequency, fill = Category)) +
  geom_col() +
  scale_fill_manual(values = category_colors) +
  labs(
    title = "Top 20 Most Frequently Purchased Products",
    x = "Product (Category)",
    y = "Purchase Frequency"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14)
  )
print(p14)

#Category Frequency Plot
item_freq_category <- itemFrequency(trans_category, type = "absolute")
category_freq_df <- data.frame(
  Category = names(item_freq_category),
  Frequency = as.numeric(item_freq_category)
)

p15 <- ggplot(category_freq_df, aes(x = reorder(Category, -Frequency), 
                                    y = Frequency, fill = Category)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = Frequency), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = category_colors) +
  labs(
    title = "Category Purchase Frequency",
    x = "Product Category",
    y = "Frequency in Transactions"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  )
print(p15)

#Association Rule Mining
rules_category <- suppressMessages(apriori(trans_category, 
                                           parameter = list(
                                             support = 0.01,
                                             confidence = 0.3,
                                             minlen = 2,
                                             maxlen = 5
                                           )))

rules_product <- suppressMessages(apriori(trans_product, 
                                          parameter = list(
                                            support = 0.005,
                                            confidence = 0.3,
                                            minlen = 2,
                                            maxlen = 5
                                          )))

rules_category_sorted <- sort(rules_category, by = "confidence", decreasing = TRUE)
rules_product_sorted <- sort(rules_product, by = "lift", decreasing = TRUE)

#Method Analysis (Category-Level)
chi2_values <- interestMeasure(rules_category_sorted, 
                               measure = "chiSquared",
                               transactions = trans_category)
chi2_pvalues <- pchisq(chi2_values, df = 1, lower.tail = FALSE)

fisher_pvalues <- interestMeasure(rules_category_sorted, 
                                  measure = "fishersExactTest",
                                  transactions = trans_category)

category_rules_hybrid <- data.frame(
  lhs = labels(lhs(rules_category_sorted)),
  rhs = labels(rhs(rules_category_sorted)),
  quality(rules_category_sorted),
  chi2_pvalue = chi2_pvalues,
  fisher_pvalue = fisher_pvalues
) %>%
  mutate(
    lhs = gsub("[{}]", "", lhs),
    rhs = gsub("[{}]", "", rhs),
    support = round(support, 3),
    confidence = round(confidence, 3),
    lift = round(lift, 2),
    chi2_pvalue = round(chi2_pvalue, 4),
    fisher_pvalue = round(fisher_pvalue, 4)
  ) %>%
  arrange(desc(confidence))

#Product-Level Rules (Fisher's Test)
product_rules_fisher <- data.frame(
  lhs = labels(lhs(rules_product_sorted)),
  rhs = labels(rhs(rules_product_sorted)),
  quality(rules_product_sorted),
  p_value = interestMeasure(rules_product_sorted, 
                            measure = "fishersExactTest",
                            transactions = trans_product)
) %>%
  mutate(
    lhs = gsub("[{}]", "", lhs),
    rhs = gsub("[{}]", "", rhs),
    support = round(support, 3),
    confidence = round(confidence, 3),
    lift = round(lift, 2),
    p_value = round(p_value, 4)
  )

chi2_values_prod <- interestMeasure(rules_product_sorted,
                                    measure = "chiSquared",
                                    transactions = trans_product)
chi2_pvalues_prod <- pchisq(chi2_values_prod, df = 1, lower.tail = FALSE)

product_rules_hybrid <- product_rules_fisher %>% 
  mutate(chi2_pvalue = round(chi2_pvalues_prod, 4))

comparison_data <- data.frame(
  Method = rep(c("Chi-Squared", "Fisher's Exact"), each = 2),
  Level  = rep(c("Category", "Product"), 2),
  Significant_Rules = c(
    sum(category_rules_hybrid$chi2_pvalue < 0.05),
    sum(product_rules_hybrid$chi2_pvalue   < 0.05),
    sum(category_rules_hybrid$fisher_pvalue < 0.05),
    sum(product_rules_hybrid$p_value        < 0.05)
  )
)


p16 <- ggplot(comparison_data, aes(x = Level, y = Significant_Rules, fill = Method)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Significant_Rules), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = c("Chi-Squared" = "steelblue", 
                               "Fisher's Exact" = "darkorange")) +
  labs(
    title = "Statistical Significance Comparison",
    subtitle = "Number of significant rules (p < 0.05)",
    y = "Number of Significant Rules",
    x = "Analysis Level"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))
print(p16)

#Confidence vs Lift Scatter Plot
p17 <- ggplot(category_rules_hybrid %>% head(30), 
              aes(x = confidence, y = lift, size = support, alpha = 0.7)) +
  geom_point(color = "steelblue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "blue", alpha = 0.5) +
  labs(
    title = "Association Rules: Confidence vs Lift",
    subtitle = "Top 30 category-level rules (sized by support)",
    x = "Confidence",
    y = "Lift",
    size = "Support"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "right") +
  guides(alpha = "none")
print(p17)

#Top 10 Rules Bar Plot
top_10_for_viz <- category_rules_hybrid %>%
  head(10) %>%
  mutate(
    rule_label = paste0(lhs, " => ", rhs),
    rule_label = factor(rule_label, levels = rev(rule_label))
  )

p18 <- ggplot(top_10_for_viz, aes(x = rule_label, y = confidence)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(confidence)), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Top 10 Association Rules by Confidence",
    x = "Rule",
    y = "Confidence"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 9)
  ) +
  ylim(0, max(top_10_for_viz$confidence) * 1.15)
print(p18)

# Network Graph Visualization (Category Level)
p19 <- plot(head(rules_category_sorted, 15), 
            method = "graph", 
            engine = "ggplot2") + 
  ggtitle("Network Graph: Top 15 Category Rules") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))
print(p19)

# Top 10 Association Rules by Lift（Category level）
top_10_lift <- category_rules_hybrid %>%
  arrange(desc(lift)) %>%
  head(10) %>%
  mutate(
    rule_label = paste0(lhs, " => ", rhs),
    rule_label = factor(rule_label, levels = rev(rule_label))
  )

p20 <- ggplot(top_10_lift, aes(x = rule_label, y = lift)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 1,
             linetype = "dashed",
             color = "red",
             size = 0.8) +
  geom_text(aes(label = lift),
            hjust = -0.1,
            size = 3) +
  coord_flip() +
  labs(
    title = "Top 10 Association Rules by Lift",
    x = "Rule",
    y = "Lift"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 9)
  ) +
  ylim(0, max(top_10_lift$lift) * 1.15)

print(p20)

output_dir <- "graph"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  message(paste("Created new directory:", output_dir))
}

plot_list <- list(
  "14_Top_20_Products" = p14,
  "15_Category_Frequency" = p15,
  "16_Significance_Comparison" = p16,
  "17_Confidence_vs_Lift" = p17,
  "18_Top_10_Rules_Confidence" = p18,
  "19_Network_Graph_Category" = p19,
  "20_Top_10_Rules_Lift" = p20
)

cat("\nStarting export for Association Rules plots to 'graph' folder...\n")
for (plot_name in names(plot_list)) {
  file_path <- file.path(output_dir, paste0(plot_name, ".png"))
  ggsave(filename = file_path, plot = plot_list[[plot_name]], 
         width = 10, height = 6, dpi = 300, bg = "white")
  
  cat(paste("Saved:", file_path, "\n"))
}
cat("Export complete!\n")