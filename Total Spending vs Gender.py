import pandas as pd
import matplotlib.pyplot as plt
from pathlib import Path
import os

# File paths (adjust if needed)
sales_fp = Path('sales_20.csv')
products_fp = Path('products_20.csv')
customers_fp = Path('customers_20.csv')

# 1) Load data
sales = pd.read_csv(sales_fp)
products = pd.read_csv(products_fp)
customers = pd.read_csv(customers_fp)

# 2) Normalize sales to line items
#    Split the "Product id list" column into individual product IDs
#    Some rows may not be quoted but are comma-separated; split regardless.
sales_expanded = (
    sales
    .assign(ProductList=sales['Product id list'].astype(str).str.split(r'\s*,\s*'))
    .explode('ProductList', ignore_index=True)
    .rename(columns={'ProductList': 'Product id'})
)

# 3) Join with products to get price per product
#    Ensure keys match exactly
line_items = sales_expanded.merge(
    products[['Product id', 'Price']],
    on='Product id',
    how='left'
)

# Guard against missing prices
missing_prices = line_items['Price'].isna().sum()
if missing_prices > 0:
    print(f'Warning: {missing_prices} line items have missing product prices and will be dropped.')
    line_items = line_items.dropna(subset=['Price'])

# 4) Compute spend per invoice line (price is the line amount since quantity is not provided)
line_items['LineAmount'] = line_items['Price']

# 5) Aggregate total spending per customer
customer_spend = (
    line_items
    .groupby('Customer id', as_index=False)['LineAmount']
    .sum()
    .rename(columns={'LineAmount': 'TotalSpend'})
)

# 6) Join with customers to bring in gender
customer_with_gender = customer_spend.merge(
    customers[['Customer id', 'Gender']],
    on='Customer id',
    how='left'
)

# 7) Compute total spending by gender (Male vs Female)
gender_totals = (
    customer_with_gender
    .groupby('Gender', as_index=False)['TotalSpend']
    .sum()
    .sort_values('Gender')
)

# 8) Plot bar chart
plt.figure(figsize=(7, 5))
bars = plt.bar(gender_totals['Gender'], gender_totals['TotalSpend'], color=['#4e79a7', '#f28e2b'])
plt.title('Total Spending by Gender')
plt.xlabel('Gender')
plt.ylabel('Total Spending')
plt.grid(axis='y', linestyle='--', alpha=0.4)

# Add labels on top of bars
for bar in bars:
    height = bar.get_height()
    plt.text(bar.get_x() + bar.get_width()/2, height, f'{height:,.0f}', ha='center', va='bottom')

# Save the figure to the 'graph' directory before showing
os.makedirs('graph', exist_ok=True)
save_path = Path('graph') / '23_Total_Spending_by_Gender.png'
plt.tight_layout()
plt.savefig(save_path, dpi=300, bbox_inches='tight')
print(f"Saved chart to: {save_path.resolve()}")