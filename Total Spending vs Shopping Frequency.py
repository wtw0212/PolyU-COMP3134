import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from pathlib import Path
import os
from datetime import datetime

# Get the directory where this script is located
script_dir = os.path.dirname(os.path.abspath(__file__))

salesFile = os.path.join(script_dir, "sales_20.csv")
productFile = os.path.join(script_dir, "products_20.csv")
customerFile = os.path.join(script_dir, "customers_20.csv")

salesData = pd.read_csv(
    salesFile,
    dtype={
        "Invoice no": "string",
        "Customer id": "string",
        "Product id list": "string",
        "Shopping mall": "string",
    },
    dayfirst=True,
    parse_dates=["Invoice date"],
)

productData = pd.read_csv(
    productFile,
    dtype={"Product id": "string", "Category": "string", "Price": "float64"},
)

customerData = pd.read_csv(
    customerFile,
    dtype={
        "Customer id": "string",
        "Gender": "category",
        "Age": "int64",
        "Payment method": "category",
    },
)

def splitProducts(temp: pd.DataFrame) -> pd.DataFrame:
    exploded = (
        temp.assign(**{"Product id list": temp["Product id list"].str.split(",")})
        .explode("Product id list")
        .rename(columns={"Product id list": "Product id"})
    )
    exploded["Product id"] = exploded["Product id"].str.strip()
    return exploded

def save_figure_png(fig, out_dir="graph", base_name="figure", overwrite=True, dpi=300):
    # Make out_dir absolute relative to script location
    if not os.path.isabs(out_dir):
        out_dir = os.path.join(script_dir, out_dir)
    os.makedirs(out_dir, exist_ok=True)
    if overwrite:
        filename = f"{base_name}.png"
    else:
        ts = datetime.now().strftime("%Y%m%d-%H%M%S")
        filename = f"{base_name}_{ts}.png"
    path = os.path.join(out_dir, filename)
    fig.savefig(path, dpi=dpi, bbox_inches="tight")
    return path

# Spending per customer
salesExpand = splitProducts(salesData)

salesWithPrices = salesExpand.merge(
    productData[["Product id", "Price"]],
    on="Product id", how="left", validate="many_to_one",
)

spendingPerCustomer = (
    salesWithPrices.groupby("Customer id", as_index=False)["Price"]
    .sum()
    .rename(columns={"Price": "Total Spending"})
)

# Compute shopping frequency per customer
# Use number of unique invoices each customer made.
frequencyPerCustomer = (
    salesData.groupby("Customer id", as_index=False)["Invoice no"]
    .nunique()
    .rename(columns={"Invoice no": "Shopping Frequency"})
)

# Merge with customer attributes
customerSpendFreq = (
    customerData.merge(spendingPerCustomer, on="Customer id", how="left", validate="one_to_one")
                 .merge(frequencyPerCustomer, on="Customer id", how="left", validate="one_to_one")
)

# Compute means
mean_spend = customerSpendFreq["Total Spending"].mean()
mean_freq = customerSpendFreq["Shopping Frequency"].mean()

# Assign 4 clusters based on means
def labelledByMean(freq, spend, f_cut=mean_freq, s_cut=mean_spend):
    if pd.isna(spend):
        spend = 0.0
    if pd.isna(freq):
        freq = 0
    if freq <= f_cut and spend <= s_cut:
        return "Low Frequency & Low Spending"
    elif freq <= f_cut and spend > s_cut:
        return "Low Frequency & High Spending"
    elif freq > f_cut and spend <= s_cut:
        return "High Frequency & Low Spending"
    else:
        return "High Frequency & High Spending"

customerSpendFreq["Freq-Spend Cluster"] = [
    labelledByMean(f, s) for f, s in zip(
        customerSpendFreq["Shopping Frequency"],
        customerSpendFreq["Total Spending"]
    )
]

# Make a categorical with fixed order (optional, helps legend order)
customerSpendFreq["Freq-Spend Cluster"] = pd.Categorical(
    customerSpendFreq["Freq-Spend Cluster"],
    categories=[
        "Low Frequency & Low Spending",
        "Low Frequency & High Spending",
        "High Frequency & Low Spending",
        "High Frequency & High Spending",
    ],
    ordered=True,
)

# =========================
# Visualise clustered scatter
# =========================
figc, axc = plt.subplots(figsize=(9, 7))

sns.scatterplot(
    data=customerSpendFreq,
    x="Shopping Frequency",
    y="Total Spending",
    hue="Freq-Spend Cluster",
    palette="Set2",
    alpha=0.8,
    ax=axc,
    legend=True,
)

# Reference lines at means
axc.axvline(mean_freq, color="gray", linestyle="--", linewidth=1, label=f"Mean Frequency = {mean_freq:.2f}")
axc.axhline(mean_spend, color="gray", linestyle="--", linewidth=1, label=f"Mean Spending = {mean_spend:.2f}")

# Title and labels
axc.set_title("Customer Total Spending vs Shopping Frequency (Mean-based Clusters)")
axc.set_xlabel("Shopping Frequency (number of invoices)")
axc.set_ylabel("Total Spending")

# Ensure x-axis ticks at every 1 unit (as you requested previously)
from matplotlib.ticker import MultipleLocator
axc.xaxis.set_major_locator(MultipleLocator(1))

# Improve layout and save
plt.tight_layout()
cluster_path = save_figure_png(
    figc,
    out_dir="graph",
    base_name="24_Customer_Total_Spending_vs_Shopping_Frequency",
    overwrite=True,
    dpi=300,
)
print("Saved clustered plot:", cluster_path)
plt.close(figc)

# Fill NaN for customers with no purchases
customerSpendFreq["Total Spending"] = customerSpendFreq["Total Spending"].fillna(0.0)
customerSpendFreq["Shopping Frequency"] = customerSpendFreq["Shopping Frequency"].fillna(0).astype(int)

# -------------------------
# Plot: Customer Total Spending vs Shopping Frequency
# -------------------------
fig, ax = plt.subplots(figsize=(9, 7))
sns.scatterplot(
    data=customerSpendFreq,
    x="Shopping Frequency",
    y="Total Spending",
    alpha=0.7,
    ax=ax,
)

ax.set_title("Customer Total Spending vs Shopping Frequency")
ax.set_xlabel("Shopping Frequency (number of invoices)")
ax.set_ylabel("Total Spending")

from matplotlib.ticker import MultipleLocator
ax.xaxis.set_major_locator(MultipleLocator(1))  # ticks at 0,1,2,3,...

plt.close(fig)
