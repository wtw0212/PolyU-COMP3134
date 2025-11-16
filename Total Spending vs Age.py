import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from pathlib import Path
import os
from datetime import datetime

salesFile = "./sales_20.csv" #Define path to the sales data, product data and customer data
productFile = "./products_20.csv"
customerFile = "./customers_20.csv"

salesData = pd.read_csv(
    salesFile,
    dtype={ #ensures proper parsing of data types
        "Invoice no": "string",
        "Customer id": "string",
        "Product id list": "string",
        "Shopping mall": "string",
    },
    dayfirst=True,  #because dates appear as dd/mm/yyyy
    parse_dates=["Invoice date"],
)

productData = pd.read_csv(
    productFile,
    dtype={"Product id": "string", "Category": "string", "Price": "float64"},
)

customerData = pd.read_csv(
    customerFile,
    dtype={ #ensures proper parsing of data types
        "Customer id": "string",
        "Gender": "category",
        "Age": "int64",
        "Payment method": "category",
    },
)

def splitProducts(temp):
    #split into rows by commmas
    exploded = (
        temp.assign(**{
            "Product id list": temp["Product id list"].str.split(",")
        })
        .explode("Product id list")
        .rename(columns={"Product id list": "Product id"})
    )
    #for cleaning whitespace
    exploded["Product id"] = exploded["Product id"].str.strip()
    return exploded

salesExpand = splitProducts(salesData)

#Join with product prices
salesWithPrices = salesExpand.merge(
    productData[["Product id", "Price"]],
    on="Product id", how="left", validate="many_to_one",
)

#Compute spending per customer
spendingPerCustomer = (
    salesWithPrices.groupby("Customer id", as_index=False)["Price"]
    .sum()
    .rename(columns={"Price": "Total Spending"})
)

#Merge spending with customer attributes
customerSpendAge = customerData.merge(
    spendingPerCustomer, on="Customer id", how="left", validate="one_to_one"
)

#Customers with no purchases will have NaN in spending; set to 0
customerSpendAge["Total Spending"] = customerSpendAge["Total Spending"].fillna(0.0)

def saveAsPng(fig, outDir="graph", base_name="figure", overwrite=True, dpi=300):
    os.makedirs(outDir, exist_ok=True)
    if overwrite:
        filename = f"{base_name}.png"
    else:
        ts = datetime.now().strftime("%Y%m%d-%H%M%S")
        filename = f"{base_name}_{ts}.png"
    path = os.path.join(outDir, filename)
    fig.savefig(path, dpi=dpi, bbox_inches="tight")
    return path

def categorize_customers_by_means(df, columnAge="Age", columnSpend="Total Spending", new_col="Customer Segment"):
    """
    Cluster customers into 4 groups with the means in age and total spending of each customer as the cutoff
    Returns a copy of df with categorical column `new_col` and a dict of cutoffs.
    """
    out = df.copy()

    #Compute mean cutoffs
    age_mean = float(out[columnAge].mean())
    spend_mean = float(out[columnSpend].fillna(0.0).mean())

    #Build labels using <= for "Younger"/"Low" sides; adjust if you prefer strict <
    age_side = np.where(out[columnAge] <= age_mean, "Younger", "Older")
    spend_vals = out[columnSpend].fillna(0.0)
    spend_side = np.where(spend_vals <= spend_mean, "Low Spending", "High Spending")

    out[new_col] = (pd.Series(age_side) + " & " + pd.Series(spend_side)).astype("category")
    out[new_col] = out[new_col].cat.set_categories(
        ["Younger & Low Spending", "Younger & High Spending", "Older & Low Spending", "Older & High Spending"],
        ordered=True
    )

    cutoffs = {"age_mean": age_mean, "spending_mean": spend_mean}
    return out, cutoffs

#Figure 21: Scatter plot
customerSpendAge_seg, cutoffs = categorize_customers_by_means(
    customerSpendAge,
    columnAge="Age",
    columnSpend="Total Spending",
    new_col="Customer Segment"
)
print("Mean cutoffs used:", cutoffs)

fig1, ax1 = plt.subplots(figsize=(9, 7))

sns.scatterplot(
    data=customerSpendAge_seg,
    x="Age",
    y="Total Spending",
    hue="Customer Segment",
    palette="Set2",
    alpha=0.8,
    ax=ax1,
    legend=True,
)

ax1.set_title("Customer Total Spending vs Age (Segmented by Mean)")
ax1.set_xlabel("Age")
ax1.set_ylabel("Total Spending")

#Optional: draw cutoff lines to visualize the 2x2 split
ax1.axvline(cutoffs["age_mean"], color="gray", linestyle="--", linewidth=1, label="Mean Age")
ax1.axhline(cutoffs["spending_mean"], color="gray", linestyle="--", linewidth=1, label="Mean Spending")

plt.tight_layout()
path1 = saveAsPng(
    fig1,
    outDir="graph",
    base_name="21_Customer_Total_Spending_vs_Age",
    overwrite=True,
    dpi=300
)
print("Saved:", path1)
plt.close(fig1)

#Figure 22: Box + strip by Age Group
fig2, ax2 = plt.subplots(figsize=(9, 7))

#Ensure Age Group exists (safe if rerun)
bins = [0, 24, 29, 34, 39, 44, 49, 54, 100]
labels = ["<=24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55+"]
if "Age Group" not in customerSpendAge.columns:
    customerSpendAge["Age Group"] = pd.cut(
        customerSpendAge["Age"], bins=bins, labels=labels, right=True, include_lowest=True
    )

sns.boxplot(
    data=customerSpendAge,
    x="Age Group",
    y="Total Spending",
    showfliers=False,
    ax=ax2,
)
sns.stripplot(
    data=customerSpendAge,
    x="Age Group",
    y="Total Spending",
    color="gray",
    alpha=0.4,
    size=3,
    ax=ax2,
)
ax2.set_title("Total Spending Distribution by Age Group")
ax2.set_xlabel("Age Group")
ax2.set_ylabel("Total Spending")

plt.tight_layout()
path2 = saveAsPng(fig2, outDir="graph", base_name="22_Total_Spending_Distribution_by_Age_Group", overwrite=True, dpi=300)
print("Saved:", path2)

plt.close(fig2) #Close figure