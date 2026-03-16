"""
    Bank Transaction Data — Exploratory Data Analysis & Fraud Detection
    Author : Antonnio Macharia
    Email  : antonniomacharia75@gmail.com
    Date   : 2026-03-16

    Skills demonstrated:
    Data Cleaning, Exploratory Data Analysis (EDA), Feature Engineering,
    Aggregation & GroupBy, Data Visualisation (matplotlib / seaborn),
    Anomaly / Fraud Flagging, Statistical Summaries, Pandas, NumPy
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import seaborn as sns
import warnings
warnings.filterwarnings('ignore')

# ─────────────────────────────────────────────────────────────
# 0. Style Config
# ─────────────────────────────────────────────────────────────
PALETTE   = ['#1F3864', '#2E75B6', '#F4B942', '#375623', '#C00000']
DARK_BLUE = '#1F3864'
MID_BLUE  = '#2E75B6'
ACCENT    = '#F4B942'
RED       = '#C00000'
sns.set_theme(style='whitegrid', palette=PALETTE)
plt.rcParams.update({
    'font.family'     : 'DejaVu Sans',
    'axes.titlesize'  : 13,
    'axes.labelsize'  : 11,
    'figure.facecolor': 'white',
})

# ─────────────────────────────────────────────────────────────
# 1. Load & Clean Data
# ─────────────────────────────────────────────────────────────
print("=" * 60)
print("  BANK TRANSACTION ANALYSIS — Antonnio Macharia")
print("=" * 60)

df = pd.read_csv('bank_transactions_data_2_augmented_clean_2.csv')

# Parse dates
df['TransactionDate'] = pd.to_datetime(df['TransactionDate'], format='mixed')

# Feature engineering
df['Year']        = df['TransactionDate'].dt.year
df['Month']       = df['TransactionDate'].dt.month
df['MonthName']   = df['TransactionDate'].dt.strftime('%b')
df['DayOfWeek']   = df['TransactionDate'].dt.day_name()
df['Hour']        = df['TransactionDate'].dt.hour
df['Quarter']     = df['TransactionDate'].dt.to_period('Q').astype(str)

# Fraud flag: multiple login attempts (>1) treated as suspicious
df['SuspiciousLogin'] = df['LoginAttempts'] > 1

# High-value flag: transactions above 90th percentile
threshold_90 = df['TransactionAmount'].quantile(0.90)
df['HighValue'] = df['TransactionAmount'] > threshold_90

# Balance-to-transaction ratio
df['BalanceToTxRatio'] = df['AccountBalance'] / (df['TransactionAmount'] + 1)

print(f"\n{'─'*60}")
print(f"  Dataset Overview")
print(f"{'─'*60}")
print(f"  Total Transactions : {len(df):,}")
print(f"  Unique Accounts    : {df['AccountID'].nunique():,}")
print(f"  Date Range         : {df['TransactionDate'].min().date()} → {df['TransactionDate'].max().date()}")
print(f"  Avg Transaction    : ${df['TransactionAmount'].mean():,.2f}")
print(f"  Total Volume       : ${df['TransactionAmount'].sum():,.2f}")
print(f"  Suspicious Logins  : {df['SuspiciousLogin'].sum():,} ({df['SuspiciousLogin'].mean()*100:.1f}%)")
print(f"  High-Value Txns    : {df['HighValue'].sum():,} (top 10%)")
print(f"  Missing Values     : {df.isnull().sum().sum()}")


# ─────────────────────────────────────────────────────────────
# 2. Summary Statistics
# ─────────────────────────────────────────────────────────────
print(f"\n{'─'*60}")
print("  Transaction Amount — Summary Statistics")
print(f"{'─'*60}")
stats = df['TransactionAmount'].describe()
for k, v in stats.items():
    print(f"  {k:<10}: ${v:>10,.2f}")


# ─────────────────────────────────────────────────────────────
# 3. Aggregations
# ─────────────────────────────────────────────────────────────

# By Transaction Type
by_type = df.groupby('TransactionType').agg(
    Count=('TransactionAmount', 'count'),
    Total_Amount=('TransactionAmount', 'sum'),
    Avg_Amount=('TransactionAmount', 'mean'),
    Avg_Balance=('AccountBalance', 'mean')
).reset_index()

# By Channel
by_channel = df.groupby('Channel').agg(
    Count=('TransactionAmount', 'count'),
    Total_Amount=('TransactionAmount', 'sum'),
    Avg_Amount=('TransactionAmount', 'mean'),
    Suspicious=('SuspiciousLogin', 'sum')
).reset_index()

# By Occupation
by_occ = df.groupby('CustomerOccupation').agg(
    Count=('TransactionAmount', 'count'),
    Total_Amount=('TransactionAmount', 'sum'),
    Avg_Amount=('TransactionAmount', 'mean'),
    Avg_Balance=('AccountBalance', 'mean'),
    Suspicious=('SuspiciousLogin', 'sum')
).reset_index().sort_values('Total_Amount', ascending=False)

# By Year
by_year = df.groupby('Year').agg(
    Count=('TransactionAmount', 'count'),
    Total_Amount=('TransactionAmount', 'sum'),
    Avg_Amount=('TransactionAmount', 'mean'),
    Suspicious=('SuspiciousLogin', 'sum')
).reset_index()

# Top locations
top_locations = df.groupby('Location')['TransactionAmount'].sum()\
    .nlargest(10).reset_index()
top_locations.columns = ['Location', 'Total_Amount']

print(f"\n{'─'*60}")
print("  Transactions by Channel")
print(f"{'─'*60}")
print(by_channel.to_string(index=False))

print(f"\n{'─'*60}")
print("  Transactions by Occupation")
print(f"{'─'*60}")
print(by_occ.to_string(index=False))


# ─────────────────────────────────────────────────────────────
# 4. Fraud / Anomaly Analysis
# ─────────────────────────────────────────────────────────────
fraud_df = df[df['SuspiciousLogin']].copy()

fraud_by_channel = fraud_df.groupby('Channel').agg(
    Suspicious_Count=('TransactionID', 'count'),
    Total_Amount=('TransactionAmount', 'sum'),
    Avg_Amount=('TransactionAmount', 'mean')
).reset_index()

fraud_by_occ = fraud_df.groupby('CustomerOccupation').agg(
    Suspicious_Count=('TransactionID', 'count'),
    Avg_Amount=('TransactionAmount', 'mean'),
    Avg_Login_Attempts=('LoginAttempts', 'mean')
).reset_index().sort_values('Suspicious_Count', ascending=False)

print(f"\n{'─'*60}")
print("  Suspicious Transaction Breakdown")
print(f"{'─'*60}")
print(f"  Total Suspicious  : {len(fraud_df):,}")
print(f"  % of All Txns     : {len(fraud_df)/len(df)*100:.2f}%")
print(f"  Avg Amount (Susp) : ${fraud_df['TransactionAmount'].mean():,.2f}")
print(f"  Avg Amount (Normal): ${df[~df['SuspiciousLogin']]['TransactionAmount'].mean():,.2f}")
print("\n  By Channel:")
print(fraud_by_channel.to_string(index=False))
print("\n  By Occupation:")
print(fraud_by_occ.to_string(index=False))


# ─────────────────────────────────────────────────────────────
# 5. Visualisations — Figure 1: Overview Dashboard
# ─────────────────────────────────────────────────────────────
fig1, axes = plt.subplots(2, 3, figsize=(18, 11))
fig1.suptitle(
    'Bank Transaction Analysis — Overview Dashboard\nAntonnio Macharia | antonniomacharia75@gmail.com',
    fontsize=15, fontweight='bold', color=DARK_BLUE, y=1.01
)

# (a) Transaction type distribution
ax = axes[0, 0]
counts = df['TransactionType'].value_counts()
bars = ax.bar(counts.index, counts.values,
              color=[DARK_BLUE, MID_BLUE], edgecolor='white', linewidth=1.2)
ax.set_title('Transaction Type Distribution')
ax.set_ylabel('Number of Transactions')
for bar in bars:
    ax.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 200,
            f'{bar.get_height():,}', ha='center', fontsize=10, fontweight='bold')
ax.yaxis.set_major_formatter(mticker.FuncFormatter(lambda x, _: f'{x:,.0f}'))

# (b) Transaction amount distribution
ax = axes[0, 1]
ax.hist(df['TransactionAmount'], bins=50, color=MID_BLUE,
        edgecolor='white', linewidth=0.5, alpha=0.85)
ax.axvline(df['TransactionAmount'].mean(), color=ACCENT,
           linewidth=2, linestyle='--', label=f'Mean: ${df["TransactionAmount"].mean():,.0f}')
ax.axvline(threshold_90, color=RED,
           linewidth=2, linestyle=':', label=f'90th %ile: ${threshold_90:,.0f}')
ax.set_title('Transaction Amount Distribution')
ax.set_xlabel('Amount ($)')
ax.set_ylabel('Frequency')
ax.legend(fontsize=9)
ax.xaxis.set_major_formatter(mticker.FuncFormatter(lambda x, _: f'${x:,.0f}'))

# (c) Channel breakdown
ax = axes[0, 2]
ch_counts = df['Channel'].value_counts()
wedges, texts, autotexts = ax.pie(
    ch_counts.values, labels=ch_counts.index, autopct='%1.1f%%',
    colors=[DARK_BLUE, MID_BLUE, ACCENT],
    startangle=90, pctdistance=0.75,
    wedgeprops=dict(edgecolor='white', linewidth=2)
)
for at in autotexts:
    at.set_fontsize(10); at.set_fontweight('bold'); at.set_color('white')
ax.set_title('Transactions by Channel')

# (d) Monthly transaction volume by year
ax = axes[1, 0]
monthly = df.groupby(['Year', 'Month'])['TransactionAmount'].sum().reset_index()
monthly.columns = ['Year', 'Month', 'Total_Amount']
for yr, grp in monthly.groupby('Year'):
    ax.plot(grp['Month'], grp['Total_Amount'] / 1e6, marker='o',
            markersize=4, linewidth=1.8, label=str(yr))
ax.set_title('Monthly Transaction Volume by Year')
ax.set_xlabel('Month'); ax.set_ylabel('Total Amount ($M)')
ax.set_xticks(range(1, 13))
ax.set_xticklabels(['J','F','M','A','M','J','J','A','S','O','N','D'])
ax.legend(fontsize=8, ncol=3)
ax.yaxis.set_major_formatter(mticker.FuncFormatter(lambda x, _: f'${x:.1f}M'))

# (e) Avg transaction by occupation
ax = axes[1, 1]
occ_avg = df.groupby('CustomerOccupation')['TransactionAmount'].mean().sort_values()
bars = ax.barh(occ_avg.index, occ_avg.values,
               color=[DARK_BLUE, MID_BLUE, ACCENT, '#375623'], edgecolor='white')
ax.set_title('Avg Transaction Amount by Occupation')
ax.set_xlabel('Average Amount ($)')
for bar in bars:
    ax.text(bar.get_width() + 1, bar.get_y() + bar.get_height()/2,
            f'${bar.get_width():,.0f}', va='center', fontsize=9)
ax.xaxis.set_major_formatter(mticker.FuncFormatter(lambda x, _: f'${x:,.0f}'))

# (f) Account balance vs transaction amount scatter
ax = axes[1, 2]
sample = df.sample(2000, random_state=42)
colors_scatter = [RED if s else MID_BLUE for s in sample['SuspiciousLogin']]
ax.scatter(sample['AccountBalance'], sample['TransactionAmount'],
           c=colors_scatter, alpha=0.4, s=15, linewidths=0)
ax.set_title('Account Balance vs Transaction Amount\n(red = suspicious login)')
ax.set_xlabel('Account Balance ($)')
ax.set_ylabel('Transaction Amount ($)')
ax.xaxis.set_major_formatter(mticker.FuncFormatter(lambda x, _: f'${x:,.0f}'))
ax.yaxis.set_major_formatter(mticker.FuncFormatter(lambda x, _: f'${x:,.0f}'))

plt.tight_layout()
plt.savefig('Python_Chart1_Overview_Dashboard.png', dpi=150, bbox_inches='tight')
plt.close()
print("\n  [Chart 1] Overview Dashboard saved.")


# ─────────────────────────────────────────────────────────────
# 6. Visualisations — Figure 2: Fraud / Suspicious Activity
# ─────────────────────────────────────────────────────────────
fig2, axes = plt.subplots(2, 3, figsize=(18, 11))
fig2.suptitle(
    'Suspicious Transaction Analysis — Fraud Detection Signals\nAntonnio Macharia | antonniomacharia75@gmail.com',
    fontsize=15, fontweight='bold', color=DARK_BLUE, y=1.01
)

# (a) Suspicious vs normal by channel
ax = axes[0, 0]
ch_sus = df.groupby(['Channel', 'SuspiciousLogin'])['TransactionID'].count().unstack()
ch_sus.columns = ['Normal', 'Suspicious']
ch_sus.plot(kind='bar', ax=ax, color=[MID_BLUE, RED],
            edgecolor='white', linewidth=1)
ax.set_title('Normal vs Suspicious by Channel')
ax.set_xlabel('Channel'); ax.set_ylabel('Count')
ax.legend(['Normal', 'Suspicious'])
ax.tick_params(axis='x', rotation=0)
ax.yaxis.set_major_formatter(mticker.FuncFormatter(lambda x, _: f'{x:,.0f}'))

# (b) Login attempts distribution
ax = axes[0, 1]
login_counts = df['LoginAttempts'].value_counts().sort_index()
bars = ax.bar(login_counts.index.astype(str), login_counts.values,
              color=[MID_BLUE if i == 0 else RED for i in range(len(login_counts))],
              edgecolor='white', linewidth=1.2)
ax.set_title('Login Attempts Distribution')
ax.set_xlabel('Number of Login Attempts')
ax.set_ylabel('Number of Transactions')
for bar in bars:
    ax.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 200,
            f'{bar.get_height():,}', ha='center', fontsize=9, fontweight='bold')
ax.yaxis.set_major_formatter(mticker.FuncFormatter(lambda x, _: f'{x:,.0f}'))

# (c) Suspicious rate by occupation
ax = axes[0, 2]
occ_sus_rate = df.groupby('CustomerOccupation')['SuspiciousLogin'].mean().sort_values() * 100
bars = ax.barh(occ_sus_rate.index, occ_sus_rate.values,
               color=[RED if v > occ_sus_rate.mean() else MID_BLUE for v in occ_sus_rate.values],
               edgecolor='white')
ax.axvline(occ_sus_rate.mean(), color=ACCENT, linewidth=2,
           linestyle='--', label=f'Avg: {occ_sus_rate.mean():.1f}%')
ax.set_title('Suspicious Transaction Rate by Occupation')
ax.set_xlabel('Suspicious Rate (%)')
ax.legend(fontsize=9)
for bar in bars:
    ax.text(bar.get_width() + 0.05, bar.get_y() + bar.get_height()/2,
            f'{bar.get_width():.1f}%', va='center', fontsize=9)

# (d) Transaction amount: suspicious vs normal (box plot)
ax = axes[1, 0]
plot_data = [
    df[df['SuspiciousLogin'] == False]['TransactionAmount'].values,
    df[df['SuspiciousLogin'] == True]['TransactionAmount'].values
]
bp = ax.boxplot(plot_data, labels=['Normal', 'Suspicious'],
                patch_artist=True, notch=False,
                medianprops=dict(color=ACCENT, linewidth=2.5))
for patch, color in zip(bp['boxes'], [MID_BLUE, RED]):
    patch.set_facecolor(color); patch.set_alpha(0.7)
ax.set_title('Transaction Amount: Normal vs Suspicious')
ax.set_ylabel('Transaction Amount ($)')
ax.yaxis.set_major_formatter(mticker.FuncFormatter(lambda x, _: f'${x:,.0f}'))

# (e) Suspicious transactions by year
ax = axes[1, 1]
yr_sus = df.groupby('Year').agg(
    Total=('TransactionID', 'count'),
    Suspicious=('SuspiciousLogin', 'sum')
).reset_index()
yr_sus['Rate'] = yr_sus['Suspicious'] / yr_sus['Total'] * 100
ax2 = ax.twinx()
ax.bar(yr_sus['Year'].astype(str), yr_sus['Suspicious'],
       color=RED, alpha=0.7, edgecolor='white', label='Suspicious Count')
ax2.plot(yr_sus['Year'].astype(str), yr_sus['Rate'],
         color=DARK_BLUE, marker='o', linewidth=2, label='Rate %')
ax.set_title('Suspicious Transactions by Year')
ax.set_xlabel('Year')
ax.set_ylabel('Suspicious Count', color=RED)
ax2.set_ylabel('Rate (%)', color=DARK_BLUE)
lines1, labels1 = ax.get_legend_handles_labels()
lines2, labels2 = ax2.get_legend_handles_labels()
ax.legend(lines1 + lines2, labels1 + labels2, fontsize=8, loc='upper left')

# (f) Heatmap — avg transaction by occupation & channel
ax = axes[1, 2]
hm_data = df.groupby(['CustomerOccupation', 'Channel'])['TransactionAmount'].mean().unstack()
sns.heatmap(hm_data, ax=ax, cmap='Blues', annot=True, fmt='.0f',
            linewidths=0.5, cbar_kws={'label': 'Avg Amount ($)'})
ax.set_title('Avg Transaction Amount\nby Occupation & Channel')
ax.set_xlabel('Channel'); ax.set_ylabel('Occupation')

plt.tight_layout()
plt.savefig('Python_Chart2_Fraud_Analysis.png', dpi=150, bbox_inches='tight')
plt.close()
print("  [Chart 2] Fraud Analysis saved.")


# ─────────────────────────────────────────────────────────────
# 7. Visualisations — Figure 3: Time Series & Trends
# ─────────────────────────────────────────────────────────────
fig3, axes = plt.subplots(2, 2, figsize=(16, 11))
fig3.suptitle(
    'Transaction Trends Over Time\nAntonnio Macharia | antonniomacharia75@gmail.com',
    fontsize=15, fontweight='bold', color=DARK_BLUE, y=1.01
)

# (a) Annual total volume
ax = axes[0, 0]
bars = ax.bar(by_year['Year'].astype(str), by_year['Total_Amount'] / 1e6,
              color=DARK_BLUE, edgecolor='white', linewidth=1.2)
ax.set_title('Annual Transaction Volume')
ax.set_xlabel('Year'); ax.set_ylabel('Total Amount ($M)')
for bar in bars:
    ax.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 0.2,
            f'${bar.get_height():.1f}M', ha='center', fontsize=9, fontweight='bold')
ax.yaxis.set_major_formatter(mticker.FuncFormatter(lambda x, _: f'${x:.0f}M'))

# (b) Transaction count by day of week
ax = axes[0, 1]
dow_order = ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday']
dow_counts = df['DayOfWeek'].value_counts().reindex(dow_order)
bars = ax.bar(range(7), dow_counts.values,
              color=[DARK_BLUE if i < 5 else ACCENT for i in range(7)],
              edgecolor='white', linewidth=1)
ax.set_xticks(range(7))
ax.set_xticklabels(['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'])
ax.set_title('Transaction Count by Day of Week')
ax.set_ylabel('Number of Transactions')
ax.yaxis.set_major_formatter(mticker.FuncFormatter(lambda x, _: f'{x:,.0f}'))

# (c) Top 10 locations by volume
ax = axes[1, 0]
bars = ax.barh(top_locations['Location'], top_locations['Total_Amount'] / 1e6,
               color=MID_BLUE, edgecolor='white', linewidth=1)
ax.set_title('Top 10 Locations by Transaction Volume')
ax.set_xlabel('Total Amount ($M)')
for bar in bars:
    ax.text(bar.get_width() + 0.05, bar.get_y() + bar.get_height()/2,
            f'${bar.get_width():.1f}M', va='center', fontsize=9)
ax.xaxis.set_major_formatter(mticker.FuncFormatter(lambda x, _: f'${x:.0f}M'))

# (d) Avg balance by occupation & transaction type
ax = axes[1, 1]
bal_data = df.groupby(['CustomerOccupation', 'TransactionType'])['AccountBalance'].mean().unstack()
bal_data.plot(kind='bar', ax=ax, color=[DARK_BLUE, ACCENT],
              edgecolor='white', linewidth=0.8)
ax.set_title('Avg Account Balance by Occupation & Type')
ax.set_xlabel('Occupation'); ax.set_ylabel('Avg Balance ($)')
ax.tick_params(axis='x', rotation=15)
ax.legend(title='Transaction Type')
ax.yaxis.set_major_formatter(mticker.FuncFormatter(lambda x, _: f'${x:,.0f}'))

plt.tight_layout()
plt.savefig('Python_Chart3_Trends.png', dpi=150, bbox_inches='tight')
plt.close()
print("  [Chart 3] Trends saved.")


# ─────────────────────────────────────────────────────────────
# 8. Export Clean Summary CSV
# ─────────────────────────────────────────────────────────────
summary_export = df.groupby(['Year', 'Channel', 'CustomerOccupation', 'TransactionType']).agg(
    Transaction_Count=('TransactionID', 'count'),
    Total_Amount=('TransactionAmount', 'sum'),
    Avg_Amount=('TransactionAmount', 'mean'),
    Avg_Balance=('AccountBalance', 'mean'),
    Suspicious_Count=('SuspiciousLogin', 'sum'),
    High_Value_Count=('HighValue', 'sum')
).reset_index()
summary_export.to_csv('Python_Transaction_Summary.csv', index=False)
print("  [Export] Summary CSV saved.")

print(f"\n{'='*60}")
print("  Analysis Complete — Antonnio Macharia")
print(f"  Files generated:")
print(f"    • Python_Chart1_Overview_Dashboard.png")
print(f"    • Python_Chart2_Fraud_Analysis.png")
print(f"    • Python_Chart3_Trends.png")
print(f"    • Python_Transaction_Summary.csv")
print(f"{'='*60}\n")
