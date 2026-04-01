"""
Infectious Disease Surveillance Model — Python Dashboard
COVID-19 Clinical Trials (ClinicalTrials.gov)
Author: 2026
Dependencies: pandas, plotly
"""

import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import warnings
warnings.filterwarnings("ignore")

# ── 1. Load & clean ──────────────────────────────────────────────────────────

df = pd.read_csv("COVID_clinical_trials.csv")

# Normalize enrollment
df["Enrollment"] = pd.to_numeric(df["Enrollment"], errors="coerce").fillna(0)

# Extract launch year from Start Date
df["Launch_Year"] = (
    df["Start Date"]
    .astype(str)
    .str.extract(r"(\d{4})")
    .astype(float)
)

# Simplify study type
df["Type"] = df["Study Type"].apply(
    lambda x: "Interventional" if "Interventional" in str(x)
    else ("Observational" if "Observational" in str(x)
    else "Expanded Access")
)

# Simplify funding
def simplify_funding(fb):
    fb = str(fb)
    if "NIH" in fb:        return "NIH"
    if "U.S. Fed" in fb:   return "U.S. Federal"
    if "Industry" in fb:   return "Industry"
    return "Other / Nonprofit"

df["Funder"] = df["Funded Bys"].apply(simplify_funding)

# Phase clean (interventional only)
df_iv = df[df["Type"] == "Interventional"].copy()
df_iv["Phase"] = df_iv["Phases"].fillna("N/A").replace("", "N/A")

# ── 2. Aggregations ──────────────────────────────────────────────────────────

status_counts = df["Status"].value_counts().head(8)

year_counts = (
    df[df["Launch_Year"].between(2016, 2021)]
    .groupby("Launch_Year")
    .size()
    .reset_index(name="Count")
)

phase_enroll = (
    df_iv[df_iv["Phase"].str.startswith("Phase")]
    .groupby("Phase")["Enrollment"]
    .sum()
    .sort_values(ascending=False)
)

funder_counts = df["Funder"].value_counts()

intervention_kws = ["Drug", "Biological", "Device", "Behavioral",
                    "Diagnostic Test", "Other", "Procedure", "Genetic"]
iv_counts = {
    kw: df["Interventions"].astype(str).str.contains(kw + ":").sum()
    for kw in intervention_kws
}
iv_series = pd.Series(iv_counts).sort_values(ascending=True)

# ── 3. Colour palette ────────────────────────────────────────────────────────

TEAL    = "#1D9E75"
BLUE    = "#378ADD"
PURPLE  = "#534AB7"
CORAL   = "#D85A30"
PINK    = "#D4537E"
AMBER   = "#BA7517"
GREEN   = "#639922"
GRAY    = "#888780"
LGRAY   = "#B4B2A9"

PALETTE = [BLUE, TEAL, PURPLE, CORAL, PINK, AMBER, GREEN, GRAY]

# ── 4. Build figure ──────────────────────────────────────────────────────────

fig = make_subplots(
    rows=3, cols=3,
    subplot_titles=(
        "Trial status distribution",
        "Launch volume by year (2016–2021)",
        "Enrollment by trial phase",
        "Study type split",
        "Funder breakdown",
        "Intervention category frequency",
        "", "", ""          # row 3 used for metric cards only
    ),
    specs=[
        [{"type": "xy"}, {"type": "xy"}, {"type": "xy"}],
        [{"type": "domain"}, {"type": "domain"}, {"type": "xy"}],
        [{"type": "xy"}, {"type": "xy"}, {"type": "xy"}],
    ],
    vertical_spacing=0.13,
    horizontal_spacing=0.10,
)

# — R1C1: Status horizontal bar -----------------------------------------------
fig.add_trace(go.Bar(
    x=status_counts.values,
    y=status_counts.index,
    orientation="h",
    marker_color=PALETTE[:len(status_counts)],
    text=status_counts.values,
    textposition="outside",
    textfont=dict(size=10),
    showlegend=False,
), row=1, col=1)

# — R1C2: Launch year bar -----------------------------------------------------
fig.add_trace(go.Bar(
    x=year_counts["Launch_Year"].astype(int).astype(str),
    y=year_counts["Count"],
    marker_color=[LGRAY, LGRAY, LGRAY, LGRAY, BLUE, TEAL],
    text=year_counts["Count"],
    textposition="outside",
    textfont=dict(size=10),
    showlegend=False,
), row=1, col=2)

# — R1C3: Enrollment by phase bar ---------------------------------------------
fig.add_trace(go.Bar(
    x=phase_enroll.index,
    y=phase_enroll.values,
    marker_color=PURPLE,
    text=[f"{v/1e6:.1f}M" if v >= 1e6 else f"{v/1e3:.0f}k" for v in phase_enroll.values],
    textposition="outside",
    textfont=dict(size=10),
    showlegend=False,
), row=1, col=3)

# — R2C1: Study type donut ----------------------------------------------------
fig.add_trace(go.Pie(
    labels=["Interventional", "Observational", "Expanded Access"],
    values=[3322, 2427, 34],
    hole=0.60,
    marker_colors=[PURPLE, PINK, GRAY],
    textinfo="label+percent",
    textfont=dict(size=10),
    showlegend=False,
), row=2, col=1)

# — R2C2: Funder donut --------------------------------------------------------
fig.add_trace(go.Pie(
    labels=funder_counts.index,
    values=funder_counts.values,
    hole=0.60,
    marker_colors=[TEAL, PURPLE, BLUE, LGRAY],
    textinfo="label+percent",
    textfont=dict(size=10),
    showlegend=False,
), row=2, col=2)

# — R2C3: Intervention horizontal bar ----------------------------------------
fig.add_trace(go.Bar(
    x=iv_series.values,
    y=iv_series.index,
    orientation="h",
    marker_color=CORAL,
    text=iv_series.values,
    textposition="outside",
    textfont=dict(size=10),
    showlegend=False,
), row=2, col=3)

# ── 5. Metric card annotations (row 3) ───────────────────────────────────────

metrics = [
    ("5,783",  "Total trials in dataset"),
    ("48.5%",  "Actively recruiting"),
    ("17.7%",  "Trials completed"),
    ("~103M",  "Projected total enrollment"),
    ("4,465",  "Trials launched in 2020"),
    ("57.4%",  "Interventional study share"),
]

for i, (val, label) in enumerate(metrics):
    x_pos = 0.08 + (i % 3) * 0.42
    y_pos = 0.20 if i < 3 else 0.08

    fig.add_annotation(
        x=x_pos, y=y_pos + 0.03,
        text=f"<b>{val}</b>",
        xref="paper", yref="paper",
        showarrow=False,
        font=dict(size=20, color=BLUE),
        xanchor="center",
    )
    fig.add_annotation(
        x=x_pos, y=y_pos - 0.01,
        text=label,
        xref="paper", yref="paper",
        showarrow=False,
        font=dict(size=10, color=GRAY),
        xanchor="center",
    )

# ── 6. Layout ────────────────────────────────────────────────────────────────

fig.update_layout(
    title=dict(
        text="<b>Infectious Disease Surveillance Model</b>   |   COVID-19 Clinical Trials   |   2026",
        font=dict(size=16, color="#2C2C2A"),
        x=0.01,
    ),
    height=900,
    paper_bgcolor="#F8F7F4",
    plot_bgcolor="#F8F7F4",
    margin=dict(t=70, b=20, l=20, r=20),
    font=dict(family="'Georgia', serif", size=11, color="#444441"),
)

fig.update_xaxes(showgrid=True, gridcolor="#E0DED8", gridwidth=0.5, zeroline=False)
fig.update_yaxes(showgrid=False, zeroline=False)

fig.write_html("covid_surveillance_dashboard.html")
print("✓  Saved: covid_surveillance_dashboard.html")

# ── 7. Correlation analysis ───────────────────────────────────────────────────

corr_df = (
    df_iv[df_iv["Phase"].str.startswith("Phase") & (df_iv["Enrollment"] > 0)]
    .groupby("Phase")
    .agg(
        Trial_Count=("Enrollment", "count"),
        Median_Enrollment=("Enrollment", "median"),
        Total_Enrollment=("Enrollment", "sum"),
    )
    .reset_index()
)

print("\n── Phase × Enrollment correlation (interventional trials) ──")
print(corr_df.to_string(index=False))

corr_year = (
    df[df["Launch_Year"].between(2016, 2021)]
    .groupby("Launch_Year")
    .agg(
        Trials=("Status", "count"),
        Avg_Enrollment=("Enrollment", "mean"),
    )
    .reset_index()
)
corr_year["Launch_Year"] = corr_year["Launch_Year"].astype(int)

print("\n── Year × Enrollment trend ──")
print(corr_year.to_string(index=False))

pearson_r = corr_year["Trials"].corr(corr_year["Avg_Enrollment"])
print(f"\nPearson r (trials vs avg enrollment, 2016–2021): {pearson_r:.4f}")
