# =============================================================================
# Infectious Disease Surveillance Model — R Dashboard
# COVID-19 Clinical Trials (ClinicalTrials.gov)
# Author: 2026
# Dependencies: tidyverse, plotly, htmlwidgets, scales
# =============================================================================

library(tidyverse)
library(plotly)
library(htmlwidgets)
library(scales)

# ── 1. Load & clean ──────────────────────────────────────────────────────────

df <- read_csv("COVID_clinical_trials.csv", show_col_types = FALSE) |>
  mutate(
    Enrollment   = suppressWarnings(as.numeric(Enrollment)),
    Enrollment   = replace_na(Enrollment, 0),
    Launch_Year  = as.numeric(str_extract(`Start Date`, "\\d{4}")),
    Type = case_when(
      str_detect(`Study Type`, "Interventional") ~ "Interventional",
      str_detect(`Study Type`, "Observational")  ~ "Observational",
      TRUE                                        ~ "Expanded Access"
    ),
    Funder = case_when(
      str_detect(`Funded Bys`, "NIH")      ~ "NIH",
      str_detect(`Funded Bys`, "U.S. Fed") ~ "U.S. Federal",
      str_detect(`Funded Bys`, "Industry") ~ "Industry",
      TRUE                                  ~ "Other / Nonprofit"
    ),
    Phase_clean = if_else(is.na(Phases) | Phases == "", "N/A", Phases)
  )

# ── 2. Palette ───────────────────────────────────────────────────────────────

pal <- list(
  blue   = "#378ADD",
  teal   = "#1D9E75",
  purple = "#534AB7",
  coral  = "#D85A30",
  pink   = "#D4537E",
  amber  = "#BA7517",
  green  = "#639922",
  gray   = "#888780",
  lgray  = "#B4B2A9",
  bg     = "#F8F7F4"
)

# ── 3. Aggregations ──────────────────────────────────────────────────────────

status_counts <- df |>
  count(Status, name = "n") |>
  arrange(desc(n)) |>
  slice_head(n = 8) |>
  mutate(Status = fct_reorder(Status, n))

year_counts <- df |>
  filter(Launch_Year >= 2016, Launch_Year <= 2021) |>
  count(Launch_Year, name = "Trials") |>
  arrange(Launch_Year)

df_iv <- df |> filter(Type == "Interventional")

phase_enroll <- df_iv |>
  filter(str_starts(Phase_clean, "Phase")) |>
  group_by(Phase_clean) |>
  summarise(Total = sum(Enrollment, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(Total)) |>
  mutate(Label = case_when(
    Total >= 1e6 ~ paste0(round(Total / 1e6, 1), "M"),
    Total >= 1e3 ~ paste0(round(Total / 1e3), "k"),
    TRUE         ~ as.character(Total)
  ))

iv_kws <- c("Drug", "Biological", "Device", "Behavioral",
            "Diagnostic Test", "Other", "Procedure", "Genetic")
iv_counts <- tibble(
  Intervention = iv_kws,
  Count = map_int(iv_kws, \(kw) sum(str_detect(df$Interventions, fixed(paste0(kw, ":"))), na.rm = TRUE))
) |> arrange(Count)

funder_counts <- df |> count(Funder, name = "n") |> arrange(desc(n))

# ── 4. Individual Plotly traces ───────────────────────────────────────────────

p_status <- plot_ly(
  status_counts,
  x = ~n, y = ~Status,
  type = "bar", orientation = "h",
  marker = list(color = pal$blue),
  text = ~n, textposition = "outside",
  hovertemplate = "%{y}: %{x}<extra></extra>"
) |>
  layout(
    title = list(text = "Trial status distribution", font = list(size = 13)),
    xaxis = list(title = "", showgrid = TRUE, gridcolor = "#E0DED8"),
    yaxis = list(title = ""),
    paper_bgcolor = pal$bg, plot_bgcolor = pal$bg,
    margin = list(l = 10, r = 30, t = 40, b = 20)
  )

p_year <- plot_ly(
  year_counts,
  x = ~as.character(Launch_Year), y = ~Trials,
  type = "bar",
  marker = list(color = c(rep(pal$lgray, 4), pal$blue, pal$teal)),
  text = ~Trials, textposition = "outside",
  hovertemplate = "%{x}: %{y} trials<extra></extra>"
) |>
  layout(
    title = list(text = "Launch volume 2016–2021", font = list(size = 13)),
    xaxis = list(title = ""),
    yaxis = list(title = "", showgrid = TRUE, gridcolor = "#E0DED8"),
    paper_bgcolor = pal$bg, plot_bgcolor = pal$bg,
    margin = list(l = 10, r = 10, t = 40, b = 20)
  )

p_phase <- plot_ly(
  phase_enroll,
  x = ~Phase_clean, y = ~Total,
  type = "bar",
  marker = list(color = pal$purple),
  text = ~Label, textposition = "outside",
  hovertemplate = "%{x}<br>Enrollment: %{text}<extra></extra>"
) |>
  layout(
    title = list(text = "Enrollment by trial phase", font = list(size = 13)),
    xaxis = list(title = ""),
    yaxis = list(title = "", showgrid = TRUE, gridcolor = "#E0DED8"),
    paper_bgcolor = pal$bg, plot_bgcolor = pal$bg,
    margin = list(l = 10, r = 10, t = 40, b = 20)
  )

p_type <- plot_ly(
  labels = c("Interventional", "Observational", "Expanded Access"),
  values = c(3322, 2427, 34),
  type = "pie", hole = 0.60,
  marker = list(colors = c(pal$purple, pal$pink, pal$gray)),
  textinfo = "label+percent",
  textfont = list(size = 10)
) |>
  layout(
    title = list(text = "Study type split", font = list(size = 13)),
    showlegend = FALSE,
    paper_bgcolor = pal$bg, plot_bgcolor = pal$bg,
    margin = list(l = 10, r = 10, t = 40, b = 10)
  )

p_funder <- plot_ly(
  funder_counts,
  labels = ~Funder, values = ~n,
  type = "pie", hole = 0.60,
  marker = list(colors = c(pal$teal, pal$purple, pal$blue, pal$lgray)),
  textinfo = "label+percent",
  textfont = list(size = 10)
) |>
  layout(
    title = list(text = "Funder breakdown", font = list(size = 13)),
    showlegend = FALSE,
    paper_bgcolor = pal$bg, plot_bgcolor = pal$bg,
    margin = list(l = 10, r = 10, t = 40, b = 10)
  )

p_iv <- plot_ly(
  iv_counts,
  x = ~Count, y = ~Intervention,
  type = "bar", orientation = "h",
  marker = list(color = pal$coral),
  text = ~Count, textposition = "outside",
  hovertemplate = "%{y}: %{x} trials<extra></extra>"
) |>
  layout(
    title = list(text = "Intervention categories", font = list(size = 13)),
    xaxis = list(title = "", showgrid = TRUE, gridcolor = "#E0DED8"),
    yaxis = list(title = ""),
    paper_bgcolor = pal$bg, plot_bgcolor = pal$bg,
    margin = list(l = 10, r = 30, t = 40, b = 20)
  )

# ── 5. Combine into dashboard ────────────────────────────────────────────────

dashboard <- subplot(
  p_status, p_year, p_phase,
  p_type,   p_funder, p_iv,
  nrows = 2, shareX = FALSE, shareY = FALSE,
  titleX = TRUE, titleY = TRUE,
  margin = 0.08
) |>
  layout(
    title = list(
      text = "<b>Infectious Disease Surveillance Model</b> · COVID-19 Clinical Trials · 2026",
      font = list(size = 15, family = "Georgia, serif", color = "#2C2C2A"),
      x = 0.01
    ),
    paper_bgcolor = "#F8F7F4",
    plot_bgcolor  = "#F8F7F4",
    font  = list(family = "Georgia, serif", size = 11, color = "#444441"),
    showlegend = FALSE,
    margin = list(t = 60, b = 20, l = 20, r = 20),
    height = 680
  )

saveWidget(dashboard, "covid_surveillance_dashboard_R.html", selfcontained = TRUE)
cat("✓  Saved: covid_surveillance_dashboard_R.html\n")

# ── 6. Correlation analysis ───────────────────────────────────────────────────

cat("\n── Phase × Enrollment correlation (interventional trials) ──\n")
phase_corr <- df_iv |>
  filter(str_starts(Phase_clean, "Phase"), Enrollment > 0) |>
  group_by(Phase_clean) |>
  summarise(
    Trial_Count       = n(),
    Median_Enrollment = median(Enrollment, na.rm = TRUE),
    Total_Enrollment  = sum(Enrollment, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(Total_Enrollment))

print(phase_corr)

cat("\n── Year × Enrollment trend ──\n")
year_trend <- df |>
  filter(Launch_Year >= 2016, Launch_Year <= 2021) |>
  group_by(Launch_Year) |>
  summarise(
    Trials         = n(),
    Avg_Enrollment = mean(Enrollment, na.rm = TRUE),
    .groups = "drop"
  )

print(year_trend)

r_val <- cor(year_trend$Trials, year_trend$Avg_Enrollment, method = "pearson")
cat(sprintf("\nPearson r (trials vs avg enrollment, 2016–2021): %.4f\n", r_val))
