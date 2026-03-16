# ============================================================
#   Bank Transaction Data — Statistical Analysis in R
#   Author : Antonnio Macharia
#   Email  : antonniomacharia75@gmail.com
#   Date   : 2026-03-16
#
#   Skills demonstrated:
#   Data Wrangling (dplyr / tidyr), Date Handling (lubridate),
#   Statistical Tests (t-test, chi-square, correlation),
#   Data Visualisation (ggplot2), Grouped Summaries,
#   String Manipulation (stringr), CSV Export
# ============================================================

# ── 0. Load Libraries ─────────────────────────────────────────
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(scales)

cat("=============================================================\n")
cat("  BANK TRANSACTION STATISTICAL ANALYSIS — Antonnio Macharia\n")
cat("=============================================================\n\n")

# ── 1. Load & Clean Data ──────────────────────────────────────
df <- read.csv("bank_transactions_data_2_augmented_clean_2.csv",
               stringsAsFactors = FALSE)

# Parse dates
df$TransactionDate <- parse_date_time(df$TransactionDate,
                                       orders = c("m/d/Y H:M", "m/d/Y"),
                                       quiet  = TRUE)

# Feature engineering
df <- df %>%
  mutate(
    Year             = year(TransactionDate),
    Month            = month(TransactionDate),
    MonthName        = month(TransactionDate, label = TRUE, abbr = TRUE),
    Quarter          = paste0("Q", quarter(TransactionDate)),
    DayOfWeek        = wday(TransactionDate, label = TRUE, abbr = FALSE),
    SuspiciousLogin  = LoginAttempts > 1,
    HighValue        = TransactionAmount > quantile(TransactionAmount, 0.90),
    AmountBand       = cut(TransactionAmount,
                           breaks = c(0, 100, 300, 600, 1000, Inf),
                           labels = c("$0–100", "$101–300", "$301–600",
                                      "$601–1000", "$1000+"),
                           right  = TRUE)
  )

cat("── Dataset Overview ─────────────────────────────────────────\n")
cat(sprintf("  Total Transactions  : %s\n",  format(nrow(df), big.mark = ",")))
cat(sprintf("  Unique Accounts     : %s\n",  format(n_distinct(df$AccountID), big.mark = ",")))
cat(sprintf("  Date Range          : %s  →  %s\n",
            format(min(df$TransactionDate), "%Y-%m-%d"),
            format(max(df$TransactionDate), "%Y-%m-%d")))
cat(sprintf("  Avg Transaction     : $%s\n",  format(round(mean(df$TransactionAmount), 2), big.mark = ",")))
cat(sprintf("  Total Volume        : $%s\n",  format(round(sum(df$TransactionAmount), 2), big.mark = ",")))
cat(sprintf("  Suspicious Logins   : %s (%.1f%%)\n",
            format(sum(df$SuspiciousLogin), big.mark = ","),
            mean(df$SuspiciousLogin) * 100))
cat(sprintf("  Missing Values      : %d\n",   sum(is.na(df))))

# ── 2. Summary Statistics ─────────────────────────────────────
cat("\n── Transaction Amount — Summary Statistics ──────────────────\n")
print(summary(df$TransactionAmount))

cat("\n── Account Balance — Summary Statistics ─────────────────────\n")
print(summary(df$AccountBalance))

# ── 3. Group Summaries ────────────────────────────────────────
cat("\n── Transactions by Channel ──────────────────────────────────\n")
by_channel <- df %>%
  group_by(Channel) %>%
  summarise(
    Count         = n(),
    Total_Amount  = sum(TransactionAmount),
    Avg_Amount    = mean(TransactionAmount),
    Suspicious    = sum(SuspiciousLogin),
    Susp_Rate_Pct = mean(SuspiciousLogin) * 100,
    .groups       = "drop"
  )
print(as.data.frame(by_channel))

cat("\n── Transactions by Occupation ───────────────────────────────\n")
by_occ <- df %>%
  group_by(CustomerOccupation) %>%
  summarise(
    Count        = n(),
    Total_Amount = sum(TransactionAmount),
    Avg_Amount   = mean(TransactionAmount),
    Avg_Balance  = mean(AccountBalance),
    Suspicious   = sum(SuspiciousLogin),
    .groups      = "drop"
  ) %>%
  arrange(desc(Total_Amount))
print(as.data.frame(by_occ))

# ── 4. Statistical Tests ──────────────────────────────────────
cat("\n── Statistical Tests ────────────────────────────────────────\n")

# T-test: do suspicious transactions differ in amount?
normal_amt  <- df$TransactionAmount[!df$SuspiciousLogin]
susp_amt    <- df$TransactionAmount[df$SuspiciousLogin]
ttest_res   <- t.test(normal_amt, susp_amt)

cat("\n  T-Test: Transaction Amount — Normal vs Suspicious Logins\n")
cat(sprintf("  Normal   mean: $%.2f\n", mean(normal_amt)))
cat(sprintf("  Suspicious mean: $%.2f\n", mean(susp_amt)))
cat(sprintf("  t-statistic: %.4f\n", ttest_res$statistic))
cat(sprintf("  p-value    : %.6f\n", ttest_res$p.value))
cat(sprintf("  Conclusion : %s\n",
    ifelse(ttest_res$p.value < 0.05,
           "Significant difference (p < 0.05)",
           "No significant difference (p >= 0.05)")))

# Chi-square: is channel independent of transaction type?
chi_table <- table(df$Channel, df$TransactionType)
chi_res   <- chisq.test(chi_table)
cat("\n  Chi-Square: Channel vs Transaction Type\n")
cat(sprintf("  Chi-squared: %.4f\n", chi_res$statistic))
cat(sprintf("  p-value    : %.6f\n", chi_res$p.value))
cat(sprintf("  Conclusion : %s\n",
    ifelse(chi_res$p.value < 0.05,
           "Channel and TransactionType are NOT independent (p < 0.05)",
           "No significant association found (p >= 0.05)")))

# Correlation: amount vs balance
cor_val <- cor(df$TransactionAmount, df$AccountBalance, method = "pearson")
cat(sprintf("\n  Pearson Correlation — Amount vs Balance: r = %.4f\n", cor_val))
cat(sprintf("  Interpretation: %s\n",
    ifelse(abs(cor_val) < 0.1, "Negligible correlation",
    ifelse(abs(cor_val) < 0.3, "Weak correlation",
    ifelse(abs(cor_val) < 0.5, "Moderate correlation", "Strong correlation")))))

# ── 5. Colour palette ─────────────────────────────────────────
pal <- c("#1F3864", "#2E75B6", "#F4B942", "#375623", "#C00000")

# ── 6. Plot 1: Overview (2×3 grid) ───────────────────────────
p1a <- ggplot(df, aes(x = TransactionType, fill = TransactionType)) +
  geom_bar(colour = "white", linewidth = 0.4) +
  geom_text(stat = "count", aes(label = scales::comma(after_stat(count))),
            vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = pal[1:2]) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Transaction Type Distribution",
       x = NULL, y = "Count") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", colour = pal[1]))

p1b <- ggplot(df, aes(x = TransactionAmount)) +
  geom_histogram(bins = 50, fill = pal[2], colour = "white", linewidth = 0.3, alpha = 0.85) +
  geom_vline(xintercept = mean(df$TransactionAmount),
             colour = pal[3], linewidth = 1.2, linetype = "dashed") +
  geom_vline(xintercept = quantile(df$TransactionAmount, 0.90),
             colour = pal[5], linewidth = 1.2, linetype = "dotted") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Transaction Amount Distribution",
       subtitle = sprintf("Dashed = Mean ($%.0f)  |  Dotted = 90th %%ile ($%.0f)",
                          mean(df$TransactionAmount),
                          quantile(df$TransactionAmount, 0.90)),
       x = "Amount ($)", y = "Frequency") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", colour = pal[1]),
        plot.subtitle = element_text(size = 9, colour = "grey40"))

p1c <- df %>%
  count(Channel) %>%
  ggplot(aes(x = "", y = n, fill = Channel)) +
  geom_col(colour = "white", linewidth = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Channel, "\n", scales::percent(n / sum(n), accuracy = 0.1))),
            position = position_stack(vjust = 0.5),
            size = 3.5, fontface = "bold", colour = "white") +
  scale_fill_manual(values = pal[1:3]) +
  labs(title = "Transactions by Channel") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", colour = pal[1], hjust = 0.5))

monthly <- df %>%
  group_by(Year, Month) %>%
  summarise(Total_Amount = sum(TransactionAmount) / 1e6, .groups = "drop")

p1d <- ggplot(monthly, aes(x = Month, y = Total_Amount,
                            colour = factor(Year), group = factor(Year))) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:12,
                     labels = c("J","F","M","A","M","J","J","A","S","O","N","D")) +
  scale_y_continuous(labels = scales::dollar_format(suffix = "M")) +
  scale_colour_manual(values = c("#1F3864","#2E75B6","#F4B942","#375623","#C00000","#7030A0")) +
  labs(title = "Monthly Transaction Volume by Year",
       x = "Month", y = "Total Amount ($M)", colour = "Year") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", colour = pal[1]),
        legend.position = "bottom")

occ_avg <- df %>%
  group_by(CustomerOccupation) %>%
  summarise(Avg_Amount = mean(TransactionAmount), .groups = "drop") %>%
  arrange(Avg_Amount)

p1e <- ggplot(occ_avg, aes(x = reorder(CustomerOccupation, Avg_Amount), y = Avg_Amount)) +
  geom_col(fill = pal[2], colour = "white", linewidth = 0.5) +
  geom_text(aes(label = scales::dollar(round(Avg_Amount))),
            hjust = -0.15, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar, expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Avg Transaction by Occupation",
       x = NULL, y = "Average Amount ($)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", colour = pal[1]))

p1f <- ggplot(df %>% sample_n(2000),
              aes(x = AccountBalance, y = TransactionAmount,
                  colour = SuspiciousLogin)) +
  geom_point(alpha = 0.4, size = 1.2) +
  scale_colour_manual(values = c(pal[2], pal[5]),
                      labels = c("Normal", "Suspicious")) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Balance vs Transaction Amount",
       subtitle = "Red = suspicious login attempt",
       x = "Account Balance ($)", y = "Transaction Amount ($)",
       colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", colour = pal[1]),
        legend.position = "bottom")

# Combine with patchwork-style layout using cowplot
library(gridExtra)

title_grob <- grid::textGrob(
  "Bank Transaction Analysis — Overview\nAntonnio Macharia  |  antonniomacharia75@gmail.com",
  gp = grid::gpar(fontsize = 14, fontface = "bold", col = "#1F3864")
)

png("R_Chart1_Overview.png", width = 1800, height = 1100, res = 120)
gridExtra::grid.arrange(
  title_grob, p1a, p1b, p1c, p1d, p1e, p1f,
  layout_matrix = rbind(c(1,1,1), c(2,3,4), c(5,6,7)),
  heights = c(0.08, 1, 1)
)
dev.off()
cat("\n  [Chart 1] R Overview saved.\n")

# ── 7. Plot 2: Fraud / Suspicious Analysis ────────────────────
p2a <- df %>%
  group_by(Channel, SuspiciousLogin) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Type = ifelse(SuspiciousLogin, "Suspicious", "Normal")) %>%
  ggplot(aes(x = Channel, y = Count, fill = Type)) +
  geom_col(position = "dodge", colour = "white", linewidth = 0.5) +
  scale_fill_manual(values = c("Normal" = pal[2], "Suspicious" = pal[5])) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Normal vs Suspicious by Channel",
       x = NULL, y = "Count", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", colour = pal[1]),
        legend.position = "bottom")

p2b <- df %>%
  count(LoginAttempts) %>%
  mutate(Colour = ifelse(LoginAttempts == 1, "Normal", "Suspicious")) %>%
  ggplot(aes(x = factor(LoginAttempts), y = n, fill = Colour)) +
  geom_col(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = scales::comma(n)), vjust = -0.4, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("Normal" = pal[2], "Suspicious" = pal[5])) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Login Attempts Distribution",
       x = "Login Attempts", y = "Count", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", colour = pal[1]),
        legend.position = "none")

p2c <- df %>%
  group_by(CustomerOccupation) %>%
  summarise(Susp_Rate = mean(SuspiciousLogin) * 100,
            avg_susp  = mean(SuspiciousLogin) * 100,
            .groups = "drop") %>%
  ggplot(aes(x = reorder(CustomerOccupation, Susp_Rate),
             y = Susp_Rate,
             fill = Susp_Rate > mean(.$Susp_Rate))) +
  geom_col(colour = "white", linewidth = 0.5) +
  geom_hline(yintercept = mean(df$SuspiciousLogin) * 100,
             linetype = "dashed", colour = pal[3], linewidth = 1.2) +
  coord_flip() +
  scale_fill_manual(values = c(pal[2], pal[5])) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Suspicious Rate by Occupation",
       x = NULL, y = "Suspicious Rate (%)",
       subtitle = "Dashed = overall average") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", colour = pal[1]),
        legend.position = "none")

p2d <- ggplot(df, aes(x = SuspiciousLogin, y = TransactionAmount,
                       fill = SuspiciousLogin)) +
  geom_boxplot(outlier.alpha = 0.2, outlier.size = 0.8, linewidth = 0.6) +
  scale_fill_manual(values = c(pal[2], pal[5])) +
  scale_x_discrete(labels = c("FALSE" = "Normal", "TRUE" = "Suspicious")) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Transaction Amount Distribution\nNormal vs Suspicious",
       x = NULL, y = "Amount ($)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", colour = pal[1]),
        legend.position = "none")

yr_sus <- df %>%
  group_by(Year) %>%
  summarise(Total = n(), Suspicious = sum(SuspiciousLogin),
            Rate = mean(SuspiciousLogin) * 100, .groups = "drop")

p2e <- ggplot(yr_sus, aes(x = factor(Year))) +
  geom_col(aes(y = Suspicious), fill = pal[5], alpha = 0.75,
           colour = "white", linewidth = 0.5) +
  geom_line(aes(y = Rate * 80, group = 1),
            colour = pal[1], linewidth = 1.5) +
  geom_point(aes(y = Rate * 80), colour = pal[1], size = 3) +
  scale_y_continuous(
    name = "Suspicious Count",
    labels = scales::comma,
    sec.axis = sec_axis(~ . / 80, name = "Rate (%)",
                        labels = function(x) paste0(round(x, 1), "%"))
  ) +
  labs(title = "Suspicious Transactions by Year",
       x = "Year") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", colour = pal[1]))

hm_data <- df %>%
  group_by(CustomerOccupation, Channel) %>%
  summarise(Avg_Amount = mean(TransactionAmount), .groups = "drop")

p2f <- ggplot(hm_data, aes(x = Channel, y = CustomerOccupation, fill = Avg_Amount)) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(aes(label = scales::dollar(round(Avg_Amount))),
            size = 3.5, fontface = "bold", colour = "white") +
  scale_fill_gradient(low = pal[2], high = pal[1],
                      labels = scales::dollar) +
  labs(title = "Avg Amount by Occupation & Channel",
       x = "Channel", y = "Occupation", fill = "Avg ($)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", colour = pal[1]))

title_grob2 <- grid::textGrob(
  "Suspicious Transaction Analysis — Fraud Detection Signals\nAntonnio Macharia  |  antonniomacharia75@gmail.com",
  gp = grid::gpar(fontsize = 14, fontface = "bold", col = "#1F3864")
)

png("R_Chart2_Fraud_Analysis.png", width = 1800, height = 1100, res = 120)
gridExtra::grid.arrange(
  title_grob2, p2a, p2b, p2c, p2d, p2e, p2f,
  layout_matrix = rbind(c(1,1,1), c(2,3,4), c(5,6,7)),
  heights = c(0.08, 1, 1)
)
dev.off()
cat("  [Chart 2] R Fraud Analysis saved.\n")

# ── 8. Plot 3: Statistical Deep Dive ─────────────────────────
p3a <- df %>%
  group_by(Year, TransactionType) %>%
  summarise(Total = sum(TransactionAmount) / 1e6, .groups = "drop") %>%
  ggplot(aes(x = factor(Year), y = Total, fill = TransactionType)) +
  geom_col(position = "dodge", colour = "white", linewidth = 0.5) +
  scale_fill_manual(values = pal[1:2]) +
  scale_y_continuous(labels = scales::dollar_format(suffix = "M")) +
  labs(title = "Annual Volume by Transaction Type",
       x = "Year", y = "Total ($M)", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", colour = pal[1]),
        legend.position = "bottom")

p3b <- df %>%
  group_by(AmountBand) %>%
  summarise(Count = n(), .groups = "drop") %>%
  filter(!is.na(AmountBand)) %>%
  ggplot(aes(x = AmountBand, y = Count, fill = AmountBand)) +
  geom_col(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = scales::comma(Count)), vjust = -0.4,
            size = 3.5, fontface = "bold") +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Transactions by Amount Band",
       x = "Amount Band", y = "Count") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", colour = pal[1]),
        legend.position = "none")

top_loc <- df %>%
  group_by(Location) %>%
  summarise(Total = sum(TransactionAmount) / 1e6, .groups = "drop") %>%
  top_n(10, Total) %>%
  arrange(Total)

p3c <- ggplot(top_loc, aes(x = reorder(Location, Total), y = Total)) +
  geom_col(fill = pal[2], colour = "white", linewidth = 0.5) +
  geom_text(aes(label = scales::dollar(round(Total, 1), suffix = "M")),
            hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar_format(suffix = "M"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Top 10 Locations by Volume",
       x = NULL, y = "Total ($M)") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", colour = pal[1]))

p3d <- df %>%
  group_by(CustomerOccupation, TransactionType) %>%
  summarise(Avg_Balance = mean(AccountBalance), .groups = "drop") %>%
  ggplot(aes(x = CustomerOccupation, y = Avg_Balance, fill = TransactionType)) +
  geom_col(position = "dodge", colour = "white", linewidth = 0.5) +
  scale_fill_manual(values = pal[1:2]) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Avg Balance by Occupation & Type",
       x = NULL, y = "Avg Balance ($)", fill = NULL) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", colour = pal[1]),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 10, hjust = 1))

title_grob3 <- grid::textGrob(
  "Transaction Trends & Statistical Summary\nAntonnio Macharia  |  antonniomacharia75@gmail.com",
  gp = grid::gpar(fontsize = 14, fontface = "bold", col = "#1F3864")
)

png("R_Chart3_Statistical_Analysis.png", width = 1600, height = 1100, res = 120)
gridExtra::grid.arrange(
  title_grob3, p3a, p3b, p3c, p3d,
  layout_matrix = rbind(c(1,1), c(2,3), c(4,5)),
  heights = c(0.08, 1, 1)
)
dev.off()
cat("  [Chart 3] R Statistical Analysis saved.\n")

# ── 9. Export Summary ─────────────────────────────────────────
r_summary <- df %>%
  group_by(Year, Channel, CustomerOccupation, TransactionType) %>%
  summarise(
    Transaction_Count  = n(),
    Total_Amount       = round(sum(TransactionAmount), 2),
    Avg_Amount         = round(mean(TransactionAmount), 2),
    Avg_Balance        = round(mean(AccountBalance), 2),
    Suspicious_Count   = sum(SuspiciousLogin),
    High_Value_Count   = sum(HighValue),
    .groups            = "drop"
  )
write.csv(r_summary, "R_Transaction_Summary.csv", row.names = FALSE)
cat("  [Export] R Summary CSV saved.\n")

cat("\n=============================================================\n")
cat("  Analysis Complete — Antonnio Macharia\n")
cat("  Files generated:\n")
cat("    * R_Chart1_Overview.png\n")
cat("    * R_Chart2_Fraud_Analysis.png\n")
cat("    * R_Chart3_Statistical_Analysis.png\n")
cat("    * R_Transaction_Summary.csv\n")
cat("=============================================================\n")
