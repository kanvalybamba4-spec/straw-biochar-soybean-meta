
# ==============================================================
# FUNNEL PLOTS (RANDOM-EFFECTS) FOR PUBLICATION BIAS ASSESSMENT
# File: Bookfull.xlsx | Folder: C:/Users/OMEN/Desktop/4eme
# Real data only. Panels shown for groups with k >= 10.
# ==============================================================
# --- 1. PACKAGES ---
packages <- c("metafor", "readxl", "dplyr", "ggplot2", "patchwork")
new_pkgs <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_pkgs)) install.packages(new_pkgs)
library(metafor); library(readxl); library(dplyr)
library(ggplot2); library(patchwork)

# --- 2. READ DATA ---
output_dir <- "C:/Users/OMEN/Desktop/4eme"
df <- read_excel(file.path(output_dir, "Bookfull.xlsx"), sheet = "clean_data")
df <- df %>% filter(!is.na(yi), !is.na(vi), vi > 0)
cat("Data loaded:", nrow(df), "rows\n")

# --- 3. FUNNEL PLOT FUNCTION (RANDOM-EFFECTS PSEUDO-CI) ---
funnel_re <- function(dat, vr, trt) {
  sub <- dat %>% filter(Variable == vr, amendment_type == trt)
  if(nrow(sub) < 10) return(NULL)
  
  res <- rma(yi = yi, vi = vi, data = sub, method = "REML")
  pooled <- as.numeric(res$b[1])
  tau2 <- res$tau2
  
  se_max <- max(sqrt(sub$vi), na.rm = TRUE) * 1.05
  se_seq <- seq(0, se_max, length.out = 200)
  
  ci95_low  <- pooled - 1.96 * sqrt(se_seq^2 + tau2)
  ci95_high <- pooled + 1.96 * sqrt(se_seq^2 + tau2)
  ci99_low  <- pooled - 2.576 * sqrt(se_seq^2 + tau2)
  ci99_high <- pooled + 2.576 * sqrt(se_seq^2 + tau2)
  
  funnel_df <- data.frame(
    se      = se_seq,
    ci95_lo = ci95_low, ci95_hi = ci95_high,
    ci99_lo = ci99_low, ci99_hi = ci99_high
  )
  
  ggplot() +
    # Pseudo-99% CI
    geom_ribbon(data = funnel_df, aes(x = ci99_lo, ymin = 0, ymax = se),
                fill = "gray80", alpha = 0.5) +
    geom_ribbon(data = funnel_df, aes(x = ci99_hi, ymin = 0, ymax = se),
                fill = "gray80", alpha = 0.5) +
    # Pseudo-95% CI
    geom_ribbon(data = funnel_df, aes(x = ci95_lo, ymin = 0, ymax = se),
                fill = "gray95", alpha = 0.8) +
    geom_ribbon(data = funnel_df, aes(x = ci95_hi, ymin = 0, ymax = se),
                fill = "gray95", alpha = 0.8) +
    # Triangle lines
    geom_line(data = funnel_df, aes(x = ci95_lo, y = se),
              color = "gray40", linetype = "dashed", linewidth = 0.4) +
    geom_line(data = funnel_df, aes(x = ci95_hi, y = se),
              color = "gray40", linetype = "dashed", linewidth = 0.4) +
    # Observed studies
    geom_point(data = sub, aes(x = yi, y = sqrt(vi)),
               shape = 19, size = 1.8, color = "#2C3E50", alpha = 0.7) +
    # Vertical lines
    geom_vline(xintercept = pooled, color = "#E74C3C",
               linetype = "solid", linewidth = 0.8) +
    geom_vline(xintercept = 0, color = "black",
               linetype = "dotted", linewidth = 0.5) +
    scale_y_reverse(expand = c(0, 0)) +
    labs(
      title = paste0(vr, " - ", trt, " (k=", res$k,
                     ", I2=", round(res$I2, 0), "%)"),
      x = "Effect size (lnRR)",
      y = "Standard error",
      caption = paste0("Pooled = ", round(pooled, 3),
                       " | tau2 = ", round(tau2, 4),
                       " | Random-effects pseudo-CI")
    ) +
    theme_classic(base_family = "Times") +
    theme(
      plot.title   = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.text    = element_text(size = 10, color = "black"),
      axis.title   = element_text(size = 11),
      plot.caption = element_text(size = 7, color = "gray40", hjust = 0.5),
      plot.margin  = margin(10, 10, 5, 10)
    )
}

# --- 4. GENERATE PANELS (REAL DATA ONLY) ---
cat("\n=== FUNNEL PLOTS (random effects) ===\n")
p1 <- funnel_re(df, "SOC",   "Biochar")
p2 <- funnel_re(df, "SOC",   "Straw")
p3 <- funnel_re(df, "Yield", "Biochar")
p4 <- funnel_re(df, "Yield", "Straw")

fig_funnel <- (p1 + p2) / (p3 + p4) +
  plot_annotation(title = "Funnel plots - Random-effects pseudo-CI",
                  theme = theme(plot.title = element_text(hjust = 0.5,
                                                          size = 15, face = "bold", family = "Times")))

ggsave(file.path(output_dir, "Fig_Funnel_RandomEffects.png"),
       fig_funnel, width = 12, height = 10, dpi = 600, bg = "white")
ggsave(file.path(output_dir, "Fig_Funnel_RandomEffects.pdf"),
       fig_funnel, width = 12, height = 10, device = cairo_pdf, bg = "white")
cat("  Fig_Funnel_RandomEffects.png / .pdf saved\n")
print(fig_funnel)

# --- 5. EGGER'S REGRESSION TEST (REAL DATA, k >= 10) ---
cat("\n=== EGGER'S REGRESSION TEST ===\n")
for(vr in c("SOC", "Yield")) {
  for(trt in c("Biochar", "Straw")) {
    sub <- df %>% filter(Variable == vr, amendment_type == trt)
    if(nrow(sub) < 10) next
    res <- rma(yi = yi, vi = vi, data = sub, method = "REML")
    reg <- regtest(res, model = "rma", predictor = "sei")
    cat(sprintf("  %s - %s : z=%.3f, p=%.4f [%s]\n",
                vr, trt, reg$zval, reg$pval,
                ifelse(reg$pval < 0.05, "possible bias", "no evidence of bias")))
  }
}
cat("\n=== DONE ===\n")
