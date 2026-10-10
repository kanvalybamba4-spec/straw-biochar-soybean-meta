# ============================================================
# GLOBAL META-ANALYSIS : EFFECT SIZES OF THREE AMENDMENT TYPES
# ============================================================

# --- 1. PACKAGES ---
packages <- c("metafor", "ggplot2", "patchwork", "readxl", "dplyr")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)>0) install.packages(new_packages)
library(metafor)
library(ggplot2)
library(patchwork)
library(readxl)
library(dplyr)

# --- 2. INPUT / OUTPUT SETTINGS (EDIT AS NEEDED) ---
# Folder containing the data file; all outputs are saved here.
output_dir <- "C:/Users/OMEN/Desktop/4eme"   # <-- change to your folder
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
# Excel file with extracted effect sizes (single sheet, first row = header).
excel_path <- file.path(output_dir, "Book1.xlsx")   # <-- your data file

# --- 3. READ THE EXCEL FILE ---
raw <- read_excel(excel_path, sheet = 1, col_names = FALSE)
header_row <- as.character(unlist(raw[1, ]))
treat_pos <- which(header_row == "Treatment")
yield_start <- treat_pos[1]
soc_start   <- treat_pos[2]

# Yield block columns
col_y_treat <- yield_start
col_y_ck    <- yield_start + 4
col_y_sdck  <- yield_start + 5
col_y_tr    <- yield_start + 6
col_y_sdtr  <- yield_start + 7
col_y_yi    <- yield_start + 8
col_y_vi    <- yield_start + 9

# SOC block columns
col_s_treat <- soc_start
col_s_ck    <- soc_start + 4
col_s_sdck  <- soc_start + 5
col_s_tr    <- soc_start + 6
col_s_sdtr  <- soc_start + 7
col_s_yi    <- soc_start + 8
col_s_vi    <- soc_start + 9

# --- 4. EXTRACT YIELD DATA ---
yield_dat <- data.frame(
  Treatment = as.character(raw[[col_y_treat]]),
  CK        = as.numeric(raw[[col_y_ck]]),
  SDck      = as.numeric(raw[[col_y_sdck]]),
  TR        = as.numeric(raw[[col_y_tr]]),
  SDtr      = as.numeric(raw[[col_y_sdtr]]),
  yi        = as.numeric(raw[[col_y_yi]]),
  vi        = as.numeric(raw[[col_y_vi]])
)
yield_dat <- yield_dat[!is.na(yield_dat$yi) & !is.na(yield_dat$Treatment), ]
yield_dat$Treatment <- trimws(yield_dat$Treatment)

# Missing or non-positive sampling variances are replaced with half the
# minimum observed variance, so that all extracted studies can be retained
# in the model. This choice is applied identically to every treatment and
# is stated here for full transparency.
yield_dat$vi[is.na(yield_dat$vi) | yield_dat$vi <= 0] <-
  min(yield_dat$vi[yield_dat$vi > 0], na.rm = TRUE) * 0.5

# --- 5. EXTRACT SOC DATA ---
soc_dat <- data.frame(
  Treatment = as.character(raw[[col_s_treat]]),
  CK        = as.numeric(raw[[col_s_ck]]),
  SDck      = as.numeric(raw[[col_s_sdck]]),
  TR        = as.numeric(raw[[col_s_tr]]),
  SDtr      = as.numeric(raw[[col_s_sdtr]]),
  yi        = as.numeric(raw[[col_s_yi]]),
  vi        = as.numeric(raw[[col_s_vi]])
)
soc_dat <- soc_dat[!is.na(soc_dat$yi) & !is.na(soc_dat$Treatment), ]
soc_dat$Treatment <- trimws(soc_dat$Treatment)
soc_dat$vi[is.na(soc_dat$vi) | soc_dat$vi <= 0] <-
  min(soc_dat$vi[soc_dat$vi > 0], na.rm = TRUE) * 0.5

# --- 6. META-ANALYSIS FUNCTION (REML, BY AMENDMENT TYPE) ---
run_meta <- function(dat, var_label) {
  treatments <- c("Biochar", "Straw", "Mixture")
  results <- data.frame()
  for (trt in treatments) {
    sub <- dat[dat$Treatment == trt, ]
    if (nrow(sub) < 2) next
    res <- rma(yi = yi, vi = vi, data = sub, method = "REML")
    results <- rbind(results, data.frame(
      Variable   = var_label,
      Treatment  = trt,
      estimate   = as.numeric(res$b[1]),
      ci.lb      = res$ci.lb,
      ci.ub      = res$ci.ub,
      pval       = res$pval,
      Q_total    = res$QE,
      Q_pvalue   = res$QEp,
      I2_percent = round(res$I2, 1),
      tau2       = round(res$tau2, 6)
    ))
  }
  return(results)
}

# --- 7. RUN META-ANALYSES ---
meta_yield <- run_meta(yield_dat, "Yield")
meta_soc   <- run_meta(soc_dat,   "SOC")
meta_all   <- rbind(meta_yield, meta_soc)

cat("\n=== META-ANALYSIS RESULTS ===\n")
print(meta_all, row.names = FALSE)

# --- 8. PLOT FUNCTION : CLEAN LABELS, FIXED POINT SIZE ---
plot_es <- function(meta_res, title_text, color_palette) {
  meta_res$Treatment <- factor(meta_res$Treatment,
                               levels = c("Biochar", "Straw", "Mixture"))
  meta_res$sig <- dplyr::case_when(
    meta_res$pval < 0.001 ~ "***",
    meta_res$pval < 0.01  ~ "**",
    meta_res$pval < 0.05  ~ "*",
    TRUE ~ "ns"
  )
  # Estimate labels placed above the upper confidence bound
  meta_res$pos_est_label <- meta_res$ci.ub + 0.008
  
  ggplot(meta_res, aes(x = Treatment, y = estimate, color = Treatment)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.45) +
    geom_errorbar(aes(ymin = ci.lb, ymax = ci.ub), width = 0.12, linewidth = 0.8) +
    geom_point(shape = 16, size = 5, stroke = 1) +
    geom_text(aes(y = pos_est_label, label = sprintf("%.3f %s", estimate, sig)),
              family = "Times", size = 3.3, fontface = "bold", color = "black") +
    scale_color_manual(values = color_palette, guide = "none") +
    labs(title = title_text, y = "Effect size (ln response ratio)", x = "") +
    theme_classic(base_family = "Times") +
    theme(
      plot.title   = element_text(hjust = 0.01, size = 14, face = "bold"),
      axis.text    = element_text(size = 11, color = "black"),
      axis.title.y = element_text(size = 12),
      axis.line    = element_line(linewidth = 0.6),
      axis.ticks   = element_line(linewidth = 0.5),
      plot.margin  = margin(22, 20, 12, 8)
    )
}

# --- 9. COLOR PALETTE ---
pal <- c("Biochar" = "#C0392B", "Straw" = "#E67E22", "Mixture" = "#2471A3")
p_soc   <- plot_es(meta_soc,   "(a) Soil organic carbon", pal)
p_yield <- plot_es(meta_yield, "(b) Grain yield",         pal)

# Combined figure with independent y-axes
fig_final <- p_soc + p_yield +
  plot_layout(ncol = 2, widths = c(1, 1))
print(fig_final)

# --- 10. EXPORT OUTPUTS ---
ggsave(file.path(output_dir, "Fig_Meta_EffectSize_3amendments.png"),
       fig_final, width = 13.2, height = 6.8, units = "in", dpi = 600, bg = "white", type = "cairo")
ggsave(file.path(output_dir, "Fig_Meta_EffectSize_3amendments.pdf"),
       fig_final, width = 13.2, height = 6.8, units = "in", device = cairo_pdf, bg = "white")
write.csv(meta_all, file.path(output_dir, "Meta_Results_3amendments.csv"), row.names = FALSE)

cat("\nDONE\n")
cat("Output folder:", output_dir, "\n")
