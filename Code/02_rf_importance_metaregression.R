# ==============================================================
# COMPOSITE FIGURE - CUSTOM PALETTE + WEIGHTED FILLED POINTS
# File: Bookfull.xlsx | Folder: C:/Users/OMEN/Desktop/4eme
# ==============================================================
# --- 1. PACKAGES ---
packages <- c("metafor", "readxl", "dplyr", "ggplot2", "patchwork", "ranger")
new_pkgs <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_pkgs)) install.packages(new_pkgs)
library(metafor); library(readxl); library(dplyr)
library(ggplot2); library(patchwork); library(ranger)

# --- 2. READ DATA ---
output_dir <- "C:/Users/OMEN/Desktop/4eme"
df <- read_excel(file.path(output_dir, "Bookfull.xlsx"), sheet = "clean_data")
cat("Data loaded:", nrow(df), "rows\n")

# --- 3. MODERATORS + CUSTOM PALETTE ---
cont_vars <- c("MAT", "MAP", "initial_SOC", "soil_pH", "exp_duration", "N_rate")
cat_vars  <- c("amendment_type")
all_mods  <- c(cont_vars, cat_vars)

# Dedicated palette per moderator
mod_palette <- c(
  "MAT"            = "#E74C3C",   # red - temperature
  "MAP"            = "#2980B9",   # blue - precipitation
  "initial_SOC"    = "#8E44AD",   # purple - soil carbon
  "soil_pH"        = "#F39C12",   # orange - pH
  "exp_duration"   = "#16A085",   # green - duration
  "N_rate"         = "#F1C40F",   # yellow - nitrogen
  "amendment_type" = "#2C3E50"    # dark blue - amendment
)

var_labels <- list(
  MAT            = "Mean annual temperature (deg C)",
  MAP            = "Mean annual precipitation (mm)",
  initial_SOC    = "Initial SOC (g kg-1)",
  soil_pH        = "Soil pH",
  exp_duration   = "Experiment duration (years)",
  N_rate         = "N rate (kg N ha-1)",
  amendment_type = "Amendment type"
)

df <- df %>%
  mutate(amendment_type = factor(amendment_type,
                                 levels = c("Straw", "Biochar", "Mixture"))) %>%
  filter(!is.na(yi), !is.na(vi))

# --- 4. DATA PREPARATION ---
prepare_data <- function(dat, var_name) {
  sub <- dat %>% filter(Variable == var_name)
  for(v in cont_vars) {
    sub[[paste0(v, "_scaled")]] <- as.numeric(scale(sub[[v]]))
  }
  w <- 1 / sqrt(sub$vi)
  sub$point_size <- 1.5 + 3.5 * (w - min(w)) / (max(w) - min(w))
  return(sub)
}
soc_data   <- prepare_data(df, "SOC")
yield_data <- prepare_data(df, "Yield")
cat("SOC:", nrow(soc_data), "obs | Yield:", nrow(yield_data), "obs\n")

# ==============================================================
# FUNCTIONS
# ==============================================================
# (a) RF importance - BARS COLORED BY VARIABLE
calc_importance <- function(dat) {
  df_rf <- dat %>% select(yi, all_of(all_mods))
  rf <- ranger(yi ~ ., data = df_rf, importance = "permutation", num.trees = 500)
  imp_df <- data.frame(
    variable = names(importance(rf)),
    MSE_increase = importance(rf)
  ) %>% arrange(desc(MSE_increase)) %>%
    mutate(variable = factor(variable, levels = rev(variable)))
  
  p <- ggplot(imp_df, aes(x = MSE_increase, y = variable, fill = variable)) +
    geom_col() +
    scale_fill_manual(values = mod_palette, guide = "none") +
    labs(x = "Increase in MSE", y = "") +
    theme_classic(base_family = "Times") +
    theme(axis.text = element_text(size = 9.5))
  
  top3 <- as.character(imp_df$variable)[1:3]
  return(list(plot = p, df = imp_df, top3 = top3))
}

# (b/c/d) Dynamic plot - POINTS COLORED BY MODERATOR
plot_dynamic <- function(dat, var_name, ylab_title) {
  var_color <- mod_palette[var_name]
  
  if(var_name %in% cont_vars) {
    # ---- CONTINUOUS ----
    var_scaled <- paste0(var_name, "_scaled")
    fit <- try(rma(yi = yi, vi = vi,
                   mods = as.formula(paste0("~", var_scaled)),
                   data = dat, method = "REML"), silent = TRUE)
    if(inherits(fit, "try-error")) {
      return(ggplot() + ggtitle(paste0(var_name, ": failed")) + theme_void())
    }
    
    x_seq <- seq(min(dat[[var_name]], na.rm = TRUE),
                 max(dat[[var_name]], na.rm = TRUE), length.out = 200)
    m <- attr(scale(dat[[var_name]]), "scaled:center")
    s <- attr(scale(dat[[var_name]]), "scaled:scale")
    pred <- predict(fit, newmods = cbind((x_seq - m) / s))
    pred_df <- data.frame(x = x_seq, y = pred$pred, lo = pred$ci.lb, hi = pred$ci.ub)
    
    pval <- fit$pval[2]
    if(is.na(pval)) {
      p_label <- ""
    } else if(pval < 0.0001) {
      p_label <- "P<0.0001"
    } else if(pval < 0.001) {
      p_label <- "P<0.001"
    } else if(pval < 0.01) {
      p_label <- "P<0.01"
    } else if(pval < 0.05) {
      p_label <- "P<0.05"
    } else {
      p_label <- paste0("P=", sprintf("%.3f", pval))
    }
    
    ggplot() +
      geom_point(data = dat,
                 aes(x = .data[[var_name]], y = yi, size = point_size),
                 shape = 16, colour = var_color, alpha = 0.55) +
      geom_ribbon(data = pred_df, aes(x = x, ymin = lo, ymax = hi),
                  fill = "#95a5a6", alpha = 0.35) +
      geom_line(data = pred_df, aes(x = x, y = y),
                colour = "#c0392b", linewidth = 1.1) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "black", linewidth = 0.4) +
      annotate("text", x = -Inf, y = Inf, label = p_label,
               hjust = -0.2, vjust = 1.5, size = 4.5,
               family = "Times", fontface = "italic") +
      scale_size_continuous(range = c(1.5, 5), guide = "none") +
      labs(x = var_labels[[var_name]], y = ylab_title) +
      theme_classic(base_family = "Times") +
      theme(axis.text = element_text(size = 10))
    
  } else {
    # ---- CATEGORICAL (amendment_type) ----
    res_list <- list()
    for(level in levels(dat[[var_name]])) {
      sub <- dat %>% filter(.data[[var_name]] == level)
      if(nrow(sub) < 2) { res_list[[level]] <- NULL; next }
      fit_sub <- try(rma(yi = yi, vi = vi, data = sub, method = "REML"), silent = TRUE)
      if(inherits(fit_sub, "try-error")) { res_list[[level]] <- NULL; next }
      res_list[[level]] <- data.frame(
        category = level,
        estimate = as.numeric(fit_sub$b[1]),
        ci_low   = fit_sub$ci.lb,
        ci_high  = fit_sub$ci.ub
      )
    }
    res <- bind_rows(res_list)
    res$category <- factor(res$category, levels = levels(dat[[var_name]]))
    
    if(nrow(res) == 0) {
      return(ggplot() + ggtitle(paste0(var_name, ": no valid subgroups")) + theme_void())
    }
    
    ggplot(res, aes(y = category, x = estimate)) +
      geom_errorbar(aes(xmin = ci_low, xmax = ci_high),
                    orientation = "y", width = 0.2, linewidth = 0.7,
                    colour = var_color) +
      geom_point(size = 4, colour = var_color) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = "black", linewidth = 0.4) +
      labs(x = "Effect size ln(RR)", y = var_labels[[var_name]]) +
      theme_classic(base_family = "Times") +
      theme(axis.text = element_text(size = 10))
  }
}

# ==============================================================
# SOC FIGURE
# ==============================================================
cat("\n--- SOC figure ---\n")
imp_soc <- calc_importance(soc_data)
cat("  RF ranking SOC:", paste(as.character(imp_soc$df$variable), collapse = " > "), "\n")
cat("  Top 3:", paste(imp_soc$top3, collapse = " > "), "\n")
p_a_soc <- imp_soc$plot
p_b_soc <- plot_dynamic(soc_data, imp_soc$top3[1], "Effect size ln(RR)")
p_c_soc <- plot_dynamic(soc_data, imp_soc$top3[2], "Effect size ln(RR)")
p_d_soc <- plot_dynamic(soc_data, imp_soc$top3[3], "Effect size ln(RR)")
fig_soc <- (p_a_soc + p_b_soc) / (p_c_soc + p_d_soc) +
  plot_annotation(tag_levels = "a", title = "Soil organic carbon (SOC)")
ggsave(file.path(output_dir, "Figure_Meta_SOC_Composite.png"), fig_soc,
       width = 14, height = 11, dpi = 600, bg = "white", type = "cairo")
ggsave(file.path(output_dir, "Figure_Meta_SOC_Composite.pdf"), fig_soc,
       width = 14, height = 11, device = cairo_pdf, bg = "white")
write.csv(imp_soc$df, file.path(output_dir, "Moderator_Importance_SOC.csv"), row.names = FALSE)
cat("  OK SOC\n")

# ==============================================================
# YIELD FIGURE
# ==============================================================
cat("\n--- Yield figure ---\n")
imp_yield <- calc_importance(yield_data)
cat("  RF ranking Yield:", paste(as.character(imp_yield$df$variable), collapse = " > "), "\n")
cat("  Top 3:", paste(imp_yield$top3, collapse = " > "), "\n")
p_a_yield <- imp_yield$plot
p_b_yield <- plot_dynamic(yield_data, imp_yield$top3[1], "Effect size ln(RR)")
p_c_yield <- plot_dynamic(yield_data, imp_yield$top3[2], "Effect size ln(RR)")
p_d_yield <- plot_dynamic(yield_data, imp_yield$top3[3], "Effect size ln(RR)")
fig_yield <- (p_a_yield + p_b_yield) / (p_c_yield + p_d_yield) +
  plot_annotation(tag_levels = "a", title = "Grain yield")
ggsave(file.path(output_dir, "Figure_Meta_Yield_Composite.png"), fig_yield,
       width = 14, height = 11, dpi = 600, bg = "white", type = "cairo")
ggsave(file.path(output_dir, "Figure_Meta_Yield_Composite.pdf"), fig_yield,
       width = 14, height = 11, device = cairo_pdf, bg = "white")
write.csv(imp_yield$df, file.path(output_dir, "Moderator_Importance_Yield.csv"), row.names = FALSE)
cat("  OK Yield\n")

# --- DISPLAY ---
print(fig_soc)
print(fig_yield)
cat("\n=== DONE ===\n")
