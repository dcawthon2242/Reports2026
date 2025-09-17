library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(ggimage)
library(ggrepel)
library(plotly)
library(car)
library(grid)
library(png)
library(showtext)

# -------------------- Font (as you had) --------------------
font_add_google("Markazi Text", "markazi", regular.wt = 600)
showtext_auto()

# ==================== ONE-TIME: set your RStudio pane ratio ====================
PANE_AR <- getOption("csuf_pane_ar", 1.617978)

# -------------------- FIXED convertHeights function --------------------
convertHeights <- function(df) {
  convert <- function(height_str) {
    if (is.na(height_str)) return(NA)
    if (is.numeric(height_str)) return(height_str)
    height_str <- trimws(gsub('["\']', '', height_str))
    if (grepl("'", height_str)) {  
      parts <- strsplit(height_str, "'")[[1]]
      feet <- as.numeric(parts[1])
      inches <- if (length(parts) > 1 && parts[2] != "") as.numeric(parts[2]) else 0
    } else {  
      feet <- 0
      inches <- as.numeric(height_str)
    }
    feet + (inches/12)
  }
  height_cols <- c("RelHeight", "RelSide", "PlateLocHeight", "PlateLocSide")
  existing_cols <- height_cols[height_cols %in% names(df)]
  if (length(existing_cols) > 0) {
    for (col in existing_cols) {
      if (is.character(df[[col]])) df[[col]] <- sapply(df[[col]], convert)
    }
  }
  if ("SpinEfficiency" %in% names(df)) {
    df$SpinEfficiency <- as.numeric(gsub("%", "", df$SpinEfficiency))
  }
  df
}

addPitcherThrows <- function(df) {
  right_handed <- c("Meyer, Gavin", "Smith, Dylan", "Langley, Aidan", "Hernandez, Chris",
                    "Turner, Derek", "Gurnea, Chad", "Faris, Grady", "Goff, Dylan", "Ritter, Tyler")
  left_handed  <- c("Negrete, Mikiah", "Harper, Jayden", "Wright, Andrew", "Hawkinson, Payton", "Dockan, Brady")
  df$PitcherThrows <- ifelse(df$Pitcher %in% right_handed, "Right",
                             ifelse(df$Pitcher %in% left_handed, "Left", NA))
  df
}

processData <- function(df) df %>% convertHeights() %>% addPitcherThrows()

arm_angle_categories <- function(df) {
  df %>%
    mutate(
      arm_angle_type = case_when(
        arm_angle_savant >= 60                          ~ "Over The Top",
        arm_angle_savant >= 50 & arm_angle_savant < 60  ~ "High Three-Quarters",
        arm_angle_savant >= 40 & arm_angle_savant < 50  ~ "Three-Quarters",
        arm_angle_savant >= 25 & arm_angle_savant < 40  ~ "Low Three-Quarters",
        arm_angle_savant >= 20 & arm_angle_savant < 25  ~ "Slinger",
        arm_angle_savant >=  0 & arm_angle_savant < 20  ~ "Sidearm",
        arm_angle_savant <  0                           ~ "Submarine",
        TRUE ~ NA_character_
      )
    )
}

# -------------------- FIXED ARM ANGLE CALC --------------------
arm_angle_calc <- function(data_frame) {
  normalize_name <- function(x) trimws(gsub("\\s+", " ", as.character(x)))
  normalize_team <- function(x) tolower(gsub("[^a-z]", "", as.character(x)))
  normalize_throws <- function(x) {
    x <- as.character(x); x <- trimws(tolower(x))
    dplyr::case_when(
      x %in% c("r","rh","rhp","right","righty") ~ "Right",
      x %in% c("l","lh","lhp","left","lefty")   ~ "Left",
      TRUE ~ NA_character_
    )
  }
  ht_raw <- D1PitcherHeights %>%
    dplyr::mutate(
      Pitcher   = ifelse(Pitcher == "Allen, Cade Van", "Van Allen, Cade", Pitcher),
      Pitcher   = normalize_name(Pitcher),
      team_norm = normalize_team(PitcherTeam)
    )
  if (!"PitcherThrows" %in% names(ht_raw)) ht_raw$PitcherThrows <- NA_character_
  ht <- ht_raw %>%
    dplyr::transmute(
      Pitcher, team_norm, PitcherHeight,
      PitcherThrows_ht = normalize_throws(PitcherThrows)
    )
  df0 <- data_frame %>%
    dplyr::mutate(
      Pitcher   = normalize_name(Pitcher),
      team_norm = normalize_team(PitcherTeam),
      PitcherThrows = normalize_throws(PitcherThrows)
    )
  df1 <- df0 %>% dplyr::left_join(ht, by = c("Pitcher","team_norm"))
  ht_by_name <- ht %>%
    dplyr::group_by(Pitcher) %>%
    dplyr::summarise(
      PitcherHeight_fallback = dplyr::first(na.omit(PitcherHeight)),
      PitcherThrows_fallback = dplyr::first(na.omit(PitcherThrows_ht)),
      .groups = "drop"
    )
  df2 <- df1 %>%
    dplyr::left_join(ht_by_name, by = "Pitcher") %>%
    dplyr::mutate(
      PitcherHeight = dplyr::coalesce(PitcherHeight, PitcherHeight_fallback),
      PitcherThrows = dplyr::coalesce(PitcherThrows, PitcherThrows_ht, PitcherThrows_fallback),
      PitcherThrows = normalize_throws(PitcherThrows)
    ) %>%
    dplyr::select(-PitcherHeight_fallback, -PitcherThrows_ht, -PitcherThrows_fallback)
  df2 %>%
    dplyr::mutate(
      arm_length   = PitcherHeight * 0.39,
      RelSide_in   = RelSide * 12,
      RelHeight_in = RelHeight * 12,
      shoulder_pos = PitcherHeight * 0.70,
      Adj = RelHeight_in - shoulder_pos,
      Opp = abs(RelSide_in),
      arm_angle_rad = atan2(Opp, Adj),
      arm_angle = arm_angle_rad * (180 / pi)
    ) %>%
    dplyr::select(-Opp, -arm_angle_rad) %>%
    dplyr::mutate(
      arm_angle_180 = dplyr::case_when(
        PitcherThrows == "Left"  ~ 180 - arm_angle,
        PitcherThrows == "Right" ~ 180 + arm_angle,
        TRUE ~ arm_angle
      ),
      arm_angle_savant = dplyr::case_when(
        is.na(arm_angle) ~ NA_real_,
        arm_angle >= 0 & arm_angle <= 90 ~ arm_angle,
        arm_angle > 90 ~ 180 - arm_angle,
        TRUE ~ abs(arm_angle)
      )
    )
}

# -------------------- Build data (CSUF25 only for plotting) ----------------
if (!exists("CSUF25")) stop("CSUF25 not found. Load it before running the app.")
if (!exists("D1PitcherHeights")) stop("D1PitcherHeights not found. Load it before running the app.")
D1PitcherHeights$Pitcher <- ifelse(D1PitcherHeights$Pitcher == "Allen, Cade Van","Van Allen, Cade", D1PitcherHeights$Pitcher)

CSUF25_proc        <- processData(CSUF25)
CSUF25_with_angles <- arm_angle_calc(CSUF25_proc) %>% arm_angle_categories()
AAFall24 <- CSUF25_with_angles

# -------------------- Your plotting functions (unchanged visuals) ----------
pitch_colors <- c(
  "Fastball"="#D22D49","Sinker"="#FE9D00","Cutter"="#933F2C","Slider"="#EEE716",
  "Curveball"="#00D1ED","Splitter"="#3BACAC","ChangeUp"="#1DBE3A","Sweeper"="#DDB33A"
)

movement_plot <- function(PitcherName, Dataset, date_value, ellipse_level = 0.80) {
  pitcher_col <- "Pitcher"; date_col <- "Date"; x_col <- "HorzBreak"; y_col <- "InducedVertBreak"; type_col <- "TaggedPitchType"
  target_date <- as.Date(date_value)
  df <- Dataset %>% mutate(.date_only = as.Date(.data[[date_col]])) %>%
    filter(.data[[pitcher_col]] == PitcherName, .date_only == target_date)
  present_types <- unique(df[[type_col]]); color_vals <- pitch_colors[names(pitch_colors) %in% present_types]
  df_ell <- df %>% group_by(.data[[type_col]]) %>% filter(n() >= 3) %>% ungroup()
  avg_points <- df %>% group_by(.data[[type_col]]) %>%
    summarise(avg_x = mean(.data[[x_col]], na.rm = TRUE),
              avg_y = mean(.data[[y_col]], na.rm = TRUE), .groups = "drop")
  ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_point(aes(fill = .data[[type_col]]), shape = 21, size = 3, alpha = 0.7, color = "black", stroke = 0.3) +
    stat_ellipse(data = df_ell, aes(fill = .data[[type_col]]), level = ellipse_level, type = "norm",
                 alpha = 0.20, geom = "polygon", color = NA) +
    geom_point(data = avg_points, aes(x = avg_x, y = avg_y, fill = .data[[type_col]]),
               inherit.aes = FALSE, shape = 21, size = 5, color = "black", stroke = 0.8) +
    scale_fill_manual(values = color_vals, drop = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    annotate("segment", x = -0.83, xend = 0.83, y = 0, yend = 0, colour = "black", size = 1) +
    coord_equal(xlim = c(-25, 25), ylim = c(-25, 25)) +
    labs(title = "Pitch Movement", x = "Horizontal Break", y = "Induced Vertical Break", fill  = "Pitch Type") +
    theme_minimal(base_size = 14, base_family = "markazi") +
    theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))
}

# ===== Base-R trajectory helper (aspect ratio locked here) =====
.avg_traj_plot_side <- function(player_name, dataset, date_value, side_label,
                                x_limits = c(-9, 9), z_limits = c(-6, 13),
                                tighten_release = FALSE, max_release_dist = 0.25,
                                compress_x = 0.8,
                                batter_image_path = "/Users/a13105/Downloads/BatterPerspective.png") {
  pitch_equation <- function(a, v, p0, t) a * t^2 + v * t + p0
  
  df <- dataset |>
    dplyr::filter(Pitcher == player_name,
                  is.finite(ZoneTime),
                  !is.na(TaggedPitchType),
                  !is.na(BatterSide)) |>
    dplyr::mutate(.date_only = as.Date(Date)) |>
    dplyr::filter(.date_only == as.Date(date_value),
                  BatterSide == side_label)
  if (nrow(df) == 0) {
    plot.new(); title(main = paste("No data for", player_name, date_value, paste0(" (", side_label, " hitters)")))
    return(invisible(NULL))
  }
  
  if (isTRUE(tighten_release)) {
    mean_x0 <- mean(df$x0, na.rm = TRUE); mean_z0 <- mean(df$z0, na.rm = TRUE)
    df <- df |>
      dplyr::mutate(release_dist = sqrt((x0 - mean_x0)^2 + (z0 - mean_z0)^2)) |>
      dplyr::filter(release_dist <= max_release_dist)
    if (nrow(df) == 0) {
      plot.new(); title(main = paste("All rows filtered by release cluster (", side_label, ")", sep = ""))
      return(invisible(NULL))
    }
  }
  
  avg_traj <- df |>
    dplyr::group_by(TaggedPitchType) |>
    dplyr::summarise(
      ax0 = mean(ax0, na.rm = TRUE),
      vx0 = mean(vx0, na.rm = TRUE),
      x0  = mean(x0,  na.rm = TRUE),
      az0 = mean(az0, na.rm = TRUE),
      vz0 = mean(vz0, na.rm = TRUE),
      z0  = mean(z0,  na.rm = TRUE),
      ZoneTime = mean(ZoneTime, na.rm = TRUE),
      .groups = "drop"
    )
  if (nrow(avg_traj) == 0) {
    plot.new(); title(main = paste("No pitch types after averaging (", side_label, ")", sep = ""))
    return(invisible(NULL))
  }
  
  op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
  par(mar = c(0, 0, 4.5, 0), family = "markazi")
  plot(NA, xlim = x_limits, ylim = z_limits, xlab = "", ylab = "",
       main = paste0("Avg Trajectory by Pitch Type vs. ", side_label, " Handed Hitters"),
       cex.main = 1.6, axes = FALSE)
  mtext("One mean trajectory per pitch type (colored). End markers at plate.",
        side = 3, line = 0.25, cex = 0.45)
  
  rect(-2.5, -3.5, 2.5, 7, border = "black", lwd = 1.5, lty = 2)
  
  xleft <- -0.75; xright <- 0.15; ybottom <- 2.75; ytop <- 2.70
  center_x <- (xleft + xright)/2; center_y <- (ybottom + ytop)/2
  angle_deg <- if (side_label == "Right") 359 else 1; angle_rad <- angle_deg * pi / 180
  corners <- data.frame(x = c(xleft,xright,xright,xleft), y = c(ybottom,ybottom,ytop,ytop))
  rotated_corners <- corners |>
    dplyr::mutate(
      x_rot = center_x + (x - center_x) * cos(angle_rad) - (y - center_y) * sin(angle_rad),
      y_rot = center_y + (x - center_x) * sin(angle_rad) + (y - center_y) * cos(angle_rad)
    )
  for (j in 1:4) {
    x1 <- rotated_corners$x_rot[j]; y1 <- rotated_corners$y_rot[j]
    x2 <- rotated_corners$x_rot[ifelse(j == 4, 1, j + 1)]
    y2 <- rotated_corners$y_rot[ifelse(j == 4, 1, j + 1)]
    lines(c(x1, x2), c(y1, y2), col = "black", lwd = 1)
  }
  x1_top <- rotated_corners$x_rot[3]; y1_top <- rotated_corners$y_rot[3]
  x2_top <- rotated_corners$x_rot[4]; y2_top <- rotated_corners$y_rot[4]
  dir_x <- x2_top - x1_top; dir_y <- y2_top - y1_top
  dir_length <- sqrt(dir_x^2 + dir_y^2)
  norm_dir_x <- dir_x / dir_length; norm_dir_y <- dir_y / dir_length
  extension <- 0.2; y_offset <- 0.19
  extended_x1 <- x1_top - (extension * norm_dir_x)
  extended_y1 <- y1_top - (extension * norm_dir_y) + y_offset
  extended_x2 <- x2_top + (extension * norm_dir_x)
  extended_y2 <- y2_top + (extension * norm_dir_y) + y_offset
  lines(c(extended_x1, extended_x2), c(extended_y1, extended_y2), col = "black", lwd = 1)
  angle_15_rad <- 194 * (pi / 180)
  edge_angle <- atan2(dir_y, dir_x)
  new_angle_right <- edge_angle - angle_15_rad
  angled_x2 <- extended_x1 + cos(new_angle_right)
  angled_y2 <- extended_y1 + sin(new_angle_right)
  lines(c(extended_x1, angled_x2), c(extended_y1, angled_y2), col = "black", lwd = 1)
  second_angle_deg <- 345
  angle_left_rad <- second_angle_deg * (pi / 180)
  new_angle_left <- edge_angle - angle_left_rad
  angled_x2_left <- extended_x2 + cos(new_angle_left)
  angled_y2_left <- extended_y2 + sin(new_angle_left)
  lines(c(extended_x2, angled_x2_left), c(extended_y2, angled_y2_left), col = "black", lwd = 1)
  pt1 <- c(angled_x2, angled_y2); pt2 <- c(angled_x2_left, angled_y2_left)
  oval_center_x <- (pt1[1] + pt2[1]) / 2; oval_center_y <- (pt1[2] + pt2[2]) / 2
  oval_width <- sqrt((pt2[1] - pt1[1])^2 + (pt2[2] - pt1[2])^2); oval_height <- 1
  oval_angle_rad <- if (side_label == "Right") 359 * pi / 180 else 1 * pi / 180
  theta <- seq(pi, 2 * pi, length.out = 200)
  a <- oval_width / 2; b <- oval_height / 2
  x_vals <- oval_center_x + a * cos(theta) * cos(oval_angle_rad) - b * sin(theta) * sin(oval_angle_rad)
  y_vals <- oval_center_y + a * cos(theta) * sin(oval_angle_rad) + b * sin(theta) * cos(oval_angle_rad)
  lines(x_vals, y_vals, col = "black", lwd = 1)
  
  # Plot trajectories (unchanged)
  for (i in seq_len(nrow(avg_traj))) {
    row <- avg_traj[i, ]
    col <- c(
      "Fastball"="#D22D49","Sinker"="#FE9D00","Cutter"="#933F2C","Slider"="#EEE716",
      "Curveball"="#00D1ED","Splitter"="#3BACAC","ChangeUp"="#1DBE3A","Sweeper"="#DDB33A"
    )[[as.character(row$TaggedPitchType)]]
    if (is.na(col)) col <- "#444444"
    t_seq <- seq(0, row$ZoneTime, length.out = 200)
    x_traj <- pitch_equation(row$ax0, row$vx0, row$x0, t_seq) * compress_x
    z_traj <- pitch_equation(row$az0, row$vz0, row$z0, t_seq)
    col_rgb <- col2rgb(col) / 255
    lines(x_traj, z_traj, col = rgb(col_rgb[1], col_rgb[2], col_rgb[3], alpha = 0.9), lwd = 2)
    points(tail(x_traj, 1), tail(z_traj, 1), pch = 21, bg = col, col = "black", cex = 1.1, lwd = 0.7)
  }
  
  # IMPROVED BATTER IMAGE POSITIONING
  if (file.exists(batter_image_path)) {
    batter_img <- tryCatch(png::readPNG(batter_image_path), error = function(e) NULL)
    if (!is.null(batter_img)) {
      batter_stands <- ifelse(side_label == "Right", "Right", "Left")
      if (batter_stands == "Right") {
        xleft_adj  <- -8.5; xright_adj <- -3.5; ybottom_adj <- -13; ytop_adj <- 20
        img_use <- batter_img[, ncol(batter_img):1, ]
      } else {
        xleft_adj  <- 3.5; xright_adj <- 8.5; ybottom_adj <- -13; ytop_adj <- 20
        img_use <- batter_img
      }
      rasterImage(img_use, xleft_adj, ybottom_adj, xright_adj, ytop_adj, interpolate = FALSE)
    }
  }
  
  # Legend (unchanged)
  used_types <- as.character(avg_traj$TaggedPitchType)
  used_cols  <- c(
    "Fastball"="#D22D49","Sinker"="#FE9D00","Cutter"="#933F2C","Slider"="#EEE716",
    "Curveball"="#00D1ED","Splitter"="#3BACAC","ChangeUp"="#1DBE3A","Sweeper"="#DDB33A"
  )[used_types]
  used_cols[is.na(used_cols)] <- "#444444"
  legend("topright", legend = used_types, pch = 15, pt.cex = 1, col = used_cols,
         bty = "n", cex = 0.7, title = "Pitch Type")
}

# Need to define the wrapper functions
trajectories_avg_by_type_RHH <- function(player_name, dataset, date_value) {
  .avg_traj_plot_side(player_name, dataset, date_value, "Right")
}
trajectories_avg_by_type_LHH <- function(player_name, dataset, date_value) {
  .avg_traj_plot_side(player_name, dataset, date_value, "Left")
}

# -------------------- Strike Zone (original ggplot) --------------------
.sz_plot <- function(PitcherName, Dataset, date_value, batter_side) {
  pitch_colors <- c("Fastball"="#D22D49","Sinker"="#FE9D00","Cutter"="#933F2C","Slider"="#EEE716",
                    "Curveball"="#00D1ED","Splitter"="#3BACAC","ChangeUp"="#1DBE3A")
  pitcher_col <- "Pitcher"; date_col <- "Date"; x_col <- "PlateLocSide"; y_col <- "PlateLocHeight"
  type_col <- "TaggedPitchType"; batter_col <- "BatterSide"
  zx_min <- -0.83; zx_max <- 0.83; zy_min <- 1.60; zy_max <- 3.50
  x1 <- zx_min + (zx_max - zx_min)/3; x2 <- zx_min + 2*(zx_max - zx_min)/3
  y1 <- zy_min + (zy_max - zy_min)/3; y2 <- zy_min + 2*(zy_max - zy_min)/3
  heart_xmin <- -0.56; heart_xmax <- 0.56; heart_ymin <- 1.83; heart_ymax <- 3.17
  shadow_xmin <- -1.11; shadow_xmax <- 1.11; shadow_ymin <- 1.17; shadow_ymax <- 3.83
  xlim_low <- -3; xlim_high <- 3; ylim_low <- -0.5; ylim_high <- 5.5
  target_date <- as.Date(date_value)
  df <- Dataset %>% mutate(.date_only = as.Date(.data[[date_col]])) %>%
    filter(.data[[pitcher_col]] == PitcherName, .date_only == target_date, .data[[batter_col]] == batter_side)
  present_types <- unique(df[[type_col]]); color_vals <- pitch_colors[names(pitch_colors) %in% present_types]
  ggplot(df, aes(x = .data[[x_col]], y = .data[[y_col]])) +
    geom_rect(xmin = zx_min, xmax = zx_max, ymin = zy_min, ymax = zy_max, fill = NA, color = "black", linewidth = 1) +
    geom_segment(x = x1, xend = x1, y = zy_min, yend = zy_max, color = "grey60", linewidth = 0.4) +
    geom_segment(x = x2, xend = x2, y = zy_min, yend = zy_max, color = "grey60", linewidth = 0.4) +
    geom_segment(y = y1, yend = y1, x = zx_min, xend = zx_max, color = "grey60", linewidth = 0.4) +
    geom_segment(y = y2, yend = y2, x = zx_min, xend = zx_max, color = "grey60", linewidth = 0.4) +
    geom_rect(xmin = shadow_xmin, xmax = shadow_xmax, ymin = shadow_ymin, ymax = shadow_ymax,
              fill = NA, color = "grey60", linetype = "dotted", linewidth = 0.4) +
    geom_rect(xmin = heart_xmin, xmax = heart_xmax, ymin = heart_ymin, ymax = heart_ymax,
              fill = NA, color = "grey60", linetype = "dotted", linewidth = 0.4) +
    annotate("segment", x = -0.83, xend = 0.83, y = 0,   yend = 0,   colour = "black", size = 0.6) +
    annotate("segment", x = -0.83, xend = -0.83, y = 0,  yend = 0.3, colour = "black", size = 0.6) +
    annotate("segment", x =  0.83, xend =  0.83, y = 0,  yend = 0.3, colour = "black", size = 0.6) +
    annotate("segment", x =  0.83, xend =  0,    y = 0.3,yend = 0.5, colour = "black", size = 0.6) +
    annotate("segment", x = -0.83, xend =  0,    y = 0.3,yend = 0.5, colour = "black", size = 0.6) +
    geom_point(aes(fill = .data[[type_col]]), shape = 21, size = 2, alpha = 1, color = "black", stroke = 0.8) +
    scale_fill_manual(values = color_vals, drop = TRUE, name = "Pitch Type") +
    coord_equal(xlim = c(xlim_low, xlim_high), ylim = c(ylim_low, ylim_high)) +
    labs(title = paste0("Total Pitches vs. ", batter_side, " Handed Hitters"), x = NULL, y = NULL, fill = "Pitch Type") +
    theme_minimal(base_size = 14, base_family = "markazi") +
    theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
          panel.grid = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
          axis.text = element_text(size = 10))
}
sz_RHH <- function(PitcherName, Dataset, date_value) .sz_plot(PitcherName, Dataset, date_value, batter_side = "Right")
sz_LHH <- function(PitcherName, Dataset, date_value) .sz_plot(PitcherName, Dataset, date_value, batter_side = "Left")

# Arm Angle plot helpers
circleFun <- function(center = c(0, 0), radius = 24, npoints = 100) {
  tt <- seq(0, 2 * pi, length.out = npoints)
  data.frame(x = center[1] + radius * cos(tt), y = center[2] + radius * sin(tt))
}
circle <- circleFun(center = c(0, 0), radius = 24)
mound <- { theta <- seq(0, pi, length.out = 100); r <- 40; data.frame(x = r * cos(theta), y = 4 * sin(theta)) }

# Fixed pitcher_plot_arm_angle function with corrected geometry
pitcher_plot_arm_angle <- function(df, name, date) {
  date_only <- as.Date(date)
  filtered_data <- df %>%
    mutate(.date_only = as.Date(Date)) %>%
    filter(Pitcher == name, .date_only == date_only)
  if (nrow(filtered_data) == 0) stop("No data found for the specified pitcher and date.")
  
  filtered_data <- filtered_data %>%
    mutate(
      arm_length = PitcherHeight * 0.39,
      RelSide_in = RelSide * 12,
      RelHeight_in = RelHeight * 12,
      shoulder_pos = PitcherHeight * 0.70,
      horizontal_offset = abs(RelSide_in),
      vertical_offset = pmax(RelHeight_in - shoulder_pos, 0.1),
      arm_angle_rad = atan2(horizontal_offset, vertical_offset),
      arm_angle_degrees = arm_angle_rad * (180 / pi),
      arm_angle_savant = pmin(pmax(90 - arm_angle_degrees, 0), 90),
      arm_angle_type = case_when(
        arm_angle_savant >= 75 ~ "Over The Top",
        arm_angle_savant >= 60 ~ "High Three-Quarters", 
        arm_angle_savant >= 45 ~ "Three-Quarters",
        arm_angle_savant >= 30 ~ "Low Three-Quarters",
        arm_angle_savant >= 15 ~ "Slinger",
        arm_angle_savant >= 0 ~ "Sidearm",
        TRUE ~ "Submarine"
      )
    )
  
  median_arm_angle_savant <- median(filtered_data$arm_angle_savant, na.rm = TRUE)
  cat("Pitcher:", name, "\n")
  cat("Raw arm angles (savant):", round(filtered_data$arm_angle_savant, 1), "\n")
  cat("Median arm angle (savant):", round(median_arm_angle_savant, 2), "\n")
  
  summary_data <- filtered_data %>%
    group_by(TaggedPitchType) %>%
    summarise(
      PitcherHeight = median(PitcherHeight, na.rm = TRUE),
      shoulder_pos = median(shoulder_pos, na.rm = TRUE),
      release_pos_x = median(RelSide * 12, na.rm = TRUE),
      release_pos_z = median(RelHeight * 12, na.rm = TRUE),
      arm_angle_savant = median(arm_angle_savant, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      relx = case_when(
        release_pos_x > 20 ~ 20,
        release_pos_z > 20 ~ 20 * (release_pos_x / (release_pos_z - shoulder_pos)),
        TRUE ~ release_pos_x
      ),
      relz = case_when(
        release_pos_x > 20 ~ 20 * ((release_pos_z - shoulder_pos) / release_pos_x),
        release_pos_z > 20 ~ 20,
        TRUE ~ release_pos_z
      ),
      arm_length = PitcherHeight * .39,
      arm_dist = sqrt((release_pos_x - 0)^2 + (release_pos_z - shoulder_pos)^2),
      arm_scale = arm_length / arm_dist,
      should_x = 0,
      should_y = case_when(
        median_arm_angle_savant >= 60 ~ 62.5,
        median_arm_angle_savant >= 15 ~ 56,
        TRUE ~ 45
      ),
      rel_x = should_x + (arm_scale * (release_pos_x - should_x)),
      rel_z = shoulder_pos + (arm_scale * (release_pos_z - shoulder_pos)) + should_y - (shoulder_pos)
    )
  
  arm_angle_type_display <- case_when(
    median_arm_angle_savant >= 75 ~ "Over The Top",
    median_arm_angle_savant >= 60 ~ "High Three-Quarters",
    median_arm_angle_savant >= 45 ~ "Three-Quarters", 
    median_arm_angle_savant >= 30 ~ "Low Three-Quarters",
    median_arm_angle_savant >= 15 ~ "Slinger",
    median_arm_angle_savant >= 0 ~ "Sidearm",
    TRUE ~ "Submarine"
  )
  
  PitcherThrows <- first(filtered_data$PitcherThrows)
  slot_bucket <- case_when(
    median_arm_angle_savant >= 60 ~ "top",
    median_arm_angle_savant >= 15 ~ "mid",
    TRUE ~ "low"
  )
  cat("Arm angle type:", arm_angle_type_display, "\n")
  cat("Slot bucket:", slot_bucket, "\n")
  cat("Pitcher throws:", PitcherThrows, "\n")
  
  image_path <- switch(
    paste0(slot_bucket, "_", PitcherThrows),
    "top_Right" = "SavantPitchers_top_right_front-svg.png",
    "mid_Right" = "SavantPitchers_mid_right_front.png", 
    "low_Right" = "SavantPitchers_low_right_front-svg.png",
    "top_Left"  = "SavantPitchers_top_left_front-svg.png",
    "mid_Left"  = "ArmAngleLeft.png",
    "low_Left"  = "SavantPitchers_low_left_front-svg.png",
    "SavantPitchers_mid_right_front.png"
  )
  cat("Selected image path:", image_path, "\n")
  cat("Current working directory:", getwd(), "\n")
  cat("Image file exists:", file.exists(image_path), "\n")
  
  possible_paths <- c(
    image_path,
    file.path("www", image_path),
    file.path("images", image_path),
    file.path(".", image_path),
    file.path("/Users/a13105/Documents/R Projects/Scripts/Pitcher Reports 2026/www", image_path),
    file.path("Documents/R Projects/Scripts/Pitcher Reports 2026/www", image_path)
  )
  image_found <- FALSE
  final_image_path <- image_path
  for (path in possible_paths) {
    if (file.exists(path)) { final_image_path <- path; image_found <- TRUE; cat("Found image at:", final_image_path, "\n"); break }
  }
  if (!image_found) {
    cat("WARNING: Could not find image file. Checked paths:\n")
    for (path in possible_paths) cat("  -", path, "(exists:", file.exists(path), ")\n")
  }
  cat("\n")
  
  pitcher_image <- NULL
  if (image_found) {
    pitcher_image <- tryCatch({
      img_data <- readPNG(final_image_path)
      rasterGrob(img_data, interpolate = TRUE)
    }, error = function(e) { cat("Error loading image:", e$message, "\n"); NULL })
  }
  
  ggplot() +
    geom_polygon(data = mound, aes(x = x, y = y), fill = "#8B4513") +
    coord_equal(xlim = c(-50, 50), ylim = c(0, 100)) +
    geom_rect(aes(xmin = -9, xmax = 9, ymin = 4, ymax = 4.5),
              fill = "white", color = "black") +
    { if (!is.null(pitcher_image))
      annotation_custom(pitcher_image, xmin = -50, xmax = 50, ymin = 0, ymax = 100) } +
    geom_segment(
      data = summary_data,
      aes(x = 0,
          y = shoulder_pos + (6 + should_y - shoulder_pos),
          xend = -rel_x,
          yend = rel_z,
          color = TaggedPitchType),
      size = 5, alpha = 0.7
    ) +
    geom_point(
      data = summary_data,
      aes(x = -rel_x, y = rel_z, color = "black", fill = TaggedPitchType),
      pch = 21, size = 3.2, stroke = 1.5
    ) +
    scale_color_manual(values = pitch_colors, guide = "none") +
    scale_fill_manual(values = pitch_colors, name = "Pitch Type") +
    guides(fill = guide_legend(override.aes = list(size = 3.2))) +
    labs(
      title = "Hitter's Perspective", x = "", y = "",
      subtitle = paste0('Arm Angle: ', round(median_arm_angle_savant), "° - ", arm_angle_type_display)
    ) +
    theme_void(base_size = 14, base_family = "markazi") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
      axis.title = element_text(face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    )
}

# ==================== One-row (wide) 6-metric summary helper ====================
compute_pitch_summary <- function(dataset, pitcher, date_value) {
  date_only <- as.Date(date_value)
  df <- dataset %>% mutate(.date_only = as.Date(Date)) %>%
    filter(Pitcher == pitcher, .date_only == date_only)
  total_pitches <- nrow(df)
  in_zone <- !is.na(df$PlateLocSide) & !is.na(df$PlateLocHeight) &
    df$PlateLocSide >= -0.83 & df$PlateLocSide <= 0.83 &
    df$PlateLocHeight >= 1.5  & df$PlateLocHeight <= 3.6
  outside_zone <- !is.na(df$PlateLocSide) & !is.na(df$PlateLocHeight) & !in_zone
  n_in_zone <- sum(in_zone, na.rm = TRUE)
  n_whiff   <- sum(df$PitchCall == "StrikeSwinging", na.rm = TRUE)
  n_csw     <- sum(df$PitchCall %in% c("StrikeCalled","StrikeSwinging"), na.rm = TRUE)
  n_outside <- sum(outside_zone, na.rm = TRUE)
  n_chase   <- sum((df$PitchCall == "StrikeSwinging") & outside_zone, na.rm = TRUE)
  first_pitch <- !is.na(df$PitchofPA) & suppressWarnings(as.numeric(df$PitchofPA) == 1)
  fps_denom   <- sum(first_pitch, na.rm = TRUE)
  fps_in_zone <- sum(first_pitch & in_zone, na.rm = TRUE)
  pct <- function(n, d) ifelse(d > 0, n / d, NA_real_)
  fmt_pct <- function(x) ifelse(is.na(x), "—", sprintf("%.1f%%", 100 * x))
  data.frame(
    Pitches = format(total_pitches, big.mark = ","),
    `Strike%` = fmt_pct(pct(n_in_zone, total_pitches)),
    `Whiff%`  = fmt_pct(pct(n_whiff, total_pitches)),
    `CSW%`    = fmt_pct(pct(n_csw, total_pitches)),
    `Chase%`  = fmt_pct(pct(n_chase, n_outside)),
    `FPS%`    = fmt_pct(pct(fps_in_zone, fps_denom)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# -------------------- SHINY UI (search pitchers, CSUF25 only) --------------
pitchers_all <- sort(unique(as.character(CSUF25$Pitcher)))
dates_for <- function(p) sort(unique(as.Date(CSUF25$Date[CSUF25$Pitcher == p])))

ui <- fluidPage(
  titlePanel("CSUF Pitch Visuals — Original Plot Geometry"),
  # ===== compact CSS & width control for the summary table =====
  tags$head(
    tags$style(HTML("
      .summary-table { margin-top: 6px; }
      .summary-table table { width: 100%; table-layout: fixed; border-collapse: collapse; }
      .summary-table th, .summary-table td { text-align: center; padding: 6px 8px; }
    "))
  ),
  sidebarLayout(
    sidebarPanel(width = 3,
                 selectizeInput("pitcher", "Pitcher", choices = pitchers_all,
                                options = list(placeholder = 'Search pitcher...')),
                 uiOutput("date_ui")
    ),
    mainPanel(width = 9,
              fluidRow(
                column(4, plotOutput("p_sz_rhh")),
                column(4, plotOutput("p_movement")),
                column(4, plotOutput("p_sz_lhh"))
              ),
              fluidRow(
                column(4, plotOutput("p_traj_rhh")),
                column(4, plotOutput("p_arm_angle")),
                column(4, plotOutput("p_traj_lhh"))
              ),
              # ===== summary table placed immediately after plots; full width of main panel =====
              fluidRow(
                column(12, div(class = "summary-table", tableOutput("p_summary")))
              )
    )
  )
)

server <- function(input, output, session) {
  showtext_auto(TRUE)
  output$date_ui <- renderUI({
    req(input$pitcher)
    ds <- dates_for(input$pitcher)
    selectInput("date", "Date", choices = as.character(ds),
                selected = if (length(ds)) as.character(max(ds)) else NULL)
  })
  # one-row table, labeled columns, centered cells
  output$p_summary <- renderTable({
    req(input$pitcher, input$date)
    compute_pitch_summary(CSUF25, input$pitcher, input$date)
  }, rownames = FALSE, colnames = TRUE, align = "c")
  # Top row (unchanged)
  output$p_sz_rhh   <- renderPlot({ req(input$pitcher, input$date); sz_RHH(input$pitcher, CSUF25, input$date) })
  output$p_movement <- renderPlot({ req(input$pitcher, input$date); movement_plot(input$pitcher, CSUF25, input$date) })
  output$p_sz_lhh   <- renderPlot({ req(input$pitcher, input$date); sz_LHH(input$pitcher, CSUF25, input$date) })
  # Bottom row — set device height to match your RStudio pane ratio
  output$p_traj_rhh <- renderPlot(
    { req(input$pitcher, input$date); trajectories_avg_by_type_RHH(input$pitcher, CSUF25, input$date) },
    height = function() session$clientData$output_p_traj_rhh_width / PANE_AR
  )
  output$p_arm_angle <- renderPlot(
    { req(input$pitcher, input$date); pitcher_plot_arm_angle(AAFall24, input$pitcher, input$date) },
    height = function() 1.15 * session$clientData$output_p_arm_angle_width / PANE_AR
  )
  output$p_traj_lhh <- renderPlot(
    { req(input$pitcher, input$date); trajectories_avg_by_type_LHH(input$pitcher, CSUF25, input$date) },
    height = function() session$clientData$output_p_traj_lhh_width / PANE_AR
  )
}

shinyApp(ui, server)
