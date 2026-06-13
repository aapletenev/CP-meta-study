#####Functions###########
plot_data = function(data, x, y, group = "Brain.areas", REGRESSION_ALL = TRUE, REGRESSION_GROUP = TRUE, SHOW_LEG_GROUP= FALSE, CONNECT_PER_PARER = FALSE, CONNECT_REGRESSION = TRUE, group_connect = c(Papers, Brain.areas),
                     LABELS = FALSE, BOXPLOT = FALSE, ADD_MEAN = FALSE, ADD_MEDIAN = TRUE, MEAN_NUDGE = FALSE, STAT_COMPARISON = FALSE, comparisons = NULL, PAIRED = FALSE, legend_position = "none", shape = NULL,
                     jitter = 0.5, cex_jitter = 2, point_size = 2.5, mean_size = 1.2, stroke = 0.2,yline = 0.5, xline = NULL, width_error = NULL, xlab = "", ylab = "mean CP", angle = 35, hjust = 1, vjust = NULL,
                     alpha_points = 0.5, regression_width = 1.5, SE_REG_GROUP = FALSE, color_reg_group =  NULL, line_type = "solid", test_cut = 0.05, title = NULL, left_margin = 10, bottom_margin = 10, right_margin = 10, ylim = NULL,
                     color_legend = "none", font_scale = 1) {

  # Ensure scales package is available for alpha()
  require(scales)

  plot = data %>%
    # --- CHANGE 1: Added shape = Brain.areas to the main aesthetic ---
    ggplot(aes(x = !!sym(x), y = !!sym(y), color = Brain.areas, group = !!sym(group), fill = Brain.areas, shape = Brain.areas)) +

    scale_color_manual(values = color_brain,
                       breaks = levels(data$Brain.areas),
                       guide = guide_legend(title = "Brain areas")) +

    scale_fill_manual(values = scales::alpha(color_brain, alpha_points),
                      breaks = levels(data$Brain.areas),
                      guide = guide_legend(title = "Brain areas")) +

    # --- CHANGE 2: Added scale_shape_manual linked to shape_brain ---
    scale_shape_manual(values = shape_brain,
                       breaks = levels(data$Brain.areas),
                       guide = guide_legend(title = "Brain areas")) +

    geom_hline(yintercept = yline, linetype = "dashed") +
    geom_vline(xintercept = xline, linetype = "dashed") +
    theme_classic() +
    labs(x = xlab, y = ylab, title = title) +
    theme(
      text = element_text(size = grid::unit(font_scale*font_labels, "pt")),
      axis.text = element_text(size = grid::unit(font_scale*font_axis, "pt")),
      legend.text = element_text(size = grid::unit(font_scale*font_legend, "pt")),
      legend.title = element_text(size = grid::unit(font_scale*font_legend, "pt")),
      axis.text.x = element_text(angle = angle, hjust = hjust, vjust = vjust),
      plot.margin = margin(10, right_margin, bottom_margin, left_margin),
      legend.position = legend_position,
      plot.title = element_text(size = grid::unit(font_scale*font_labels, "pt"))
    )

  if (!is.null(ylim)) plot = plot + ylim(ylim)

  if (BOXPLOT) plot = plot +  geom_boxplot(aes(group = !!sym(x)), outlier.shape = NA, coef = 0)

  if (REGRESSION_GROUP)  {
    areas_include = data %>% pull(Brain.areas) %>% unique()
    if (group == "Brain.areas") areas_include <- data %>%
        filter(!is.na(!!sym(x))) %>%
        count(Brain.areas) %>%
        filter(n > 6) %>%
        pull(Brain.areas)

    if (is.null(color_reg_group)) {
      plot = plot + geom_smooth(data = data %>% filter(Brain.areas %in% areas_include), method = "lm", se = SE_REG_GROUP, size = 0.7, show.legend = SHOW_LEG_GROUP)
    } else {
      plot = plot +
        geom_smooth(aes(linetype = !!sym(group)), data = data %>% filter(Brain.areas %in% areas_include), color = color_reg_group, method = "lm", se = SE_REG_GROUP, size = 0.7, show.legend = SHOW_LEG_GROUP) +
        scale_linetype_manual(values = line_type)
    }
  }

  if (REGRESSION_ALL)  plot = plot +  geom_smooth(aes(group = NA), method = "lm", color = "black", size = regression_width, show.legend = FALSE)

  if (CONNECT_PER_PARER) {
    if (x == "Brain.areas") {
      plot = plot + geom_line(aes(group = paste(Papers, Monkey, Epoch), color = "grey"))
    } else if(CONNECT_REGRESSION) {
      plot = plot + geom_smooth(aes(group = paste(!!!syms(group_connect))), method = "lm", se = FALSE, size = 0.7)
    } else {
      plot = plot + geom_line(aes(group = paste(!!!syms(group_connect))))
    }
  }

  if (LABELS) {
    plot = plot + geom_text(aes(label = symbol), position = position_beeswarm(cex =  cex_jitter), size = font_labels)
  } else {
    if (is.null(shape)) {
      # --- CHANGE 3: Removed hardcoded `shape = 21` so it inherits Brain.areas mapping ---
      plot = plot + geom_quasirandom(width = jitter, size = point_size, stroke = stroke)  + guides(color = color_legend)
    } else {
      # Keeps your custom shape override functionality intact if needed
      plot = plot +
        geom_quasirandom(aes(shape = !!sym(group)), width = jitter, size = point_size, stroke = stroke, alpha = alpha_points)  + guides(color = color_legend) +
        scale_shape_manual(values = shape)
    }
  }

  position_mean = "identity"
  if(MEAN_NUDGE) position_mean = position_nudge(x = c(-0.1, 0.1))

  if (ADD_MEAN) {
    plot = plot +
      stat_summary(aes(group = !!sym(x)), fun = mean, geom = "point", size = mean_size * point_size, shape = 23, stroke = 1.5*stroke, color = "black", fill = NA, position = position_mean) +
      stat_summary(aes(group = !!sym(x)), fun.data = mean_se, geom = "errorbar", width = ifelse(is.null(width_error), jitter/3, width_error), color = "black", position = position_mean)
  }

  if (ADD_MEDIAN) {
    plot = plot +
      stat_summary(aes(group = !!sym(x)), fun =  function(x) {if(length(x) > 5) c(y = median(x, na.rm = TRUE)) else  c(y = NA)}, geom = "point", size = mean_size  * point_size, shape = 23, stroke = 1.5*stroke, color = "black", fill = "white", position = position_mean) +
      stat_summary(aes(group = !!sym(x)), fun.data = function(x) {
        q <- quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
        if (length(x) > 5)  data.frame(y = q[2], ymin = q[1], ymax = q[3]) else data.frame(y = NA, ymin = NA, ymax = NA)},
        geom = "errorbar", width = ifelse(is.null(width_error), jitter/3, width_error), color = "black", position = position_mean)
  }

  if(STAT_COMPARISON) {
    if(is.null(comparisons)) {
      levels = data.frame (Area1 = levels(data[[x]]), Area2 = levels(data[[x]]), L1 = 1: length(levels(data[[x]])), L2 = 1: length(levels(data[[x]])))

      comparisons = test_pairs(data, y = y,var = x) %>%
        filter(p < test_cut) %>%
        mutate(Area1 = Area1 %>% as.character(),
               Area2 = Area2 %>% as.character()) %>%
        left_join(levels %>% dplyr::select(Area1, L1)) %>%
        left_join(levels %>% dplyr::select(Area2, L2)) %>%
        mutate(L_min = pmin(L1,L2), L_max =  pmax(L1,L2)) %>%
        arrange(L_min, L_max) %>%
        dplyr::select(Area1, Area2) %>%
        split(1:nrow(.)) %>%
        map(~unlist(.))
    }
    plot = plot + stat_compare_means(comparisons = comparisons, method = "wilcox.test", method.args = list(paired = PAIRED), label = "p.signif", tip.length = 0.01, vjust = 0.7,
                                     symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, Inf), symbols = c("***", "**", "*", "ns")), step.increase = 0.05)
  }

  return(plot)
}

plot_legend = function(data, legend_position = "right", alpha_points = 0.5, ADD_LINES = TRUE) {

  plot = ggplot(data, aes(x = Year, y = CP, color = Brain.areas, fill = Brain.areas, shape = Brain.areas)) +
    # --- CHANGE 1: Removed hardcoded shape = 21 ---
    geom_point(size = 2.5, stroke = 0.2) +
    theme_classic() +

    # --- CHANGE 2: Added identical guides to ensure legends merge ---
    scale_color_manual(values = color_brain,
                       guide = guide_legend(title = "Brain areas")) +
    scale_fill_manual(values = scales::alpha(color_brain, alpha_points),
                      guide = guide_legend(title = "Brain areas")) +

    # --- CHANGE 3: Added scale_shape_manual ---
    scale_shape_manual(values = shape_brain,
                       guide = guide_legend(title = "Brain areas")) +

    theme(
      legend.position = legend_position,
      legend.text = element_text(size = grid::unit(font_legend, "pt")),
      legend.title = element_text(size = grid::unit(font_legend, "pt"))
    )

    if (ADD_LINES) plot = plot + geom_smooth(method = "lm", se = FALSE, aes(color = Brain.areas))

  return(plot)
}

#delete all spaces and ~
delete_spaces = function(x) {
 x %>% gsub(pattern = " ", replacement = "") %>% gsub(pattern = "~", replacement = "")
}

###angle between target and the stimulus
get_inside_bracket = function(x) {
  x = x %>% str_split("\\(", simplify = TRUE) %>% .[,2] %>% str_split("\\)", simplify = TRUE)  %>% .[,1]
  x = ifelse(grepl(",",x, fixed = TRUE), x, NA)
}

before_bracket = function(x) {
  x %>% str_split("\\(", simplify = TRUE) %>% .[,1] %>% as.numeric()
}

get_eccentricity = function(x) {
  x %<>% delete_spaces()
  #two options 1) needed value(range); 2) (x,y)
  x1 = x %>% before_bracket()
  x2 = x %>% get_inside_bracket()
  #calculate excentricity of x2 using "x,y"
  x2 = x2 %>% str_split(",", simplify = TRUE) %>% apply(2, as.numeric) %>% data.frame() %>% mutate(ecc = sqrt(X1^2 + X2^2)) %>% pull(ecc)
  #now combine the two in one vector where the !NA value is taken
  x = ifelse(!is.na(x1), x1, x2)
  return(x)
}

test_pairs = function(data, var = "Brain.areas" , y = "CP") {
  vars = data %>% pull(var) %>% unique()
  test_btw_vars = data.frame()
    for (i in 1:(length(vars) - 1)) {
        CP1 = data %>% filter(!!sym(var) == vars[i]) %>% pull(y)
        for (j in (i+1):length(vars)) {
        CP2 = data %>% filter(!!sym(var) == vars[j]) %>% pull(y)
        test_btw_vars %<>% rbind(data.frame(Area1 = vars[i], Area2 = vars[j], p = suppressWarnings(wilcox.test(CP1, CP2) %>% .$p.value %>% round(4))))
        }
    }
    return(test_btw_vars)
}

#as Grand.CP is the most common method, we will use it whenever possible
filter_best_method = function(data) {
  data %>%
    arrange(method) %>%
    group_by(Papers, Monkey, Brain.areas, Epoch) %>%
    slice(1) %>%
    ungroup()
}

safe_split_num <- function(x, part = 1, sep = "-") {
  # Returns a numeric vector or NA if malformed
  split <- str_split(x, sep, simplify = TRUE)
  if (ncol(split) < part) return(rep(NA_real_, nrow(split)))
  suppressWarnings(as.numeric(split[, part]))
}

split_find_significance <- function(data, var = "CP", DEBUG = FALSE, baseline = 0.5, CI_sep = "-") {
  # Dynamically define column names based on var
  sign_col <- if (var == "CP") "Sign" else paste0("Sign_", var)
  sign_method_col <- if (var == "CP") "Sign_method" else paste0("Sign_method_", var)
  SIGN_col <- if (var == "CP") "SIGN" else paste0("SIGN_", var)

  # Remove spaces and split value from significance
  data %<>%
    mutate(!!sym(var) := gsub(" ", "", !!sym(var))) %>%
    separate(!!sym(var),
             into = c(var, sign_col),
             sep = "\\(",
             extra = "merge",
             fill = "right") %>%
    mutate(
      !!sym(sign_col) := gsub(")", "", .data[[sign_col]], fixed = TRUE),
      !!sym(var) := as.numeric(.data[[var]])
    )

  if (DEBUG) {
    print("After splitting significance:")
    View(data)
  }

  # Determine significance method
  data %<>%
    mutate(
      !!sym(sign_method_col) := case_when(
        is.na(.data[[sign_col]]) ~ NA_character_,
        grepl("p", .data[[sign_col]], fixed = TRUE) ~ "p-value",
        grepl("SEM", .data[[sign_col]], fixed = TRUE) ~ "SEM",
        TRUE ~ "CI"
      )
    )

  if (DEBUG) {
    print("After sign_method:")
    View(data)
  }

  # Compute boolean significance
  data %<>%
    mutate(
      !!sym(SIGN_col) := case_when(
        is.na(.data[[sign_method_col]]) ~ NA,
        .data[[sign_method_col]] == "p-value" ~ case_when(
          grepl("<", .data[[sign_col]], fixed = TRUE) ~ TRUE,
          grepl(">", .data[[sign_col]], fixed = TRUE) ~ FALSE,
          grepl("=", .data[[sign_col]], fixed = TRUE) ~
            suppressWarnings(gsub("p| |=", "", .data[[sign_col]]) %>% as.numeric()) < 0.05
        ),
        .data[[sign_method_col]] == "SEM" & !is.na(.data[[var]]) ~
          suppressWarnings(gsub(" |=|SEM", "", .data[[sign_col]]) %>% as.numeric() * 1.96) <
          abs(.data[[var]] - baseline),
      .data[[sign_method_col]] == "CI" & .data[[var]] >= baseline & !is.na(.data[[var]]) ~
          safe_split_num(.data[[sign_col]], 1, sep = CI_sep) > baseline,
        .data[[sign_method_col]] == "CI" & .data[[var]] < baseline & !is.na(.data[[var]]) ~
          safe_split_num(.data[[sign_col]], 2, sep = CI_sep) < baseline
      )
    )

  return(data)
}


get_pval_string = function(data, y, x) {

  # 1. Fit the linear model
  # We use as.formula to handle string inputs for column names
  model <- lm(as.formula(paste(y, "~", x)), data = data)

  # 2. Extract the p-value for the slope (2nd coefficient)
  # summary(model)$coefficients returns a matrix: [Estimate, Std. Error, t value, Pr(>|t|)]
  # Row 2 corresponds to 'x', Column 4 is the p-value
  pval <- summary(model)$coefficients[2, 4]

  # 3. Format the string based on cutoffs
  if (pval < 0.0001) {
    return("p < 0.0001")
  } else if (pval < 0.001) {
    return("p < 0.001")
  } else if (pval < 0.01) {
    return("p < 0.01")
  } else {
    # Round to 2 decimal places and ensure it prints with 2 decimals (e.g., 0.50 not 0.5)
    return(paste0("p = ", format(round(pval, 2), nsmall = 2)))
  }
}

get_segments = function(data, slope_var, asp, L = 0.15) {
  data %>%
    mutate(
      # Use {{ }} to tell R to evaluate the column name passed to slope_var
      dx = (L / 2) / sqrt(1 + ({{ slope_var }} * asp)^2),
      dy = (L / 2) * ({{ slope_var }} * asp) / sqrt(1 + ({{ slope_var }} * asp)^2),
      x_start = x - dx,
      x_end = x + dx,
      y_start = y - dy / asp,
      y_end = y + dy / asp
    )
}