# Testing area
# load question data_list rds
# saveRDS(question_list,"question_data_list.rds")
# saveRDS(cuts_df,"cuts_df.rds")
#
# gs_data <- readRDS("gs_data.rds")
# question_list <- gs_data$gs_scoregroups
# cuts_df <- gs_data$cuts_df

# remove NAs from the list
caption_style <- fp_text(font.size = 20, font.family = "Calibri")

# Generate question group slides ----
# Called from shiny with list of dataframes
gs_makebriefmain <- function(question_list, df_canvas_adj, missing_roster, cuts_df,
                             cut_filter_threshold, progress.tot,
                             courseTitle, eventTitle, output_file = "question_groups.pptx") {
  
  # === INPUT VALIDATION ===
  tryCatch({
    # Check for NULL inputs
    if (is.null(question_list)) stop("question_list cannot be NULL. Make sure you added questions.")
    if (is.null(df_canvas_adj)) stop("df_canvas_adj cannot be NULL. Make sure you selected canvas courses.")
    #if (is.null(missing_roster)) stop("missing_roster cannot be NULL")
    if (is.null(cuts_df)) stop("cuts_df cannot be NULL. Make sure you uploaded the csv for the cuts.")
    if (is.null(courseTitle)) stop("courseTitle cannot be NULL. Make sure you entered a course Title.")
    if (is.null(eventTitle)) stop("eventTitle cannot be NULL. Make sure you entered an event Title.")
    
    # Check for empty data
    if (!is.list(question_list) || length(question_list) == 0) {
      stop("question_list must be a non-empty list")
    }
    if (!is.data.frame(df_canvas_adj) || nrow(df_canvas_adj) == 0) {
      stop("df_canvas_adj must be a non-empty data frame")
    }
    if (!is.data.frame(cuts_df)) {
      stop("cuts_df must be a data frame")
    }
    
    # Check required columns in df_canvas_adj
    required_cols_canvas <- c("version", "pre_percent", "post_percent", "mge_percent", 
                               "pre_grade", "post_grade", "mge_grade")
    missing_cols <- setdiff(required_cols_canvas, names(df_canvas_adj))
    if (length(missing_cols) > 0) {
      stop(paste("df_canvas_adj missing required columns:", paste(missing_cols, collapse = ", ")))
    }
    
    # Check required columns in cuts_df
    if (!"question" %in% names(cuts_df)) {
      stop("cuts_df must have a 'question' column")
    }
    
    # Check for NA in critical numeric columns
    if (any(is.na(df_canvas_adj$version))) {
      warning("df_canvas_adj contains NA values in 'version' column - these will be filtered out")
      df_canvas_adj <- df_canvas_adj[!is.na(df_canvas_adj$version), ]
    }
    
    cat("✓ Input validation passed\n")
    
  }, error = function(e) {
    cat("=====================================\n")
    cat("Input Validation Error in gs_makebriefmain:\n")
    cat("=====================================\n")
    cat("Error message:", conditionMessage(e), "\n\n")
    cat("Full traceback:\n")
    print(rlang::trace_back())
    cat("=====================================\n")
    stop(e)
  })
  
  # browser()
  # clean question+list and cut list of underscores
  # remove understores from all the question in cuts_df
  cuts_df$question <- gsub("_", " ", cuts_df$question)
  
  # Check if version column has valid data before creating palette
  if (length(unique(df_canvas_adj$version)) == 0) {
    stop("df_canvas_adj has no valid version data")
  }
  version_palette <- setNames(version_palette, unique(df_canvas_adj$version))

  # browser()
  # df = question_list[[1]]
  # Check if input is a named list of dataframes
  if (!is.list(question_list) || is.null(names(question_list))) {
    stop("Input must be a named list of dataframes.")
  }
  ## 1a. Title ----
  incProgress(1 / progress.tot, detail = paste("Making Title Slide"))
  ppt <- ppt_init(courseTitle, eventTitle)
  # browser()

  ## 1b. Pre/post ----
  incProgress(1 / progress.tot, detail = paste("Making Pre/Post Slide"))
  ppt <- ppt_prepost(ppt, df_canvas_adj)

  ## 1c. Pre-score population comparison ----
  incProgress(1 / progress.tot, detail = paste("Making Pre/Post Slide"))
  ppt <- ppt_prescorepop(ppt, df_canvas_adj)

  ## 1d. Makeup Cadets ----
  incProgress(1 / progress.tot, detail = paste("Making Makeup Cadets Slide"))
  ppt <- ppt_makeupcadets(ppt, missing_roster)

  ## 1e. Version comparison slide ----
  # sort by versions ascending
  df_canvas_adj <- df_canvas_adj[order(df_canvas_adj$version), ]

  incProgress(1 / progress.tot, detail = paste("Making Version Comparison Slide"))
  ppt <- ppt_versioncomp(ppt, df_canvas_adj, eventTitle)

  ## 1f. Loop through each version ----
  # sort by version ascending
  incProgress(1 / progress.tot, detail = paste("Making Version Slides"))
  ppt <- ppt_versionstats(ppt, df_canvas_adj)

  # question_df <- question_list[[1]]
  # df_name = names(question_list)[1]

  cat("Question loop start\n")
  ## 2. Loop through each question group ----
  for (df_name in names(question_list)) {
    tryCatch({
      # browser()
      cat(paste("Processing question:", df_name, "\n"))
      incProgress(1 / progress.tot, detail = paste0("Making Question slide ", df_name))

      question_df <- question_list[[df_name]]
      
      # Validate question_df
      if (is.null(question_df) || !is.data.frame(question_df)) {
        warning(paste("Skipping", df_name, "- not a valid data frame"))
        next
      }
      
      if (nrow(question_df) == 0) {
        warning(paste("Skipping", df_name, "- empty data frame"))
        next
      }
      
      # Check for required columns
      if (!"label" %in% names(question_df)) {
        warning(paste("Skipping", df_name, "- missing 'label' column"))
        next
      }
      if (!"value" %in% names(question_df)) {
        warning(paste("Skipping", df_name, "- missing 'value' column"))
        next
      }

      ## DEBUG - loads question groups for testing
      ## question_df = question_df_list[[1]]
      ## question_group_name = names(question_df_list)[1]

      # clean of underscores to help matching
      question_df$label <- gsub("_", " ", question_df$label)

      # extract the version from the question label, it is the last two characters
      if (any(nchar(question_df$label) < 2)) {
        warning(paste("Some labels in", df_name, "are too short to extract version"))
        question_df$version <- ifelse(nchar(question_df$label) >= 2,
          substr(question_df$label, nchar(question_df$label) - 1, nchar(question_df$label)),
          "??"
        )
      } else {
        question_df$version <- substr(
          question_df$label,
          nchar(question_df$label) - 1,
          nchar(question_df$label)
        )
      }

      # remove everything outside of the :: and :: from the question label
      question_df$name <- gsub(".*::(.*)::.*", "\\1", question_df$label)
      
      # Check if name extraction worked
      if (all(question_df$name == question_df$label)) {
        warning(paste("Question name extraction may have failed for", df_name, "- labels don't contain :: delimiters"))
      }

      # get the max points
      question_df$max <- as.numeric(ifelse(
        grepl(
          "\\(\\d+(\\.\\d+)? (pts|points)\\)",
          question_df$name
        ), # Match rows with numeric values
        sub(
          ".*\\((\\d+(\\.\\d+)?) (pts|points)\\).*", "\\1",
          question_df$name
        ), # Extract numbers
        NA # Assign NA when no match
      ))

      if (any(is.na(question_df$max))) {
        stop(paste("Could not extract max points from question label in", df_name,
           ".\nCheck that your question titles don't contain parentheses in Gradescope and try again.",
           "\nFirst few labels:", paste(head(question_df$name, 3), collapse = ", ")))
      }
      
      # Check for zero or negative max points
      if (any(question_df$max <= 0, na.rm = TRUE)) {
        stop(paste("Invalid max points (≤0) found in", df_name))
      }
      
      # get the percentage - protect against division by zero
      question_df$percent <- ifelse(question_df$max > 0,
        question_df$value / question_df$max,
        0
      )

      # get the grades
      if (!exists("letter_grade") || !exists("breaks") || !exists("grades")) {
        stop("Required variables 'letter_grade', 'breaks', or 'grades' not found in environment")
      }
      question_df$grade <- sapply(question_df$percent, letter_grade, breaks, grades)

      # format the percentage
      question_df$percent <- question_df$percent * 100

      # get the question group name
      question_group_name <- df_name
      question_df <- question_df %>% arrange(version)

      ppt <- gen_question_slide(ppt, question_df, question_group_name, cuts_df, cut_filter_threshold)
      
    }, error = function(e) {
      cat("=====================================\n")
      cat(paste("Error processing question group:", df_name, "\n"))
      cat("=====================================\n")
      cat("Error message:", conditionMessage(e), "\n")
      cat("Error call:", deparse(conditionCall(e)), "\n\n")
      cat("Full traceback:\n")
      print(rlang::trace_back())
      cat("=====================================\n")
      stop(e)  # Re-throw to halt execution
    })
  }
  return(ppt)
  ## Save the PowerPoint file ----
  # print(ppt, target = output_file)
  # print(ppt,target = "test.pptx")
  # message(paste("Slides have been saved to", output_file))
}




# 1. Summaries ----

## 1A. PPT Initialize ----
ppt_init <- function(courseTitle = "PH2XX", eventTitle = "WPRX") {
  tryCatch({
    # Validate inputs
    if (is.null(courseTitle) || courseTitle == "") {
      warning("courseTitle is empty - using default")
      courseTitle <- "PH2XX"
    }
    if (is.null(eventTitle) || eventTitle == "") {
      warning("eventTitle is empty - using default")
      eventTitle <- "WPRX"
    }
    
    # Check if wd exists
    if (!exists("wd")) {
      stop("'wd' (working directory) variable not found in environment")
    }
    
    # Check if template file exists
    template_path <- str_c(wd, "/www/template_gs.pptx")
    if (!file.exists(template_path)) {
      stop(paste("Template file not found at:", template_path))
    }
    
  }, error = function(e) {
    cat("Error in ppt_init:\n")
    cat("Message:", conditionMessage(e), "\n")
    print(rlang::trace_back())
    stop(e)
  })
  
  #### Load PPT based on input data####
  ppt <- read_pptx(str_c(wd, "/www/template_gs.pptx"))


  for (n in rev(seq_len(length(ppt)))) {
    remove_slide(ppt, n)
  }
  #### TITLE SLIDE ####
  ppt <- add_slide(ppt, layout = "Title Slide", master = "Office Theme")

  ppt <- ph_with(ppt,
    value = str_c(courseTitle, ": ", eventTitle, " Grade Brief"),
    location = ph_location_label(ph_label = "Title")
  )

  return(ppt)
}

## 1B. PRE Post Function ----

ppt_prepost <- function(ppt, df_canvas_adj) {
  tryCatch({
    # Validate inputs
    if (is.null(df_canvas_adj) || !is.data.frame(df_canvas_adj) || nrow(df_canvas_adj) == 0) {
      stop("df_canvas_adj must be a non-empty data frame")
    }
    
    # Check required columns
    required_cols <- c("pre_percent", "post_percent", "mge_percent", "pre_grade", "post_grade")
    missing_cols <- setdiff(required_cols, names(df_canvas_adj))
    if (length(missing_cols) > 0) {
      stop(paste("ppt_prepost: df_canvas_adj missing columns:", paste(missing_cols, collapse = ", ")))
    }
    
    # Check for all NA columns
    if (all(is.na(df_canvas_adj$pre_percent))) {
      stop("pre_percent column contains only NA values")
    }
    if (all(is.na(df_canvas_adj$post_percent))) {
      stop("post_percent column contains only NA values")
    }
    
  }, error = function(e) {
    cat("Error in ppt_prepost:\n")
    cat("Message:", conditionMessage(e), "\n")
    print(rlang::trace_back())
    stop(e)
  })
  
  # Count, mean medium min and max tables
  #browser()
  ### Pre/post table ----
  pre_stats <- df_canvas_adj %>%
    mutate(
      pre_percent = pre_percent,
      post_percent = post_percent,
      mge_percent = mge_percent
    ) %>%
    summarise(
      Type = "Pre",
      Number = n(),
      Average = mean(pre_percent, na.rm = TRUE),
      Median = median(pre_percent, na.rm = TRUE),
      Min = min(pre_percent, na.rm = TRUE),
      Max = max(pre_percent, na.rm = TRUE)
    )

  post_stats <- df_canvas_adj %>%
    mutate(
      post_percent = post_percent,
      mge_percent = mge_percent
    ) %>%
    summarise(
      Type = "Post",
      Number = n(),
      Average = mean(post_percent, na.rm = TRUE),
      Median = median(post_percent, na.rm = TRUE),
      Min = min(post_percent, na.rm = TRUE),
      Max = max(post_percent, na.rm = TRUE)
    )

  comp.stats <- rbind(pre_stats, post_stats)

  comp_stats_ft <- comp.stats %>%
    flextable() %>%
    align_nottext_col(align = "center") %>%
    fontsize(size = 16, part = "header") %>%
    autofit() %>%
    fontsize(size = 18) %>%
    align_text_col(align = "center") %>%
    colformat_double(digits = 1) %>%
    set_caption(as_paragraph(
      as_chunk("Pre/Post Score Comparisons", props = caption_style)
    ), word_stylename = "Table Caption")
  # show(comp_stats_ft)

  ### table grades ----
  grade_levels <- rev(grades.desc)  # reverse to go F → A
  
  # Validate grades exist in data
  if (!all(c("pre_grade", "post_grade") %in% names(df_canvas_adj))) {
    stop("df_canvas_adj missing pre_grade or post_grade columns")
  }
  
  grade_summary <- df_canvas_adj %>%
    mutate(
      pre_grade  = factor(pre_grade,  levels = grade_levels),
      post_grade = factor(post_grade, levels = grade_levels)
    ) %>%
    reframe(
      pre_count  = tabulate(pre_grade,  nbins = length(grade_levels)),
      post_count = tabulate(post_grade, nbins = length(grade_levels))
    ) %>%
    as_tibble() %>%
    mutate(
      grade = grade_levels,
      percent_change = round(100 * (post_count - pre_count) / pmax(pre_count, 1), 0)
    ) %>%
    select(grade, pre_count, post_count, percent_change)
  

  colnames(grade_summary)[1] <- "Grade"
  colnames(grade_summary)[2] <- "Pre Count"
  colnames(grade_summary)[3] <- "Post Count"
  colnames(grade_summary)[4] <- "Percent Change"

  # Transpose the table
  transposed_summary <- t(grade_summary)
  colnames(transposed_summary) <- transposed_summary[1, ] # Set column names to the grades
  transposed_summary <- transposed_summary[-1, , drop = FALSE] # Remove the first row used for column names


  # Convert the transposed table to a data frame
  transposed_df <- as.data.frame(transposed_summary, stringsAsFactors = FALSE)

  # Add the row names as a column for `flextable`
  transposed_df <- transposed_df %>%
    rownames_to_column(var = "Grade")


  # Create a color matrix for the transposed data frame
  colormatrix <- apply(transposed_df[, -1], 1:2, function(x) {
    if (!is.na(as.numeric(x))) { # Check if the value can be converted to numeric
      ifelse(as.numeric(x) < 0, "#FDEFEF", "#F4FFEF") # Red for negative, green for positive
    } else {
      "#FFFFFF" # White for non-numeric cells
    }
  })
  # writes the first two rows as white
  colormatrix[1:2, ] <- "white"
  colormatrix[3, 1] <- "white"

  transposed_df[3, 1:ncol(transposed_df)] <- paste0(as.character(transposed_df[3, 1:ncol(transposed_df)]), "%")
  # Apply the color matrix in a flextable
  comp_grades_ft <- transposed_df %>%
    flextable() %>%
    bg(i = seq_len(nrow(transposed_df)), j = seq_len(ncol(transposed_df) - 1), bg = colormatrix) %>% # Apply colors
    align_nottext_col(align = "center") %>%
    align_text_col(align = "center") %>%
    fontsize(size = 20, part = "header") %>%
    fontsize(size = 18) %>%
    set_caption(
      as_paragraph(
        as_chunk("Pre/Post Grade Comparisons", props = caption_style)
      ),
      word_stylename = "Table Caption"
    )

  # Print the flextable
  # comp_grades_ft


  pre.A.count <- as.numeric(transposed_df[[1, 10]]) + as.numeric(transposed_df[[1, 11]]) +
    as.numeric(transposed_df[[1, 12]])
  pre.DF.count <- as.numeric(transposed_df[[1, 2]]) + as.numeric(transposed_df[[1, 3]])
  pre.count <- pre_stats$Number
  post.A.count <- as.numeric(transposed_df[[2, 10]]) + as.numeric(transposed_df[[2, 11]]) +
    as.numeric(transposed_df[[2, 12]])
  post.DF.count <- as.numeric(transposed_df[[2, 2]]) + as.numeric(transposed_df[[2, 3]])
  post.count <- post_stats$Number
  vers.grades.header <- c("", grades)

  comp_grades_ft <- add_footer_row(comp_grades_ft,
    values = c(
      "POST D/F's:",
      str_c(
        post.DF.count, " (",
        round(post.DF.count / post.count * 100, 0), "%)"
      ),
      "POST A's:",
      str_c(
        post.A.count, " (",
        round(post.A.count / post.count * 100, 0), "%)"
      )
    ),
    colwidths = c(3, 3, 3, 3), top = TRUE
  ) %>%
    align_text_col(align = "center") %>%
    align(i = NULL, j = 1, align = "right", part = "footer") %>%
    align(i = NULL, j = 4, align = "left", part = "footer") %>%
    color(i = 1, j = c(1, 4), color = "#c21313", part = "footer") %>%
    align(i = NULL, j = 7, align = "right", part = "footer") %>%
    align(i = NULL, j = 10, align = "left", part = "footer") %>%
    color(i = 1, j = c(7, 8, 9, 10, 11), color = "forestgreen", part = "footer") %>%
    add_footer_row(
      values = c(
        "PRE D/F's:", str_c(pre.DF.count, " (", round(pre.DF.count / pre.count * 100, 1), "%)"),
        "PRE A's:", str_c(pre.A.count, " (", round(pre.A.count / pre.count * 100, 1), "%)")
      ),
      colwidths = c(3, 3, 3, 3), top = TRUE
    ) %>%
    align_text_col(align = "center") %>%
    align(i = NULL, j = 1, align = "right", part = "footer") %>%
    align(i = NULL, j = 4, align = "left", part = "footer") %>%
    color(i = 1, j = c(1, 4), color = "#c21313", part = "footer") %>%
    align(i = NULL, j = 7, align = "right", part = "footer") %>%
    align(i = NULL, j = 10, align = "left", part = "footer") %>%
    color(i = 1, j = c(7, 8, 9, 10, 11), color = "forestgreen", part = "footer") %>%
    fontsize(size = 22, part = "footer") %>%
    bold(bold = TRUE, part = "footer") %>%
    autofit()




  # show(comp.grades.ft)
  ### Plots ----

  # Comparison plot for pre-post
  df.pre <- df_canvas_adj %>% select(pre_percent)
  df.pre$type <- "Pre-Test"
  colnames(df.pre)[1] <- "percent"
  df.post <- df_canvas_adj %>% select(post_percent)
  df.post$type <- "Post-Test"
  colnames(df.post)[1] <- "percent"
  df.compare <- rbind(df.pre, df.post)
  df.compare$percent <- df.compare$percent
  df.compare$type <- factor(df.compare$type, levels = c("Post-Test", "Pre-Test"))
  p.comp <- ggplot(df.compare, aes(x = type, y = percent, fill = type)) +
    geom_hline(yintercept = 60, linetype = "dashed", linewidth = 1, color = "red") +
    geom_boxplot(alpha = 0.25) +
    scale_fill_manual(breaks = c("Pre-Test", "Post-Test"), values = c("blue", "orange")) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      text = element_text(size = 18),
      axis.title.y = element_blank()
    ) +
    scale_y_continuous(
      breaks = seq(0, 100, 5), name = "Percent (%)", sec.axis =
        sec_axis(~.x, breaks = seq(0, 100, 5))
    ) +
    labs(title = "Pre and Post Event Score Distribution Comparison") +
    coord_flip()
  # show(p.comp)

  #
  ### Slides ----
  ppt <- add_slide(ppt, layout = "Summary", master = "Office Theme")
  ppt <- ph_with(ppt,
    value = "Pre/Post Course Score Comparisons",
    location = ph_location_label(ph_label = "Title")
  )

  ## Adding plots
  ppt <- ph_with(ppt, value = p.comp, location = ph_location(left = 0.1, top = 4, width = 8, height = 4))

  ## Adding tables
  ppt <- ph_with(ppt, value = comp_grades_ft, location = ph_location(left = 2, top = 1))
  ppt <- ph_with(ppt, value = comp_stats_ft, location = ph_location(left = 8.5, top = 5))

  ## Large Plot Slide


  return(ppt)
}

## 1C. Pre Score Distribution by Version ----
ppt_prescorepop <- function(ppt, df_canvas_adj) {
  tryCatch({
    # Validate inputs
    if (is.null(df_canvas_adj) || !is.data.frame(df_canvas_adj) || nrow(df_canvas_adj) == 0) {
      stop("df_canvas_adj must be a non-empty data frame")
    }
    
    # Check required columns
    if (!"version" %in% names(df_canvas_adj)) {
      stop("df_canvas_adj missing 'version' column")
    }
    if (!"pre_percent" %in% names(df_canvas_adj)) {
      stop("df_canvas_adj missing 'pre_percent' column")
    }
    if (!"pre_grade" %in% names(df_canvas_adj)) {
      stop("df_canvas_adj missing 'pre_grade' column")
    }
    
    # Check if version_palette exists
    if (!exists("version_palette")) {
      stop("version_palette not found in environment")
    }
    
  }, error = function(e) {
    cat("Error in ppt_prescorepop:\n")
    cat("Message:", conditionMessage(e), "\n")
    print(rlang::trace_back())
    stop(e)
  })
  
  P <- ggplot(df_canvas_adj, aes(x = pre_percent, fill = version)) +
    geom_density(alpha = 0.2) +
    theme_bw() +
    theme(text = element_text(size = 20), ) +
    labs(title = "Pre-WPR Scores for Version populations", x = "Score (%)", y = "Density") +
    scale_fill_manual(values = version_palette)


  df.vers.sum <- df_canvas_adj %>%
    group_by(version) %>%
    summarise(
      "Count" = n(),
      "Mean (%)" = round(mean(pre_percent, na.rm = TRUE), 1),
      "Median (%)" = round(median(pre_percent, na.rm = TRUE), 1),
      "Std. Dev (%)" = round(sd(pre_percent, na.rm = TRUE), 1),
      "# D/F's" = sum(pre_grade == "D" | pre_grade == "F", na.rm = TRUE),
      "# A's" = sum(pre_grade == "A" | pre_grade == "A+" | pre_grade == "A-", na.rm = TRUE)
    )

  df.vers.sum <- df.vers.sum %>%
    mutate(
      "# D/F's" = str_c(`# D/F's`, " (", round(`# D/F's` / `Count` * 100, 0), "%)"),
      "# A's" = str_c(`# A's`, " (", round(`# A's` / `Count` * 100, 0), "%)")
    )

  df.vers.sum.ft <- flextable(df.vers.sum) %>%
    align_text_col(align = "center") %>%
    align_nottext_col(align = "center") %>%
    colformat_double(digits = 1) %>%
    fontsize(size = 20, part = "header") %>%
    fontsize(size = 18) %>%
    color(color = "red", j = 6) %>%
    color(color = "forestgreen", j = 7) %>%
    bold(j = 6:7) %>%
    set_caption(
      as_paragraph(
        as_chunk("Pre Test score distribution comparison by version population",
          props = fp_text_default(font.family = "Calibri", font.size = 20, bold = TRUE)
        )
      ),
      word_stylename = "Table Caption"
    ) %>%
    autofit()

  ppt <- add_slide(ppt, layout = "Summary", master = "Office Theme") %>%
    ph_with(ppt, value = "Pre Score Comparisons between versions", location = ph_location_label(ph_label = "Title")) %>%
    ph_with(value = P, location = ph_location(left = 3, top = 0.75, width = 8, height = 4)) %>%
    ph_with(value = df.vers.sum.ft, location = ph_location(left = 2, top = 4.75, width = 8, height = 2))

  # only does and applies the t test if there are two versions.
  if (length(unique(df_canvas_adj$version)) == 2) {
    t.test.results <- t.test(pre_percent ~ version, data = df_canvas_adj, var.equal = FALSE)
    test_summary <- data.frame(
      Method = t.test.results$method,
      result = names(t.test.results$null.value),
      t_value = round(t.test.results$statistic, 3),
      # Means = t.test.results$estimate,
      p_value = format(t.test.results$p.value, scientific = TRUE, digits = 3)
    )

    test_summary.ft <- flextable(test_summary) %>%
      theme_booktabs() %>%
      align(align = "center", part = "all") %>%
      autofit()

    ppt <- ph_with(ppt, value = test_summary.ft, location = ph_location(left = 2, top = 6.5, width = 8, height = 1))
  }

  return(ppt)
}

## 1D. Makeup Cadets  ----
ppt_makeupcadets <- function(ppt, missing_roster) {
  tryCatch({
    # Validate inputs
    if (is.null(missing_roster) || !is.data.frame(missing_roster)) {
      warning("missing_roster is NULL or not a data frame - skipping makeup cadets slide")
      return(ppt)
    }
    
    if (nrow(missing_roster) == 0) {
      cat("No makeup cadets to display\n")
      return(ppt)
    }
    
    # Check required columns exist
    required_cols <- c("user.sortable_name", "instructor", "section_hour")
    missing_cols <- setdiff(required_cols, names(missing_roster))
    if (length(missing_cols) > 0) {
      warning(paste("missing_roster missing columns:", paste(missing_cols, collapse = ", ")))
      # Try to work with available columns
      available_cols <- intersect(required_cols, names(missing_roster))
      if (length(available_cols) == 0) {
        warning("No valid columns found in missing_roster - skipping slide")
        return(ppt)
      }
      missing_roster <- missing_roster[, available_cols, drop = FALSE]
    } else {
      missing_roster <- missing_roster %>% select(user.sortable_name, instructor, section_hour)
    }
    
  }, error = function(e) {
    cat("Error in ppt_makeupcadets:\n")
    cat("Message:", conditionMessage(e), "\n")
    print(rlang::trace_back())
    warning("Skipping makeup cadets slide due to error")
    return(ppt)
  })
  
  missing_roster <- missing_roster %>% rename(
    "Cadet Name" = user.sortable_name,
    "Instructor" = instructor,
    "Section" = section_hour
  )

  makeupcadets_ft <- missing_roster %>%
    flextable() %>%
    align_nottext_col(align = "center") %>%
    align_text_col(align = "center") %>%
    colformat_double(digits = 1) %>%
    fontsize(size = 12) %>%
    fontsize(size = 12, part = "header") %>%
    autofit() %>%
    set_caption(as_paragraph(
      as_chunk("Cadets who are not included in these versions", props = caption_style)
    ), word_stylename = "Table Caption")

  # show(no.entries.ft)
  ppt <- add_slide(ppt, layout = "chart and table", master = "Office Theme")
  ppt <- ph_with(ppt, value = "Cadets not accounted for in this brief", location = ph_location_label(ph_label = "Title"))
  ppt <- ph_with(ppt, value = makeupcadets_ft, location = ph_location(left = 2.5, top = 1, width = 8, height = 4))

  return(ppt)
}


## 1e. Version comparison ----
ppt_versioncomp <- function(ppt, df_canvas_adj, eventTitle) {
  tryCatch({
    # Validate inputs
    if (is.null(df_canvas_adj) || !is.data.frame(df_canvas_adj) || nrow(df_canvas_adj) == 0) {
      stop("df_canvas_adj must be a non-empty data frame")
    }
    
    # Check required columns
    required_cols <- c("version", "mge_percent", "mge_grade")
    missing_cols <- setdiff(required_cols, names(df_canvas_adj))
    if (length(missing_cols) > 0) {
      stop(paste("ppt_versioncomp: missing columns:", paste(missing_cols, collapse = ", ")))
    }
    
  }, error = function(e) {
    cat("Error in ppt_versioncomp:\n")
    cat("Message:", conditionMessage(e), "\n")
    print(rlang::trace_back())
    stop(e)
  })
  
  df.vers.sum <- df_canvas_adj %>%
    group_by(version) %>%
    summarise(
      "Count" = n(), 
      "Mean (%)" = round(mean(mge_percent, na.rm = TRUE), 1),
      "Median (%)" = round(median(mge_percent, na.rm = TRUE), 1),
      "Std. Dev (%)" = round(sd(mge_percent, na.rm = TRUE), 1),
      "Min (%)" = round(min(mge_percent, na.rm = TRUE), 1),
      "Max (%)" = round(max(mge_percent, na.rm = TRUE), 1),
      "# D/F's" = sum(mge_grade == "D" | mge_grade == "F", na.rm = TRUE),
      "# A's" = sum(mge_grade == "A" | mge_grade == "A+" | mge_grade == "A-", na.rm = TRUE)
    )
  df.vers.sum <- df.vers.sum %>% mutate(
    "# D/F's" = str_c(
      `# D/F's`, " (", round(`# D/F's` / `Count` * 100, 0),
      "%)"
    ),
    "# A's" = str_c(`# A's`, " (", round(`# A's` / `Count` * 100, 0), "%)")
  )

  df.vers.sum.ft <- flextable(df.vers.sum) %>%
    align_text_col(align = "center") %>%
    align_nottext_col(align = "center") %>%
    colformat_double(digits = 1) %>%
    fontsize(size = 20, part = "header") %>%
    fontsize(size = 18) %>%
    color(color = "red", j = 8) %>%
    color(color = "forestgreen", j = 9) %>%
    bold(j = 8:9) %>%
    autofit()

  # show(df.vers.sum.ft)


  version.unique <- unique(df_canvas_adj$version)
  df_canvas_adj$version <- factor(df_canvas_adj$version, rev(version.unique))
  
  #### :Box and Whisker ####
  v.comp <- ggplot(df_canvas_adj, aes(x = version, y = mge_percent, fill = version)) +
    geom_hline(yintercept = 60, linetype = "dashed", linewidth = 1, color = "red") +
    geom_boxplot(alpha = 0.5) +
    scale_fill_manual(values = version_palette) +
    theme_bw() +
    labs(title = "Version Comparisons", x = "", y = "Score (%)") +
    scale_y_continuous(
      breaks = seq(0, 100, 5), sec.axis =
        sec_axis(~.x, breaks = seq(0, 100, 5))
    ) +
    theme_bw() +
    theme(
      legend.position = "right",
      text = element_text(size = 20),
      axis.title.y = element_blank()
    ) +
    coord_flip()

  # show(v.comp)

  #### :Slide####
  ppt <- add_slide(ppt, layout = "chart and table", master = "Office Theme")
  ppt <- ph_with(ppt, value = str_c(eventTitle, " Version Comparisons"), location = ph_location_label(ph_label = "Title"))

  ## Adding version chart comparison
  ppt <- ph_with(ppt, value = df.vers.sum.ft, location = ph_location(left = .75, top = 5.6, width = 8))

  ppt <- ph_with(ppt, value = v.comp, location = ph_location(left = 2, top = .8, width = 10, height = 4.5))
}


## 1f. Version Summaries ----
ppt_versionstats <- function(ppt, df_canvas_adj) {
  tryCatch({
    # Validate inputs
    if (is.null(df_canvas_adj) || !is.data.frame(df_canvas_adj) || nrow(df_canvas_adj) == 0) {
      stop("df_canvas_adj must be a non-empty data frame")
    }
    
    # Check required columns
    required_cols <- c("version", "mge_percent", "mge_grade")
    missing_cols <- setdiff(required_cols, names(df_canvas_adj))
    if (length(missing_cols) > 0) {
      stop(paste("ppt_versionstats: missing columns:", paste(missing_cols, collapse = ", ")))
    }
    
  }, error = function(e) {
    cat("Error in ppt_versionstats:\n")
    cat("Message:", conditionMessage(e), "\n")
    print(rlang::trace_back())
    stop(e)
  })
  
  # df_version <- df_canvas_adj %>% filter(version==unique(df_canvas_adj$version)[1])
  df.vers.sum <- df_canvas_adj %>%
    group_by(version) %>%
    summarise(
      "Count" = n(), 
      "Mean (%)" = round(mean(mge_percent, na.rm = TRUE), 1),
      "Median (%)" = round(median(mge_percent, na.rm = TRUE), 1),
      "Std. Dev (%)" = round(sd(mge_percent, na.rm = TRUE), 1),
      "Min (%)" = round(min(mge_percent, na.rm = TRUE), 1),
      "Max (%)" = round(max(mge_percent, na.rm = TRUE), 1),
      "# D/F's" = sum(mge_grade == "D" | mge_grade == "F", na.rm = TRUE),
      "# A's" = sum(mge_grade == "A" | mge_grade == "A+" | mge_grade == "A-", na.rm = TRUE)
    )
  df.vers.sum <- df.vers.sum %>% mutate(
    "# D/F's" = str_c(
      `# D/F's`, " (", round(`# D/F's` / `Count` * 100, 0),
      "%)"
    ),
    "# A's" = str_c(`# A's`, " (", round(`# A's` / `Count` * 100, 0), "%)")
  )

  #### Version summary ####
  for (i in 1:length(unique(df_canvas_adj$version))) {
    df_version <- df_canvas_adj %>% filter(version == unique(df_canvas_adj$version)[i])
    # df_version <- gb[[i]][[1]]

    vers.sum <- df.vers.sum %>% filter(version == i)
    #### :Table ####
    vers.sum.ft <- flextable(vers.sum) %>%
      align_text_col(align = "center") %>%
      align_nottext_col(align = "center") %>%
      colformat_double(digits = 1) %>%
      fontsize(size = 20, part = "header") %>%
      fontsize(size = 18) %>%
      color(color = "red", j = 6) %>%
      color(color = "forestgreen", j = 7) %>%
      bold(j = 6:7) %>%
      autofit()

    #### :Plot ####
    # Histogram distribution "chart 1"

    # Version bar plot for scores
    v.plot1 <- plotHisto(
      df_version,
      "mge_percent",
      str_c("Version ", i, " Score Distribution"),
      "Score (%)",
      "Count",
      version_palette[i], bin.width
    )
    # show(v.plot1)

    # Version bar plot for grades
    df_version$mge_grade <- factor(df_version$mge_grade, grades)
    v.plot2 <- df_version %>%
      count(mge_grade) %>%
      mutate(label = paste(n)) %>%
      ggplot(aes(x = factor(mge_grade), y = n)) +
      geom_col(width = 0.7, fill = version_palette[i]) +
      geom_text(
        aes(y = -0.15 * max(n), label = label),
        vjust = 0,
        color = "black",
        size = 7,
        nudge_y = 0
      ) +
      theme_hc() +
      theme(text = element_text(size = 18)) +
      labs(title = str_c("Version ", i, " Grades Distribution"), x = "", y = "Count")

    # show(v.plot2)

    #### :Slide ####
    ppt <- add_slide(ppt, layout = "version summary", master = "Office Theme")
    ppt <- ph_with(ppt, value = str_c("Version ", i, " Summary"), location = ph_location_label(ph_label = "Title"))
    ppt <- ph_with(ppt, value = vers.sum.ft, location = ph_location(left = 1, top = 6, width = 8, height = 1.5))
    ppt <- ph_with(ppt, value = v.plot1, location = ph_location(left = 7, top = 1, width = 7, height = 4.5))
    ppt <- ph_with(ppt, value = v.plot2, location = ph_location(left = 0, top = 1, width = 7, height = 4.5))
  }
  return(ppt)
}




# 2. Make question slide ----
# This actually makes the single slide
# question_df is the dataframe of the questions in a group, could be N questions, likely 1 per version
# ppt is the powerpoint object to add the slide


# question_group_name - the name of the question group
# question_df - the dataframe of the question group (`value` and `label` columns)
#
# DEBUG - question group names
# question_group_name = names(question_list)[1]

gen_question_slide <- function(ppt, question_df, question_group_name, cuts_df, cut_filter_threshold) {
  tryCatch({
    # browser()
    
    # Validate inputs
    if (is.null(question_df) || !is.data.frame(question_df) || nrow(question_df) == 0) {
      stop("question_df must be a non-empty data frame")
    }
    
    # Check required columns exist
    required_cols <- c("version", "name", "value", "percent", "grade", "max")
    missing_cols <- setdiff(required_cols, names(question_df))
    if (length(missing_cols) > 0) {
      stop(paste("question_df missing required columns:", paste(missing_cols, collapse = ", ")))
    }
    
    # Check for NA values in critical columns
    if (all(is.na(question_df$value))) {
      stop("All values in 'value' column are NA")
    }
    if (all(is.na(question_df$percent))) {
      stop("All values in 'percent' column are NA")
    }
    
  }, error = function(e) {
    cat("=====================================\n")
    cat("Error in gen_question_slide:\n")
    cat("Question group:", question_group_name, "\n")
    cat("=====================================\n")
    cat("Error message:", conditionMessage(e), "\n\n")
    cat("Full traceback:\n")
    print(rlang::trace_back())
    cat("=====================================\n")
    stop(e)
  })
  
  # Make the slide
  ppt <- add_slide(ppt, layout = "question", master = "Office Theme")
  ppt <- ph_with(ppt,
    value = str_c(question_group_name),
    location = ph_location_label(ph_label = "Title")
  )
  ### a. Score PDF ----
  # score PDF distribution: make and add (Bottom LEFT)
  q.pdf.plot <- plot_question(question_df)

  padding <- 0.125

  left_top_start <- 3.5

  left_width <- 6

  right_top_start <- 0.1
  right_col_start <- 6 + 2 * padding # from the left of the slide (in)
  right_width <- 8 - padding

  plot_height <- 3.75

  ppt <- ph_with(ppt,
    value = q.pdf.plot,
    location = ph_location(
      left = right_col_start, top = right_top_start,
      width = right_width,
      height = plot_height
    )
  )

  ### b. Grades Histo ----
  # TODO: Add grades histogram, below should work in the app
  # grades histogram: Make and add (TOP LEFT)
  grades_table_ft <- question_grades_table(question_df)
  dims <- dim_pretty(grades_table_ft)

  grades_table_height <- sum(dims$heights)
  grades_table_width <- sum(dims$width)
  grades_table_start <- left_top_start + plot_height + padding

  ppt <- ph_with(ppt,
    value = grades_table_ft,
    location = ph_location(
      left = 0.05, top = left_top_start,
      width = left_width,
      height = grades_table_height
    )
  )
  ### c. Summary Table ----

  # TODO: Add summary Table, below should work in the app
  # question summary table: make and add (MIDDLE LEFT)
  sum_flexTable <- summary_table_question(question_df)
  sum_flextable <- sum_flexTable %>% width(width = left_width)

  dims <- dim_pretty(sum_flexTable)
  sum_flex_table_height <- sum(dims$heights)
  sum_flex_table_width <- sum(dims$width)
  flex_table_start <- left_top_start + grades_table_height + 6 * padding

  ppt <- ph_with(ppt,
    value = sum_flexTable,
    location = ph_location(
      left = 0.05 + (left_width - sum_flex_table_width - .6) / 2,
      top = flex_table_start,
      # width=left_width,
      height = sum_flex_table_height
    )
  )

  ### d. Cuts Table ----
  # get the questoin names in the group
  # cut_filter_threshold = 0.15
  cuts_flextable <- cuts_table(question_df, cuts_df, cut_filter_threshold)
  ppt <- ph_with(ppt,
    value = cuts_flextable,
    location = ph_location(
      left = right_col_start,
      top = right_top_start + plot_height + padding,
      width = right_width
    )
  )
  # print(ppt,target = "test.pptx")



  return(ppt)
}
## 2A. Score PDF FNC ----
# q.score.plot <- plotHisto(x,"percent",str_c("V",version.num,":", question.num," Score Distribution"),"Score (%)","Count",version_palette[i],bin.width)
# df = question_df
plot_question <- function(df, max_pts) {
  tryCatch({
    # browser()
    
    # Validate inputs
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      stop("df must be a non-empty data frame")
    }
    
    # Check required columns
    if (!"label" %in% names(df)) {
      stop("df missing 'label' column")
    }
    if (!"value" %in% names(df)) {
      stop("df missing 'value' column")
    }
    if (!"percent" %in% names(df)) {
      stop("df missing 'percent' column")
    }
    if (!"max" %in% names(df)) {
      stop("df missing 'max' column")
    }
    
    # Check for all NA in critical columns
    if (all(is.na(df$value))) {
      stop("All values in 'value' column are NA")
    }
    
  }, error = function(e) {
    cat("Error in plot_question:\n")
    cat("Message:", conditionMessage(e), "\n")
    print(rlang::trace_back())
    stop(e)
  })
  
  # here I want to check the length of the df legend
  # if I exceed the length of the version_palette then I want to set version_palette to a longer color palette

  version_palette_q <- version_palette

  if (length(unique(df$label)) > length(version_palette_q)) {
    version_palette_q <- colorRampPalette(c("green", "orange"))(length(unique(df$label)))
  }
  if (length(unique(df$value)) > 6) {
    col <- "percent"
    xaxis <- "Score (%)"
    yaxis <- "Count"

    # remove the parenthesis at the end of the question name
    max_pts <- max(df$max)

    df$legend <- str_c(df$version, " - Q", df$name)
    # Create the histogram with color grouping
    q.score.plot <- ggplot(df, aes(x = get(col), color = legend, fill = legend)) +
      geom_density(
        alpha = 0.08, adjust = 0.3,
        aes(y = after_stat(density)), size = 1.1
      ) + # Use density for normalized comparison
      theme_classic() +
      scale_color_manual(values = version_palette_q) +
      scale_fill_manual(values = version_palette_q) +
      labs(x = xaxis, y = "Density", fill = NULL, color = NULL) +
      scale_x_continuous(
        name = xaxis,
        breaks = seq(0, max(df[[col]]), 5),
        sec.axis = sec_axis(
          trans = ~ . * (max_pts / max(df[[col]])), # Transformation for secondary axis
          name = "Points" # Label for secondary x-axis
        )
      ) +
      scale_y_continuous(
        name = "Density"
      ) +
      geom_vline(xintercept = 88, linewidth = 0.8, alpha = 0.4, linetype = "longdash", color = "forestgreen") +
      geom_vline(xintercept = 76, linewidth = 0.8, alpha = 0.4, linetype = "longdash", color = "blue") +
      geom_vline(xintercept = 60, linewidth = 0.8, alpha = 0.4, linetype = "longdash", color = "red") +
      theme(
        text = element_text(size = 16),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.position = c(0.025, 0.925),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.box.background = element_rect(color = "black", linewidth = 0.8)
      ) +
      guides(
        color = guide_legend(ncol = 1), # Arrange legend items in 1 column
        fill = guide_legend(ncol = 1)
      )
  } else { # ADDED 16MAY HISTO PLOT

    col <- "score" # Primary axis in points
    xaxis <- "Score (Points)"
    max_pts <- max(df$max) # Make sure this reflects the actual max points

    df$legend <- str_c(df$version, " - Q", df$name)
    df$score <- df$percent * max(df$max, na.rm = TRUE) / 100

    df_hist <- df %>%
      group_by(legend, value) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(legend) %>%
      mutate(percent = count / sum(count) * 100)


    q.score.plot <- ggplot(df_hist, aes(x = value, y = percent, fill = legend)) +
      geom_col(
        position = position_dodge(width = 0.6),
        width = 0.6,
        color = "black",
        alpha = 0.6
      ) +
      theme_classic() +
      scale_fill_manual(values = version_palette_q) +
      labs(x = "Score (Points)", y = "Percent of Students", fill = NULL) +
      scale_x_continuous(
        breaks = seq(0, max(df$value, na.rm = TRUE), 1),
        sec.axis = sec_axis(
          trans = ~ . / max_pts * 100,
          name = "Score (%)"
        )
      ) +
      scale_y_continuous(
        breaks = seq(0, 100, 10), # Grid and tick marks every 10%
        limits = c(0, 100), # Optional: cap at 100%
        expand = expansion(mult = c(0, 0.05)) # Optional: small padding at top
      ) +
      geom_vline(
        xintercept = c(0.6, 0.76, 0.88) * max_pts,
        linewidth = 0.8, alpha = 0.4, linetype = "longdash",
        color = c("red", "blue", "forestgreen")
      ) +
      theme(
        text = element_text(size = 16),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.position = c(0.025, 0.925),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.box.background = element_rect(color = "black", linewidth = 0.8),
        panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
        panel.grid.minor.y = element_blank(), # Optional: turn off minor lines
        panel.grid.major.x = element_blank(), # Optional: no vertical grid lines
      ) +
      guides(fill = guide_legend(ncol = 1))
  }
  # Display the plot
  # print(q.score.plot)


  return(q.score.plot)
}
# df <- question_df
## 2B. Grade Counts table----
question_grades_table <- function(df) {
  tryCatch({
    # Validate inputs
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      stop("df must be a non-empty data frame")
    }
    
    # Check required columns
    if (!"version" %in% names(df)) {
      stop("df missing 'version' column")
    }
    if (!"grade" %in% names(df)) {
      stop("df missing 'grade' column")
    }
    
    # Check if grades variable exists
    if (!exists("grades")) {
      stop("'grades' variable not found in environment")
    }
    
  }, error = function(e) {
    cat("Error in question_grades_table:\n")
    cat("Message:", conditionMessage(e), "\n")
    print(rlang::trace_back())
    stop(e)
  })
  
  # Summarize grades by version
  grades_sum <- df %>%
    group_by(version, grade) %>%
    summarize(n = round(n(), 0), .groups = "drop") # Summarize grade counts

  # Complete missing combinations of version and grade
  grades_sum <- grades_sum %>%
    complete(version, grade = grades, fill = list(n = 0))

  # Pivot to wide format
  grades_sum_wide <- grades_sum %>%
    pivot_wider(names_from = grade, values_from = n, values_fill = 0)

  refactor <- c("version", grades)

  grades_sum_wide <- grades_sum_wide[, refactor]

  colnames(grades_sum_wide)[1] <- " "

  # make a flextable with the grade data
  q_grades_flexTable <- flextable(grades_sum_wide) %>%
    align_text_col(align = "center") %>%
    align_nottext_col(align = "center") %>%
    colformat_double(digits = 0) %>%
    fontsize(size = 15, part = "header") %>%
    fontsize(size = 14) %>%
    color(color = "red", j = 2) %>%
    color(color = "forestgreen", j = 12) %>%
    bold(j = 2) %>%
    bold(j = 12) %>%
    autofit()
  # set_table_properties(width = grades_table_width, height = grades_table_height) %>%  # Force size
  return(q_grades_flexTable)
}

## 2C. Make Summary Table ----

summary_table_question <- function(df) {
  tryCatch({
    # Validate inputs
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      stop("df must be a non-empty data frame")
    }
    
    # Check required columns
    required_cols <- c("version", "percent", "grade")
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      stop(paste("df missing required columns:", paste(missing_cols, collapse = ", ")))
    }
    
  }, error = function(e) {
    cat("Error in summary_table_question:\n")
    cat("Message:", conditionMessage(e), "\n")
    print(rlang::trace_back())
    stop(e)
  })
  
  df.q.t.sum <- df %>%
    group_by(version) %>%
    summarise(
      "Count" = n(),
      "Mean" = round(mean(percent, na.rm = TRUE), 1),
      "SD" = round(sd(percent, na.rm = TRUE), 1),
      "# D/F's" = sum(grade == "D" | grade == "F", na.rm = TRUE),
      "# A's" = sum(grade == "A" | grade == "A+" | grade == "A-", na.rm = TRUE)
    )
  df.q.t.sum <- df.q.t.sum %>%
    mutate(
      "# D/F's" = str_c(`# D/F's`, " (", round(`# D/F's` / `Count` * 100, 0), "%)"),
      "# A's" = str_c(`# A's`, " (", round(`# A's` / `Count` * 100, 0), "%)")
    )

  df.q.sum.ft <- flextable(df.q.t.sum) %>%
    align_text_col(align = "center") %>%
    align_nottext_col(align = "center") %>%
    colformat_double(digits = 1) %>%
    fontsize(size = 15, part = "header") %>%
    fontsize(size = 14) %>%
    color(color = "red", j = 5) %>%
    color(color = "forestgreen", j = 6) %>%
    bold(j = 5:6) %>%
    autofit()

  return(df.q.sum.ft)
}

## 2D. Cut Table ----
cuts_table <- function(question_df, cuts_df, cut_filter_threshold) {
  tryCatch({
    # Validate inputs
    if (is.null(question_df) || !is.data.frame(question_df) || nrow(question_df) == 0) {
      warning("question_df is NULL, empty, or not a data frame - returning empty cuts table")
      return(flextable(data.frame(Message = "No cuts data available")))
    }
    
    if (is.null(cuts_df) || !is.data.frame(cuts_df) || nrow(cuts_df) == 0) {
      warning("cuts_df is NULL, empty, or not a data frame - returning empty cuts table")
      return(flextable(data.frame(Message = "No cuts data available")))
    }
    
    # Check required columns in question_df
    if (!"name" %in% names(question_df)) {
      stop("question_df missing 'name' column")
    }
    
    # Check required columns in cuts_df
    required_cols <- c("version", "number", "question", "cut_percent_true")
    missing_cols <- setdiff(required_cols, names(cuts_df))
    if (length(missing_cols) > 0) {
      warning(paste("cuts_df missing columns:", paste(missing_cols, collapse = ", ")))
      return(flextable(data.frame(Message = "Cuts data missing required columns")))
    }
    
    # Validate cut_filter_threshold
    if (is.null(cut_filter_threshold) || !is.numeric(cut_filter_threshold)) {
      warning("Invalid cut_filter_threshold - using default 0.15")
      cut_filter_threshold <- 0.15
    }
    
  }, error = function(e) {
    cat("Error in cuts_table:\n")
    cat("Message:", conditionMessage(e), "\n")
    print(rlang::trace_back())
    return(flextable(data.frame(Message = "Error generating cuts table")))
  })
  
  q_names <- unique(question_df$name)
  # browser()
  cuts_df <- cuts_df %>% mutate(key = paste0("V", version, "-", number, ": ", question))

  # filter cuts_df based on matches to q_names
  cuts_df_filtered <- cuts_df %>%
    filter(sapply(key, function(q) any(agrepl(q, q_names, max.distance = 0.1))))

  # cuts_df_filtered <- cuts_df %>%
  #  filter(sapply(question, function(q) any(agrepl(q, q_names, max.distance = 0.2))) )

  if (nrow(cuts_df_filtered) == 0) {
    cuts_df_filtered <- cuts_df %>%
      filter(sapply(key, function(q) any(agrepl(q, q_names, max.distance = 0.2))))
  }
  
  # If still no matches, return empty table with message
  if (nrow(cuts_df_filtered) == 0) {
    cat("No matching cuts found for this question\n")
    return(flextable(data.frame(Message = "No cuts applied for this question")))
  }
  
  # filter out the cuts that are less than threshold percent
  cuts_df_filtered <- cuts_df_filtered %>%
    filter(cut_percent_true > cut_filter_threshold)
  
  # If no cuts pass threshold, return message
  if (nrow(cuts_df_filtered) == 0) {
    cat("No cuts above threshold for this question\n")
    return(flextable(data.frame(Message = paste0("No cuts above ", cut_filter_threshold*100, "% threshold"))))
  }
  
  # now I want to make a flextable of the cuts, but I want to sort them by version then by cut_percent_true
  #
  cuts_df_filtered <- cuts_df_filtered %>%
    arrange(desc(cut_percent_true), version) # descending order
  # I want to arrange so that it goes versions descending (1,2) then questions descending (1,2,3) then cut_percent_true descending (100,90,80))

  cuts_df_filtered <- cuts_df_filtered %>%
    arrange(version, number, desc(cut_percent_true)) # descending order

  cuts_df_filtered <- cuts_df_filtered %>%
    mutate(cut_percent_true = round(cut_percent_true * 100, 0))

  # TODO: Build flextable logic
  # cuts_flexTable <-
  # combine version and question into one column
  cuts_df_filtered <- mutate(cuts_df_filtered, VQ = paste0("V", version, " - Q", number))
  #browser()
  colnames(cuts_df_filtered) <- c("V", "Q", "Name", "Pts", "Cut", "% Applied", "Question")

  cuts_flextable <- flextable(cuts_df_filtered %>% select(`Question`, Pts, Cut, `% Applied`)) %>%
    theme_vanilla() %>%
    bold(part = "header", bold = TRUE) %>% # Bold the headers
    fontsize(size = 13, part = "all") %>% # Set font size for entire table
    align(j = c("Question", "Cut"), align = "left", part = "all") %>% # Left-align the 'Cut' column
    align(j = c("Pts", "% Applied"), align = "center", part = "all") %>%
    autofit() %>%
    padding(padding = 2.5, part = "all") %>%
    height_all(height = 0.1, part = "all") #



  return(cuts_flextable)
}
