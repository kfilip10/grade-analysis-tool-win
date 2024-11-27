
#Testing area
#load question data_list rds
#saveRDS(question_list,"question_data_list.rds")
#saveRDS(cuts_df,"cuts_df.rds")
#
#gs_data <- readRDS("gs_data.rds")
#question_list <- gs_data$gs_scoregroups
#cuts_df <- gs_data$cuts_df

#remove NAs from the list

# Generate question group slides ----
# Called from shiny with list of dataframes
gs_makebriefmain <- function(question_list,cuts_df,cut_filter_threshold,output_file = "question_groups.pptx") {
  #browser()
  #clean question+list and cut list of underscores
  # remove understores from all the question in cuts_df
  cuts_df$question <- gsub("_", " ", cuts_df$question)  
  
  #df = question_list[[1]]
  # Check if input is a named list of dataframes
  if (!is.list(question_list) || is.null(names(question_list))) {
    stop("Input must be a named list of dataframes.")
  }
  # Create a new PowerPoint object
  ppt <- ppt_init()
  #browser()
  
  
  #question_df <- question_list[[6]]
  #df_name = names(question_list)[6]
  ## Loop through each question group ----
  for (df_name in names(question_list)) {
    
    question_df <- question_list[[df_name]]
    
    ## DEBUG
    ## question_df = question_df_list[[1]]
    ## question_group_name = names(question_df_list)[1]
    
    #clean of underscores to help matching
    question_df$label <- gsub("_", " ", question_df$label)  

    #extract the version from the question label, it is the last two characters
    question_df$version <- substr(question_df$label,
                                  nchar(question_df$label)-1,
                                  nchar(question_df$label))
    
    #remove everything outside of the :: and :: from the question label
    question_df$name <- gsub(".*::(.*)::.*", "\\1", question_df$label)
    
    #get the max points
    question_df$max <- as.numeric(ifelse(
      grepl("\\(\\d+(\\.\\d+)? (pts|points)\\)", 
            question_df$name), # Match rows with numeric values
      sub(".*\\((\\d+(\\.\\d+)?) (pts|points)\\).*", "\\1", 
          question_df$name), # Extract numbers
      NA # Assign NA when no match
    ))
    
    if(any(is.na(question_df$max))){
      stop("Could not extract max points from question label. 
         Check that your question titles don't contain parentheses in Gradescope and try again.")
    }
    #get the percentage
    question_df$percent <- question_df$value/question_df$max
    
    #get the grades
    question_df$grade <- sapply(question_df$percent,letter_grade,breaks,grades)
    
    #format the percentage
    question_df$percent <- question_df$percent*100
    
    #get the question group name
    question_group_name <- df_name
    question_df <- question_df %>% arrange(version)
    
    ppt <- gen_question_slide(ppt,question_df,question_group_name,cuts_df,cut_filter_threshold)

      }
  # Save the PowerPoint file
  print(ppt, target = output_file)
  #print(ppt,target = "test.pptx")
  message(paste("Slides have been saved to", output_file))
}

# PPT Settings and Title slide ----
ppt_init <- function(courseTitle="PH2X1",eventTitle="WPR1",numberVersions=1,progress.tot=1){

  ####Load PPT based on input data####
  ppt <- read_pptx(str_c(wd,"/www/GS_questions.pptx")) 
  
  
  for(n in rev(seq_len(length(ppt)))){
    remove_slide(ppt,n)
  }
  #### TITLE SLIDE ####
  ppt <- add_slide(ppt, layout = "Title Slide", master = "Office Theme")

    ppt <- ph_with(ppt,value = str_c(courseTitle,": ",eventTitle," Grade Brief"),
                   location = ph_location_label(ph_label = "Title"))

  return(ppt)
}

# 2. Make question slide ----
# This actually makes the single slide
#question_df is the dataframe of the questions in a group, could be N questions, likely 1 per version
#ppt is the powerpoint object to add the slide


#question_group_name - the name of the question group
#question_df - the dataframe of the question group (`value` and `label` columns)
#
#DEBUG LINES
#ppt = ppt_GS
#question_group_name = names(question_list)[1]

gen_question_slide<- function(ppt,question_df,question_group_name,cuts_df,cut_filter_threshold){
  #browser()
  # Make the slide
  ppt <- add_slide(ppt, layout = "question", master = "Office Theme")
  ppt <- ph_with(ppt,value = str_c(question_group_name),location = ph_location_label(ph_label = "Title"))
  ### a. Score PDF ----
  # score PDF distribution: make and add (Bottom LEFT)
  q.pdf.plot <- plot_question(question_df)
  
  padding = 0.125
  
  left_top_start <- 3.5
  
  left_width <- 6
  
  right_top_start <- 0.1
  right_col_start <- 6+2*padding #from the left of the slide (in)
  right_width <- 8-padding
  
  plot_height <- 3.75
  
  ppt <- ph_with(ppt,value = q.pdf.plot,
                 location = ph_location(left=right_col_start,top=right_top_start,
                                        width=right_width,
                                        height=plot_height))
  
  ### b. Grades Histo ----
  #TODO: Add grades histogram, below should work in the app
  # grades histogram: Make and add (TOP LEFT)
  grades_table_ft <- question_grades_table(question_df)
  dims <- dim_pretty(grades_table_ft)
  
  grades_table_height <- sum(dims$heights)
  grades_table_width <- sum(dims$width)
  grades_table_start <- left_top_start+plot_height+padding
  
  ppt <- ph_with(ppt,value = grades_table_ft,
                 location = ph_location(left=0.05,top=left_top_start,
                                        width=left_width,
                                        height=grades_table_height))
  ### c. Summary Table ----
  
  #TODO: Add summary Table, below should work in the app
  #question summary table: make and add (MIDDLE LEFT)
  sum_flexTable <- summary_table_question(question_df)
  sum_flextable <- sum_flexTable %>% width(width=left_width)
  
  dims <- dim_pretty(sum_flexTable)
  sum_flex_table_height <- sum(dims$heights)
  sum_flex_table_width <- sum(dims$width)
  flex_table_start <- left_top_start+grades_table_height+6*padding
  
  ppt <- ph_with(ppt,value = sum_flexTable,
                 location = ph_location(left=0.05+(left_width-sum_flex_table_width-.6)/2,
                                        top=flex_table_start,
                                        #width=left_width,
                                        height=sum_flex_table_height))

  ### d. Cuts Table ----
  #get the questoin names in the group
  #cut_filter_threshold = 0.15
   cuts_flextable <- cuts_table(question_df,cuts_df,cut_filter_threshold)
   ppt <- ph_with(ppt,value = cuts_flextable,
                  location = ph_location(left=right_col_start,
                                         top=right_top_start+plot_height+padding,
                                         width=right_width))
   print(ppt,target = "test.pptx")
   
   
  
  return(ppt)
  
}







#A. Score PDF FNC ----
#q.score.plot <- plotHisto(x,"percent",str_c("V",version.num,":", question.num," Score Distribution"),"Score (%)","Count",version.palette[i],bin.width)
#df = question_df
plot_question <- function(df,max_pts){
  
  col <- "percent"
  xaxis <- "Score (%)"
  yaxis <- "Count"
  
  #remove the parenthesis at the end of the question name
  max_pts <- max(df$max)
  
  df$legend <- str_c(df$version," - Q",df$name)
  
  # Create the histogram with color grouping
  q.score.plot <- ggplot(df, aes(x = get(col), color = legend, fill = legend)) +
    geom_density(alpha = 0.08, adjust = 0.3, 
                 aes(y = after_stat(density)), size = 1.1) +  # Use density for normalized comparison
    theme_classic() +
    scale_color_manual(values = version.palette) +
    scale_fill_manual(values = version.palette) +
    labs(x = xaxis, y = "Density", fill = NULL, color = NULL) +
    scale_x_continuous(
      name = xaxis, 
      breaks = seq(0, max(df[[col]]), 5),
      sec.axis = sec_axis(
        trans = ~ . * (max_pts / max(df[[col]])),  # Transformation for secondary axis
        name = "Points"                            # Label for secondary x-axis
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
      color = guide_legend(ncol = 1),  # Arrange legend items in 1 column
      fill = guide_legend(ncol = 1)
    )
  
  
  # Display the plot
  #print(q.score.plot)
  
  
  return(q.score.plot)

}
#df <- question_df
#A. Grade Counts table----
question_grades_table <- function(df){

  #summarize grades by version
  grades_sum <- df %>%
    group_by(version,grade) %>%
    summarize(n = round(n(),0))
  
  #put the long data into wide
  grades_sum_wide <- grades_sum %>%
    spread(grade,n,fill=0)
  #check the column names for grades_sum_wide and find that ones that are not in grades.missing
  #
  grades.missing <- grades[!(grades %in% colnames(grades_sum_wide))]
  for (col in grades.missing) {
    grades_sum_wide[[col]] <- 0
  }

  refactor <- c("version",grades)
  
  grades_sum_wide <- grades_sum_wide[,refactor]
  
  colnames(grades_sum_wide)[1] = " "
  
  #make a flextable with the grade data
  q_grades_flexTable <- flextable(grades_sum_wide)%>% 
    align_text_col(align = "center") %>%
    align_nottext_col(align = "center") %>% 
    colformat_double( digits = 0)  %>% 
    fontsize(size=15,part = "header")%>%
    fontsize(size=14)%>%
    color(color="red",j=2)%>%
    color(color="forestgreen",j=12)%>%
    bold(j=2)%>%
    bold(j=12) %>%autofit()
  #set_table_properties(width = grades_table_width, height = grades_table_height) %>%  # Force size
  return(q_grades_flexTable)
}

#C. Make Summary Table ----

summary_table_question <- function(df){
  
  df.q.t.sum <- df %>%group_by(version)%>% summarise("Count"=n(),
                                 "Mean"=round(mean(percent),1),
                                 "SD"=round(sd(percent),1),
                                 "# D/F's"=sum(grade=="D"|grade=="F"),
                                 "# A's"=sum(grade=="A"|grade=="A+"|grade=="A-")) 
  df.q.t.sum <- df.q.t.sum %>% 
    mutate("# D/F's"=str_c(`# D/F's`," (",round(`# D/F's`/`Count`*100,0),"%)"),
           "# A's"=str_c(`# A's`," (",round(`# A's`/`Count`*100,0),"%)")
    )
  
  df.q.sum.ft <- flextable(df.q.t.sum) %>% align_text_col(align = "center") %>%
    align_nottext_col(align = "center") %>% 
    colformat_double( digits = 1)  %>% 
    fontsize(size=15,part = "header")%>%
    fontsize(size=14)%>%
    color(color="red",j=5)%>%
    color(color="forestgreen",j=6)%>%
    bold(j=5:6)%>%
    autofit()
  
  return(df.q.sum.ft)
}

# D. Cut Table ----
cuts_table <- function(question_df,cuts_df,cut_filter_threshold){
  q_names <- unique(question_df$name)
  
  cuts_df_filtered <- cuts_df %>%
    filter(sapply(question, function(q) any(agrepl(q, q_names, max.distance = 0.2))) ) 
  
  if(nrow(cuts_df_filtered) == 0){
    cuts_df_filtered <- cuts_df %>%
      filter(sapply(question, function(q) any(agrepl(q, q_names, max.distance = 2))) ) 
    
  }
  #filter out the cuts that are less than 15 percent
  #
  cuts_df_filtered <- cuts_df_filtered %>%
    filter(cut_percent_true > cut_filter_threshold)
  #now I want to make a flextable of the cuts, but I want to sort them by version then by cut_percent_true
  #
  cuts_df_filtered <- cuts_df_filtered %>%
    arrange(desc(cut_percent_true),version) #descending order
  
  
  cuts_df_filtered <- cuts_df_filtered %>%
    mutate(cut_percent_true = round(cut_percent_true*100,0))
  
  #TODO: Build flextable logic
  #cuts_flexTable <- 
  #combine version and question into one column
  cuts_df_filtered <- mutate(cuts_df_filtered, VQ = paste0("V",version," - Q",number))
  colnames(cuts_df_filtered) <- c("V","Q","Name","Pts","Cut","Applied (%)","#")
  
  cuts_flextable <- flextable(cuts_df_filtered %>%select(`#`,Pts,Cut,`Applied (%)`)) %>% 
    theme_vanilla() %>%
    bold(part = "header", bold = TRUE) %>%  # Bold the headers
    fontsize(size = 13, part = "all") %>%      # Set font size for entire table
    align(j = c("#","Cut"), align = "left", part = "all") %>%          # Left-align the 'Cut' column
    align(j = c("Pts", "Applied (%)"), align = "center", part = "all") %>% 
    autofit() %>%
    padding(padding = 2.5, part = "all") %>%
    height_all(height = 0.1, part = "all") #
  
  
  
  return(cuts_flextable)
}

