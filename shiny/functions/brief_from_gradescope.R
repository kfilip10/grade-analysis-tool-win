
#Testing area
#load question data_list rds
#saveRDS(question_list,"question_data_list.rds")
#saveRDS(cuts_df,"cuts_df.rds")
#
#gs_data <- readRDS("gs_data.rds")
#question_list <- gs_data$gs_scoregroups
#cuts_df <- gs_data$cuts_df

#remove NAs from the list
caption_style <- fp_text(font.size = 20,font.family = "Calibri")

# Generate question group slides ----
# Called from shiny with list of dataframes
gs_makebriefmain <- function(question_list,df_canvas_adj,missing_roster,cuts_df,
                             cut_filter_threshold,progress.tot,
                             courseTitle,eventTitle,output_file = "question_groups.pptx") {
  #browser()
  #clean question+list and cut list of underscores
  # remove understores from all the question in cuts_df
  cuts_df$question <- gsub("_", " ", cuts_df$question)  
  version_palette <- setNames(version_palette,unique(df_canvas_adj$version))
  
  #browser()
  #df = question_list[[1]]
  # Check if input is a named list of dataframes
  if (!is.list(question_list) || is.null(names(question_list))) {
    stop("Input must be a named list of dataframes.")
  }
  ##1a. Title ----
  incProgress(1/progress.tot, detail = paste("Making Title Slide"))
  ppt <- ppt_init(courseTitle,eventTitle)
  #browser()
  
  ##1b. Pre/post ----
  incProgress(1/progress.tot, detail = paste("Making Pre/Post Slide"))
  ppt <- ppt_prepost(ppt, df_canvas_adj)
  
  ##1c. Pre-score population comparison ----
  incProgress(1/progress.tot, detail = paste("Making Pre/Post Slide"))
  ppt <- ppt_prescorepop(ppt, df_canvas_adj)
  
  ##1d. Makeup Cadets ----
  incProgress(1/progress.tot, detail = paste("Making Makeup Cadets Slide"))
  ppt <- ppt_makeupcadets(ppt, missing_roster)
  
  ##1e. Version comparison slide ----
  #sort by versions ascending
  df_canvas_adj <- df_canvas_adj[order(df_canvas_adj$version),]
  
  incProgress(1/progress.tot, detail = paste("Making Version Comparison Slide"))
  ppt <- ppt_versioncomp(ppt, df_canvas_adj,eventTitle)
  
  ##1f. Loop through each version ----
  #sort by version ascending
  incProgress(1/progress.tot, detail = paste("Making Version Slides"))
  ppt <- ppt_versionstats(ppt, df_canvas_adj)
  
  #question_df <- question_list[[1]]
  #df_name = names(question_list)[1]
  
  
  ##2. Loop through each question group ----
  for (df_name in names(question_list)) {
    
    question_df <- question_list[[df_name]]
    
    ## DEBUG - loads question groups for testing
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
  return(ppt)
  ## Save the PowerPoint file ----
  #print(ppt, target = output_file)
  #print(ppt,target = "test.pptx")
  #message(paste("Slides have been saved to", output_file))
}




#1. Summaries ----

##1A. PPT Initialize ----
ppt_init <- function(courseTitle="PH2XX",eventTitle="WPRX"){
  
  ####Load PPT based on input data####
  ppt <- read_pptx(str_c(wd,"/www/template_gs.pptx")) 
  
  
  for(n in rev(seq_len(length(ppt)))){
    remove_slide(ppt,n)
  }
  #### TITLE SLIDE ####
  ppt <- add_slide(ppt, layout = "Title Slide", master = "Office Theme")
  
  ppt <- ph_with(ppt,value = str_c(courseTitle,": ",eventTitle," Grade Brief"),
                 location = ph_location_label(ph_label = "Title"))
  
  return(ppt)
}

##1B. PRE Post Function ----

ppt_prepost <- function(ppt, df_canvas_adj){
  
  # Count, mean medium min and max tables 
  ### Pre/post table ----
  pre_stats <- df_canvas_adj %>% 
    mutate(pre_percent = pre_percent,
           post_percent = post_percent,
           mge_percent = mge_percent) %>% 
    summarise(Type="Pre",
              Number = n(),
              Average = mean(pre_percent),
              Median = median(pre_percent),
              Min = min(pre_percent),
              Max =max(pre_percent))
  
  post_stats <- df_canvas_adj%>% 
    mutate( post_percent = post_percent,
           mge_percent = mge_percent)   %>% 
    summarise(Type="Post",
              Number = n(),
              Average = mean(post_percent),
              Median = median(post_percent),
              Min = min(post_percent),
              Max =max(post_percent)) 
  
  comp.stats <- rbind(pre_stats,post_stats)
  
  comp_stats_ft <- comp.stats %>% flextable() %>% align_nottext_col(align = "center") %>%    
    fontsize(size=16,part = "header")%>%autofit()%>%
    fontsize(size=18)%>%
    align_text_col(align = "center") %>% colformat_double( digits = 1)  %>%
    set_caption(    as_paragraph(
      as_chunk("Pre/Post Score Comparisons", props = caption_style)
    ), word_stylename = "Table Caption"
    )
  #show(comp_stats_ft)
  
  ### table grades ----
  
  grade_summary <- df_canvas_adj %>%
    reframe(
      grade = unique(c(pre_grade,post_grade)),  # Combine all unique grades
      pre_count = sapply(grades.desc, function(g) sum(pre_grade == g)),
      post_count = sapply(grades.desc, function(g) sum(post_grade == g))
    ) %>%
    mutate(
      percent_change = round((post_count - pre_count) / pre_count * 100,0)
    )  %>% 
    mutate(grade = factor(grade, levels = grades)) %>% 
  arrange(grade)
  
  colnames(grade_summary)[1] <- "Grade"
  colnames(grade_summary)[2] <- "Pre Count"
  colnames(grade_summary)[3] <- "Post Count"
  colnames(grade_summary)[4] <- "Percent Change"
  
  # Transpose the table
  transposed_summary <- t(grade_summary)
  colnames(transposed_summary) <- transposed_summary[1, ]  # Set column names to the grades
  transposed_summary <- transposed_summary[-1, , drop = FALSE]  # Remove the first row used for column names
  
  
  # Convert the transposed table to a data frame
  transposed_df <- as.data.frame(transposed_summary, stringsAsFactors = FALSE)
  
  # Add the row names as a column for `flextable`
  transposed_df <- transposed_df %>%
    rownames_to_column(var = "Grade")
  

  # Create a color matrix for the transposed data frame
  colormatrix <- apply(transposed_df[, -1], 1:2, function(x) {
    if (!is.na(as.numeric(x))) {  # Check if the value can be converted to numeric
      ifelse(as.numeric(x) < 0, "#FDEFEF", "#F4FFEF")  # Red for negative, green for positive
    } else {
      "#FFFFFF"  # White for non-numeric cells
    }
  })
  #writes the first two rows as white
  colormatrix[1:2,] <- "white" 
  colormatrix[3,1] <- "white"

  transposed_df[3,1:ncol(transposed_df)] <- paste0(as.character(transposed_df[3,1:ncol(transposed_df)]),"%")
  # Apply the color matrix in a flextable
  comp_grades_ft <- transposed_df %>%
    flextable() %>%
    bg(i = seq_len(nrow(transposed_df)), j = seq_len(ncol(transposed_df) - 1), bg = colormatrix) %>%  # Apply colors
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
  #comp_grades_ft

  
  pre.A.count <- as.numeric(transposed_df[[1,10]])+as.numeric(transposed_df[[1,11]])+
    as.numeric(transposed_df[[1,12]])
  pre.DF.count <- as.numeric(transposed_df[[1,2]])+as.numeric(transposed_df[[1,3]])
  pre.count <- pre_stats$Number
  post.A.count <- as.numeric(transposed_df[[2,10]])+as.numeric(transposed_df[[2,11]])+
    as.numeric(transposed_df[[2,12]])
  post.DF.count <- as.numeric(transposed_df[[2,2]])+as.numeric(transposed_df[[2,3]])
  post.count <- post_stats$Number
  vers.grades.header <- c("",grades)
  
  comp_grades_ft <- add_footer_row(comp_grades_ft,
                                   values = c("POST D/F's:",
                                              str_c(post.DF.count," (",
                                                    round(post.DF.count/post.count*100,0),"%)"),
                                              "POST A's:",
                                              str_c(post.A.count," (",
                                                    round(post.A.count/post.count*100,0),"%)")),
                                   colwidths=c(3,3,3,3),top=TRUE)%>% align_text_col(align = "center") %>%
    align(i = NULL, j = 1, align = "right", part = "footer") %>%
    align(i = NULL, j = 4, align = "left", part = "footer") %>%
    color(i=1,j=c(1,4),color="#c21313",part="footer") %>%
    align(i = NULL, j = 7, align = "right", part = "footer") %>%
    align(i = NULL, j = 10, align = "left", part = "footer") %>%
    color(i=1,j=c(7,8,9,10,11),color="forestgreen",part="footer") %>%
    add_footer_row(
      values = c("PRE D/F's:",str_c(pre.DF.count," (",round(pre.DF.count/pre.count*100,1),"%)"),
                 "PRE A's:",str_c(pre.A.count," (",round(pre.A.count/pre.count*100,1),"%)")),
      colwidths=c(3,3,3,3),top=TRUE)%>% align_text_col(align = "center") %>%
    align(i = NULL, j = 1, align = "right", part = "footer") %>%
    align(i = NULL, j = 4, align = "left", part = "footer") %>%
    color(i=1,j=c(1,4),color="#c21313",part="footer") %>%
    align(i = NULL, j = 7, align = "right", part = "footer") %>%
    align(i = NULL, j = 10, align = "left", part = "footer") %>%
    color(i=1,j=c(7,8,9,10,11),color="forestgreen",part="footer") %>%
    fontsize(size=22,part="footer") %>%
    bold(bold=TRUE,part="footer") %>% autofit()
  
  
  
  
  #show(comp.grades.ft)
  ### Plots ----
  
  #Comparison plot for pre-post
  df.pre <- df_canvas_adj %>% select(pre_percent)
  df.pre$type <- "Pre-Test"
  colnames(df.pre)[1] <- "percent"
  df.post <- df_canvas_adj %>% select(post_percent)
  df.post$type <- "Post-Test"
  colnames(df.post)[1] <- "percent"
  df.compare <- rbind(df.pre,df.post)
  df.compare$percent <- df.compare$percent
  df.compare$type <- factor(df.compare$type, levels = c("Post-Test","Pre-Test"))
  p.comp <- ggplot(df.compare,aes(x=type, y=percent, fill=type))+ 
    geom_hline(yintercept=60, linetype="dashed",linewidth=1, color = "red")+
    geom_boxplot(alpha=0.25)+
    scale_fill_manual(breaks=c("Pre-Test","Post-Test"),values = c("blue", "orange"))+
    theme_bw()+theme(legend.position="bottom",
                     text = element_text(size = 18),
                     axis.title.y=element_blank())+
    scale_y_continuous(breaks = seq(0,100, 5),name = "Percent (%)",sec.axis = 
                         sec_axis(~.x,breaks = seq(0,100, 5)))+   
    labs(title = "Pre and Post Event Score Distribution Comparison")+
    coord_flip()
  #show(p.comp)
  
  #
  ### Slides ----
  ppt <- add_slide(ppt, layout = "Summary", master = "Office Theme")
  ppt <- ph_with(ppt,value = "Pre/Post Course Score Comparisons",
                 location = ph_location_label(ph_label = "Title"))
  
  ## Adding plots
  ppt <- ph_with(ppt, value = p.comp, location = ph_location(left = 0.1, top = 4,width=8,height=4))
  
  ## Adding tables
  ppt <- ph_with(ppt,value = comp_grades_ft,location=ph_location(left=2,top=1))
  ppt <- ph_with(ppt,value = comp_stats_ft,location=ph_location(left=8.5,top=5))
  
  ## Large Plot Slide
  
  
  return(ppt)
}

## 1C. Pre Score Distribution by Version ----
ppt_prescorepop <- function(ppt,df_canvas_adj){
  
  
  P <- ggplot(df_canvas_adj, aes(x = pre_percent, fill = version)) +
    geom_density(alpha = 0.2)+
    theme_bw()+theme(text = element_text(size = 20),)+
    labs(title = "Pre-WPR Scores for Version populations", x = "Score (%)", y = "Density")+
    scale_fill_manual(values=version_palette)
  
  
  df.vers.sum <- df_canvas_adj %>% group_by(version) %>% 
    summarise("Count"=n(),
              "Mean (%)"=round(mean(pre_percent),1),
              "Median (%)"=round(median(pre_percent),1),
              "Std. Dev (%)"=round(sd(pre_percent),1),
              "# D/F's"=sum(pre_grade=="D"|pre_grade=="F"),
              "# A's"=sum(pre_grade=="A"|pre_grade=="A+"|pre_grade=="A-"))
  
  df.vers.sum <- df.vers.sum %>% 
    mutate("# D/F's"=str_c(`# D/F's`," (",round(`# D/F's`/`Count`*100,0),"%)"),
           "# A's"=str_c(`# A's`," (",round(`# A's`/`Count`*100,0),"%)")
  )
  
  df.vers.sum.ft <- flextable(df.vers.sum) %>% align_text_col(align = "center") %>%
    align_nottext_col(align = "center") %>% 
    colformat_double( digits = 1)  %>% 
    fontsize(size=20,part = "header")%>%
    fontsize(size=18)%>%
    color(color="red",j=6)%>%
    color(color="forestgreen",j=7)%>%
    bold(j=6:7)%>%
    set_caption(
      as_paragraph(
        as_chunk("Pre Test score distribution comparison by version population", 
                 props = fp_text_default(font.family = "Calibri",font.size=20,bold=TRUE))
      ), word_stylename = "Table Caption"
    )%>%  autofit()
  
  ppt <- add_slide(ppt, layout = "Summary", master = "Office Theme") %>%
    ph_with(ppt,value = "Pre Score Comparisons between versions",location = ph_location_label(ph_label = "Title")) %>%
    ph_with(value = P, location = ph_location(left=3,top=0.75,width=8,height=4)) %>%
    ph_with(value = df.vers.sum.ft, location = ph_location(left=2,top=4.75,width=8,height=2)) 
  
  #only does and applies the t test if there are two versions.
  if(length(unique(df_canvas_adj$version))==2){
    t.test.results <- t.test(pre_percent ~ version,data=df_canvas_adj, var.equal=FALSE)
    test_summary <- data.frame(
      Method = t.test.results$method,
      result = names(t.test.results$null.value),
      t_value = round(t.test.results$statistic,3),
      #Means = t.test.results$estimate,
      p_value = format(t.test.results$p.value, scientific = TRUE,digits=3)
    )
    
    test_summary.ft <- flextable(test_summary) %>% 
      theme_booktabs() %>% 
      align(align = "center", part = "all") %>% 
      autofit()
    
    ppt <- ph_with(ppt,value = test_summary.ft, location = ph_location(left=2,top=6.5,width=8,height=1))
  }
  
  return(ppt)
}

## 1D. Makeup Cadets  ----
ppt_makeupcadets <- function(ppt, missing_roster){
  missing_roster <- missing_roster %>% select(user.sortable_name,instructor,section_hour)
  missing_roster <- missing_roster %>% rename("Cadet Name" = user.sortable_name,
                                              "Instructor" = instructor,
                                              "Section" = section_hour)
  
  makeupcadets_ft <- missing_roster %>% flextable() %>% align_nottext_col(align = "center") %>%
    align_text_col(align = "center") %>% colformat_double( digits = 1) %>%  
    fontsize(size=12)%>%
    fontsize(size=12,part = "header")%>%
    autofit() %>%
    set_caption(    as_paragraph(
      as_chunk("Cadets who are not included in these versions", props = caption_style)
    ), word_stylename = "Table Caption"
    )
  
  #show(no.entries.ft)
  ppt <- add_slide(ppt, layout = "chart and table", master = "Office Theme")
  ppt <- ph_with(ppt,value = "Cadets not accounted for in this brief",location = ph_location_label(ph_label = "Title"))
  ppt <- ph_with(ppt,value = makeupcadets_ft,location = ph_location(left = 2.5, top = 1,width=8,height=4))
  
  return(ppt)
  
}


## 1e. Version comparison ----
ppt_versioncomp <- function(ppt, df_canvas_adj,eventTitle){
  
  
  df.vers.sum <- df_canvas_adj %>% group_by(version) %>% 
    summarise("Count"=n(),"Mean (%)"=round(mean(mge_percent),1),
              "Median (%)"=round(median(mge_percent),1),
              "Std. Dev (%)"=round(sd(mge_percent),1),
              "Min (%)"=round(min(mge_percent),1),
              "Max (%)"=round(max(mge_percent),1),
              "# D/F's"=sum(mge_grade=="D"|mge_grade=="F"),
              "# A's"=sum(mge_grade=="A"|mge_grade=="A+"|mge_grade=="A-"))
  df.vers.sum <- df.vers.sum %>% mutate("# D/F's"=str_c(`# D/F's`," (",round(`# D/F's`/`Count`*100,0),
                                                        "%)"),
                                        "# A's"=str_c(`# A's`," (",round(`# A's`/`Count`*100,0),"%)")
  )
  
  df.vers.sum.ft <- flextable(df.vers.sum) %>% align_text_col(align = "center") %>%
    align_nottext_col(align = "center") %>% 
    colformat_double( digits = 1)  %>% 
    fontsize(size=20,part = "header")%>%
    fontsize(size=18)%>%
    color(color="red",j=8)%>%
    color(color="forestgreen",j=9)%>%
    bold(j=8:9)%>%
    autofit()
  
  #show(df.vers.sum.ft)
  
  
  version.unique <- unique(df_canvas_adj$version)
  df_canvas_adj$version <- factor(df_canvas_adj$version, rev(version.unique))
  #### :Box and Whisker ####
  v.comp <-ggplot(df_canvas_adj, aes(x=version, y=mge_percent, fill=version)) +
    geom_hline(yintercept=60, linetype="dashed",linewidth=1, color = "red")+
    geom_boxplot(alpha=0.5)+  scale_fill_manual(values=version_palette)+
    theme_bw()+     labs(title = "Version Comparisons",x="",y="Score (%)")+
    scale_y_continuous(breaks = seq(0,100, 5),sec.axis = 
                         sec_axis(~.x,breaks = seq(0,100, 5)))+
    theme_bw()+theme(legend.position="right",
                     text = element_text(size = 20),
                     axis.title.y=element_blank())+
    coord_flip()
  
  #show(v.comp)
  
  ####:Slide####
  ppt <- add_slide(ppt, layout = "chart and table", master = "Office Theme")
  ppt <- ph_with(ppt,value = str_c(eventTitle, " Version Comparisons"),location = ph_location_label(ph_label = "Title"))
  
  ## Adding version chart comparison
  ppt <- ph_with(ppt,value = df.vers.sum.ft,location = ph_location(left = .75, top = 5.6,width=8))
  
  ppt <- ph_with(ppt,value = v.comp,location = ph_location(left = 2, top = .8,width=10,height=4.5))
  
  
  
  
}


## 1f. Version Summaries ----
 ppt_versionstats <- function(ppt, df_canvas_adj){
   #df_version <- df_canvas_adj %>% filter(version==unique(df_canvas_adj$version)[1])
   df.vers.sum <- df_canvas_adj %>% group_by(version) %>% 
     summarise("Count"=n(),"Mean (%)"=round(mean(mge_percent),1),
               "Median (%)"=round(median(mge_percent),1),
               "Std. Dev (%)"=round(sd(mge_percent),1),
               "Min (%)"=round(min(mge_percent),1),
               "Max (%)"=round(max(mge_percent),1),
               "# D/F's"=sum(mge_grade=="D"|mge_grade=="F"),
               "# A's"=sum(mge_grade=="A"|mge_grade=="A+"|mge_grade=="A-"))
   df.vers.sum <- df.vers.sum %>% mutate("# D/F's"=str_c(`# D/F's`," (",round(`# D/F's`/`Count`*100,0),
                                                         "%)"),
                                         "# A's"=str_c(`# A's`," (",round(`# A's`/`Count`*100,0),"%)")
   )
   
   #### Version summary ####
   for (i in 1:length(unique(df_canvas_adj$version))){

     df_version <- df_canvas_adj %>% filter(version==unique(df_canvas_adj$version)[i])
     #df_version <- gb[[i]][[1]] 
     
     vers.sum <- df.vers.sum %>% filter(version==i)
     #### :Table ####
     vers.sum.ft <- flextable(vers.sum) %>% align_text_col(align = "center") %>%
       align_nottext_col(align = "center") %>% 
       colformat_double( digits = 1)  %>% 
       fontsize(size=20,part = "header")%>%
       fontsize(size=18)%>%
       color(color="red",j=6)%>%
       color(color="forestgreen",j=7)%>%
       bold(j=6:7)%>%
       autofit()
     
     #### :Plot ####
     # Histogram distribution "chart 1"
     
     # Version bar plot for scores 
     v.plot1 <- plotHisto(df_version,
                          "mge_percent",
                          str_c("Version ",i," Score Distribution"),
                          "Score (%)",
                          "Count",
                          version_palette[i],bin.width)
     #show(v.plot1)
     
     # Version bar plot for grades 
     df_version$mge_grade <- factor(df_version$mge_grade, grades)
     v.plot2 <- df_version%>%count(mge_grade)%>%mutate(label=paste(n))%>%
       ggplot(aes(x=factor(mge_grade),y=n)) + 
       geom_col(width=0.7,fill=version_palette[i]) +
       geom_text(
         aes(y=-0.15*max(n),label = label),
         vjust=0,
         color="black"
         ,size=7,
         nudge_y=0)+
       theme_hc()+
       theme(text = element_text(size = 18))+
       labs(title = str_c("Version ",i," Grades Distribution"),x="",y="Count")
     
     #show(v.plot2)
     
     #### :Slide ####
     ppt <- add_slide(ppt, layout = "version summary", master = "Office Theme")
     ppt <- ph_with(ppt,value = str_c("Version ", i, " Summary"),location = ph_location_label(ph_label = "Title"))
     ppt <- ph_with(ppt,value = vers.sum.ft,location = ph_location(left = 1, top = 6,width=8,height=1.5))
     ppt <- ph_with(ppt,value = v.plot1,location = ph_location(left=7,top=1,width=7,height=4.5))
     ppt <- ph_with(ppt,value = v.plot2,location = ph_location(left=0,top=1,width=7,height=4.5))
     
   }
   return(ppt)
   
 }




 # 2. Make question slide ----
 # This actually makes the single slide
 #question_df is the dataframe of the questions in a group, could be N questions, likely 1 per version
 #ppt is the powerpoint object to add the slide
 
 
 #question_group_name - the name of the question group
 #question_df - the dataframe of the question group (`value` and `label` columns)
 #
 #DEBUG - question group names
 #question_group_name = names(question_list)[1]
 
 gen_question_slide<- function(ppt,question_df,question_group_name,cuts_df,cut_filter_threshold){
   #browser()
   # Make the slide
   ppt <- add_slide(ppt, layout = "question", master = "Office Theme")
   ppt <- ph_with(ppt,value = str_c(question_group_name),
                  location = ph_location_label(ph_label = "Title"))
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
##2A. Score PDF FNC ----
#q.score.plot <- plotHisto(x,"percent",str_c("V",version.num,":", question.num," Score Distribution"),"Score (%)","Count",version_palette[i],bin.width)
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
    scale_color_manual(values = version_palette) +
    scale_fill_manual(values = version_palette) +
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
##2B. Grade Counts table----
question_grades_table <- function(df){

    # Summarize grades by version
    grades_sum <- df %>%
    group_by(version, grade) %>%
    summarize(n = round(n(), 0), .groups = "drop")  # Summarize grade counts
  
  # Complete missing combinations of version and grade
  grades_sum <- grades_sum %>%
    complete(version, grade = grades, fill = list(n = 0))
  
  # Pivot to wide format
  grades_sum_wide <- grades_sum %>%
    pivot_wider(names_from = grade, values_from = n, values_fill = 0)

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

##2C. Make Summary Table ----

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

##2D. Cut Table ----
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

