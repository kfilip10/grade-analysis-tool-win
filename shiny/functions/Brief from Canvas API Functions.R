# This file contains the logic to import the grade data, parse it, and create the brief

#takes a dataframe from a single sheet of the grade report generator excel template
#takes a version number, the grade breaks, grades, and a version color
#returns a list of dataframe elements pertaining for that version of the exam

import_WPR_excel <- function(list.df,numberVersions){
  #This gets the df list element for each version and pulls the required
  #data from the header and reformats the data to be clean
  gb <- list()
  
  for (i in 1:numberVersions){
    list.el <- parse_version(list.df[[i]], i,version.palette[[i]])
    gb[[i]] <- list.el
    rm(list.el)
  }
  
  #gb is
  # big summary table with all students
  # version header info
  
  #combines for summary df and question df
  # df.total is used for the summary data
  # df.q dataframe is used for the question data
  # df.q needs an entry for every student that took times the number of problems (247x8x2 roughly 3705)
  # question, score, concept, cut.page, max (points), v.label ("Version 1"), version (num)
  #i=1
  df.roster <- gb[[1]][[3]]
  #df.roster2 <- gb[[1]][[4]]
  
  #browser()
  did_not_take_any_exam <- df.roster$Name
  
  for(i in 1:numberVersions){
    df <- gb[[i]][[1]]
    df.head <- gb[[i]][[2]]
    
    #df total summarization
    if(i==1){
      df.total <- df %>% 
        select(ID,Section,Name,version,pre.grade,pre.percent,post.grade,post.percent,mge.percent,mge.grade,mge.points)
    }else{
      df.total <- rbind(df.total,gb[[i]][[1]]%>% select(ID,Section,Name,version,pre.grade,pre.percent,
                                                        post.grade,post.percent,mge.percent,mge.grade,mge.points))
    }
    vec.questions <- df.head$question
    
    df.q.t <- df %>% select(all_of(vec.questions))%>% pivot_longer(cols=all_of(vec.questions),names_to="question",values_to="score")
    
    df.q.t <- left_join(df.q.t,df.head,by="question")
    df.q.t$v.label <- str_c("Version ", i)
    df.q.t$version <- i
    
    if(i==1){
      df.q <- df.q.t
    }else{
      df.q <- rbind(df.q,df.q.t)
      rm(df.q.t)
    }
    
  }
  df.total$pre.percent <-df.total$pre.percent*100 
  df.total$post.percent <-df.total$post.percent*100 
  df.total$mge.percent <-df.total$mge.percent*100 
  did_not_take_any_exam <- did_not_take_any_exam[!(did_not_take_any_exam %in% df.total$Name)]
  
  #retrieve the section from df.roster for each element in did_not_take_any_exam
  no.entries <- df.roster[df.roster$Name %in% did_not_take_any_exam,]
  df.q$percent <- df.q$score/df.q$max
  df.q$grade <- sapply(df.q$percent,letter_grade,breaks,grades)
  df.q$percent <-     df.q$percent*100
  
  #Need to look at the 
  
  #find duplicates in df.total Student NAME
  duplicate.entries <- df.total[duplicated(df.total$ID),]
  dup.entries.df <- df.roster[df.roster$Name %in% duplicate.entries$Name,]
  
  #browser()
  #left join df.total and df.roster by ID
  df.total <- left_join(df.total,df.roster%>%select(ID,Course.ID,Assignment.ID),by="ID")
  list.df=list()
  list.df[[1]] <- df.total
  list.df[[2]] <- df.q
  list.df[[3]] <- dup.entries.df #these will be those that are in both versions
  list.df[[4]] <- no.entries #this will be those that are in neither version
  
  
  return(list.df)
}




parse_version <- function(df,versionNum,vers.color) {
  #takes the dataframe
  # Find the row that has ID in it
  #browser()
  row_ID <- which(apply(df, 1, function(row) any(row == "ID")))
  row_Section<- which(apply(df, 1, function(row) any(row == "Section")))
  if(row_ID==row_Section){
    # Then it is working
  }else{
    stop("Make sure your excel follows the template exactly. An error was encountered in locating the Student ID and Section Row")}
  #split the dataframe into the header and the data
  df.vers.header <- df[1:(row_ID-1),]
  df.data.colNames <- df[row_ID,]
  df.data <- df[(row_ID+1):nrow(df),]
  
  #set column names from df.data.colNames
  colnames(df.data) <- df.data.colNames
  concept_ID.col <- which(apply(df.vers.header, 1, function(col) any(grepl("concept", col, ignore.case = TRUE))))
  
  total.col <- which(grepl("Total", df.data.colNames, ignore.case = TRUE))[1]
  
  #max score for the exam, reads direct from excel in case we want
  #bonus points to be an option
  mge.max <- as.numeric(df.vers.header[[3,total.col]])
  
  #remove all columns after total from df.vers.header
  df.vers.header <- df.vers.header[,1:total.col-1]
  
  #Finds the indices for concepts, cut pages, and points
  col.start <- which(grepl("cut sheet", df.vers.header, ignore.case = TRUE))[1]
  cutID.row <- which(apply(df.vers.header, 1, function(row) any(grepl("cut sheet", row, ignore.case = TRUE))))
  concept.row <- cutID.row-1
  question.row <-  cutID.row+1
  df.vers.header <- df.vers.header[concept.row:question.row,col.start:ncol(df.vers.header)]
  #transpose df.vers.header
  df.vers.header <- t(df.vers.header)
  #remove rownames
  rownames(df.vers.header) <- NULL
  #set column names from first row of df.vers.header
  colnames(df.vers.header) <- df.vers.header[1,]
  df.vers.header <- data.frame(df.vers.header[-1,])
  
  #get the question numbers from df.data.colNames between Q1 and Total
  question.index <- which(grepl("Q1", df.data.colNames, ignore.case = TRUE))[1]
  questions <- df.data.colNames[question.index:(total.col-1)]
  q <- as.vector(t(questions[1,]))
  df.vers.header$question.num <- q
  colnames(df.vers.header) <- c("concept","cut.page","max","question")
  
  #make row 2 and 3 of df.vers.header numeric
  df.vers.header[,2:3] <- sapply(df.vers.header[,2:3],as.numeric)
  
  #remove entries that have a 0 in the total column from df.data
  #remove NA cols from df.data
  #find index of column by name
  col.start <-  which(colnames(df.data)=="ID")[1]
  col.end <- which(colnames(df.data)=="Assignment ID")[1]
  df.data <- df.data[,col.start:col.end]
  
  course.roster <- data.frame(df.data["ID"],
                              df.data["Instructor"],
                              df.data["Section"],
                              df.data["Name"],
                              df.data["Course ID"],
                              df.data["Assignment ID"])
  #remove entries with NA in the ID column
  course.roster <- course.roster[!is.na(course.roster$ID),]
  
  #convert all columns to numeric from just after name until the end
  col.Numeric<- which(colnames(df.data)=="Name")[1]+1
  
  df.data[,col.Numeric:ncol(df.data)] <- sapply(df.data[,col.Numeric:ncol(df.data)],as.numeric)
  
  #remove rows that have a 0 in the total column from df.data
  df.data <- df.data %>% filter(Total!=0)
  #if pre max is NA set it to 1
  df.data$`Pre Max`[is.na(df.data$`Pre Max`)] <- 1
  #if pre points is NA set it to 1
  df.data$`Pre Points`[is.na(df.data$`Pre Points`)] <- 1
  
  #extract the ID, section, and student name from df.data
  complete.Entries <- nrow(df.data)


  #finds any NA values in df.data2 and sets it to 0
  df.data[is.na(df.data)] <- 0
  df.version <- df.data
  
  df.version$version = str_c("Version ", versionNum) #declare version
  

  colnames(df.version)[colnames(df.version) == "Total"] <- "mge.points"
  colnames(df.version)[colnames(df.version) == "Pre Points"] <- "pre.points"
  colnames(df.version)[colnames(df.version) == "Pre Max"] <- "pre.max"
  
  #### Points calculating ####
  ##This fixes an error where it can't calculate the scores if the canvas instructor hasn't uploaded grades yet
  #if pre.points is NA, set it to 1
  df.version$pre.points[is.na(df.version$pre.points)] <- 1
  #if pre.max is NA set it to 1
  df.version$pre.max[is.na(df.version$pre.max)] <- 1
  
  df.version$pre.percent = df.version$pre.points/df.version$pre.max # calc post percent
  
  df.version$pre.grade = sapply(df.version$pre.percent,letter_grade,breaks,grades) #apply grade function
  
  df.version$mge.percent = df.version$mge.points/mge.max # calc MGE percent
  df.version$mge.grade = sapply(df.version$mge.percent,letter_grade,breaks,grades) #apply grade function
  
  df.version <-  df.version %>% mutate(post.max= df.version$pre.max+mge.max,
                                       post.score = df.version$pre.points + df.version$mge.points)
  
  df.version$post.percent = df.version$post.score/df.version$post.max # calc pre percent
  
  df.version$post.grade = sapply(df.version$post.percent,letter_grade,breaks,grades)
  
  version = list() #container for test version
  version[[1]]  <-  df.version
  version[[2]]  <-  df.vers.header
  version[[3]]  <-  course.roster
  return(version)
}





# Main function to make the powerpoint explicitly
# Input: 
# l is the output from the import_WPR_excel function
#courseTitle: string, from user input
#eventTitle: string, from user input
#numberVersions: numeric, from user input
#cutSheet: pdf object, from user upload.
#bin.width: numeric, from user slider input
#sortStyle: string from user selection of listed options
make_ppt <- function(l, courseTitle, eventTitle,cutSheet,bin.width,sortStyle,progress.tot){
  #### Variables ####
  df.total <- l[[1]]
  df.q <- l[[2]]
  duplicate.entries <- l[[3]]
  no.entries <- l[[4]]
  
  temp.dir.r <- file.path(tempDir,"r")
  if(!dir.exists(temp.dir.r)){dir.create(temp.dir.r)}

  numberVersions <- length(unique(df.total$version))
  #Palette for versions

  #### ::Slide Style and locations ####
  caption_style <- fp_text(font.size = 20,font.family = "Calibri")
  
  ####Load PPT based on input data####
  ppt <- read_pptx(str_c(wd,"/www/template.pptx")) 
  
  
  for(n in rev(seq_len(length(ppt)))){
    remove_slide(ppt,n)
  }
  #### TITLE SLIDE ####
  ppt <- add_slide(ppt, layout = "Title Slide", master = "Office Theme")
  ppt <- ph_with(ppt,value = str_c(courseTitle,": ",eventTitle," Outbrief"),location = ph_location_label(ph_label = "Title"))
  ppt <- ph_with(ppt, value = str_c("Number of Versions: ",numberVersions), location = ph_location_type(type = "subTitle"))
  incProgress(1/progress.tot, detail = paste("Making Title Slide"))
  #### PRE/POST ####
  
  ####:Table avg comparison ####
  
  # Count, mean medium min and max tables 
  
  pre.stats <- df.total %>% 
    mutate(pre.percent = pre.percent,
           post.percent = post.percent,
           mge.percent = mge.percent) %>% 
    summarise(Type="Pre",
              Number = n(),
              Average = mean(pre.percent),
              Median = median(pre.percent),
              Min = min(pre.percent),
              Max =max(pre.percent))
  
  post.stats <- df.total%>% 
    mutate(pre.percent = pre.percent,
          post.percent = post.percent,
          mge.percent = mge.percent)   %>% 
    summarise(Type="Post",
              Number = n(),
              Average = mean(post.percent),
              Median = median(post.percent),
              Min = min(post.percent),
              Max =max(post.percent)) 
  
  comp.stats <- rbind(pre.stats,post.stats)
  
  comp.stats.ft <- comp.stats %>% flextable() %>% align_nottext_col(align = "center") %>%    
    fontsize(size=16,part = "header")%>%autofit()%>%
    fontsize(size=18)%>%
    align_text_col(align = "center") %>% colformat_double( digits = 1)  %>%
    set_caption(    as_paragraph(
      as_chunk("Pre/Post Score Comparisons", props = caption_style)
    ), word_stylename = "Table Caption"
    )
  #show(comp.stats.ft)
  
  #### :Table grades ####
  comp.grades <-  data.frame(grades.desc)
  
  for (i in 1:length(grades.desc)){
    n <- length(which(df.total$pre.grade==grades.desc[i]))
    n2 <- length(which(df.total$post.grade==grades.desc[i]))
    #n <- length(grep(paste("^",grades[i],"$", sep=""), df.total$pre.grade))
    comp.grades[i,2] <- n
    comp.grades[i,3] <- n/length(df.total$pre.grade)*100
    comp.grades[i,4] <- n2
    comp.grades[i,5] <- n2/length(df.total$pre.grade)*100
  }
  colnames(comp.grades)[1] <- "Grade"
  colnames(comp.grades)[2] <- "Pre"
  colnames(comp.grades)[3] <- "Pre %"
  colnames(comp.grades)[4] <- "Post"
  colnames(comp.grades)[5] <- "Post %"
  comp.grades$Percent.Change <- round((comp.grades$Post - comp.grades$Pre)/(comp.grades$Pre)*100,1)
  
  comp.long <- comp.grades %>% select(Grade,Pre,Post) %>% pivot_longer(cols=c('Pre','Post'),
                                                                       names_to='type',
                                                                       values_to='count')
  incProgress(1/progress.tot, detail = paste("Wrangling a moose"))
  
  comp.grades.new <-  data.frame(grades)
  
  for (i in 1:length(grades)){
    n <- length(which(df.total$pre.grade==grades[i]))
    n2 <- length(which(df.total$post.grade==grades[i]))
    #n <- length(grep(paste("^",grades[i],"$", sep=""), df.total$pre.grade))
    comp.grades.new[i,2] <- n
    comp.grades.new[i,3] <- n2
  }
  colnames(comp.grades.new)[1] <- "Grade"
  colnames(comp.grades.new)[2] <- "Pre Count"
  colnames(comp.grades.new)[3] <- "Post Count"
  comp.grades.new$`Percent Change` <- percent((comp.grades.new$Post - comp.grades.new$Pre)/(comp.grades.new$Pre),accuracy=1)
  
  comp.grades.df <- comp.grades.new %>% t() %>% as.data.frame() %>% rownames_to_column() 
  names <- comp.grades.df[1,]
  colnames(comp.grades.df) <- names
  comp.grades.df <- comp.grades.df[-1,]
  
  #conditional formatting for grade table
  colormatrix <- apply(comp.grades.df[, -1], 1:2, function(x) ifelse(grepl("-", x), "#FDEFEF", "#F4FFEF"))
  colormatrix[1:2,] <- "white" 
  
  comp.grades.ft <- comp.grades.df%>% flextable() %>% align_nottext_col(align = "center") %>%
    align_text_col(align = "center") %>% colformat_double( digits = 1) %>%  
    bg(j = 2:12, bg=colormatrix) %>%
    fontsize(size=20,part = "header")%>%
    fontsize(size=18)%>%
    set_caption(    as_paragraph(
      as_chunk("Pre/Post Grade Comparisons", props = caption_style)
    ), word_stylename = "Table Caption"
    )
  
  pre.A.count <- as.numeric(comp.grades.df[[1,10]])+as.numeric(comp.grades.df[[1,11]])+
    as.numeric(comp.grades.df[[1,12]])
  pre.DF.count <- as.numeric(comp.grades.df[[1,2]])+as.numeric(comp.grades.df[[1,3]])
  pre.count <- pre.stats$Number
  post.A.count <- as.numeric(comp.grades.df[[2,10]])+as.numeric(comp.grades.df[[2,11]])+
    as.numeric(comp.grades.df[[2,12]])
  post.DF.count <- as.numeric(comp.grades.df[[2,2]])+as.numeric(comp.grades.df[[2,3]])
  post.count <- post.stats$Number
  vers.grades.header <- c("",grades)
  
  comp.grades.ft <- add_footer_row(comp.grades.ft,
                                   values = c("POST D/F's:",str_c(post.DF.count," (",round(post.DF.count/post.count*100,1),"%)"),
                                              "POST A's:",str_c(post.A.count," (",round(post.A.count/post.count*100,1),"%)")),
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
  #### :Plots####
  
  #Comparison plot for pre-post
  df.pre <- df.total %>% select(pre.percent)
  df.pre$type <- "Pre-Test"
  colnames(df.pre)[1] <- "percent"
  df.post <- df.total %>% select(post.percent)
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
  
  #### :Slides####
  ppt <- add_slide(ppt, layout = "Summary", master = "Office Theme")
  ppt <- ph_with(ppt,value = "Pre/Post Course Score Comparisons",location = ph_location_label(ph_label = "Title"))
  
  ## Adding plots
  ppt <- ph_with(ppt, value = p.comp, location = ph_location(left = 0.1, top = 4,width=8,height=4))
  
  ## Adding tables
  ppt <- ph_with(ppt,value = comp.grades.ft,location=ph_location(left=2,top=1))
  ppt <- ph_with(ppt,value = comp.stats.ft,location=ph_location(left=8.5,top=5))
  
  ## Large Plot Slide
  #ppt <- add_slide(ppt, layout = "One Chart", master = "Office Theme")
  #ppt <- ph_with(ppt,value = str_c("Pre-Post Comparison"),location = ph_location_label(ph_label = "Title"))
  #ppt <- ph_with(ppt,value = p.comp,location = ph_location_label(ph_label = "chart"))
  
  
  #### 2 version pre test population comparison ####
  ppt <- pop.pre.t.test(df.total,ppt)
  
  
  #### students who didn't take it ####
  
  no.entries.ft <- no.entries %>% flextable() %>% align_nottext_col(align = "center") %>%
    align_text_col(align = "center") %>% colformat_double( digits = 1) %>%  
    fontsize(size=14)%>%
    fontsize(size=14,part = "header")%>%
    autofit() %>%
    set_caption(    as_paragraph(
      as_chunk("Cadets who are not included in these versions", props = caption_style)
    ), word_stylename = "Table Caption"
    )
  
  #show(no.entries.ft)
  ppt <- add_slide(ppt, layout = "chart and table", master = "Office Theme")
  ppt <- ph_with(ppt,value = "Cadets not accounted for in this brief",location = ph_location_label(ph_label = "Title"))
  ppt <- ph_with(ppt,value = no.entries.ft,location = ph_location(left = 2.5, top = 1,width=8,height=4))
  
  #### VERSION SUM ####
  
  
  #### :Comparison Table ####
  df.vers.sum <- df.total %>% group_by(version) %>% summarise("Count"=n(),
                                                              "Mean (%)"=round(mean(mge.percent),1),
                                                              "Median (%)"=round(median(mge.percent),1),
                                                              "Std. Dev (%)"=round(sd(mge.percent),1),
                                                              "Min (%)"=round(min(mge.percent),1),
                                                              "Max (%)"=round(max(mge.percent),1),
                                                              "# D/F's"=sum(mge.grade=="D"|mge.grade=="F"),
                                                              "# A's"=sum(mge.grade=="A"|mge.grade=="A+"|mge.grade=="A-"))
  df.vers.sum <- df.vers.sum %>% mutate("# D/F's"=str_c(`# D/F's`," (",round(`# D/F's`/`Count`*100,0),"%)"),
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
  
  version.palette <- setNames(version.palette,unique(df.vers.sum$version))
  
  version.unique <- unique(df.total$version)
  df.total$version <- factor(df.total$version, rev(version.unique))
  #### :Box and Whisker ####
  v.comp <-ggplot(df.total, aes(x=version, y=mge.percent, fill=version)) +
    geom_hline(yintercept=60, linetype="dashed",linewidth=1, color = "red")+
    geom_boxplot(alpha=0.5)+  scale_fill_manual(values=version.palette)+
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
  
  #### EACH VERSION####
  #### Version summary ####
  for (i in 1:length(unique(df.total$version))){
    df.version <- df.total %>% filter(version==unique(df.total$version)[i])
    #df.version <- gb[[i]][[1]] 
    incProgress(1/progress.tot, detail = paste("Adding Version ",i, "Summary"))
    
    vers.sum <- df.vers.sum %>% filter(version==paste0("Version ",i))
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
    v.plot1 <- plotHisto(df.version,
                         "mge.percent",
                         str_c("Version ",i," Score Distribution"),
                         "Score (%)",
                         "Count",
                         version.palette[i],bin.width)
    #show(v.plot1)
    
    # Version bar plot for grades 
    df.version$mge.grade <- factor(df.version$mge.grade, grades)
    v.plot2 <- df.version%>%count(mge.grade)%>%mutate(label=paste(n))%>%
      ggplot(aes(x=factor(mge.grade),y=n)) + 
      geom_col(width=0.7,fill=version.palette[i]) +
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
  
  
  # IF concept grouping = TRUE then group by concept factored by version, 
  
  #Use an lapply 
  # X is the concepts
  # in the function
  # For each version 
  # group by version and function
  # Make PPT slide
  
  # df.q is all questions for all versions

  
  #Empty grades list for data that doesn't have all grades
  grades.empty <- as.data.frame(grades)
  colnames(grades.empty) <- c("grade")
  grades.empty$n <- 0
  #x <- df.q.list[[1]]
  #### Question Slide Function ####
  question_slide_concepts <- function(x){
    version.num <- unique(x$version)
    cut.page <- unique(x$cut.page)
    question.num <- unique(x$question)
    concept <- unique(x$concept)
    incProgress(1/progress.tot, detail = paste("Adding Version ",version.num, "Question ",
                                               question.num))
    
    
    
    
    ####Make graph top left####
    q.grades <- x%>%count(grade)
    
    grades.missing <- grades[!(grades %in% q.grades$grade)]
    
    # Append the missing values to the dataframe
    if (length(grades.missing) > 0) {
      new_rows <- data.frame(grade = grades.missing, n =0)
      q.grades <- rbind(q.grades, new_rows)
    }
    
    q.grades$grade <- factor(q.grades$grade, grades)
    q.grades   <- q.grades  %>%mutate(label=paste(n))
    
    q.grades.plot <- q.grades %>% ggplot(aes(x=factor(grade),y=n)) + 
      geom_col(width=0.7,fill=version.palette[version.num]) +
      geom_text(
        aes(y=-0.15*max(n),label = label),
        vjust=0,
        color="black"
        ,size=7,
        nudge_y=0)+
      theme_hc()+
      theme(text = element_text(size = 18))+
      labs(x="",y="Count")
    #show(q.grades.plot)
    ####Make Table middle####
    df.q.t.sum <- x %>% summarise("Count"=n(),
                                  "Mean (%)"=round(mean(percent),1),
                                  "Std. Dev (%)"=round(sd(percent),1),
                                  "# D/F's"=sum(grade=="D"|grade=="F"),
                                  "# A's"=sum(grade=="A"|grade=="A+"|grade=="A-"))
    df.q.t.sum <- df.q.t.sum %>% mutate("# D/F's"=str_c(`# D/F's`," (",round(`# D/F's`/`Count`*100,0),"%)"),
                                        "# A's"=str_c(`# A's`," (",round(`# A's`/`Count`*100,0),"%)")
    )
    
    df.q.sum.ft <- flextable(df.q.t.sum) %>% align_text_col(align = "center") %>%
      align_nottext_col(align = "center") %>% 
      colformat_double( digits = 1)  %>% 
      fontsize(size=20,part = "header")%>%
      fontsize(size=18)%>%
      color(color="red",j=4)%>%
      color(color="forestgreen",j=5)%>%
      bold(j=4:5)%>%
      autofit()
    #show(df.q.t.sum)
    #### Make graph bottom left####
    #q.score.plot <- plotHisto(x,"percent",str_c("V",version.num,":", question.num," Score Distribution"),"Score (%)","Count",version.palette[i],bin.width)

    #plotHisto <- function(df,col,title,xaxis,yaxis,color,bin.width){
    df <- x
    col <- "percent"
    xaxis <- "Score (%)"
    yaxis <- "Count"
    color <- version.palette[version.num]

      max.counts <- max(hist(df[[col]], breaks=seq(0,max(df[[col]])+bin.width,by=bin.width), plot=FALSE)$counts)
      max.pts <- max(x$max)
      
      q.score.plot <- ggplot(df,aes(get(col))) + 
        geom_histogram(stat="bin",binwidth=bin.width,fill="gray",col="black",closed="left",boundary=0) + 
        theme_hc()+
        labs(x=xaxis,y="Count")+
        geom_density(aes(y = after_stat(scaled)*max.counts),fill=color, colour = "black", #*max.counts (multiply by after_stat(scaled)*max.counts)
                     position = "identity", alpha = 0.07,adjust=0.5) +
        scale_y_continuous(name = yaxis, breaks = round(seq(0,max.counts,max.counts/5)))+
        geom_vline(xintercept=88,linewidth=1,linetype="dashed",color="forestgreen")+
        geom_vline(xintercept=76,linewidth=1,linetype="dashed",color="blue")+
        geom_vline(xintercept=60,linewidth=1,linetype="dashed",color="red")+
        theme(text = element_text(size = 18))+
          scale_x_continuous(name = xaxis,breaks=seq(0,max(df[[col]]),bin.width),sec.axis = sec_axis(~ .*max.pts/100, name = "Points",
                                                 breaks = seq(0, max(df[[col]]) *max.pts/100, 5))) 

    
    
    
    
    
    ppt <- add_slide(ppt, layout = "question", master = "Office Theme")
    #### Add to slide####
    ppt <- ph_with(ppt,value = str_c("V",version.num,":",question.num,"   ",concept),location = ph_location_label(ph_label = "Title"))
    ppt <- ph_with(ppt,value = q.grades.plot,location = ph_location(left=0,top=.75,width=7,height=3))
    ppt <- ph_with(ppt,value = df.q.sum.ft,location = ph_location(left=0,top=3.75,width=7,height=1.5))
    ppt <- ph_with(ppt,value = q.score.plot,location = ph_location(left=0,top=5,width=7,height=3))
    
    #### Bring in cut sheet ####
    if(!is.null(cutSheet)){
      img.path = paste0(tempDir,"image",version.num,cut.page,".png")
      cutSheet[cut.page] %>% image_write(., img.path, format = "png")
      ppt <- ph_with(ppt, value = external_img(img.path), location = ph_location_label(ph_label = "cut sheet"))
      
    }
    
    
    
    
    return(cut.page)
  } 
  
  #Get list of unique concepts
  concept.unique <- unique(df.q$concept)
  
  #Get list of unique version labels
  v.label.unique <- unique(df.q$v.label)
  
  #If concept grouping is true:
  df.q$v.label <- factor(df.q$v.label,version.unique)
  df.q$concept <- factor(df.q$concept,concept.unique)
  #concept.exp <- expand.grid(v.label.unique,concept.unique)
  
  #ELSE
  #df.q$question <- factor(df.q$concept,concept.unique)
  # Get the number of questions in each version c(seq(1,4))
  # Expand so it's generic Q# and version
  # so Q#1-n and version
  # Then call new function to iterate through questions by version
  #concept.exp <- expand.grid(v.label.unique,numberQuestions)
  #test.list <- apply(concept.exp,1,question_slide_concepts)
  
  #factor to preserve question order in brief
  df.q$question <- factor(df.q$question, levels = unique(df.q$question))
  
  if(sortStyle=="Group By Concept"){
    df.q.list <- split(df.q,f=list(df.q$version,df.q$concept))
    df.q.list <- df.q.list[sapply(df.q.list,function(x) nrow(x)>0)]
    questions.slides <- lapply(df.q.list,question_slide_concepts)
    
    #Sort by concept
    #create a list of dataframes, one for each question
    #sorted by group and version
    
  }else{
    #sort by question order
    #splits into grouping by version and question
    df.q.list <- split(df.q,f=list(df.q$question,df.q$version))
    df.q.list <- df.q.list[sapply(df.q.list,function(x) nrow(x)>0)]
    questions.slides <- lapply(df.q.list,question_slide_concepts)
    
  }
  return(ppt)
  
}


# create a function to compare the two version populations score before the test
# Takes df.total, version palette (make that global?) 
pop.pre.t.test <- function(df.total,ppt){
  
  version.palette <- setNames(version.palette,unique(df.total$version))
  
  P <- ggplot(df.total, aes(x = pre.percent, fill = version)) +
    geom_density(alpha = 0.2)+
    theme_bw()+theme(text = element_text(size = 20),)+
    labs(title = "Pre-WPR Scores for Version populations", x = "Score (%)", y = "Density")+
    scale_fill_manual(values=version.palette)
  
  
  df.vers.sum <- df.total %>% group_by(version) %>% summarise("Count"=n(),
                                                              "Mean (%)"=round(mean(pre.percent),1),
                                                              "Median (%)"=round(median(pre.percent),1),
                                                              "Std. Dev (%)"=round(sd(pre.percent),1),
                                                              "# D/F's"=sum(pre.grade=="D"|pre.grade=="F"),
                                                              "# A's"=sum(pre.grade=="A"|pre.grade=="A+"|pre.grade=="A-"))
  df.vers.sum <- df.vers.sum %>% mutate("# D/F's"=str_c(`# D/F's`," (",round(`# D/F's`/`Count`*100,0),"%)"),
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
  if(length(unique(df.total$version))==2){
    t.test.results <- t.test(pre.percent ~ version,data=df.total, var.equal=FALSE)
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


#Makes a histogram and pdf for grade data
#df is the dataframe
#col is the column name within the dataframe to be plotted

plotHisto <- function(df,col,title,xaxis,yaxis,color,bin.width){
  max.counts <- max(hist(df[[col]], breaks=seq(0,max(df[[col]])+bin.width,by=bin.width), plot=FALSE)$counts)
  
  P <- ggplot(df,aes(get(col))) + 
    geom_histogram(stat="bin",binwidth=bin.width,fill="gray",col="black",closed="left",boundary=0) + 
    theme_hc()+
    labs(title = title,x=col,y="Count")+
    geom_density(aes(y = after_stat(scaled)*max.counts),fill=color, colour = "black", #*max.counts (multiply by after_stat(scaled)*max.counts)
                 position = "identity", alpha = 0.07,adjust=0.5) +
    scale_x_continuous(name = xaxis,breaks=seq(0,max(df[[col]]),bin.width)) + 
    scale_y_continuous(name = yaxis, breaks = round(seq(0,max.counts,max.counts/5)))+
    geom_vline(xintercept=88,linewidth=1,linetype="dashed",color="forestgreen")+
    geom_vline(xintercept=76,linewidth=1,linetype="dashed",color="blue")+
    geom_vline(xintercept=60,linewidth=1,linetype="dashed",color="red")+
    theme(text = element_text(size = 18))#,
  #sec.axis = sec_axis(~.x/nrow(df)/10*100, name = "Density"))
  return(P)
}


#Takes a vector of grades for an event and the A-F grades vector
#Counts the numbers by grade, determines percents, counts As and D/Fs
#Formats into a flextable and returns the flextable
grade.table <- function(df.grades,grades){
  vers.grades <- data.frame(grades)
  
  for (k in 1:length(grades)){
    n <- length(which(df.grades==grades[k]))
    #n <- length(grep(paste("^",grades[i],"$", sep=""), df.total$pre.grade))
    vers.grades[k,2] <- n
    vers.grades[k,3] <- str_c(round(n/length(df.grades)*100,1),"%")
  }
  colnames(vers.grades)[1] <- 'Grade'
  colnames(vers.grades)[2] <- 'Count'
  colnames(vers.grades)[3] <- 'Percent'
  rownames(vers.grades) <- grades
  # Create a factor vector with the desired order
  vers.A.count <- vers.grades[[1,2]]+vers.grades[[2,2]]+vers.grades[[3,2]]
  vers.F.count <- vers.grades[[10,2]]+vers.grades[[11,2]]
  vers.tot <- sum(vers.grades$Count)
  vers.grades.header <- c("",grades)
  ft <-  vers.grades %>% select(Count,Percent) %>% t() %>% as.data.frame() %>% rownames_to_column() %>%  flextable()   %>% 
    colformat_double( digits = 1) %>% set_header_labels(values = vers.grades.header) %>%
    align_nottext_col(align = "center") %>% align(align = "center", part = "footer") %>% 
    width(width=0.625,unit="in") %>%
    void(j = 1, part = "body") %>%
    width(j=1,0)
  
  ft <- add_footer_row(ft ,values = c("A's:",str_c(vers.A.count," (",round(vers.A.count/vers.tot*100,1),"%)"),
                                      "D/F's:",str_c(vers.F.count," (",round(vers.F.count/vers.tot*100,1),"%)")),
                       colwidths=c(3,3,3,3),top=TRUE)%>% align_text_col(align = "center") %>%
    align(i = NULL, j = 1, align = "right", part = "footer") %>%
    align(i = NULL, j = 4, align = "left", part = "footer") %>%
    color(i=1,j=c(1,4),color="forestgreen",part="footer") %>%
    align(i = NULL, j = 7, align = "right", part = "footer") %>%
    align(i = NULL, j = 10, align = "left", part = "footer") %>%
    color(i=1,j=c(7,8,9,10,11),color="#c21313",part="footer") %>%
    fontsize(size=12)  %>%
    fontsize(size=18,part="footer") %>%
    bold(bold=TRUE,part="footer")
  
  l = list()
  l[[1]] <- ft
  l[[2]] <- vers.grades
  return(l)
}

