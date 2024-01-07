# This file is nearly identical to the brief_canvas functions but uses a backup excel template

#Takes a score, determines which letter grade it receives based on 
#score breaks passed from a csv


letter_grade <- function(score, breaks, grades) {
  n <- length(breaks)
  for (i in 1:(n-1)) {
    if (score < breaks[i+1]) {
      grade <- grades[i] # found the grade the score belongs to
      break  # exit the for-loop
    }
  }
  grade
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




#takes a dataframe from a single sheet of the grade report generator excel template
#takes a version number, the grade breaks, grades, and a version color
#returns a list of elements pertaining to that version of the exam
parse_version_manual <- function(df,versionNum,breaks,grades,vers.color) {
  #takes the dataframe
  version = list() #container for test version
  df.version = df #removes entries with NAs
  
  question.index = agrep("questions",df.version[1,],max.distance=3,value=FALSE)
  if (length(question.index)==0){
    question.index <- (length(df.version[1,])-2)
    #stop("Make sure you have a 'questions' cell to the left of your first question in your excel following the template")
    #could also add error handling here
  }

  #first row of first version, gets length minus question index
  numberQuestions <- length(df.version[1,])-question.index 
  
  questions = df.version[1,(question.index+1):length(df.version[1,])] #First row is where question headers are
  colnames(questions) = questions
  concepts = df.version[2,(question.index+1):length(df.version[2,])] #Second row is where concepts are
  colnames(concepts) = questions  
  cutpages = as.numeric(df.version[3,(question.index+1):length(df.version[3,])])
  questionPoints =  data.frame(as.numeric(df.version[4,(question.index+1):length(df.version[4,])]))
  df.version = df.version[5:nrow(df.version),] #take out header info
  df.version = df.version[complete.cases(df.version), ] #removes entries with NAs
  
  df.version[,] = lapply(df.version[,],as.numeric) #turn everything into numeric
  df.version$version = str_c("Version ", versionNum) #declare version
  max.pts = as.numeric(sum(questionPoints))
  df.version$mge.max = max.pts #max points for test
  
  colnames(df.version)[question.index+1:numberQuestions] = questions #rename columns to questions
  colnames(df.version)[1] = "pre.max" #rename columns
  colnames(df.version)[2] = "pre.score" #rename columns
  df.version$pre.percent = df.version$pre.score/df.version$pre.max # calc post percent
  df.version$pre.grade = sapply(df.version$pre.percent,letter_grade,breaks,grades) #apply grade function
  
  df.version$mge.score = rowSums(df.version[,question.index+1:numberQuestions]) #calc MGE sum by row
  df.version$mge.percent = df.version$mge.score/df.version$mge.max # calc MGE percent
  df.version$mge.grade = sapply(df.version$mge.percent,letter_grade,breaks,grades) #apply grade function
  
  df.version = df.version %>% mutate(post.max= df.version$pre.max+df.version$mge.max,
                                     post.score = df.version$pre.score + df.version$mge.score)
  
  df.version$post.percent = df.version$post.score/df.version$post.max # calc pre percent
  
  df.version$post.grade = sapply(df.version$post.percent,letter_grade,breaks,grades)
  
  version[[1]] = df.version
  version[[2]] = questions
  version[[3]] = concepts
  version[[4]] = cutpages
  version[[5]] = questionPoints
  version[[6]] = vers.color
  rm(df.version,questions,concepts,cutpages,questionPoints)
  return(version)
}



import_excel_manual <- function(list.df,numberVersions){
  version.palette<-brewer.pal(6,"Dark2")
  wd <- getwd() 
  
  # grades.file <- paste0(wd,"/www/GradeThresholds.csv")
  # grades.csv <- read_csv(grades.file,show_col_types = FALSE)
  # grades <- rev(as.vector(unlist(grades.csv[,1])))
  # grades.desc <- as.vector(unlist(grades.csv[,1]))
  # breaks <- rev(as.vector(unlist(grades.csv[,2])))
  # breaks.desc <- as.vector(unlist(grades.csv[,2]))
  # breaks[12] <- Inf #have to add the last as infinite
  
  #### Parse Data ####
  #This gets the df list element for each version and pulls the required
  #data from the header and reformats the data to be clean
  gb <- list()
  for (i in 1:numberVersions){
    list.el <- parse_version_manual(list.df[[i]], i,breaks,grades,version.palette[[i]])
    gb[[i]] <- list.el
    rm(list.el)
  }
  
  
  #combines for summary df and question df
  # df.total is used for the summary data
  # df.q dataframe is used for the question data
  
  #i=1
  for(i in 1:numberVersions){
    df <- gb[[i]][[1]]
    vec.questions <- as.character(gb[[i]][[2]][1,])
    vec.concepts <- gb[[i]][[3]][1,] %>% pivot_longer(cols=everything(),names_to="question",values_to="concept")
    vec.cut.page  <- gb[[i]][[4]] %>% t()
    vec.concepts$cut.page <-   as.numeric(vec.cut.page)
    vec.pts  <- gb[[i]][[5]] %>% t()
    vec.concepts$max <-   as.numeric(vec.pts)
    df.q.t <- df %>% select(all_of(vec.questions)) %>% pivot_longer(cols=all_of(vec.questions),names_to="question",values_to="score")
    df.q.t <- left_join(df.q.t,vec.concepts,by="question")
    df.q.t$v.label <- str_c("Version ", i)
    df.q.t$version <- i
    if(i==1){
      df.total <- df %>% 
        select(version,pre.grade,pre.percent,post.grade,post.percent,mge.percent,mge.grade)
      df.q <- df.q.t
    }
    else{
      df.total <- rbind(df.total,gb[[i]][[1]]%>% select(version,pre.grade,pre.percent,post.grade,post.percent,mge.percent,mge.grade))
      df.q <- rbind(df.q,df.q.t)
      rm(df.q.t)}
  }
  rm(vec.questions,vec.concepts,vec.cut.page,vec.pts,df)
  df.total$pre.percent <-df.total$pre.percent*100 
  df.total$post.percent <-df.total$post.percent*100 
  df.total$mge.percent <-df.total$mge.percent*100 
  
  list.df=list()
  list.df[[1]] <- df.total
  list.df[[2]] <- df.q
  
  
  return(list.df)
}

# Main function to make the powerpoint explicitly
# Input: list.df - list of dataframes, each df is a sheet from the grade excel template
#courseTitle: string, from user input
#eventTitle: string, from user input
#numberVersions: numeric, from user input
#cutSheet: pdf object, from user upload.
#bin.width: numeric, from user slider input
#sortStyle: string from user selection of listed options
manual_ppt <- function(df.total,df.q, courseTitle, eventTitle, numberVersions,cutSheet,bin.width,sortStyle,progress.tot){
  #### Variables ####
  temp.dir <- str_c(tempdir(),"\\r\\")
  if(!dir.exists(temp.dir)){dir.create(temp.dir)}
  wd <- getwd() #copied
  #Palette for versions
  version.palette<-brewer.pal(6,"Dark2")
  
  #### ::Slide Style and locations ####
  caption_style <- fp_text(font.size = 20,font.family = "Calibri")

  
  #load the grades break file into the function for assigning letter grades
  grades.file <- paste0(wd,"/www/GradeThresholds.csv")
  grades.csv <- read_csv(grades.file,show_col_types = FALSE)
  grades <- rev(as.vector(unlist(grades.csv[,1])))
  grades.desc <- as.vector(unlist(grades.csv[,1]))
  breaks <- rev(as.vector(unlist(grades.csv[,2])))
  breaks.desc <- as.vector(unlist(grades.csv[,2]))
  
  breaks[12] <- Inf #have to add the last as infinite

  
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
  
  pre.stats <- df.total %>% mutate(pre.percent = pre.percent,
                                       post.percent = post.percent,
                                       mge.percent = mge.percent) %>% 
                                  summarise(Type="Pre",
                                      Number = n(),
                                      Average = mean(pre.percent),
                                      Median = median(pre.percent),
                                      Min = min(pre.percent),
                                      Max =max(pre.percent))
  post.stats <- df.total%>% mutate(pre.percent = pre.percent,
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
  ppt <- ph_with(ppt,value = df.vers.sum.ft,location = ph_location(left = 2.5, top = 6,width=8))
  
  ppt <- ph_with(ppt,value = v.comp,location = ph_location(left = 2.8, top = .8,width=8,height=5))
  
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
        aes(y=-4,label = label),
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
    ppt <- ph_with(ppt,value = vers.sum.ft,location = ph_location(left = 2.5, top = 6,width=8,height=1.5))
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
    df.q$percent <- df.q$score/df.q$max
    df.q$grade <- sapply(df.q$percent,letter_grade,breaks,grades)
    df.q$percent <-     df.q$percent*100
    
    #Empty grades list for data that doesn't have all grades
    grades.empty <- as.data.frame(grades)
    colnames(grades.empty) <- c("grade")
    grades.empty$n <- 0
    
    #### Question Slide Function ####
    question_slide_concepts <- function(x){
      version.num <- unique(x$version)
      cut.page <- unique(x$cut.page)
      question.num <- unique(x$question)
      concept <- unique(x$concept)
      incProgress(1/progress.tot, detail = paste("Adding Version ",version.num, "Question ",question.num))



    
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
          aes(y=-10,label = label),
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
      q.score.plot <- plotHisto(x,"percent",str_c("V",version.num,":", question.num," Score Distribution"),"Score (%)","Count",version.palette[i],bin.width)
      
      
      ppt <- add_slide(ppt, layout = "question", master = "Office Theme")
      #### Add to slide####
      ppt <- ph_with(ppt,value = str_c("V",version.num,":",question.num,"   ",concept),location = ph_location_label(ph_label = "Title"))
      ppt <- ph_with(ppt,value = q.grades.plot,location = ph_location(left=0,top=.75,width=7,height=3))
      ppt <- ph_with(ppt,value = df.q.sum.ft,location = ph_location(left=0,top=3.75,width=7,height=1.5))
      ppt <- ph_with(ppt,value = q.score.plot,location = ph_location(left=0,top=5,width=7,height=3))
      
      #### Bring in cut sheet ####
      if(!is.null(cutSheet)){
        img.path = paste0(temp.dir,"image",version.num,cut.page,".png")
        cutSheet[cut.page] %>% image_write(., img.path, format = "png")
        ppt <- ph_with(ppt, value = external_img(img.path), location = ph_location_label(ph_label = "cut sheet"))
        
      }
      

      
      
      return(cut.page)
    } 
    
    df.q$concept <-paste(df.q$question,df.q$concept,sep=".")
    #Get list of unique concepts
    concept.unique <- unique(df.q$concept)
    
    #Get list of unique version labels
    v.label.unique <- unique(df.q$v.label)
    
    #IF concept grouping is true:
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




