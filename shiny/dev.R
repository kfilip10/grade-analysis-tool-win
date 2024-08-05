#### DEV / DEBUG BLOCK ####
#### Things I run while debugging NOT USED IN THE MAIN APP####

# saveRDS(df,file ="env/uploaded template graded.rds")


df <- readRDS("env/uploaded template graded.rds")

#### loading ####


read_excel

cutSheet=NULL
file.pdf <- "C:/Users/kev/OneDrive - West Point/6. R Projects/grade report/WPR2 cut.pdf"
pdf <- magick::image_read_pdf(file.pdf)
#### Load list manually #######################################################
file.excel <- "C:/Users/kev/OneDrive - West Point/6. R Projects/template 18JAN.xlsx"

list.df <- list()
versions <- excel_sheets(file.excel)
numberVersions <- length(versions)
if(length(versions)!=numberVersions){
  #Message: You said you have three versions but uploaded a different number of sheets
  print("test")
  
}else{
  versions <- tolower(versions)
  for(n in 1:numberVersions){
    sheetN <- match(1,str_detect(versions,as.character(n)))
    if(is.na(sheetN)){
      sheetN <- match(1,str_detect(versions,as.character(as.english(n))))
      if(is.na(sheetN)){
        print("Make sure you only have one sheet for each version and there is a number '1' or 'one' corresponding to the version number in the sheet name")
        
      }
      else{  list.df[[n]] <- read_excel(file.excel,sheetN)
      }
    }
    else{  list.df[[n]] <- read_excel(file.excel,sheetN)
    }
  }
}

#parse_version <- function(df,versionNum,vers.color) {

i=1
versionNum = i

vers.color <- version.palette[[i]]
df <- list.df[[i]]

incProgress <- function(n,detail){
  s = detail
  print(s)
  return(NULL)
}
numberVersions <- 1
a <- import_WPR_excel(list.df,numberVersions)

df.total <- a[[1]]
df.q <- a[[2]]
duplicate.entries <- a[[3]]
no.entries <- a[[4]]

#make_ppt <- function(l, courseTitle, eventTitle, numberVersions,cutSheet,bin.width,sortStyle,progress.tot){
courseTitle="PHYS 201" #courseTitle
eventTitle="WPR3" #eventTitle
bin.width = 5

#### Run full ppt function ####
ppt1 <- make_ppt(a[[1]],a[[2]], courseTitle, eventTitle, numberVersions,pdf,bin.width,)

print(ppt1,target="ppt-draft.pptx")
print(ppt,target="ppttest.pptx")





#### Box and whisker for v1 pre vs v2 pre ####
df.v.pop.comp <- df.total %>% group_by(version) %>% 
  summarise(pre.avg <- mean(pre.percent), pre.sd <-sd(pre.percent) )

v1.pre <- df.total %>%filter(version=="Version 1") %>% select(pre.percent)
v2.pre <- df.total %>%filter(version=="Version 2") %>% select(pre.percent)

mean(v2.pre$pre.percent)

shapiro.test(v1.pre$pre.percent)
version.palette <- setNames(version.palette,unique(df.total$version))

ggplot(df.total, aes(x = pre.percent, fill = version)) +
  geom_density(alpha = 0.2)+
  theme_bw()+theme(text = element_text(size = 20),)+
labs(title = "Pre-WPR Scores for v1 and v2 populations", x = "Score (%)", y = "Density")+
  scale_fill_manual(values=version.palette)
  
t.test(pre.percent ~ version,data=df.total, var.equal=FALSE)

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


show(df.vers.sum.ft)
print(df.vers.sum.ft, preview = 'docx') # this will open a new word file with the table pasted in



#### Testing snippets #### 
numquestions <- df.q %>% group_by(version) %>% summarise(questions = n_distinct(question))

numConcepts <- df.q %>% summarise(questions = n_distinct(concept))

question.Conc.Comp <- !any(FALSE %in% apply(numquestions,1,function(x)
  if(x[2]==numConcepts[1]){
    return(TRUE)
  }
else{
  print(FALSE)
}))

if(question.Conc.Comp){
  return(str_c("There are ", numConcepts[1], " unique concepts and each version has that many questions. 
              It is recommended to group by concept"))
}else{
  return(str_c("There are ", numConcepts[1], " unique oncepts which exceeds the number of questions. 
              It is recommended to group by question."))
}

#If each version questions = numConcepts then 
# The number of concepts is equal to the number of questions in each version,
#it is recommended to sort by concepts

# Else: There are X concepts but only Y questions. It is advised to group by question number  

print("There are ", numConcepts[1], "Concepts and each version has ")

test[,2:3]
apply(test,1,function(x) if(x[2]!=x[3]){print(x)}else{str_c("Version ",x[1],"has a mismatched number of questions ")})


#### CANVAS TESTING ####
# import_canvas <- function(gb.csv, currPoints.str, currScore.str, rowAssignments, rowStudents){

gb.csv <-  c("C:/Users/kev/OneDrive - West Point/6. R Projects/gb1.csv",
             "C:/Users/kev/OneDrive - West Point/6. R Projects/gb1 - Copy.csv")
currPoints.str <- "Current Points"
currScore.str <- "Current Score"
section.str <- "Section"
student.str<- "Student"
studentID.str <- "ID"
rowStudents <- 4
tempDir <- tempdir()
#gb.csv <-  "C:/Users/kev/OneDrive - West Point/6. R Projects/gb1.csv"

data <- lapply(gb.csv, import_canvas,
               student.str=student.str,
               studentID.str=studentID.str,
               section.str=section.str,
               currPoints.str=currPoints.str, 
               currScore.str=currScore.str,
               rowStudents= rowStudents)

#This is the reactive combined_data()
gb <- do.call(rbind, data)
#changes the third and fourth columns into numbers
#gb.comb[,3:4] <- sapply(gb.comb[,3:4],as.numeric)




#gb.comb <- combined_data()
#find the score column based on if the column name contains the string "score"
score.col <- grep("score",colnames(gb.comb),ignore.case = TRUE)
points.col <- grep("point",colnames(gb.comb),ignore.case = TRUE)
if(length(score.col)==0|length(points.col)==0){
# stop function and inform user to change the column names
  stop("Please change the canvas column names to include the word 'score' (for their course percentage) and/or 'point'(for their points) in the column names 
       so I can find the one that is correct")
}

#divide the current points by the score to get the max points
gb.comb[,ncol(gb.comb)+1] <- round(gb.comb[,points.col]/gb.comb[,score.col]*100,1)
#change the column name to "max points"
colnames(gb.comb)[ncol(gb.comb)] <- "Max Points"



#Open the input template excel file in the www folder
wb <- loadWorkbook(file.path(getwd(),'www',"input template.xlsx"))

# Get the names of sheets in the workbook

#write student names
writeData(wb, sheet = 1, x = data_to_insert, startRow = 3, startCol = 3, colNames = FALSE)

#write IDs

#find point
# Example data frame to be inserted as a column
data_to_insert <- data.frame(
  NewColumn = c("Value1", "Value2", "Value3") # Replace this with your column data
)

# Add the column data to each sheet starting at row 3, column C (third column)
for (sheet_name in sheet_names) {
  # Adjust startRow and startCol based on the position where you want to insert the column
  # Here, startRow = 3 and startCol = 3 represent row 3 and column C (third column)
}

# Save the modified workbook
saveWorkbook(wb, file = "modified_file.xlsx", overwrite = TRUE)

#df.csv <- df.total %>% select(ID,`CADET NAME`,Section,mge.points)

grades.csv[2]*100
as.numeric(grades.csv[2])
#convert the second column of grades.csv to a number
grades.csv[[2]] <- as.numeric(grades.csv[[2]])*100

min(grades.csv[[2]])
max(grades.csv[[2]])


#### API ####
settings_path <- file.path(Sys.getenv("USERPROFILE"),"Documents",settings_folder)
canvas_api_token_path <- file.path(settings_path,"token.rds")

canvas_api_token <- "15453~eJc7obNn7wertD2Z6jpEcLzMvn1n1xVPp2sBQfmM3SYGk2X7PY48hUMsbBgfxbHW"
saveRDS(canvas_api_token, canvas_api_token_path)
domain = "https://westpoint.instructure.com"

set_canvas_token(canvas_api_token)
set_canvas_domain(domain)

g <- get_course_list() %>% select(id)




#### Dynamic Table Testing 4AUG ####

#finds the year within the parenthesis
course_list_df <-  course_list_df%>%mutate(tableUI_year = str_extract(name, "\\b\\d{4}-[12](?=-)"))

#finds the name right after the parenthesis
course_list_df <-  course_list_df%>%  mutate(tableUI_course = str_extract(name, "^[^(]+"))

#Finds the four digit alphanumeric hours number
course_list_df <- course_list_df %>%
  mutate(tableUI_hours = str_extract(name, "\\b[A-Z0-9]{4}\\b(?=\\s+\\d+)"))

# Extract the numeric pattern following the alphanumeric code
course_list_df <- course_list_df %>%
  mutate(tableUI_section = str_extract(str_extract(name, "\\b[A-Z0-9]{4}\\b\\s+\\d+"), "\\d+"))

hours <- course_list_df$name %>% str_extract("\\b[A-Z0-9]{4}\\b\\s+\\d+")
# extract the section from hours, which is the ending numeric
section <- str_extract(hours, "\\d+\\s*$")

course_list_df <- course_list_df %>%
  mutate(tableUI_section = section)
hours <- str_extract(hours, "\\b[A-Z0-9]{4}\\b")
course_list_df <- course_list_df %>%
  mutate(tableUI_hours = hours)


