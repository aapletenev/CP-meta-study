meta_data =  read.csv("list_papers.csv")
paths_pdf = meta_data$File.Attachments

#split each item based on ; splitter and return on the element with main pdf
paths_pdf = sapply(strsplit(paths_pdf, ";"), function(x) {
  #check if the first element name include "pdf" and do not include "1-s2.0-" and do not include "41593_2007"
  for (i in 1:length(x)){
    if (grepl("pdf", x[i]) & !grepl("1-s2.0-", x[i]) & !grepl("41593_2007", x[i])){
      return(x[i])
    }
  }
})
#remove all spaces at the start of each element
paths_pdf = gsub("^\\s+|\\s+$", "", paths_pdf)

#create the folder "Papers" and copy all the pdfs there
dir.create("Papers")
#delete  all files from the folder
file.remove(list.files("Papers", full.names = TRUE))

#copy
for (i in 1:length(paths_pdf)){
  file.copy(from =  paths_pdf[i], to = "Papers")
}