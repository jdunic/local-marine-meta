library(RCurl)


# General stats/numbers for meta
study_site_counts <- 
data.frame('all_studies' = length(unique(fl_combined$Study.ID)), 
           'all_sites' = length(fl_combined$id),
           'noevent_studies' = length(unique(fl_noevent$Study.ID)), 
           'noevent_sites' = dim(unique(fl_noevent[, c('Study.ID', 'Site')]))[1], 
           'event_studies' = length(unique(fl_event$Study.ID)), 
           'event_sites' = dim(unique(fl_event[, c('Study.ID', 'Site')]))[1], 
           'negative_studies' = length(unique(fl_event[which(fl_event$Expected.Change.Direction == 'Negative'), ]$Study.ID)),
           'negative_sites' = dim(unique(fl_event[which(fl_event$Expected.Change.Direction == 'Negative'), ][, c('Study.ID', 'Site')]))[1], 
           'positive_studies' = length(unique(fl_event[which(fl_event$Expected.Change.Direction == 'Positive'), ]$Study.ID)),
           'positive_sites' = dim(unique(fl_event[which(fl_event$Expected.Change.Direction == 'Positive'), ][, c('Study.ID', 'Site')]))[1], 
           'uncertain_studies' = length(unique(fl_event[which(fl_event$Expected.Change.Direction == 'Uncertain'), ]$Study.ID)),
           'uncertain_sites' = dim(unique(fl_event[which(fl_event$Expected.Change.Direction == 'Uncertain'), ][, c('Study.ID', 'Site')]))[1]
           )
study_site_counts

original_paper_listURL <- getURL('https://docs.google.com/spreadsheet/pub?key=0AoWXIZo5tNL1dEQ5ZXhIeGNydHVVZ2VBaFA5Z1Byamc&single=true&gid=0&output=csv')
original_paper_list <- read.csv(textConnection(original_paper_listURL), 
                                na.strings=c("na", "NA", "", "Na", "nA", "N/A", 
                                             ".", "M", "No"), 
                                skip = 2)


paper_list1_URL <- getURL("https://docs.google.com/spreadsheet/pub?key=0Ami0dbUk6zuSdEhKNndpYlFkYmZMVV9CVW5FZmVOcHc&single=true&gid=5&output=csv")
paper_list1 <- read.csv(textConnection(paper_list1_URL), 
                                na.strings=c("na", "NA", "", "Na", "nA", "N/A", 
                                             ".", "M", "No"))

paper_list2_URL <- getURL("https://docs.google.com/spreadsheet/pub?key=0Ami0dbUk6zuSdEhKNndpYlFkYmZMVV9CVW5FZmVOcHc&single=true&gid=7&output=csv")
paper_list2 <- read.csv(textConnection(paper_list2_URL), 
                                na.strings=c("na", "NA", "", "Na", "nA", "N/A", 
                                             ".", "M", "No"))

dim(paper_list1)

initial_exclusion = dim(original_paper_list)[1] - dim(paper_list1)[1]
second_round = initial_exclusion - dim(original_paper_list)[1]

extracting_list <- rbind(paper_list1[, c('Study.ID', 'Completed.', 'Study.Used.')], 
                         paper_list2[, c('Study.ID', 'Completed.', 'Study.Used.')])

errorType <- gsub("\n", "", errorType)

length(extracting_list$Study.ID)
length(unique(extracting_list$Study.ID))