# ===============================
# 02_cleaning_and_srqa.R
# Part 1: Clean transcription data, merge demographics 
# Part 2: Binarize pauses, compute sRQA metrics 
# Input: mu3d_sequential_timestamps.csv, final_analysis.csv
# Output: deception_srqa_with_pause_1120_final.csv
# ===============================

library(stringr)
library(dplyr)
library(readr)
library(sRQA)

# ===============================
# SET PATHS - update these to your local directory

data_dir <- "."  # UPDATE to your local data directory

# =============================================
# PART 1: CLEAN TRANSCRIPTION DATA

mu3d_sequential = read.csv(file.path(data_dir, "mu3d_sequential_timestamps.csv"))

# FIX ALL Video.Filename issues FIRST (before any other processing)
mu3d_sequential$Video.Filename <- gsub("^WM19_", "WM019_", mu3d_sequential$Video.Filename)
mu3d_sequential$Video.Filename <- gsub("^WM13_", "WM013_", mu3d_sequential$Video.Filename)
mu3d_sequential$Video.Filename <- gsub("^WM001_PT\\.wav$", "WM001_1PT.wav", mu3d_sequential$Video.Filename)

# Cleaning: get rid of punctuation, whitespace, and convert to lowercase for Word (except for [*])
mu3d_sequential$Word <- str_trim(str_to_lower(str_remove_all(mu3d_sequential$Word, "[^\\w\\s\\[\\*\\]]")))

# Make new col: participant ID (grab the first 5 characters of Video.Filename)
mu3d_sequential$participant_ID <- substr(mu3d_sequential$Video.Filename, 1, 5)

# Make new col: Word.Duration where we do Word.End-Word.Start 
mu3d_sequential$Word.Duration = mu3d_sequential$Word.End - mu3d_sequential$Word.Start

# Adding outcome data 
lookup <- read.csv(file.path(data_dir, "final_analysis.csv"))

mu3d_sequential_final <- mu3d_sequential %>%
  mutate(VideoID = gsub("\\.wav$", "", Video.Filename)) %>%
  left_join(lookup %>% select(VideoID, Valence, Veracity, Sex, Race, VidLength_sec, WordCount), 
            by = "VideoID")

# Reorder cols 
mu3d_sequential_final <- mu3d_sequential_final %>%
  select(participant_ID, everything(), -Video.Filename, -VideoID)

# =============================================
# PART 2: BINARIZE PAUSES AND COMPUTE sRQA
deception_data <- mu3d_sequential_final

#add binarized pause column
deception_data <- deception_data %>%
  mutate(
    #binary: 0 = no pause, 1 = has pause before
    Pause_binary = ifelse(Pause.Before.Word > 0, 1, 0)
  )

#convert POS to numeric, make readable labels
deception_data <- deception_data %>%
  mutate(
    Valence_label = ifelse(Valence == 1, "Positive", "Negative"),
    Veracity_label = ifelse(Veracity == 1, "Truth", "Lie")
  )

#re-aggregate with pause binary vector
participant_conditions_pause <- deception_data %>%
  arrange(participant_ID, Valence, Veracity, Segment.Start, Word.Start) %>% #sorts in this order
  group_by(participant_ID, Valence, Veracity) %>% #so that each participant has 4 rows
  summarise(
    n_words = n(), #count how many words per condition
    #collect pause binary vector
    pause_binary_vec = list(Pause_binary), #collects all binary values into a single vector 
    #demographics
    Sex = first(Sex),
    Race = first(Race),
    #labels 
    Valence_label = first(Valence_label),
    Veracity_label = first(Veracity_label),
    Condition = paste(first(Valence_label), first(Veracity_label), sep = "_"),
    #pause proportions
    prop_pauses = mean(Pause_binary),  #AKA what % of words have pauses
    
    .groups = "drop"
  )

#this is the final analysis csv before proceeding 

# ===============================
# PROCESS PAUSE BINARY (alr binary so no need to symbolize )

process_pause_binary <- function(row_data) {
  
  pid <- row_data$participant_ID #takes one participant at a time, per condition
  condition <- row_data$Condition
  
  ts <- unlist(row_data$pause_binary_vec) #extract the binary pause vector per row 
  ts <- as.numeric(ts) 
  ts <- ts[!is.na(ts)]
  
  if (length(ts) < 30) { #make sure each row has words in it. if it doesnt then return NA 
    return(data.frame(
      participant_ID = pid,
      Condition = condition,
      pause_RR = NA, pause_DET = NA, pause_L = NA, 
      pause_Lmax = NA, pause_DIV = NA, pause_ENTR = NA, 
      pause_LAM = NA, pause_TT = NA, pause_Vmax = NA,
      pause_VENTR = NA, pause_MRT = NA, pause_RTE = NA, 
      pause_NMPRT = NA, pause_TREND = NA
    ))
  }
  
  #check if there's variation (not all 0s or all 1s). if not return NA because can't symbolize 
  if (length(unique(ts)) < 2) {
    return(data.frame(
      participant_ID = pid,
      Condition = condition,
      pause_RR = NA, pause_DET = NA, pause_L = NA, 
      pause_Lmax = NA, pause_DIV = NA, pause_ENTR = NA, 
      pause_LAM = NA, pause_TT = NA, pause_Vmax = NA,
      pause_VENTR = NA, pause_MRT = NA, pause_RTE = NA, 
      pause_NMPRT = NA, pause_TREND = NA
    ))
  }
  
  result <- tryCatch({ #if an error, then return NA
    # already binary (0/1), treat as symbols directly
    # no symbolization needed - the pattern IS the data?
    smat <- symbolic_recurrence_matrix(ts, embedding_dimension = 1, time_delay = 1) #create recurrence matrix 
    #embedding dimension and time delay =1 is from example srqa from austen. ed=3 gave worse predictive results
    #embedding dimension - 1 looks at every pause event? timedelay - spacing btw points? 
    
    # compute RQA metrics
    rqa <- quant_rqa(smat)
    data.frame(
      participant_ID = pid,
      Condition = condition,
      pause_RR = rqa$RR,
      pause_DET = rqa$DET,
      pause_L = rqa$L,
      pause_Lmax = rqa$Lmax,
      pause_DIV = rqa$DIV,
      pause_ENTR = rqa$ENTR,
      pause_LAM = rqa$LAM,
      pause_TT = rqa$TT,
      pause_Vmax = rqa$Vmax,
      pause_VENTR = rqa$VENTR,
      pause_MRT = rqa$MRT,
      pause_RTE = rqa$RTE,
      pause_NMPRT = rqa$NMPRT,
      pause_TREND = rqa$TREND
    )
  }, error = function(e) { #if errors, return NA instead of crashing
    data.frame(
      participant_ID = pid,
      Condition = condition,
      pause_RR = NA, pause_DET = NA, pause_L = NA, 
      pause_Lmax = NA, pause_DIV = NA, pause_ENTR = NA, 
      pause_LAM = NA, pause_TT = NA, pause_Vmax = NA,
      pause_VENTR = NA, pause_MRT = NA, pause_RTE = NA, 
      pause_NMPRT = NA, pause_TREND = NA
    )
  })
  
  return(result)
}
#process all, and just show the progress to make sure it's working 
pause_results <- lapply(1:nrow(participant_conditions_pause), function(i) {
  if (i %% 20 == 0) cat("Pause:", i, "/", nrow(participant_conditions_pause), "\n")
  process_pause_binary(participant_conditions_pause[i,])
})

pause_rqa <- do.call(rbind, pause_results)

# ===============================
# MERGE PAUSE DATA INTO FINAL DATASET

final_data_with_pause <- participant_conditions_pause %>% #merge w original participant info 
  dplyr::select(participant_ID, Valence, Veracity, Valence_label, Veracity_label, 
                Condition, Sex, Race, n_words, prop_pauses, pause_binary_vec) %>%
  left_join(pause_rqa, by = c("participant_ID", "Condition"))

final_data_with_pause

write_csv(final_data_with_pause, 
          file.path(data_dir, "deception_srqa_with_pause_1120_final.csv"))