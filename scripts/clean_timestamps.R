# Correct puzzle timestamps based on sound file length
library(tidyverse)
library(googlesheets)
library(hms)


correct_times <- 
    gs_title("puzzle_biograph-hang hossz") %>% 
    gs_read(1, col_types = "ccc") %>% 
    mutate(biograph_length = paste0("0:",biograph_length) %>% parse_hms(),
           sound_file_length = paste0("0:",sound_file_length) %>% parse_hms(),
           time_correction = as.numeric(biograph_length)/as.numeric(sound_file_length))

puzzle_markers <-
    gs_title("Charlotte_puzzle_kieg_KL  2.xls") %>% 
    gs_read("osszes", col_types = "ccccc") %>% 
    drop_na(id) %>%
    rename(marker_name = marker) %>% 
    left_join(correct_times %>% select(id, time_correction), by = "id") %>%
    # filter(!is.na(time_correction)) %>% # This is here for test purposes only
    mutate(
        time = if_else(!is.na(time_correction), 
                        (parse_hms(time) %>% as.numeric() * time_correction %>% round()), 
                       parse_hms(time) %>% as.numeric()
                       )
        ) %>% 
    select(id, session, time, marker_name)
    
aap_markers <-
    gs_title("Bence_AAPvideokódolás_javitott_kl.xls") %>% 
    gs_read("vegleges", col_types = "ccccc") %>% 
    drop_na(id) %>%
    rename(marker_name = marker) %>% 
    mutate(time = parse_hms(time) %>% as.numeric()) %>% 
    select(id, session, time, marker_name) %>% 
    arrange(id, time)

all_markers <- 
    bind_rows(aap_markers, puzzle_markers) %>% 
    group_by(id, session) %>% 
    mutate(marker = 1:n())

    
write_excel_csv(all_markers, "data/all_markers_clean.csv", na = "")
