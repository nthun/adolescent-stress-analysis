# Correct puzzle timestamps based on sound file length
library(tidyverse)
library(googlesheets)
library(hms)


sampling_rate <- 256

correct_times <- 
    gs_title("puzzle_biograph-hang hossz") %>% 
    gs_read(1, col_types = "ccc") %>% 
    mutate(biograph_length = paste0("0:",biograph_length) %>% parse_hms(),
           sound_file_length = paste0("0:",sound_file_length) %>% parse_hms(),
           time_correction = as.numeric(biograph_length)/as.numeric(sound_file_length))


puzzle_markers <- 
    gs_title("Charlotte_puzzle") %>% 
    gs_read(1, col_types = "ccccc") %>% 
    drop_na(id) %>%
    rename(marker_name = marker) %>% 
    left_join(correct_times %>% select(id, time_correction), by = "id") %>%
    # filter(!is.na(time_correction)) %>%
    mutate(
        time = if_else(!is.na(time_correction), (parse_hms(time) %>% as.numeric() * time_correction) %>% 
                                round() %>% 
                                as.hms(), parse_hms(time)),
        sample_time = as.numeric(time) * sampling_rate
        ) %>% 
    select(id, session, sample_time, marker_name)
    
aap_markers <- 
    gs_title("Bence_AAPvideokódolás") %>% 
    gs_read(1, col_types = "ccccc") %>% 
    drop_na(id) %>%
    rename(marker_name = marker) %>% 
    mutate(sample_time = parse_hms(time) %>% as.numeric() * sampling_rate) %>% 
    select(id, session, sample_time, marker_name) %>% 
    arrange(id, sample_time)

all_markers <- 
    bind_rows(aap_markers, puzzle_markers) %>% 
    mutate(marker = 1L) %>% 
    arrange(id, session, sample_time)
    
write_excel_csv(all_markers, "data/all_markers_clean.csv", na = "")


all_markers %>% 
    filter(!str_detect(marker_name, ".* end$"))
