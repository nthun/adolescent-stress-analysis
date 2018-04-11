# This file contains the steps to process SCR data
# 1. Raw scr is read to one object from all files
# 2. Downsampled to 32 Hz
# 3. 

# Add markers to gsr files

library(tidyverse)
library(fs)
library(hms)
library(fuzzyjoin)

source("scripts/downsample.R")
source("scripts/near_inner_join.R")

# Increase memory limit (this somehow worked to process these enormous data objects, otherwise didn't work)
memory.limit(12000000)

# dir.create("data/3_gsr_with_markers")

markers <- read_csv("data/all_markers_clean.csv", col_types = "ccici")



# Read all SCR data and put to one data frame
raw_scr <-
    tibble(
        # Read all SCR files, and parse varaibles from file names (important for joining markers)
        file = list.files("data/1_headerless/", "_SCR.txt")) %>%
    separate(file, c("id", "session", "measurement"), sep = "_", remove = FALSE) %>%
    mutate(role = str_extract(id, ".$")) %>% 
    mutate( measurement = str_replace(measurement, ".txt", ""),
            data = map(file,
                       ~ read_csv(paste0("data/1_headerless/", .x), 
                                  col_names = FALSE, 
                                  col_types = "dd") %>% 
                           select(time = 1, value = 2))) %>% 
    unnest(data)


# Downsample all SCR data to 32Hz
scr_ds <- 
    raw_scr %>% 
    select(-time, scr = value) %>% 
    group_by(file, id, session, measurement) %>% 
    nest() %>% 
    mutate(scr = map(data, ~downsample(.x, "scr", 256, 32))) %>%
    select(-data) %>% 
    unnest(scr)

# Save downsampled data
write_excel_csv(scr_ds, "data/3_downsampled/scr_32hz.csv")
scr_ds <- read_csv("data/3_downsampled/scr_32hz.csv")

scr_clean <-
    scr_ds %>% 
    left_join(markers, by = c("id", "session", "time")) %>% 
    group_by(file, id, session) %>% 
    # Fill up all rows with marker names, but only in groups
    fill(marker_name, marker) %>% 
    ungroup() %>% 
    # Drop all rows that don't have markers. This happens mostly in the beginning of files
    drop_na(marker_name) %>% 
    # Remove all "end" markers, as those are uninetresting, from now on
    filter(!str_detect(marker_name, " end")) %>% 
    # Change the marker names, as those don't need start anymore
    mutate(marker_name = str_remove(marker_name, " start"))

# Create marker levels for plotting    
marker_levels <-
    scr_clean %>% 
    distinct(marker_name) %>% 
    pull()

scr_sum <-
    scr_clean %>% 
    group_by(file, id, session, measurement, marker_name, marker) %>% 
    summarise(pos_change = change(value, "positive", mean)) %>% 
    ungroup() %>% 
    mutate(marker_name =  fct_relevel(marker_name, marker_levels)) %>% 
    group_by(file, id, session, marker_name) %>%
    summarise(scr_increase = mean(pos_change, na.rm = TRUE)) %>% 
    ungroup() %>% 
    arrange(id, session, marker_name) %>% 
    select(-file) %>% 
    group_by(id) %>% 
    mutate(scr_increase_std = scr_increase %>% scale() %>% as.numeric()) %>% 
    ungroup()

scr_sum %>% 
    print(n = 100)

scr_sum %>% 
    mutate(role = str_extract(id, ".$")) %>% 
    group_by(id) %>% 
    mutate(scr_increase = scr_increase %>% scale() %>% as.numeric()) %>% 
    group_by(session, role, marker_name) %>% 
    summarise(mean_increase = mean(scr_increase),
              se = sd(scr_increase)/sqrt(n())) %>% 
    ggplot() +
        aes(y = mean_increase, x = marker_name) +
        geom_col() +
        geom_linerange(aes(ymax = mean_increase + se, ymin = mean_increase - se)) +
        facet_grid(role~session, scales = "free_x")


write_excel_csv(scr_sum, "data/4_summarised/scr_summarised.csv")


# BUGFIX ------------------------------------------------------------------

markers %>% filter(str_detect(id, "174") & session == "PUZZLE")

scr_clean %>% 
    filter(str_detect(id, "174") & session == "PUZZLE") %>% 
    distinct(marker_name)

scr_puzzle_ <-
scr_ds  %>% 
    filter(session == "PUZZLE") %>% 
    near_inner_join(markers %>% filter(session == "PUZZLE"), id_col = "id", num_col = time)

                  