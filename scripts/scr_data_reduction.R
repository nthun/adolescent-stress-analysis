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
source("scripts/change.R")

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

scr_with_markers <-
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
    scr_with_markers %>% 
    distinct(marker_name) %>% 
    pull()

scr_sum <-
    scr_with_markers %>% 
    group_by(file, id, session, measurement, marker_name, marker) %>% 
    # Calculate SC positive variable
    summarise(pos_change = change(value, "positive", mean)) %>% 
    ungroup() %>% 
    mutate(marker_name =  fct_relevel(marker_name, marker_levels)) %>% 
    group_by(file, id, session, marker_name) %>%
    summarise(scr_increase = mean(pos_change, na.rm = TRUE)) %>% 
    ungroup() %>% 
    arrange(id, session, marker_name) %>% 
    select(-file) %>% 
    ungroup()

scr_sum %>% 
    mutate(role = str_extract(id, ".$")) %>% 
    # group_by(id, session) %>% 
    # mutate(scr_increase = scr_increase %>% scale() %>% as.numeric()) %>% 
    group_by(session, role, marker_name) %>% 
    summarise(mean_increase = mean(scr_increase),
              se = sd(scr_increase)/sqrt(n())) %>% 
    ggplot() +
        aes(y = mean_increase, x = marker_name) +
        geom_col() +
        geom_linerange(aes(ymax = mean_increase + se, ymin = mean_increase - se)) +
        facet_grid(role~session, scales = "free_x")

# Add order and reactivity to the table
scr_summarised <-
    scr_sum %>% 
    group_by(id, session) %>% 
    mutate(baseline = if_else(marker_name == "Baseline", scr_increase, NA_real_)) %>% 
    fill(baseline) %>% 
    mutate(scr_reactivity = scr_increase - baseline) %>% 
    group_by(id) %>% 
    mutate(scr_increase_std = scr_increase %>% scale() %>% as.numeric()) %>% 
    arrange(id, session, -scr_increase_std) %>% 
    group_by(id, session) %>%
    mutate(order_scr_increase = row_number()) %>% 
    arrange(id, session, marker_name) %>% 
    ungroup() %>% 
    select(-baseline)

write_excel_csv(scr_summarised, "data/4_summarised/scr_summarised.csv")

# Calculate which task/picture yielded the largest mean for each participant for each task
scr_largest <- 
    scr_sum %>% 
    arrange(id, session, -scr_increase) %>% 
    group_by(id, session) %>%
    slice(1) %>% 
    rename(largest_scr_increase = marker_name) %>% 
    select(-starts_with("scr"))
write_excel_csv(scr_largest, "data/4_summarised/scr_largest.csv")
    

# Show which picture was associated with the largest scr increase most frequently
scr_largest %>% 
    mutate(role = str_extract(id, "\\D")) %>% 
    ggplot() +
        aes(x = largest_scr_increase) +
        geom_bar() +
        facet_grid(role~session, scale = "free_x")


# 3. Calculate an aggregate variable for tasks (AAP: 2-8 pic, 1 person and 2 person pics; Puzzle: all tasks) and baseline separately

# AAP dyadic pics: 3, 5, 6; 1-person pics: 2, 4, 7, 8

aap_dyadic <- c("Picture three", "Picture five", "Picture six")
aap_single <- c("Picture two", "Picture four", "Picture seven", "Picture eight")

scr_aggregated <- 
    scr_sum %>% 
    mutate(aggregated_marker = case_when(marker_name %in% aap_dyadic ~ "aap_dyadic",
                                         marker_name %in% aap_single ~ "aap_single",
                                         session == "AAP" & marker_name == "Baseline" ~ "aap_baseline",
                                         str_detect(marker_name, "Puzzle") ~ "puzzle_task",
                                         session == "PUZZLE" & marker_name == "Baseline" ~ "puzzle_baseline",
                                         )) %>% 
    drop_na(aggregated_marker) %>% 
    group_by(id, session, aggregated_marker) %>%
    summarise(scr_increase = mean(scr_increase, na.rm = TRUE)) %>% 
    mutate(baseline = if_else(str_detect(aggregated_marker, "baseline"), scr_increase, NA_real_)) %>% 
    fill(baseline) %>% 
    mutate(scr_increase_std = scale(scr_increase),
           scr_reactivity = scr_increase - baseline) %>% 
    ungroup() %>% 
    select(-baseline) %>% 
    arrange(id, session, aggregated_marker)

write_excel_csv(scr_aggregated, "data/4_summarised/scr_aggregated.csv")

scr_aggregated %>% 
    group_by(session, aggregated_marker) %>% 
    summarise(mean_increase = mean(scr_increase_std),
              se = sd(scr_increase_std)/sqrt(n())) %>%     
    ggplot() +
        aes(y = mean_increase, x = aggregated_marker) +
        geom_col() +
        geom_linerange(aes(ymax = mean_increase + se, ymin = mean_increase - se)) +
        facet_grid(~session, scales = "free_x")




