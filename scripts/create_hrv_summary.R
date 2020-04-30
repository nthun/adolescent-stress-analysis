# Merge HRV data

library(tidyverse)
library(vroom)
library(janitor)

# Read marker file
markers_notime <- 
    read_csv("data/all_markers_clean.csv") %>% 
    select(-time) %>% 
    mutate(marker_name = str_remove(marker_name, " start"))

# Read raw data
by_marker_raw <- 
    vroom(list.files("data/9_hrv_by_marker/", full.names = TRUE),
      id = "filename",
      col_names = c("variable", "value")) %>% 
    select(-X3) 


# Process data ----------------------------------------------------------------------

hrv_summary <- 
    by_marker_raw %>% 
    # Create id variables from filename
    extract(filename, 
            into = c("id", "session", "marker"), 
            regex = "ibi_(.*)_(.*)_(.*)_hrvResults.txt", 
            convert = TRUE) %>% 
    # Clean the hrv variable names 
    mutate(variable = make_clean_names(string = variable) %>% 
                      str_remove("_\\d$")) %>%
    # Add marker names
    left_join(markers_notime, by = c("id", "session", "marker")) %>% 
    pivot_wider(names_from = variable, 
                values_from = value) %>% 
    arrange(id, session, marker)


# Save final hrv file ---------------------------------------------------------------

write_excel_csv("data/4_summarised/hrv_summary.csv")
