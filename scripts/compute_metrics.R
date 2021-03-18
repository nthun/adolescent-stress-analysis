# Calculate RMSSD and mean HR

library(tidyverse)
library(vroom)

# Read marker file
markers_notime <- 
    read_csv("data/all_markers_clean.csv") %>% 
    select(-time) %>% 
    mutate(marker_name = str_remove(marker_name, " start"))

ibis <-
    list.files("data/81_ibi_converted_filtered/", full.names = TRUE) %>%
    set_names(.) %>%
    map_dfr(read_csv, col_names = "ibi", .id = "filename")

hrv <- 
    ibis %>% 
    group_by(filename) %>% 
    mutate(ssd = (ibi - lag(ibi))^2) %>% 
    summarise(
              rmssd = sqrt(mean(ssd, na.rm = TRUE)),
              meanhr = 60000/mean(ibi, na.rm = TRUE),
              .groups = "drop")

hrv_summary <-
    hrv %>% 
    extract(filename,
            into = c("id", "session", "marker"),
            regex = "data/81_ibi_converted_filtered/(.*)_(.*)_(.*).txt",
            convert = TRUE) %>%
    arrange(id, session, marker) %>% 
    # Add marker names
    left_join(markers_notime, by = c("id", "session", "marker")) %>% 
    filter(abs(scale(rmssd)) < 3)
    

hrv_wide <- 
    hrv_summary %>% 
    select(-marker_name) %>% 
    pivot_wider(names_from = c(session, marker),
                values_from = c(rmssd, meanhr))

# Save final hrv file ---------------------------------------------------------------

write_excel_csv(hrv_summary, "data/4_summarised/hrv_summary.csv")
write_excel_csv(hrv_wide, "data/4_summarised/hrv_wide.csv")

