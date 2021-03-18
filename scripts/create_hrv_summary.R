# Merge HRV data

library(tidyverse)
library(vroom)
library(janitor)

theme_set(theme_light())
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
  extract(
          filename,
          into = c("id", "session", "marker"),
          regex = "data/9_hrv_by_marker/(.*)_(.*)_(.*)_hrvResults.txt",
          convert = TRUE) %>%
  # Clean the hrv variable names
  mutate(variable = make_clean_names(string = variable) %>%
           str_remove("_\\d+$")) %>%
  # Add marker names
  left_join(markers_notime, by = c("id", "session", "marker")) %>%
  pivot_wider(names_from = variable,
              values_from = value) %>%
  arrange(id, session, marker) %>%
  filter(abs(scale(rmssd)) < 3)


# Save final hrv file ---------------------------------------------------------------

write_excel_csv(hrv_summary, "data/4_summarised/hrv_summary.csv")

hrv_summary %>% 
  ggplot() +
  aes(x = rmssd, y = hf_abs) +
  geom_point()

hrv_summary %>% 
  select(id, session, rmssd, hf_abs, hf_percent) %>% 
  pivot_longer(cols = c(rmssd, hf_abs, hf_percent),
               names_to = "hrv",
               values_to = "value") %>% 
  ggplot() +
  aes(y = value, x = hrv) +
  geom_boxplot(outlier.alpha = .2) +
  facet_wrap(~hrv, scales = "free")

hrv_summary %>% 
  filter(scale(rmssd) < 3) %>% 
  select(id, session, rmssd, hf_abs, lf_abs, hf_percent, meanhr, sdnn, recordingtime_s) %>% 
  pivot_longer(cols = -c(id, session),
               names_to = "hrv",
               values_to = "value") %>% 
  ggplot() +
  aes(x = value) +
  geom_histogram() +
  facet_wrap(~hrv, scales = "free")


qplot(log(hrv_summary$hf_abs), bins = 60)
