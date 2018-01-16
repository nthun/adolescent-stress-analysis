library(tidyverse)


# Convert files between old and new ---------------------------------------
# Convert all ,-s to .-s in the new files, not just bvp but gsr files too
comma2dot <- 
    tibble(filename = list.files("data/raw/", ".txt", full.names = TRUE),
       number = str_replace(filename, "^data/raw/(\\d+)._.*", "\\1") %>% as.numeric()) %>% 
    filter(number > 150) %>% 
    mutate(file = map(filename, ~read_file(.x))) %>% 
    mutate(corrected_file = map(file, ~str_replace_all(.x, ",",".")))

# Save the corrected files
walk2(comma2dot$corrected_file, comma2dot$filename, ~write_file(.x, .y))

# Convert all tabs to ,-s in the new files
tab2comma <- 
    tibble(filename = list.files("data/raw/", ".txt", full.names = TRUE),
           number = str_replace(filename, "^data/raw/(\\d+)._.*", "\\1") %>% as.numeric()) %>% 
    filter(number > 150) %>% 
    mutate(file = map(filename, ~read_file(.x))) %>% 
    mutate(corrected_file = map(file, ~str_replace_all(.x, "\\t",",")))

# Save the corrected files
walk2(tab2comma$corrected_file, tab2comma$filename, ~write_file(.x, .y))

# Cut unimportant parts based on relative timing (256Hz) ------------------



# Remove all but the BVP value --------------------------------------------
preproc_library <- "data/bvp_preproc"
dir.create(preproc_library)

df <- 
    tibble(filename = list.files("data/raw/", "BVP", full.names = TRUE)) %>% 
    mutate(data = map(filename, ~read_csv(.x, skip = 8, col_names = FALSE, col_types = "_d")))%>% 
    mutate(new_filename = str_replace(filename, "data/raw", preproc_library))

walk2(df$data, df$new_filename, ~write_csv(.x, .y, col_names = FALSE))

