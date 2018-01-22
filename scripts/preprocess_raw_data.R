library(tidyverse)


# Convert files between old and new ---------------------------------------
# Convert all ,-s to .-s in the new files, not just bvp but gsr files too
comma2dot <- 
    tibble(filename = list.files("data/0_raw/", ".txt", full.names = TRUE),
       number = str_replace(filename, "^data/0_raw/(\\d+)._.*", "\\1") %>% as.numeric()) %>% 
    filter(number > 150) %>% 
    mutate(file = map(filename, ~read_file(.x))) %>% 
    mutate(corrected_file = map(file, ~str_replace_all(.x, ",",".")))

# Save the corrected files
walk2(comma2dot$corrected_file, comma2dot$filename, ~write_file(.x, .y))

# Convert all tabs to ,-s in the new files
tab2comma <- 
    tibble(filename = list.files("data/0_raw/", ".txt", full.names = TRUE),
           number = str_replace(filename, "^data/0_raw/(\\d+)._.*", "\\1") %>% as.numeric()) %>% 
    filter(number > 150) %>% 
    mutate(file = map(filename, ~read_file(.x))) %>% 
    mutate(corrected_file = map(file, ~str_replace_all(.x, "\\t",",")))

# Save the corrected files
walk2(tab2comma$corrected_file, tab2comma$filename, ~write_file(.x, .y))

# Cut unimportant parts based on relative timing (256Hz) ------------------



# Remove all but the BVP value --------------------------------------------
input_dir <- "data/1_headerless/"
preproc_dir <- "data/2_bvp_bare/"
dir.create(preproc_dir)

df <- 
    tibble(filename = list.files(input_dir, "BVP")) %>% 
    mutate(data = map(filename, ~read_csv(glue("{input_dir}{.x}"), col_names = FALSE, col_types = "_d")))

walk2(df$data, df$filename, ~write_excel_csv(.x, glue("{preproc_dir}{.y}"), col_names = FALSE))

