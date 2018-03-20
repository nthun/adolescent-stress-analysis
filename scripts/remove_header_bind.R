# Bind the associated files together
# Also, strip all headers from the beginning of files

library(tidyverse)

headerless_dir <- "data/1_headerless/"
# dir.create(headerless_dir)

# Bind separated files together, and remove the header --------------------
partial_files <-
    tibble(
        # Get all files with a number at the end of filename
        file = list.files("data/0_raw/", "_\\d.txt")) %>%
    separate(file, c("id", "session", "measurement", "piece"), sep = "_", remove = FALSE) %>% 
    mutate( piece = str_replace(piece, ".txt", ""),
            data = map(file,
                ~ read_csv(paste0("data/0_raw/", .x), skip = 8, col_names = FALSE, col_types = "dd") %>% 
                    select(time = 1, value = 2)),
            # Extract the last timesamp from each file
            last_time = map_dbl(data, ~last(.x$time)),
            # Set the last time for the second parts as 0, as they are irrelevant
            last_time = if_else(piece == 2, 0, last_time),
            # Correct the time of the second recording with the last time of the first
            data = map2(data, lag(last_time, default = 0), ~ mutate(.x, time = time + .y))) %>% 
    unnest(data) %>% 
    group_by(id, measurement) %>% 
    nest(time, value) %>% 
    mutate(filename = str_glue("{id}_AAP_{measurement}.txt"))

# Write the files
walk2(partial_files$data,
      partial_files$filename,
      ~ write_excel_csv(.x, path = str_glue("{headerless_dir}{.y}"), na = "", col_names = FALSE))

# Remove header for all the other files (to make them the same) -----------
# Get all files that does not contain numbers at the end
good_files <- 
    tibble(filename = setdiff(list.files("data/0_raw/"), list.files("data/0_raw/", "\\d.txt$"))) %>% 
    mutate(data = map(filename,
                      ~ read_csv(str_glue("data/0_raw/{.x}"), 
                                 skip = 8, 
                                 col_names = FALSE
                                 # col_types = "dd"
                                 ) #%>% 
                          # select(time = 1, value = 2)
                      )
           )

# Write the files
walk2(good_files$data,
      good_files$filename,
      ~ write_excel_csv(.x, path = str_glue("{headerless_dir}{.y}"), na = "", col_names = FALSE))
