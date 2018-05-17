# Bind the associated files together
# Also, strip all headers from the beginning of files

library(tidyverse)

headerless_dir <- "data/1_headerless/"
# dir.create(headerless_dir)

# Bind separated files together, and remove the header --------------------
partial_files <-
    tibble(
        # Get all files with a number at the end of filename
        file = list.files("data/0_raw_new/csv", "_\\d.txt")) %>%
    separate(file, c("id", "session", "measurement", "piece"), sep = "_", remove = FALSE) %>% 
    mutate( piece = str_replace(piece, ".txt", ""),
            data = map(file,
                ~ read_csv(paste0("data/0_raw_new/csv/", .x), skip = 8, col_names = FALSE, col_types = "dd") %>% 
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
# There are two formats in two directories. Read them separately, and bind them together later
good_files_csv <- 
    tibble(filename = setdiff(list.files("data/0_raw_new/csv/"), list.files("data/0_raw_new/csv/", "\\d.txt$"))) %>% 
    mutate(data = map(filename,
                      ~ read_csv(str_glue("data/0_raw_new/csv/{.x}"), 
                                 skip = 8, 
                                 col_names = FALSE,
                                 col_types = "dd") %>% 
                          select(time = 1, value = 2)
                      )
           )

good_files_tsv <- 
    tibble(filename = list.files("data/0_raw_new/tsv/")) %>% 
    mutate(data = map(filename,
                      ~ read_delim(str_glue("data/0_raw_new/tsv/{.x}"), 
                                 skip = 8, 
                                 delim = "\t",
                                 locale = locale(decimal_mark = ","),
                                 col_names = c("time", "ecg"),
                                 col_types = "dd")
                        )
            )

all_files <- bind_rows(good_files_csv, good_files_tsv)


# Write the files in a unified format, without header
walk2(all_files$data,
      all_files$filename,
      ~ write_excel_csv(.x, path = str_glue("{headerless_dir}{.y}"), na = "", col_names = FALSE))
