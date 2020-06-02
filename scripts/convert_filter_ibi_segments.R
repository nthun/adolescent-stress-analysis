# Convert files back to txt, filter files that are too short
library(tidyverse)
library(vroom)

output_path <- "data/81_ibi_converted_filtered"

files <- list.files("data/8_ibi_corrected/", 
                    pattern = "IBI_artifactCorrected_.*.csv$", 
                    full.names = TRUE)


artifact_files <- list.files("data/8_ibi_corrected/", 
                    pattern = "IBI_artifacts_.*.csv$", 
                    full.names = TRUE)

corrected_raw <- vroom(files, id = "file", delim = ",")
    
corrected <-
    corrected_raw %>% 
    mutate(file = str_remove_all(file, 
                          "data/8_ibi_corrected/IBI_artifactCorrected_ibi_|.txt.csv")) %>%
    # Drop files that are less than 30 ibis
    group_by(file) %>% 
    filter(n() >= 30) %>% 
    nest()

corrected %>% 
    print(n = 500)
 
walk2(corrected$data, 
      str_glue("{output_path}/{corrected$file}.txt"),
      ~write_csv(.x, path = .y, col_names = FALSE))    

