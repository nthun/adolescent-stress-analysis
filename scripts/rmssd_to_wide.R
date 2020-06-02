# RMSSD to wide_format

library(tidyverse)


markers <- 
    read_csv("data/all_markers_clean.csv") %>% 
    distinct(marker_name) %>% 
    filter(str_detect(marker_name, "start")) %>% 
    mutate(marker_name = str_remove(marker_name, " start"))

hrv <- 
    read_csv("data/4_summarised/hrv_summary.csv") %>% 
    filter(scale(rmssd) < 3)

hrv %>% 
    count(session, marker_name)


# RMSSD -----------------------------------------------------------------------------


hrv %>% 
    select(id, session, marker, rmssd) %>% 
    unite(c("session", "marker"), col = "measurement") %>% 
    spread(measurement, rmssd) %>% view()

hrv %>% 
    mutate(marker_name = factor(marker_name, levels = markers$marker_name)) %>% 
    group_by(session, marker_name) %>% 
    summarise(avg_rmssd = mean(rmssd),
              se_rmssd = sd(rmssd)/sqrt(n())) %>% 
    drop_na() %>% 
    ggplot() +
    aes(x = marker_name, y = avg_rmssd, group = session, color = session,
        ymin = avg_rmssd - se_rmssd, ymax = avg_rmssd + se_rmssd) +
    geom_line(size = 1.2) +
    geom_errorbar(alpha = .5, width = .2) +
    facet_wrap(~session, scales = "free_x")


# HR --------------------------------------------------------------------------------

hrv %>% 
    select(id, session, marker, meanhr) %>% 
    unite(c("session", "marker"), col = "measurement") %>% 
    spread(measurement, meanhr)

hrv %>% 
    mutate(marker_name = fct_inorder(marker_name)) %>% 
    group_by(session, marker_name) %>% 
    summarise(avg_hr = mean(meanhr),
              se_hr = sd(meanhr)/sqrt(n())) %>% 
    drop_na() %>% 
    ggplot() +
    aes(x = marker_name, y = avg_hr, group = session, color = session,
        ymin = avg_hr - se_hr, ymax = avg_hr + se_hr) +
    geom_line(size = 1.2) +
    geom_errorbar(alpha = .5, width = .2) +
    facet_wrap(~session, scales = "free_x")
