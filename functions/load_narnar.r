#### load Narnar .csv file ####
# requires tidyverse


load_narnar <- function (file, ID) {
    
    df <- file %>% read_csv(na = c("", "NA", "-")) %>% cbind("ID" = ID, .) %>%
      rename(Time = "Interval Time (s)") %>%
      relocate(Time, .after = "Interval Index") %>%
      select(!c("Shift X", "Shift Y", "Frame number")) %>%
      pivot_longer(cols = `ROI 1`:`ROI 6`, names_to = "ROI", values_to = "VI") %>%
      filter(VI != "NA") %>%
      arrange(ID, ROI) %>% 
      rename("Interval_Index" = "Interval Index") %>%
      group_by(ID, ROI) %>% nest() %>%
      mutate(data = map(data, back_subtract)) %>%
      unnest(c("data")) %>% mutate(exclude = FALSE)
}