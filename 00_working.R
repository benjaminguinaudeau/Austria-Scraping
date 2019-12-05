# 13/07/2019

## Pasting
paste0(1:12, c("st", "nd", "rd", rep("th", 9)))
paste0("https://www.parlament.gv.at", "/PAKT/VHG/XXV/A/A_00008/index.shtml")

## Name setting in dyplr pipes
setNames(., c("web_date", "type", "desc_title", "id"))

## Cleaning archive IDs for filenames
gsub("[^A-Za-z0-9]", "" , .)

# 18/07/2019

## 

# 20/07/2019

## Proposal scrape test

proposalpage <- read_html("https://www.parlament.gv.at/PAKT/VHG/XXV/I/I_00998/index.shtml")

#status scrape   ###### working var status #### FINISHED
status <- proposalpage %>%
  html_node(., "div.floatLeft p") %>%
  html_text(.)

#status string manipulation    ##### 21/07/2019
status_BR <- status %>%
  str_extract()

#bill id   ##### FINISHED
bill_id <- proposalpage %>%
  html_node(., "div.floatLeft p") %>%
  html_text(.) %>%
  str_extract(., "\\d+\\/\\w{3}")

#bill_filename  ###### FINISHED
bill_filename <- bill_id %>%
  gsub("[^A-Za-z0-9]", "" , .)

#bill link scrape    ##### 21/07/2019
bill_id <- proposalpage %>%
  html_node(., "div.floatLeft p") %>%
  html_attr(., "href") %>%
  str_extract(., "regex bill link")