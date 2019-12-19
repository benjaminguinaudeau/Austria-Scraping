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


## old status scrape 09/12/19 removed

#scrape_proposal
scrape_proposal <- function(url) {
  
  # for every page, scrape additional metadata
  for(i in 1:length(url)) {
    
    # read proposalpage
    proposalpage <- read_html(url[i])
    
    # status scrape
    status[i] <- proposalpage %>%
      html_node(., "div.floatLeft p") %>%
      html_text() %>%
      str_extract(., "Beschlossen im") # aus html_text oder aus Logik (beschlossen = Link BNR da) -> Wahlverhalten NR/BR? 
    
    # corresponding bill scrape
    bill_link[i] <- proposalpage %>%
      html_nodes(tespage, "div.floatLeft p a") %>%
      html_attr(., "href") %>% 
      str_subset(., "BNR") # select link containing BNR
    # maybe bill link needs to be mutated (forwarding link ahora)
    
    # download proposal ### PROPOSALS NUR BEI REGIERUNGSVORLAGEN, NICHT ABGEORDNETEN ##
    proposalpage %>%
      html_node(., "ul.fliesstext a")[2] %>%
      html_attr(., "href")
  }
}

