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

# tibbling 20/12/19

test <- read_html("https://www.parlament.gv.at/PAKT/VHG/XXI/I/I_01286/index.shtml")
test <- read_html("https://www.parlament.gv.at/PAKT/VHG/XXIV/I/I_02446/index.shtml")


# Dafür list
pro <- test %>%
  html_node(., "div.floatLeft p") %>%
  html_text(.) %>%
  str_to_lower(.) %>%
  str_extract(., "daf.+d") %>%
  gsub("[^A-Za-z0-9]", "" , .) %>%
  str_sub(., start = 5L, end = -2L) %>%
  str_split(., "")

# Dagegen list
contra <- test %>%
  html_node(., "div.floatLeft p") %>%
  html_text(.) %>%
  str_to_lower(.) %>%
  str_extract(., "dag.+") %>%
  gsub("[^A-Za-z0-9]", "" , .) %>%
  str_sub(., start = 8L, end = -1L) %>%
  str_split(., "")
  # to change character(0) to NA in list objects 
  # ifelse(length(.[[1]]) == 0, NA, .)

#
# list-column <- c(pro, contra)

# Get Pro Conta (1/0)
procontra <- c(rep(1, lengths(pro)), rep(0, lengths(contra)))

# Join objects and write list-column ()
testlist <- list(party = c(pro[[1]], contra[[1]]), pro = procontra)

# list_final <- join #see cheatsheet

### HOW TO PUT IN LIST_COLUMN??


### 27/12/2019
# how to mutate/remove parts of strings
mutate(antwort_ministerium = antwort_ministerium %>% 
         str_remove("Bundesregierung, ") %>%
         str_remove("\\(federfÃ¼hrend\\)") %>%
         str_trim %>%
         str_remove(".+(?=AuswÃ¤rtiges Amt$)"))


### 28/12/19
# was tun bei sonderfällen? (zurückgezogen hat statusbild von beschlossen.....)


### Sleep function

Sys.sleep(runif(1, 0.1, 0.5))

### parl verfahren scrape
dates <- proposalpage_red[[1]] %>%
  html_nodes("table.table-nonresponsive") %>%
  html_nodes("tr.historyShowAlways") %>%
  html_text(trim = T) %>%
  str_subset("[0-9]{2}.[0-9]{2}.[0-9]{4}") %>%
  str_extract("[0-9]{2}.[0-9]{2}.[0-9]{4}")

processes <- proposalpage_red[[1]] %>%
  html_nodes("a.historieOverviewToggle") %>%
  html_text()

parl_verfahren[[1]] <- list(date = dates,
                            process = processes)

parl_verfahren <- vector(mode = "list", length = 1)
