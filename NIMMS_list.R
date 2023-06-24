library(tidyverse)
library(rvest)
library(googlesheets4)
library(yaml)

write_mdyaml <- function(x, file) {
  cat("---\n", file=file)
  con <- file(file, "a")
  write_yaml(x, con)
  close(con)
  cat("---\n", file=file, append=TRUE)
}

## get NIMSS table
d0 <- local({
  wp <- read_html("https://www.nimss.org/projects/view/participant_list/18798")
  wpts <- html_nodes(wp, "table")
  tab <- html_table(wpts[1])[[1]]
  tab[1,1:3] <- tab[1,4:6]
  tab <- tab[,1:3] |> rename("Name"="Participant Name",
                             "Head"="Is Head",
                             "Contact" = "Contact Info")
  tab |> separate(Name, c("Last", "First Email"), sep=",")
})

## get google sheet
gs4_auth("rend0020@umn.edu")
d <- read_sheet("1NNMWOoCZCW4COgzmy13xn-JzqD_g5DqAhahL3BnqQv8")

## check that google sheet matches NIMSS
all(d0$Last %in% d$Last)
d0 |> anti_join(d, by="Last")
d |> mutate(inNIMSS = Last %in% d0$Last) |> count(NIMSS, inNIMSS)

## get test yaml
foo <- read_yaml("content/authors/aaronrendahl/_index.md")
f <- "content/authors/aaronrendahl/tmp.md"
write_mdyaml(foo, f)

## get existing files
fs <- list.files("content/authors")
foo <- read_yaml(file.path("content", "authors", fs[2], "_index.md"))
names(foo)

tmp <- filter(d, NIMSS=="NIMSS")[1,]
tmp |> select(first_name="First", last_name="Last", 
              organizations="Affiliation") |> 
  mutate(user_groups="Members") |>
  as.list()
