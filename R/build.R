library(tidyverse)

## based on xfun::yaml_body
yaml_body <- function (x)  {
  i = grep("^---\\s*$", x)
  n = length(x)
  res = if (n < 2 || length(i) < 2 || (i[1] > 1 && !all(is_blank(x[seq(i[1] - 1)])))) {
    list(yaml = list(), body = x)
  } else 
    list(yaml = x[i[1]:i[2]], body = tail(x, n - i[2]))
  if ((n <- length(res$yaml)) >= 3) {
    res$yaml = yaml::yaml.load(res$yaml[-c(1, n)])
  }
  res
}

yaml_flexible <- function(x, verbose=TRUE) {
  if(verbose) message(sprintf(" - %s", x[[1]]))
  if(length(grep("^---\\s*$", x))==1) { 
    yaml_body(c("---", x))
  } else { 
    list(yaml=yaml::yaml.load(x))
  }
}

split_time <- function(x) {
  if(!is.null(x$yaml$time)) {
    sp <- str_split(x$yaml$time, " ", n=2)[[1]]
    if(length(sp)==2) {
      x$yaml$time <- sp[1]
      x$yaml$about <- sp[2]
      nn <- c("time", "about")
      x$yaml <- x$yaml[c(nn, setdiff(names(x$yaml), nn))]
    }
  }
  x
}

fix_time <- function(x) {
  if(!is.null(x$yaml$time)) {
    x$yaml$time <- x$yaml$time |> str_replace(" *a$", " AM") |> str_replace(" *p$", " PM")
  }
  x
}

get_dates <- function(x) {
  has_date <- sapply(x, function(x) !is.null(x$yaml$date))
  if(any(has_date)) {
    dates <- sapply(x[has_date], function(x) x$yaml$date)
    x[[1]]$yaml$date <- dates[1]
    if(length(dates) > 1) {
      x[[1]]$yaml$date_end <- dates[length(dates)]
    }
  }
  x[[1]]$yaml$all_day <- "true"
  x
}

fill_dates <- function(x) {
  cur_date <- NA
  for(idx in seq_along(x)) {
    cur_yaml <- x[[idx]]$yaml
    if(!is.null(cur_yaml$date)) {
      cur_date <- cur_yaml$date
    } else if (!is.na(cur_date)) {
      if(!is.null(cur_yaml$time)) {
        cur_yaml$date <- lubridate::ymd_hm(paste(cur_date, cur_yaml$time)) |> 
          format("%Y-%m-%dT%H:%M:%S")
        cur_yaml$time <- NULL
      } else {
        cur_yaml$date <- cur_date
      }
      nn <- names(cur_yaml)
      x[[idx]]$yaml <- cur_yaml[c("date", setdiff(nn, "date"))]
    }
  }
  only_date <- sapply(x, function(x) {
    !is.null(x$yaml$date) & is.null(x$body) & (length(x$yaml)==1)
  })
  x[!only_date]
}

make_list <- function(x, n) {
  for(ni in n) {
    if(length(x$yaml[[ni]])==1) {
      x$yaml[[ni]] <- list(x$yaml[[ni]])
    }
  }
  x
}

add_types <- function(x) {
  x[[1]]$yaml$type <- "meeting"
  x[[1]]$yaml$view <- "schedule"
  for(idx in 2:length(x)) {
    x[[idx]]$yaml$type <- "talk"
  }
  x
}

find_authors <- function(m, authorlist) {
  if(is.null(m$yaml$authors)) return(m)
  yaml <- m$yaml
  x <- unlist(yaml$authors)
  a <- authorlist$code[str_detect(paste(x, collapse=" | "), fixed(authorlist$name))]
  if(length(a)==0) yaml$authors <- NULL else yaml$authors <- a
  for(idx in seq_len(nrow(authorlist))) {
    x <- x |> str_replace(fixed(authorlist$name[idx]), 
                          fixed(sprintf('{{%% mention_name "%s" "%s" %%}}',
                                  authorlist$code[idx], authorlist$name[idx]))
    )
  }
  yaml$authorlist <- x
  m$yaml <- yaml
  m
}

read_meeting <- function(file, verbose=TRUE, authorlist) {
  if(verbose) message(sprintf("Reading %s..", file))
  txt <- readLines(file)
  k <- (str_detect(txt, "^date:") | str_detect(txt, "^time:")) |> cumsum()
  out <- split(txt, k) |> 
    lapply(yaml_flexible, verbose=verbose) |> 
    lapply(split_time) |>
    lapply(fix_time) |>
    get_dates() |>
    fill_dates() |>
    lapply(find_authors, authorlist=authorlist) |>
    lapply(make_list, c("authors", "authorlist")) |>
    add_types()
  out
}

write_yaml <- function(yaml, file, dir=".", path=file.path(dir, paste0(file, ".md")), verbose=TRUE) {
  if(verbose) message(sprintf(" - %s", file))
  "---\n" |> cat(file=path)  
  yaml$yaml |> yaml::as.yaml() |> cat(file=path, append=TRUE)
  "---\n" |> cat(file=path, append=TRUE)
  if(!is.null(yaml$body)) {
    yaml$body |> cat(sep="\n", file=path, append=TRUE)
    "\n" |> cat(file=path, append=TRUE)
  }
}

write_meeting <- function(m, outdir, verbose=TRUE) {
  if(verbose) message(sprintf("Writing files for %s...", m[[1]]$yaml$title))
  for(idx in seq_along(m)) {
    x <- m[[idx]]
    if(idx==1) {
      file <- "_index"
    } else {
      file <- x$yaml$date |> str_remove_all("[-: ]") |> str_replace("T", "_")
    }
    write_yaml(x, file, dir=outdir, verbose=verbose)
  }
}

alist <- read_csv("static/data/authorlist.csv") |>
  filter(!is.na(name)) |>
  mutate(code=if_else(is.na(code), name, code))
years <- list.files("static/data", pattern=".txt$") |> str_remove(".txt$")
ms <- lapply(years, function(y) {
  m <- read_meeting(sprintf("static/data/%s.txt", y), authorlist=alist)
  outdir <- sprintf("content/event/%s", y)
  if(!file.exists(outdir)) dir.create(outdir)
  file.remove(list.files(outdir, pattern="*.md", full.names = TRUE))
  write_meeting(m, outdir)
  m
})

# a <- lapply(ms, function(m) lapply(m, function(x) x$yaml$authors)) |> unlist() |> unname()
# list.files("content/authors/")

figs <- list.files("static/data", pattern=".jpg")
file.copy(file.path("static/data", figs),
          paste0("content/event/", str_replace(figs, ".jpg", "/featured.jpg")))
