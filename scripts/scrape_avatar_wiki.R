library(rvest)
library(dplyr)
library(polite)
library(purrr)
library(stringr)

earth_url <- "https://avatar.fandom.com/wiki/Category:Earth_Kingdom_characters"
fire_url <- "https://avatar.fandom.com/wiki/Category:Fire_Nation_characters"
water_url <- "https://avatar.fandom.com/wiki/Category:Water_Tribe_characters"
air_url <- "https://avatar.fandom.com/wiki/Category:Air_Nomad_characters"


get_character_info <- function(bender_url) {
  bender_label <- str_extract(bender_url, "Fire|Earth|Water|Air")

  cat(paste0("\nStarting: ", bender_label, "!\n"))
  ## Get name and page link
  session <- bow(bender_url)

  char_link <- scrape(session) %>%
    html_nodes(".category-page__member-link") %>%
    html_attr("href")

  char_name <- scrape(session) %>%
    html_nodes(".category-page__member-link") %>%
    html_text()

  base_url <- "https://avatar.fandom.com/"

  char_df <- data.frame(
    char_link,
    char_name
  ) %>%
    ## Strip out 'Category:' pages that aren't single character page links
    filter(!str_detect(char_link, "Category")) %>%
    mutate(char_link = paste0(base_url, char_link))

  cat("\nNames and page link done!\n")

  ## Get gender
  get_gender <- function(data, link) {
    cat(paste0("\nStarting: ", link))
    session2 <- bow(link)

    gender_is <- scrape(session2) %>%
      html_nodes(".portable-infobox") %>%
      html_text() %>%
      stringr::str_extract("Female|Male|Man|Woman") ## Some are "Non-binary" or gender isn't listed

    cat(paste0("\nGender: ", gender_is))

    final_df <- data %>%
      filter(char_link == link)

    if (class(gender_is) != "character") {
      final_df <- final_df %>%
        mutate(gender = NA_character_)
    } else {
      final_df <- final_df %>%
        mutate(gender = gender_is)
    }

    cat("\nDone!\n")

    return(final_df)
  }

  char_namegender_df <- map(
    char_df$char_link,
    ~ get_gender(data = char_df, link = .x)
  ) %>%
    purrr::reduce(bind_rows)

  saveRDS(char_namegender_df, file = here::here(paste0("data/", bender_label, "_namegender_df.RDS")))

  cat(paste0("\nScript for: ", bender_label, " done!\n"))
}


get_character_info(bender_url = earth_url)
Sys.sleep(5)
get_character_info(bender_url = air_url)

beepr::beep(8)


## Misc:

session2 <- bow("https://avatar.fandom.com/wiki/Azula")

gender_is <- scrape(session2) %>%
  html_nodes(".portable-infobox") %>%
  html_text() %>%
  stringr::str_extract("Female|Male|Man|Woman") ## Some are "Non-binary" or gender isn't listed



## one nation
bender_url <- earth_url
bender_label <- str_extract(bender_url, "Fire|Earth|Water|Air")

cat(paste0("\nStarting: ", bender_label, "!\n"))
## Get name and page link
session <- bow(bender_url)

char_link <- scrape(session) %>%
  html_nodes(".category-page__member-link") %>%
  html_attr("href")

char_name <- scrape(session) %>%
  html_nodes(".category-page__member-link") %>%
  html_text()

base_url <- "https://avatar.fandom.com/"

earth_df <- data.frame(
  char_link,
  char_name
) %>%
  ## Strip out 'Category:' pages that aren't single character page links
  filter(!str_detect(char_link, "Category")) %>%
  mutate(char_link = paste0(base_url, char_link))

cat("\nNames and page link done!\n")


## find all genders at once

earth_namegender_df <- map(
  earth_df$earth_link,
  ~ get_gender(data = earth_df, link = .x)
) %>%
  purrr::reduce(earth_namegender_df, bind_rows)

beepr::beep(8)

saveRDS(earth_namegender_df, file = "data/earth_namegender_df.RDS")





