######################################
# Scraping the IMF LIC DSA List
# Teal Emery
#
# What this script does:  The IMF publishes the results of the LIC DSAs as a pdf.
# This script scrapes that pdf and outputs the data into a .csv file and a chart.



##### Load packages
library(tidyverse) #data manipulation
library(lubridate) # dealing with dates
library(pdftools) # reading pdf data
library(countrycode) # standardizing country names
library(janitor) # helper tools for snake_case
library(here) # relative file paths


###### Define Functions
# extract_as_of_date() extracts the as of date of the DSA List from the entire character string extracted from the PDF.
extract_as_of_date <- function(txt_input) {
  txt_input |>
    # pulls data that says "As of [date] and ends with a line break
    stringr::str_extract(pattern =
                           "As of \\w+ \\d{1,2}, \\d{4}\n") |>
    # extracts just the written date
    stringr::str_extract(pattern =
                           "\\w+ \\d{1,2}, \\d{4}") |>
    # converts it into a date object
    as.Date("%B %d, %Y")
}


# extract_dsa_flags() extracts the superscript next to the country names in the DSA list (e.g. Cameroon 3/) that are defined in the footnotes.
extract_dsa_flags <- function(input_string) {
  input_string |>
    # extracts digit followed by forward slash
    str_extract_all( pattern = "\\d{1}/", simplify = TRUE) |>
    as.vector() |>
    paste(sep = "", collapse = "")
}

######### Read in the Data

# url where the pdf is posted
imf_lic_dsa_pdf_url <- "https://www.imf.org/external/pubs/ft/dsa/dsalist.pdf"

# read the pdf to text using pdftools
txt <- pdf_text(imf_lic_dsa_pdf_url)

######### Process the Data

# get the "as of date" on the pdf
dsa_list_as_of_date <- txt |>
  extract_as_of_date()

# read the data into a tibble
pdf_tbl_raw <- read_delim(txt, delim = "\n", col_names = "pdf_line")


# process the tibble
pdf_processed <- pdf_tbl_raw |>

  # separate out lines that are part of the table
  ## Lines with countries have dates or `...`
  mutate(is_country = str_detect(pdf_line,
                                 pattern = "\\d{1,2}/\\d{1,2}/\\d{2}|\\.\\.\\.")) |>
  ## filter for lines that are in the table
  filter(is_country) |>

  # Separate the lines into the 5 columns by dividing where there are at least 3 spaces.
  separate(col = pdf_line,
           into = c("country", "date_latest_pub",
                    "risk_of_distress","joint_w_wb",
                    "date_latest_exec_board_not_published"),
           sep = "\\s{3,}") |>
  # Make sure there is no white space around text
  mutate(across(everything(), str_trim)) |>
  # #turn "…" into NAs
  mutate(across(everything(),~na_if(., "…"))) |>
  # fix dates
  mutate(across(contains("date"), lubridate::mdy)) |>
  # fix errors in joint_w_wb
  mutate(joint_w_wb = case_when(
    joint_w_wb %in% c("Yes", "No") ~ joint_w_wb,
    .default = NA)) |>

  # Country Standardizing using countrycode
  ## take out the superscript from the country names
  mutate(country_clean = str_remove_all(country, "\\d{1}/") |>
           # get rid of any remaining whitespace
           str_trim()) |>
  mutate(iso3c = countrycode(country_clean,
                             origin = "country.name",
                             destination = "iso3c",
                             origin_regex = TRUE,
                             custom_match = c('Micronesia' = 'FSM')),
         country_name = countrycode(iso3c,
                                    origin = "iso3c",
                                    destination = "country.name")) |>

  # extract the superscript DSA flags
  # extract dsa flags
  rowwise() |>
  mutate(country_notes = extract_dsa_flags(country)) |>
  ungroup() |>

  select(country_name, iso3c,date_latest_pub,
         risk_of_distress, joint_w_wb,
         date_latest_exec_board_not_published, country_notes) |>
  add_column(dsa_list_date = dsa_list_as_of_date)

###### Write the data
file_name <- paste0("imf_lic_dsa_list_as_of_", dsa_list_as_of_date, ".csv")


write_csv(pdf_processed, here("data", file_name))


