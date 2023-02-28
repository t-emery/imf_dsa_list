Scraping the IMF DSA List
================
Teal Emery

# Introduction

This script is meant to scrape the [IMF’s DSA
List](https://www.imf.org/external/pubs/ft/dsa/dsalist.pdf) showing the
debt distress classification for PRGT eligible countries

# Setup

## Load Libraries

``` r
library(tidyverse) #data manipulation
library(lubridate) # dealing with dates
library(pdftools) # reading pdf data
library(countrycode) # standardizing country names
library(janitor) # helper tools
library(here)
```

## Define Functions

`extract_as_of_date()` extracts the *as of date* of the DSA List from
the entire character string extracted from the PDF.

``` r
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
```

`extract_dsa_flags()` extracts the superscript next to the country names
in the DSA list (e.g. `Cameroon 3/`) that are defined in the footnotes.

``` r
extract_dsa_flags <- function(input_string) {
  input_string |> 
    str_extract_all( pattern = "\\d{1}/", simplify = TRUE) |> 
    as.vector() |> 
    paste(sep = "", collapse = "")
}
```

## Read in the Data

The DSA list is posted on [this
URL](https://www.imf.org/external/pubs/ft/dsa/dsalist.pdf)

``` r
imf_lic_dsa_pdf_url <- "https://www.imf.org/external/pubs/ft/dsa/dsalist.pdf"

txt <- pdf_text(imf_lic_dsa_pdf_url)

txt
```

    [1] "                                       List of LIC DSAs for PRGT-Eligible Countries\n                                                   As of January 31, 2023\n\n\n                                                        Per latest DSA publication\n                                                                                                     Latest DSA discussed by\n                                                                                                     the Executive Board but\n                                       Latest publication       Risk of debt      Joint with the\nCountry                                                                                                not yet published 2/\n                                              date               distress 1/       World Bank\n\n\nAfghanistan                                  6/28/21                High                Yes                      …\nBangladesh                                    3/7/22                Low                 Yes                   1/30/23\nBenin                                        12/20/22             Moderate            1/0/00                     …\nBhutan                                       5/24/22              Moderate              Yes                      …\nBurkina Faso                                 11/18/20             Moderate              Yes                      …\nBurundi                                      7/29/22                High                Yes                      …\nCambodia                                     12/18/22               Low                 Yes                      …\nCameroon 3/                                   8/4/22                High                Yes                      …\nCabo Verde 3/                                7/21/22              Moderate              Yes                      …\nCentral African Republic                      2/1/21                High                Yes                      …\nChad                                         1/23/23                High                Yes                      …\nComoros                                      10/29/21               High                Yes                      …\nCongo, Democratic Republic of                 7/5/22              Moderate              Yes                      …\nCongo, Republic of 3/                        7/18/22           In debt distress         Yes                      …\nCôte d'Ivoire                                 7/1/22              Moderate              Yes                      …\nDjibouti                                     5/12/20                High                Yes                   2/23/22\nDominica 3/                                  2/14/22                High                Yes                      …\nEritrea                                         …                    …                   …                    7/22/19\nEthiopia                                      5/6/20                High                Yes                      …\nGambia, The                                  12/21/22               High                Yes                      …\nGhana                                        7/23/21                High                Yes                      …\nGrenada 3/                                   5/10/22           In debt distress         Yes                      …\nGuinea                                       1/23/23              Moderate              Yes                      …\nGuinea-Bissau                                6/27/22                High                Yes                   1/30/23\nGuyana 4/                                    9/27/22              Moderate              Yes                      …\nHaiti                                         7/1/22                High                Yes                   1/23/23\nHonduras                                     9/14/21                Low                 Yes                      …\nKenya                                        12/20/22               High                Yes                      …\nKiribati                                     1/24/19                High                Yes                   4/26/21\nKyrgyz Republic                               8/2/21              Moderate              Yes                   1/18/23\nLao P.D.R.                                    8/8/19                High                Yes                   3/2/22\nLesotho                                       6/7/22              Moderate              Yes                      …\nLiberia                                      9/14/22              Moderate              Yes                      …\nMadagascar                                   3/16/22              Moderate              Yes                      …\nMalawi                                       11/23/22          In debt distress         Yes                      …\nMaldives                                     4/23/20                High                Yes                  11/23/22\nMali                                         3/30/21              Moderate              Yes                      …\nMarshall Islands                             5/27/21                High                Yes                      …\nMauritania                                   9/16/20                High                Yes                   1/25/23\nMicronesia                                   11/1/21                High                Yes                      …\nMoldova 3/                                   1/12/23                Low                 Yes                      …\nMozambique                                   4/29/20           In debt distress         Yes                   5/9/22\nMyanmar                                      1/28/21                Low                 Yes                      …\nNepal                                        1/27/22                Low                 Yes                      …\nNicaragua                                    1/27/23              Moderate              Yes                      …\nNiger                                        1/27/23              Moderate              Yes                      …\nPapua New Guinea 3/                          9/20/22                High                Yes                      …\nRwanda                                       12/19/22             Moderate              Yes                      …\nSamoa                                        3/19/21                High                Yes                      …\nSão Tomé and Príncipe                        9/20/22           In debt distress         Yes                      …\nSenegal                                      6/27/22              Moderate              Yes                      …\nSierra Leone                                 7/29/22                High                Yes                      …\nSolomon Islands                              1/21/22              Moderate              Yes                      …\nSomalia                                      7/19/22           In debt distress         Yes                      …\nSouth Sudan                                   8/3/22                High                Yes                   1/23/23\nSt. Lucia 3/ 5/                               9/9/11              Moderate              No                   10/17/11\nSt. Vincent and the Grenadines 3/            11/17/22               High                Yes                      …\nSudan                                         7/1/21           In debt distress         Yes                      …\nTajikistan                                   2/18/22                High                Yes                      …\nTanzania                                      8/5/22              Moderate              Yes                      …\nTimor Leste 3/                               9/22/22              Moderate              Yes                      …\nTogo                                         4/16/20              Moderate              Yes                      …\nTonga                                        8/26/22                High                Yes                      …\nTuvalu                                        8/4/21                High                Yes                      …\nUganda                                       3/15/22              Moderate              Yes                      …\nUzbekistan 3/                                6/22/22                Low                 Yes                      …\nVanuatu                                      9/14/21              Moderate              Yes                      …\nYemen, Republic of                           9/24/14              Moderate              Yes                   6/1/16\nZambia                                        9/6/22           In debt distress         Yes                      …\nZimbabwe 3/                                   4/8/22           In debt distress         Yes                      …\n\n*/ While there is no binding minimum concessionality requirement in the absence of a Fund-supported program, concessional\nflows remain the most appropriate source of external finance for LICs, highlighting the need for continued efforts by the\ninternational community to improve the availability and predictability of concessional financing (PIN No. 06/136).\n1/ As of January 31, 2023 and based on the most recently published data, 9 countries are in debt distress, 28 countries are at high risk,\n25 countries are at moderate risk, and 7 countries are at low risk of debt distress.\n2/ May reflect usual lags in the publication. Includes DSAs presented to the Executive Board on lapse of time basis.\n3/ PRGT-eligible IDA-blend countries.\n4/ Non PRGT-eligible country using the LIC DSF.\n5/ A market-access countries (MACs) DSA has been completed and published within the past 24 months.\n"

### Extract as of date

``` r
dsa_list_as_of_date <- txt |> 
  extract_as_of_date()

dsa_list_as_of_date
```

    [1] "2023-01-31"

### Read the data into a tibble

``` r
pdf_tbl_raw <- read_delim(txt, delim = "\n", col_names = "pdf_line")
```

    Rows: 87 Columns: 1
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: "\001"
    chr (1): pdf_line

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pdf_tbl_raw
```

    # A tibble: 87 × 1
       pdf_line                                                                     
       <chr>                                                                        
     1 "                                       List of LIC DSAs for PRGT-Eligible C…
     2 "                                                   As of January 31, 2023"  
     3 "                                                        Per latest DSA publ…
     4 "                                                                           …
     5 "                                                                           …
     6 "                                       Latest publication       Risk of deb…
     7 "Country                                                                    …
     8 "                                              date               distress 1…
     9 "Afghanistan                                  6/28/21                High   …
    10 "Bangladesh                                    3/7/22                Low    …
    # … with 77 more rows

# Process the Table

``` r
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

  
  


pdf_processed
```

    # A tibble: 70 × 8
       country_name   iso3c date_lat…¹ risk_…² joint…³ date_lat…⁴ count…⁵ dsa_list…⁶
       <chr>          <chr> <date>     <chr>   <chr>   <date>     <chr>   <date>    
     1 Afghanistan    AFG   2021-06-28 High    Yes     NA         ""      2023-01-31
     2 Bangladesh     BGD   2022-03-07 Low     Yes     2023-01-30 ""      2023-01-31
     3 Benin          BEN   2022-12-20 Modera… <NA>    NA         ""      2023-01-31
     4 Bhutan         BTN   2022-05-24 Modera… Yes     NA         ""      2023-01-31
     5 Burkina Faso   BFA   2020-11-18 Modera… Yes     NA         ""      2023-01-31
     6 Burundi        BDI   2022-07-29 High    Yes     NA         ""      2023-01-31
     7 Cambodia       KHM   2022-12-18 Low     Yes     NA         ""      2023-01-31
     8 Cameroon       CMR   2022-08-04 High    Yes     NA         "3/"    2023-01-31
     9 Cape Verde     CPV   2022-07-21 Modera… Yes     NA         "3/"    2023-01-31
    10 Central Afric… CAF   2021-02-01 High    Yes     NA         ""      2023-01-31
    # … with 60 more rows, and abbreviated variable names ¹​date_latest_pub,
    #   ²​risk_of_distress, ³​joint_w_wb, ⁴​date_latest_exec_board_not_published,
    #   ⁵​country_notes, ⁶​dsa_list_date

``` r
file_name <- paste0("imf_lic_dsa_list_as_of_", dsa_list_as_of_date, ".csv")

file_name
```

    [1] "imf_lic_dsa_list_as_of_2023-01-31.csv"

# Write The Data to `.csv`

``` r
write_csv(pdf_processed, here("data", file_name))
```
