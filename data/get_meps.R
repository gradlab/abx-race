#!/usr/bin/env Rscript

# Downloads the raw data for each year, scrapes the Codebook, and parses
# the downloaded file with the Codebook. See the README.txt for more details.

library(dplyr)
library(foreign) # for reading the SAS file
library(readr) # for reading the tsv's
library(magrittr) # for easier data piping
library(hashmap) # for data decoding

# names of desired columns and files
variable_names = read_tsv('variable_names.tsv')

rename_variables = function(df) {
  old_names = variable_names$old_name
  new_names = variable_names$new_name
  
  # only rename the variables that are present in the input
  name_is_present = old_names %in% names(df)
  old_names = old_names[name_is_present]
  new_names = new_names[name_is_present]
  
  select(df, !!!setNames(as.list(old_names), new_names))
}

download_data = function(puf_id) {
  zip_basename = str_c(tolower(puf_id), 'ssp.zip')
  zip_url = str_c('https://www.meps.ahrq.gov/mepsweb/data_files/pufs/', zip_basename)
  
  zip_fn = tempfile()
  download.file(zip_url, zip_fn)
  unzip_fn = unzip(zip_fn, exdir=tempdir())
  dat = foreign::read.xport(unzip_fn)
  
  unlink(zip_fn)
  unlink(unzip_fn)
  
  dat %>%
    as_tibble() %>%
    rename_variables() %>%
    mutate_if(is.factor, as.character)
}


# abx Multum codes
abx_codes = read_tsv('abx_codes.tsv') %>%
  mutate(category=as.numeric(category))

abx_group_codes = abx_codes %>%
  filter(abx_desc %in% c('penicillins', 'macrolides', 'cephalosporins', 'quinolones', 'sulfonamides'))

read_year = function(year, consol_id, drug_id) {
  consol_dat = download_data(consol_id)
  drug_dat = download_data(drug_id)

  abx_any_id = drug_dat %>%
    select(person_id, rx_id, starts_with('tc')) %>%
    gather('key', 'category', -person_id, -rx_id) %>%
    inner_join(abx_codes, by='category') %>%
    select(person_id, rx_id) %>%
    distinct()
  
  # get rx_id's only for specific antibiotic groups
  abx_group_id = drug_dat %>%
    select(rx_id, starts_with('tc')) %>%
    gather('key', 'category', -rx_id) %>%
    inner_join(abx_group_codes, by='category') %>%
    select(rx_id, abx_desc) %>%
    distinct()
  
  # assert that each rx_id has only one abx desc
  if(any(duplicated(abx_group_id$rx_id))) stop('rx_id has multiple abx desc')
  
  abx_id = abx_any_id %>%
    left_join(abx_group_id, by='rx_id') %>%
    replace_na(list(abx_desc='other'))
  
  abx_by_person_total = abx_id %>%
    count(person_id) %>%
    mutate(abx_desc='n_abx')

  abx_by_person = abx_id %>%
    count(person_id, abx_desc) %>%
    mutate(abx_desc=str_c('n_', abx_desc)) %>%
    bind_rows(abx_by_person_total) %>%
    spread(abx_desc, n, fill=0L)
  
  # decode factor variables
  sex_hash = hashmap(1:2, c('male', 'female'))
  race_hash = hashmap(1:5, c('Hispanic', 'white', 'black', 'other', 'other'))
  region_hash = hashmap(c(-1, 1:4), c('no_region', 'Northeast', 'Midwest', 'South', 'West'))

  consol_dat %>%
    mutate(race=race_hash[[race]],
           sex=sex_hash[[sex]],
           region=region_hash[[region]]) %>%
    left_join(abx_by_person, by='person_id') %>%
    mutate_at(vars(starts_with('n_')), funs(if_else(is.na(.), 0L, .))) %>%
    mutate(year=year)
}

meps_dat = bind_rows(
  read_year(2014, 'H171', 'H168A'),
  read_year(2015, 'H181', 'H178A')
)

#  write_tsv('meps.tsv')
