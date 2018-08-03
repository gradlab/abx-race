#!/usr/bin/env Rscript

# This script downloads and extracts SAS files from the MEPS website.
# See README.md for more details.

library(dplyr)
library(foreign) # for reading the SAS file
library(readr) # for reading the tsv's
library(magrittr) # for easier data piping

# names of desired columns and files
variable_names = read_tsv('variable_names.tsv')

# Multum codes for antibiotics
abx_codes = read_tsv('abx_codes.tsv') %>%
  mutate(category=as.numeric(category))

abx_group_codes = abx_codes %>%
  filter(abx_desc %in% c('penicillins', 'macrolides', 'cephalosporins', 'quinolones', 'sulfonamides'))


download_data = function(puf_id) {
  # e.g., "H171" corresponds to a file "h171ssp.zip"
  zip_basename = str_c(tolower(puf_id), 'ssp.zip')
  zip_url = str_c('https://www.meps.ahrq.gov/mepsweb/data_files/pufs/', zip_basename)
  
  # download the file in a temporary location
  zip_fn = tempfile()
  download_flag = download.file(zip_url, zip_fn, method='wget')
  
  # unzip and parse the SAS file
  unzip_fn = unzip(zip_fn, exdir=tempdir())
  dat = foreign::read.xport(unzip_fn)

  # remove temporary files
  unlink(zip_fn)
  unlink(unzip_fn)
  
  dat
}

# transform the data into a tibble, and rename the columns as per
# what's in variable_names.tsv
clean_data = function(df) {
  df = as_tibble(df)
  
  old_names = variable_names$old_name
  new_names = variable_names$new_name
  
  # only rename the variables that are present in the input
  name_is_present = old_names %in% names(df)
  old_names = old_names[name_is_present]
  new_names = new_names[name_is_present]
  
  df %>%
    select(!!!setNames(as.list(old_names), new_names)) %>%
    mutate_if(is.factor, as.character)
}

# combine Consolidate Household and Prescribed Medicines data
combine_data = function(raw_consol_dat, raw_drug_dat) {
  consol_dat = clean_data(raw_consol_dat)
  drug_dat = clean_data(raw_drug_dat)
  
  # look for prescription records that are antibiotics
  abx_any_id = drug_dat %>%
    select(person_id, rx_id, starts_with('tc')) %>%
    gather('key', 'category', -person_id, -rx_id) %>%
    inner_join(abx_codes, by='category') %>%
    select(person_id, rx_id) %>%
    distinct()
  
  # get prescription records only for specific antibiotic groups
  abx_group_id = drug_dat %>%
    select(rx_id, starts_with('tc')) %>%
    gather('key', 'category', -rx_id) %>%
    inner_join(abx_group_codes, by='category') %>%
    select(rx_id, abx_desc) %>%
    distinct()
  
  # assert that each prescription has only one abx group
  if(any(duplicated(abx_group_id$rx_id))) stop('rx_id has multiple abx desc')
  
  abx_id = abx_any_id %>%
    left_join(abx_group_id, by='rx_id') %>%
    replace_na(list(abx_desc='other'))
  
  # count up overall antibiotics by individual
  abx_by_person_total = abx_id %>%
    count(person_id) %>%
    mutate(abx_desc='n_abx')
  
  # count up antibiotics by individual and drug
  abx_by_person = abx_id %>%
    count(person_id, abx_desc) %>%
    mutate(abx_desc=str_c('n_', abx_desc)) %>%
    bind_rows(abx_by_person_total) %>%
    spread(abx_desc, n, fill=0L)
  
  replace_values = function(x, keys, values) values[match(x, keys)]
  
  # replace some coded values and join in the drug data
  consol_dat %>%
    mutate(race = replace_values(race, 1:5, c('Hispanic', 'white', 'black', 'Asian', 'other')),
           white = replace_values(white, 1:3, c('any_white', 'any_white', 'no_white'))) %>%
    left_join(abx_by_person, by='person_id') %>%
    # replace missing drug counts with 0
    mutate_at(vars(starts_with('n_')), funs(if_else(is.na(.), 0L, .)))
}

# download, clean, and combine data
read_year = function(year, consol_id, drug_id) {
  consol_dat = download_data(consol_id)
  drug_dat = download_data(drug_id)

  combine_data(consol_dat, drug_dat) %>%
    mutate(year=year)
}

meps_dat = bind_rows(
  read_year(2014, 'H171', 'H168A'),
  read_year(2015, 'H181', 'H178A')
)

write_tsv(meps_dat, 'meps.tsv')
