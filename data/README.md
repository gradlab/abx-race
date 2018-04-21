% Getting and cleaning the MEPS data

The script `get_meps.R` downloads and cleans the data.

# Downloading data files

First, given a PUF ID, the script downloads the zipped SAS version of the
corresponding file. For example, the 2015 Full Year Consolidated Data File has
PUF ID HC-181, which gets translated into "H181":

`https://www.meps.ahrq.gov/mepsweb/data_files/pufs/h181ssp.zip`

Second, it keeps only a subset of the columns in the original data and renames
them according to `variable_names.tsv`.

# Cleaning the data

Each year has two relevant data files, the Full Year Consolidated Data File,
which has demographic information about the individuals in the survey, and the
Prescribed Medicines Data File, which has information about antibiotic
prescription fills.

First, the script downloads both these files, as discussed above.

Next, the Prescribed Medicines file is filtered for antibiotics using the
antibiotic category codes in `abx_codes.tsv`. These codes are matched against
all the "therapeutic class" variables; a prescription fill with any one of
these codes is an antibiotic. In a second pass, antibiotic fills are labeled
according to a few classes. The total antibiotic fills and fills for specific
classes are aggregated by individual.

Finally, the individual-level antibiotic use data is merged with the
information in the Consolidated Data File, and encoded values (e.g., 1=male,
2=female) are decoded.

The results are placed in `meps.tsv`.
