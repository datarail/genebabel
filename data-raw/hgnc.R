library(tidyverse)
# library(jsonlite)
library(devtools)
library(stringr)

if (!file.exists("data-raw/hgnc_complete_set.json")) {
  download.file(
    "ftp://ftp.ebi.ac.uk/pub/databases/genenames/new/json/hgnc_complete_set.json",
    "data-raw/hgnc_complete_set.json",
    quiet = TRUE
  )
}

# Decided to use JSON because the encoding of columns with multiple entries as
# list columns is much more convenient than the encoding as "|" delimited string
# in the TSV or CSV files. But have to reorder columns based on the order in
# the TSV
gene_names_json <- jsonlite::fromJSON("data-raw/hgnc_complete_set.json")
hgnc <- as_tibble(gene_names_json$response$docs) %>%
  dplyr::select("hgnc_id", "symbol", "name", "locus_group", "locus_type", "status",
                "location", "location_sortable", "alias_symbol", "alias_name",
                "prev_symbol", "prev_name", "gene_group", "gene_group_id",
                "date_approved_reserved", "date_symbol_changed", "date_name_changed",
                "date_modified", "entrez_id", "ensembl_gene_id", "vega_id", "ucsc_id",
                "ena", "refseq_accession", "ccds_id", "uniprot_ids", "pubmed_id",
                "mgd_id", "rgd_id", "lsdb", "cosmic", "omim_id", "mirbase", "homeodb",
                "snornabase", "bioparadigms_slc", "orphanet", "pseudogene.org",
                "horde_id", "merops", "imgt", "iuphar", "kznf_gene_catalog",
                "mamit-trnadb", "cd", "lncrnadb", "enzyme_id", "intermediate_filament_db",
                "rna_central_id", "gtrnadb", "lncipedia")

# if (!file.exists("data-raw/hgnc_complete_set.txt")) {
#   download.file("ftp://ftp.ebi.ac.uk/pub/databases/genenames/new/tsv/hgnc_complete_set.txt", destfile =  "data-raw/hgnc_complete_set.txt", quiet = TRUE)
# }
#
# hgnc <- readr::read_tsv("data-raw/hgnc_complete_set.txt")

devtools::use_data(hgnc, overwrite = TRUE, compress = "xz")

# Metadata about the columns in the hgnc dataset are take from https://beta.genenames.org/help/statistics-and-downloads/
hgnc_desc <- readr::read_file("data-raw/hgnc_meta.txt")
hgnc_desc_table <- stringr::str_split(hgnc_desc, fixed("\n\n"))[[1]] %>%
  stringr::str_split_fixed(fixed("\n"), 2) %>%
  tibble::as.tibble() %>%
  dplyr::rename(name = V1, description = V2) %>%
  dplyr::mutate(description = stringr::str_replace(description, fixed("\n"), " ")) %>%
  dplyr::mutate(roxy_line = paste0("#'   \\item{", name, "}{", description, "}"))
write(paste0(hgnc_desc_table$roxy_line), "data-raw/hgnc_meta_roxygen.txt")
