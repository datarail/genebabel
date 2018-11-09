# genebabel

The `genebabel` package is designed to make it easy to convert gene identifiers
and symbols between different databases.

Included at the moment are the [HUGO Gene Nomenclature Consortium (HGNC)](http://genenames.org) database of human genes. A convenient feature is that
also alternative and previous symbols, which are no longer in use, are included
and can be queried.

# Installation

```r
devtools::install_github("datarail/genebabel")
```

# Example uses

Query the HGNC databse using a character vector of gene symbols. Some of them
might be out of date, therefore also previous and alias symbols are queried.

```r
library(genebabel)
query_hgnc(c("FLG", "SGK2"), c("symbol", "alias_symbol", "prev_symbol"))
#> # A tibble: 2 x 52
#>   query hgnc_id name  locus_group locus_type status location
#>   <chr> <chr>   <chr> <chr>       <chr>      <chr>  <chr>   
#> 1 FLG   HGNC:3… fila… protein-co… gene with… Appro… 1q21.3  
#> 2 SGK2  HGNC:1… SGK2… protein-co… gene with… Appro… 20q13.12
#> # ... with 45 more variables: location_sortable <chr>,
#> #   alias_symbol <list>, alias_name <list>, prev_symbol <list>,
#> #   prev_name <list>, gene_family <list>, gene_family_id <list>,
#> #   date_approved_reserved <chr>, date_symbol_changed <chr>,
#> #   date_name_changed <chr>, date_modified <chr>, entrez_id <chr>,
#> #   ensembl_gene_id <chr>, vega_id <chr>, ucsc_id <chr>, ena <list>,
#> #   refseq_accession <list>, ccds_id <list>, uniprot_ids <list>,
#> #   pubmed_id <list>, mgd_id <list>, rgd_id <list>, lsdb <list>,
#> #   cosmic <chr>, omim_id <list>, mirbase <chr>, homeodb <int>,
#> #   snornabase <chr>, bioparadigms_slc <chr>, orphanet <int>,
#> #   pseudogene.org <chr>, horde_id <chr>, merops <chr>, imgt <chr>,
#> #   iuphar <chr>, kznf_gene_catalog <int>, `mamit-trnadb` <int>, cd <chr>,
#> #   lncrnadb <chr>, enzyme_id <list>, intermediate_filament_db <chr>,
#> #   rna_central_ids <list>, lncipedia <chr>, symbol <chr>, match__ <chr>
```

Query the HGNC databse using a data frame. The `b` column contains gene symbols,
some of which might be out of date, therefore also previous and alias symbols
are queried.

``` r
library(genebabel)
d <- data.frame(a = 1:3, b = c("FLG", "SGK2", "CDK1"))
join_hgnc(d, "b",
  match_cols = c("symbol", "alias_symbol", "prev_symbol"),
  select_cols = c("entrez_id", "symbol", "refseq_accession"))
#>   a    b entrez_id symbol refseq_accession
#> 1 1  FLG      2312    FLG        NM_002016
#> 2 2 SGK2     10110   SGK2     NM_001199264
#> 3 3 CDK1       983   CDK1        NM_001786
```
