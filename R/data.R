#' IDs and names of all genes in the HUGO Gene Naming Consortium (HGNC)
#'
#' A dataset containing the symbols and IDs from various external databases
#' for all genes in the HGNC database. Some columns, such as `alias_symbol` are
#' list columns, because each entry can be associated with multiple alias symbols.
#' 
#' Version 2019-09-19
#'
#' @format A tibble with 42726 rows and 49 variables:
#' \describe{
#'   \item{hgnc_id}{HGNC ID. A unique ID created by the HGNC for every approved symbol.}
#'   \item{symbol}{The HGNC approved gene symbol. Equates to the "Approved symbol" field within the gene symbol report.}
#'   \item{name}{HGNC approved name for the gene. Equates to the "Approved name" field within the gene symbol report.}
#'   \item{locus_group}{A group name for a set of related locus types as defined by the HGNC (e.g. non-coding RNA).}
#'   \item{locus_type}{The locus type as set by the HGNC.}
#'   \item{status}{Status of the symbol report, which can be either "Approved" or "Entry Withdrawn".}
#'   \item{location}{Cytogenetic location of the gene (e.g. 2q34).}
#'   \item{location_sortable}{Same as "location" but single digit chromosomes are prefixed with a 0 enabling them to be sorted in correct numerical order (e.g. 02q34).}
#'   \item{alias_symbol}{Other symbols used to refer to this gene as seen in the "Alias symbols" field in the gene symbol report.}
#'   \item{alias_name}{Other names used to refer to this gene as seen in the "Alias names" field in the gene symbol report.}
#'   \item{prev_symbol}{Gene symbols previously approved by the HGNC for this gene. Equates to the "Previous symbols" field within the gene symbol report.}
#'   \item{prev_name}{Gene names previously approved by the HGNC for this gene. Equates to the "Previous names" field within the gene symbol report.}
#'   \item{gene_group}{The gene group name as set by the HGNC and seen at the top of the gene group reports.}
#'   \item{gene_group_id}{ID used to designate a gene group the gene has been assigned to.}
#'   \item{date_approved_reserved}{The date the entry was first approved.}
#'   \item{date_symbol_changed}{The date the approved symbol was last changed.}
#'   \item{date_name_changed}{The date the approved name was last changed.}
#'   \item{date_modified}{Date the entry was last modified.}
#'   \item{entrez_id}{NCBI gene ID. Found within the "Gene resources" section of the gene symbol report.}
#'   \item{ensembl_gene_id}{Ensembl gene ID. Found within the "Gene resources" section of the gene symbol report.}
#'   \item{vega_id}{Vega gene ID. Found within the "Gene resources" section of the gene symbol report.}
#'   \item{ucsc_id}{UCSC gene ID. Found within the "Gene resources" section of the gene symbol report.}
#'   \item{ena}{International Nucleotide Sequence Database Collaboration (GenBank, ENA and DDBJ) accession number(s). Found within the "Nucleotide resources" section of the gene symbol report.}
#'   \item{refseq_accession}{RefSeq nucleotide accession(s). Found within the "Nucleotide resources" section of the gene symbol report.}
#'   \item{ccds_id}{Consensus CDS ID. Found within the "Nucleotide resources" section of the gene symbol report.}
#'   \item{uniprot_ids}{UniProt protein accession. Found within the "Protein resource" section of the gene symbol report.}
#'   \item{pubmed_id}{Pubmed and Europe Pubmed Central PMID(s).}
#'   \item{mgd_id}{Mouse genome informatics database ID. Found within the "Homologs" section of the gene symbol report.}
#'   \item{rgd_id}{Rat genome database gene ID. Found within the "Homologs" section of the gene symbol report.}
#'   \item{lsdb}{The name of the Locus Specific Mutation Database and URL for the gene separated by a | character, e.g. Mutations of the ATP-binding Cassette Transporter Retina|http://www.retina-international.org/files/sci-news/abcrmut.htm}
#'   \item{cosmic}{Symbol used within the Catalogue of somatic mutations in cancer for the gene. (No longer updated!).}
#'   \item{omim_id}{Online Mendelian Inheritance in Man (OMIM) ID http://www.omim.org/entry/<ID>}
#'   \item{mirbase}{miRBase ID http://www.mirbase.org/cgi-bin/mirna_entry.pl?acc=<ID>}
#'   \item{homeodb}{Homeobox Database ID http://homeodb.cbi.pku.edu.cn/gene_info.get?id=<ID>}
#'   \item{snornabase}{snoRNABase ID https://www-snorna.biotoul.fr//plus.php?snoid=<ID>}
#'   \item{bioparadigms_slc}{Symbol used to link to the SLC tables database at bioparadigms.org for the gene http://slc.bioparadigms.org/protein?GeneName=<SYMBOL>}
#'   \item{orphanet}{Orphanet ID http://www.orpha.net/consor/cgi-bin/OC_Exp.php?Lng=GB&Expert=<ID>}
#'   \item{pseudogene.org}{Pseudogene.org ID http://tables.pseudogene.org/<ID>}
#'   \item{horde_id}{Symbol used within HORDE for the gene http://genome.weizmann.ac.il/horde/card/index/symbol:<SYMBOL>}
#'   \item{merops}{ID used to link to the MEROPS peptidase database http://merops.sanger.ac.uk/cgi-bin/pepsum?mid=<ID>}
#'   \item{imgt}{Symbol used within international ImMunoGeneTics information system http://www.imgt.org/IMGT_GENE-DB/GENElect?query=2+<SYMBOL>&species=Homo+sapiens}
#'   \item{iuphar}{The objectId used to link to the IUPHAR/BPS Guide to PHARMACOLOGY database. To link to IUPHAR/BPS Guide to PHARMACOLOGY database only use the number (only use 1 from the result objectId:1) in the example URL http://www.guidetopharmacology.org/GRAC/ObjectDisplayForward?objectId=<ID>}
#'   \item{kznf_gene_catalog}{ID used to link to the Human KZNF Gene Catalog http://znf.igb.uiuc.edu/human/action/exploreView?type=locus&id=<ID>}
#'   \item{mamit-trnadb}{ID to link to the Mamit-tRNA database http://mamit-trna.u-strasbg.fr/mutations.asp?idAA=<ID>}
#'   \item{cd}{Symbol used within the Human Cell Differentiation Molecule database for the gene http://www.hcdm.org/index.php?option=com_molecule&cdnumber=<SYMBOL>}
#'   \item{lncrnadb}{lncRNA Database ID http://www.lncrnadb.org/<ID>}
#'   \item{enzyme_id}{ENZYME EC accession number http://enzyme.expasy.org/EC/<EC ACCESSION NUMBER>}
#'   \item{intermediate_filament_db}{ID used to link to the Human Intermediate Filament Database http://www.interfil.org/details.php?id=<ID>}
#' }
#' @source \url{ftp://ftp.ebi.ac.uk/pub/databases/genenames/new/tsv/hgnc_complete_set.txt}
"hgnc"

