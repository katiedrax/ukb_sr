# 00_clean_csv.R

Clean csv of 564 included UKB articles which were screened then classified in Rayyan and creates search string of DOIs for Scopus. Exports search string, cleaned csv containing all articles and cleaned csv containing only obeservational epidemiology articles.

# 01_merge_refs_cits_scopus.R

Merges csv of all articles, their citations and the Scopus results produced by the search string

# 02_ukb_clean_merged.R

Cleans the csv outputed by 01_merge_refs_cits_scopus.R and removes unnecessary data exported by Scopus

# Procedure

1. Run 00_clean_csv.R
1. Paste scopus_search_string into advanced search in Scopus > click search > select 'All', click drop down arrow next to 'RIS export' > select 'CSV' > select all items you want to export (see X) click 'Export' and save csv as 'scopus' in this project's 'data' folder
1. Run 01_merge_refs_cits_scopus.R
1. Run 02_ukb_clean_merged.R