#!/bin/bash
echo "Site,idigbio,gbif,combined" >combined_species_list/summary_records.csv
for site in `cat site.txt`; do
	cat Site_range_idigbio/${site}_idigbio_specieslist.csv|sed '1d' >combined_species_list/${site}.tmp
	cat Site_range_gbif/${site}_gbif_specieslist.csv|sed '1d' >>combined_species_list/${site}.tmp
	ii=$(wc -l Site_range_idigbio/${site}_idigbio_specieslist.csv|cut -f1 -d' ')
	gg=$(wc -l Site_range_gbif/${site}_gbif_specieslist.csv|cut -f1 -d' ')
	sort combined_species_list/${site}.tmp|uniq >combined_species_list/${site}.txt
	nn=$(wc -l combined_species_list/${site}.txt|cut -f1 -d' ')
	echo -e "$site,$ii,$gg,$nn" >>combined_species_list/summary_records.csv
	rm combined_species_list/*.tmp
done
