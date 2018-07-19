# making the query

---

[TOC]

---

## Scopus

- allow for exporting 20k citation at once in `.csv`

### work-flow

1. perform the query
2. select years to restrict the results to under 20k entries
3. save entries as `.csv` : 
	+ on `Linux`, use `Chromium`, the export does not work with `Firefox`
	+ wait for an email with the link for downloading, this might takes 10 minutes
4. once all entries are saved, merge the `.csv` files with `./R/querying/main.R`
5. import the `.csv` to `Publish or Perish`
6. save the `.csv` into a `.ris`
7. import the `.ris` into `EndNote`


### query with English terms

```
TITLE-ABS-KEY ( ( ( access ) OR ( agricultural ) OR ( aquifer* ) OR ( bay* ) OR ( catchment* ) OR ( change* AND of AND storage ) OR ( climate AND change ) OR ( contamination* ) OR ( cost-benefit* AND analysis ) OR ( crop* AND coefficient* ) OR ( dam* ) OR ( decision-maker* ) OR ( delta* ) OR ( demand* ) OR ( desalination* ) OR ( drought* ) OR ( ecosystem* AND service* ) OR ( environment* ) OR ( environmental AND degradation* ) OR ( environmental AND issue* ) OR ( environmental AND justice ) OR ( estuar* ) OR ( evapotranspiration* ) OR ( flood* ) OR ( focus AND group* ) OR ( fog* ) OR ( gis ) OR ( groundwater ) OR ( hydroeconomic* ) OR ( hydrologic ) OR ( hydrologic AND region* ) OR ( in-situ AND monitoring ) OR ( interview* ) OR ( irrigation ) OR ( irrigation AND district* ) OR ( lake* ) OR ( modeling ) OR ( municipal ) OR ( non-point AND source* AND pollution* ) OR ( ocean* ) OR ( optimization ) OR ( over-draft* ) OR ( point AND source* AND pollution* ) OR ( pollution* ) OR ( precipitation* ) OR ( protected ) OR ( rain-fed ) OR ( recharge* ) OR ( recreation* ) OR ( reliability ) OR ( remote AND sensing ) OR ( reservoir* ) OR ( resilience ) OR ( river* ) OR ( river* ) OR ( rural ) OR ( scenari* ) OR ( sea* ) OR ( sea AND water AND intrusion* ) OR ( sewage* ) OR ( shortage* ) OR ( simulation* ) OR ( snowmelt ) OR ( social AND justice ) OR ( ( social OR political ) AND issue* ) OR ( stakeholder* ) OR ( stream* ) OR ( sub-basin* ) OR ( subsidence ) OR ( supply ) OR ( surface AND water ) OR ( surveying ) OR ( sustainability AND index ) OR ( transboundary ) OR ( unprotected ) OR ( urban ) OR ( vulnerability ) OR ( wastewater ) OR ( water ) OR ( water AND footprint ) OR ( water AND governance* ) OR ( water AND market* ) OR ( water AND over-allocation* ) OR ( water AND price* ) OR ( water AND treatment* ) OR ( watershed* ) OR ( well* ) OR ( wetland* ) ) AND ( ( puerto AND rico ) OR ( belize ) OR ( turks AND caicos AND islands ) OR ( aruba ) OR ( argentina ) OR ( jamaica ) OR ( virgin AND islands ) OR ( dominican AND republic ) OR ( haiti ) OR ( colombia ) OR ( grenada ) OR ( antigua AND barbuda ) OR ( saint AND vincent AND grenadines ) OR ( chile ) OR ( saint AND lucia ) OR ( bahamas ) OR ( saint AND kiss AND nevis ) OR ( guadeloupe ) OR ( french AND guyana ) OR ( peru ) OR ( mexico ) OR ( suriname ) OR ( honduras ) OR ( panama ) OR ( brazil ) OR ( martinique ) OR ( cuba ) OR ( costa AND rica ) OR ( ecuador ) OR ( nicaragua ) OR ( saint AND barthelemy ) OR ( guatemala ) OR ( uruguay ) OR ( trinidad AND tobago ) OR ( venezuela ) OR ( bolivia ) OR ( cayman AND islands ) OR ( paraguay ) OR ( barbados ) OR ( el AND salvador ) ) ) AND DOCTYPE ( ar ) AND ( EXCLUDE ( PREFNAMEAUID,"Anon#1 " ) OR EXCLUDE ( PREFNAMEAUID," Undefined#Undefined " ) ) AND ( EXCLUDE ( SUBJAREA,"MEDI" ) OR EXCLUDE ( SUBJAREA,"BIOC" ) OR EXCLUDE ( SUBJAREA,"IMMU" ) OR EXCLUDE ( SUBJAREA,"ARTS" ) OR EXCLUDE ( SUBJAREA,"VETE" ) OR EXCLUDE ( SUBJAREA,"PHAR" ) OR EXCLUDE ( SUBJAREA,"NURS" ) OR EXCLUDE ( SUBJAREA,"PSYC" ) OR EXCLUDE ( SUBJAREA,"PHYS" ) OR EXCLUDE ( SUBJAREA,"NEUR" ) OR EXCLUDE ( SUBJAREA,"HEAL" ) OR EXCLUDE ( SUBJAREA,"DENT" ) OR EXCLUDE ( SUBJAREA,"MATH" ) OR EXCLUDE ( SUBJAREA,"Undefined" ) ) 
```

---

_Hervé Guillon, July 18th 2018_