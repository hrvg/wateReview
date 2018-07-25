# making the query

---

[TOC]

---

## method for selecting a database

Choice should be based on the following criteria : 
- inclusion of latin america paper databases (e.g. SciELO)
- number of results in English but also in Spanish and Portuguese
- alleged noise level from expert assessment of the relevancy of the results

## Web of Science

## Scopus

- allow for exporting 20k citation at once in `.csv`
- Scopus includes part of SciELO journals and has more journal in languages that are not English

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
KEY(
(Water) 
)
AND 
KEY
(
(Access) OR (Agricultural) OR (Aquifer*) OR (bay*) OR (catchment*) OR ("Change of storage") OR ("Climate change") OR (Contamination*) OR ("Cost-benefit analysis") OR ("Crop Coefficient?") OR (Dam*) OR ("Decision?maker?") OR (delta*) OR (Demand*) OR (Desalination*) OR (Drought*) OR ("Ecosystem service?") OR (Environment*) OR ("Environmental degradation?") OR (Environmental AND issue*) OR (Environmental AND justice) OR (estuar*) OR (Evapotranspiration*) OR (Flood*) OR ("Focus group?") OR (Fog*) OR (GIS) OR (Groundwater) OR (Hydroeconomic*) OR (Hydrologic) OR ("hydrologic region?") OR ("In-situ monitoring") OR (Interview*) OR (Irrigation) OR ("irrigation district?") OR (lake*) OR (Modeling) OR (Municipal) OR ("Non-point source?") OR (Optimization) OR (Over-draft*) OR ("Point source?") OR (Pollution*) OR (Precipitation*) OR (Protected) OR (Rain-fed) OR (Recharge*) OR (Recreation*) OR (Reliability) OR ("Remote sensing") OR (Reservoir*) OR (Resilience) OR (river*) OR (River*) OR (Rural) OR (Scenari*) OR ("Sea water intrusion?") OR ("Seawater intrusion?") OR (Sewage*) OR (Shortage*) OR (Simulation*) OR (Snowmelt) OR (Social AND justice) OR ("Social issue?") OR ("Political issue?") OR (Stakeholder*) OR (stream*) OR (Sub-basin*) OR (Subsidence) OR (Supply) OR (Surface AND water) OR (Surveying) OR ("Sustainability ind?") OR (Transboundar*) OR (Unprotected) OR (Urban) OR (Vulnerabilit*) OR (Wastewater) OR (Water) OR (footprint) OR (governance*) OR (market*) OR (over-allocation*) OR (price*) OR (treatment*) OR (watershed*) OR (Well*) OR (Wetland*) AND NOT (Marine)
)
AND KEY
(
("puerto rico") OR (belize) OR (turks AND caicos AND islands) OR (aruba) OR (argentina) OR (jamaica) OR ("virgin islands") OR (dominican AND republic) OR (haiti) OR (colombia) OR (grenada) OR (antigua AND barbuda) OR ("saint vincent" AND grenadines) OR (chile) OR ("saint lucia") OR (bahamas) OR ("saint kiss" AND nevis) OR (guadeloupe) OR (french AND guyana) OR (peru) OR (mexico AND NOT ("New Mexico")) OR (suriname) OR (honduras) OR (panama) OR (brazil) OR (martinique) OR (cuba) OR ("costa rica") OR (ecuador) OR (nicaragua) OR ("saint barthelemy") OR (guatemala) OR (uruguay) OR (trinidad AND tobago) OR (venezuela) OR (bolivia) OR ("cayman islands") OR (paraguay) OR (barbados) OR ("latin america?") OR ("south america?") OR (caribbean) OR ("central america?") OR (el AND salvador)
) 
AND  DOCTYPE ( ar )  AND  ( EXCLUDE ( PREFNAMEAUID ,  "Anon#1" )  OR  EXCLUDE ( PREFNAMEAUID ,  "Undefined#Undefined" ) )  AND  ( EXCLUDE ( SUBJAREA ,  "MEDI" )  OR  EXCLUDE ( SUBJAREA ,  "BIOC" )  OR  EXCLUDE ( SUBJAREA ,  "IMMU" )  OR  EXCLUDE ( SUBJAREA ,  "ARTS" )  OR  EXCLUDE ( SUBJAREA ,  "VETE" )  OR  EXCLUDE ( SUBJAREA ,  "PHAR" )  OR  EXCLUDE ( SUBJAREA ,  "NURS" )  OR  EXCLUDE ( SUBJAREA ,  "PSYC" )  OR  EXCLUDE ( SUBJAREA ,  "PHYS" )  OR  EXCLUDE ( SUBJAREA ,  "NEUR" )  OR  EXCLUDE ( SUBJAREA ,  "HEAL" )  OR  EXCLUDE ( SUBJAREA ,  "DENT" )  OR  EXCLUDE ( SUBJAREA ,  "MATH" )  OR  EXCLUDE ( SUBJAREA ,  "Undefined" ) )  AND  ( LIMIT-TO ( LANGUAGE ,  "English" ) )
```

## webscrapping with EndNote

EndNote is unable to search for more than 1000 `.pdf` at once.

1. Create a Smart Group with the files without `.pdf`
2. Select all the bibliography with `ctrl + A`
3. Right click $\to$ `Find Full Text` 
4. `EndNote` starts searching for Full text
5. After $\sim 6$ hours, the process should terminate
6. Click on the Smart Groups `Found URL` and `Not Found` and move the references that `EndNote` was unable to find a `.pdf`for to the trash
7. Go back to step 2.


---

_Hervé Guillon, July 18th 2018_