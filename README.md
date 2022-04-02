# Interactive visualization of peacekeeping activites
This is an interactive Shiny visualization of data on UN Peacekeeping Operations, created by Felix Kube during a two-month internship at the Department of Peace and Conflict Research. The project aims to give researchers a preview of the newly collected and highly interesting data on peacekeeping activities on the ground, which are not yet published. 

The data was collected in two turns. PACT 1.0 was initiated by Hannah Smidt in 2017 (?) for Peacekeeping Operations within Africa and later extended to the other continents in the project ["The Civilian Dimension of Peacekeeping Operations and Human Rights Promotion"](https://pcr.uu.se/research/research-themes/human-rights/the-civilian-dimension-of-peacekeeping-operations-and-human-rights-promotion/) (Sabine Otto, Constantin Ruhe, Hannah Smidt, Lisa Hultman, Jair van der Lijn).

The app can be accessed [here](https://flixi67.shinyapps.io/pact/).

## Data

The dataset covers 40 peacekeeping missions between 1989 and 2018. In the first round of data collection, 37 activities have been recorded. The codebook was slightly adjusted for the second round of data collection. ***Peace Process*** and ***Ceasefire*** were only recorded in PACT 1.0 (African countries), while ***National Reconciliation*** was only included for PACT 2.0 data. This should be kept in mind when analyzing these activities, as the visualization interprets the missing values as not implemented when including these activities in the aggregation. Also, for the ***Operations*** variable containing patrols and the use of force, coding categories were adjusted, but the substantive meaning remained the same. Therefore, data could be merged in the data cleaning process and only appears as ***Operations: Patrols/ Interventions*** and ***Operations: Use of Force*** in the Shiny app.

Further, for each of the activities the engagement category was coded. This is particularly interesting, because this not only allows to see *what* but also *how* activities were implemented. The data records 7 main engagement types - ***Monitor***, ***Advocate***, ***Outreach***, ***Material Support***, ***Assist*** and ***Implement***. It is possible that the same activity was implemented using different engagement types. Special engagement types and more precise sub-categories were included for some activities and will be available with the official release of the data.

The basis of data is the reporting of the Secretary General to the Security Council. For the dataset, more than 850 reports including their annexes and addenda were coded. For a discussion of the reports and data validity, see [Blair et al. (2021, p. 7-8)](https://onlinelibrary.wiley.com/doi/full/10.1111/ajps.12650).

## Visualization

The Shiny app consists of two main functionalities. First, a modular plotting tool allowing the user to disaggregate and aggregate the data according to mission groups, single missions, activities and engagement categories. This flexibility allows to deeply engage in the specific activities implemented and the way they are carried out on the ground, as well as to compare differences across time or duration of the mission (months since mission start).

This allows for manyfold applications, as the user can delve in to very specific aspects of the large data. However, this also requires some predefined idea what question to ask the data as well as some knowledge on the missions and the activities recorded. The interface gives some information on the mission - like name, duration, continent (in the icon) and years of deployment. This helps in aggregating the data for example between different generations of peacekeeping or prevailing types of operations (traditional vs. multidimensional peacekeeping). However, it is strongly encouraged to use one's own theory and categorization of missions and activities. For information on the activities and engagement categories we added a [codebook](doku/PACT_codebook.pdf) to the repository.

Second, the app offers an activity map with an overview of the implementation of activities and peacekeeping missions across the globe. Up to three activities can be plotted and animated over time to get an overview of temporal and spatial variation in peacekeeping.

We added an overview page of the data sources that shows the coverage, reporting time frames and links to official UN documents, from which the data was coded and later aggregated into a mission-month format.

## Examples

### Does Security come first? (Trying to find) an answer to the lengthy discussion in peackeeping research
