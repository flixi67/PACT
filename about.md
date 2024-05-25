# About

This interactive Shiny visualization presents the Extended Peacekeeping Activities Dataset (EPACT), which includes PACT 1.0 covering Africa and the novel PACT 2.0 covering Asia, the Americas, and Europe.

PACT 1.0 was initiated by [Robert Blair](https://robblair.net/) and [Hannah Smidt](https://hannahsmidt.com/) in 2017 for Peacekeeping Operations within Africa and later extended to the other continents in the project ["The Civilian Dimension of Peacekeeping Operations and Human Rights Promotion"](https://pcr.uu.se/research/research-themes/human-rights/the-civilian-dimension-of-peacekeeping-operations-and-human-rights-promotion/) (Sabine Otto, Constantin Ruhe, Hannah Smidt, Lisa Hultman, Jair van der Lijn).

#### Data

The EPACT dataset covers 40 peacekeeping missions from 1989 to 2018. While both PACT 1.0 and PACT 2.0 include 37 activities, there are differences between them. **Peace Process** and **Ceasefire** were only recorded in PACT 1.0, whereas **National Reconciliation** was only collected in PACT 2.0. When analyzing these activities, keep in mind that the visualization interprets missing values as not implemented when aggregating these activities.

Also, for the **Operations** variable containing patrols and the use of force, coding categories were adjusted, but the substantive meaning remained the same. Therefore, data could be merged in the data cleaning process and only appears as **Operations: Patrols/ Interventions** and **Operations: Use of Force** in the Shiny app.

Moreover, we coded for each activity its engagement category, allowing users to see not only what activities were implemented but also how they were implemented. The data records seven main engagement types: **Monitor**, **Advocate**, **Outreach**, **Material Support**, **Assist**, and **Implement**. An activity might have been implemented using multiple engagement types.

The dataset is based on reports from the Secretary General to the Security Council. Over 850 reports, including their annexes and addenda, were coded to create the dataset. For a detailed discussion on the data source and its validity, refer to [Blair et al. (2021, pp. 7-8)](https://onlinelibrary.wiley.com/doi/full/10.1111/ajps.12650).

#### Visualization

The Shiny app offers two main functionalities:

1. **Modular Plotting Tool:** This tool allows users to disaggregate and aggregate data based on UNPKO groups, individual UNPKOs, activities, and engagement categories. This flexibility enables in-depth analysis of specific activities and their implementation on the ground, as well as comparisons over time or by mission duration (months since mission start). This feature supports numerous applications, allowing users to explore specific aspects of the large dataset. The interface provides information about each mission, such as name, duration, continent (via icon), and deployment years, aiding in data aggregation. Users are encouraged to apply their own theories and categorizations of missions and activities. A [codebook](doku/PACT_codebook.pdf) is available in the repository for details on activities and engagement categories.

2. **Activity Map:** This map provides an overview of the implementation of activities and peacekeeping missions worldwide. Users can plot and animate up to three activities over time to observe temporal and spatial variations in peacekeeping activities.

Additionally, the app includes an overview page of the data sources, displaying coverage, reporting time frames, and links to official UN documents from which the data was coded and later aggregated into a mission-month format.

#### Funding

We thank the Swedish Research Council for their generous project funding. We also extend our gratitude to the Department of Peace and Conflict Research at Uppsala University for their support throughout the project. Special thanks to Hannah Smidt and Rob Blair for sharing the PACT 1.0 for visualization purposes here.

</br>

<img src="data/logos/VR_logo.png" alt="Kapradet Logo" width="60%" style="float: left"/> <img src="data/logos/UU_logo.svg" alt="Uni Uppsala Logo" width="40%" style="float: left"/>

#### Contact

This website and the interactive visualisations were created by Felix Kube during an internship at the Department of Peace and Conflict Research. You can learn more about the script used to produce this application or contribute on [GitHub](https://github.com/flixi67/PACT). Since this is an evolving project, we would love to hear if you have any suggestion for improvement, or if you have encountered any issue while using the app.

If you found a bug, don't kill it! It will ruin your karma. Rather report it to [me](mailto:felix.kube@uni.kn?subject=PACT%20Shiny%20app) or open up an issue on [GitHub](https://github.com/flixi67/PACT/issues).

For any questions about the PACT dataset or the project, do not hesitate to contact Sabine Otto ([sabine.otto\@pcr.uu.se](mailto:sabine.otto@pcr.uu.se?subject=PACT)).
