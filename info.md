This tool aims to let researchers create their own insights from the Peacekeeping Activities (PACT) dataset. To do so, you can find a modular plotting tool, links to our basis of data and a map overview of implementation of certain peackeeping activities across the world.

#### Plotting tool

For activities and engagement categories, the tool offers plot creation in "Aggregated" and "Mission" mode. The difference is, that "Aggregated" allows for the grouping of missions and multiple activity groups, while "Mission" draws a separate plot for each mission selected in the analysis. The latter is recommended for insights into the implementation of specific activities in missions.

##### Explanation of plotting parameters

###### Aggregated

- Timeline: Changes the Y-axis to either mission month, where the months within each mission are indexed or by date. Aggregation per mission month allows to compare implemented activies over the lifespan of missions, whereas aggregation by calendar date can give insights into developments over generations of peacekeeping.
- Missions: Create grouping for missions. Different groups will be displayed in separate plots. Make sure each missions is only added to one mission group.
- Activities: Create groups for activity aggregation. The activities in each group will be added up and displayed as a line in the plot.

In the current version of the tool, the X-axis in the "Aggregated" tab shows absolute numbers and not shares. Since the number of missions and their length in months varies, the graphs need careful interpretation, as a direct comparison of lines is not possible.

###### Mission

- Timeline: Changes the Y-axis to either mission month, where the months within each mission are indexed or by date. Aggregation per mission month allows to compare implemented activies over the lifespan of missions, whereas aggregation by calendar date can give insights into developments over generations of peacekeeping.
- Missions: Missions to include. Each mission will be represented by a seperate facet.
- Activities: The activities to consider. The line in the graph will show the share of activities implemented, if multiple are selected.

#### Example: What comes first? Security- vs. peacebuilding-related tasks

A long-standing debate in peacekeeping research is, if and when security-related tasks are implemented first. The prioritization of security-related tasks such as civilian protection, demilitarization, the enforcement of arms embargos or even military operations would hint at a sequentation within missions and therefore also have implications for prioritizing the necessary equipment for the troop, therefore the mission planning and other operational demands.

To get a first overview, we use the 'Aggregated' mode to depict the implementation of peacebuilding, security-related and cross-cutting tasks (<a href="https://onlinelibrary.wiley.com/doi/full/10.1111/ajps.12650" target="_blank">Blair et. al (2020)</a>). For the timeline, we choose 'Mission month', as we want to see developments within the missions.

![](http://127.0.0.1:3201/example1.png)

We can see that across all missions, security-related tasks (Activities 1) implementation starts slightly faster. After less than a year, the implemented tasks reach their maximum and slightly decline  until the four year mark. For the peacebuilding activities (Activities 2), the implementation is more dynamic. After a bit more than a year, most tasks are implemented with a comparably stronger decline until the 4 year mark compared to security tasks. It is important to note that at the point of creating the example, the tool only allowed to show absolute numbers and not shares, which is problematic when the activity groups have different amounts of activities aggregated. In this example, there are 18 peacebuilding tasks and only 10 security-related tasks. Therefore, the interpretation needs to be careful as only the progression of the line can be compared to each other. Also, some missions end earlier than others, limiting the comparability on the Y-axis with absolute values further.

To get a better overview, we add mission groups and plot three stabilization missions, a mission type with high military component often deployed in unstable theatres of operation, against a comparison group of three other randomly selected missions of equal length in Africa. The results show that overall the differences are not that significant, but that stabilization missions implement more of the tasks in general (higher multidimensionality) and that the implementation of security-related tasks is slightly higher and endures longer over the course of the mission.

![](http://127.0.0.1:3201/example2.png)
