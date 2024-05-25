# Guide

This tool enables researchers to generate insights from the Extended Peacekeeping Activities (EPACT) dataset. It includes a modular plotting tool, links to the data sources, and a map displaying the implementation of specific peacekeeping activities globally.

### Plotting tool

For activities and engagement categories, the tool offers plot creation in two modes:

- **Aggregated Mode:** This mode allows for the grouping of UNPKOs and multiple activities.
- **Mission Mode:** This mode draws a separate plot for each mission selected in the analysis.

#### Explanation of plotting parameters

##### Aggregated Mode

**Timeline:**
- Mission Month: Displays the count of months since the mission started on the Y-axis.
- Calendar Month: Displays the calendar month on the Y-axis.
- Aggregation by mission month allows comparison of activities over the lifespan of missions, while aggregation by calendar date provides insights into peacekeeping developments over generations.

**Missions:** Create grouping by UNPKOs. Different groups are displayed in separate plots. Make sure each missions is only added to one mission group.

**Activities:** Create groups of activities. The activities in each group will be added up and displayed as a line in the plot.

Note: In the current version of the tool, the X-axis in the "Aggregated" tab shows absolute numbers and not shares. Since the number and duration of missions vary, the graphs require careful interpretation, as a direct comparison of lines is not feasible.

##### Mission Mode

**Timeline:**
- Mission-Month: Displays the count of months since the mission started on the Y-axis.
- Calendar Month: Displays the calendar month on the Y-axis.
- Aggregation by mission month allows comparison of activities over the lifespan of missions, while aggregation by calendar date provides insights into peacekeeping developments over generations.

**Missions:** Select missions to include. Each mission is represented by a separate facet in the plot.

**Activities:** The activities to consider. The line in the graph will show the share of activities implemented, if multiple are selected.

#### Example: What comes first? Security- vs. peacebuilding-related tasks

A key debate in peacekeeping research is whether security-related or peacebuilding-related activities are implemented first. Prioritizing security-related activities, such as civilian protection, demilitarization, enforcing arms embargoes, or military operations, suggests a security-first approach. This would have implications for troop equipment prioritization, mission planning, and operational demands.

To get an initial overview, we use the 'Aggregated Mode' to depict the implementation of peacebuilding and security-related tasks (<a href="https://onlinelibrary.wiley.com/doi/full/10.1111/ajps.12650" target="_blank">Blair et. al 2020</a>). For the timeline, we choose 'Mission Month', as we want to see the development over the life span of UNPKOS.

<div style="width: 80vw; margin: auto">
<img src="example1.png" alt="Graph showing the implementation of peacekeeping activities versus security activities for all missions", style="max-width: 100%; height: auto">
</div>

We can see that across all missions, security-related tasks (Activities 1) implementation starts slightly faster. After less than a year, the implemented tasks reach their maximum and slightly decline  until the four year mark. For the peacebuilding activities (Activities 2), the implementation is more dynamic. After a bit more than a year, most tasks are implemented with a comparably stronger decline until the 4 year mark compared to security tasks. It is important to note that at the point of creating the example, the tool only allowed to show absolute numbers and not shares, which is problematic when the activity groups have different amounts of activities aggregated. In this example, there are 18 peacebuilding tasks and only 10 security-related tasks. Therefore, the interpretation needs to be careful as only the progression of the line can be compared to each other. Also, some missions end earlier than others, limiting the comparability on the Y-axis with absolute values further.

To gain a better overview, we add mission groups and plot three stabilization missions - a mission type with high military component often deployed in unstable theatres of operation - against a comparison group of three other randomly selected missions of equal length in Africa. The results indicate that overall the differences are not that significant However, stabilization missions generally implement more tasks (higher multidimensionality) and have slightly higher and more enduring implementation of security-related tasks over the course of the mission.

<div style="width: 80vw; margin: auto">
<img src="example2.png" alt="Graph showing the implementation of peacekeeping activities versus security activities for two subgroups of 3 missions each", style="max-width: 100%; height: auto">
</div>
