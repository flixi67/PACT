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

<span style="color:red">Note: In the current version of the tool, the X-axis in the **Aggregated Mode** shows absolute numbers. Since the number and duration of missions may vary in your custom analysis, the graphs require careful interpretation. Keep in mind that in different months a different number of your selected missions may be active.</span>

##### Mission Mode

**Timeline:**
- Mission-Month: Displays the count of months since the mission started on the Y-axis.
- Calendar Month: Displays the calendar month on the Y-axis.
- Aggregation by mission month allows comparison of activities over the lifespan of missions, while aggregation by calendar date provides insights into peacekeeping developments over generations.

**Missions:** Select missions to include. Each mission is represented by a separate facet in the plot.

**Activities:** The activities to consider. The line in the graph will show the share of activities implemented, if multiple are selected.

#### Example: What comes first? Security- vs. peacebuilding-related tasks

A key debate in peacekeeping research is whether security-related or peacebuilding-related activities are implemented first. Prioritizing security-related activities, such as civilian protection, demilitarization, enforcing arms embargoes, or military operations, suggests a security-first approach. This would have implications for troop equipment prioritization, mission planning, and operational demands.

To get an initial overview, we use the 'Aggregated Mode' to depict the implementation of peacebuilding and security-related tasks (<a href="https://onlinelibrary.wiley.com/doi/full/10.1111/ajps.12650" target="_blank">Blair et. al 2020</a>). For the timeline, we choose 'Mission month', as we want to see the development over the life span of UNPKOs.

<div style="width: 80vw; margin: auto">
<img src="example1.png" alt="Graph showing the implementation of peacekeeping activities versus security activities for all missions", style="max-width: 100%; height: auto">
</div>

We can see that across all missions, security-related tasks (*Activities 1*) implementation starts slightly faster. After less than a year, the implemented tasks reach their maximum and slightly decline  until the four year mark. For the peacebuilding activities (*Activities 2*), the implementation is more dynamic. After a bit more than a year, most tasks are implemented with a comparably stronger decline until the four year mark compared to security tasks. Remember it plots the number of activities. In this example, there are 18 peacebuilding activities and only 10 security-related activities. Therefore, the interpretation needs to be careful as only the progression of the line can be compared to each other. Also, some missions end earlier than others, limiting the comparability on the Y-axis.

To gain a better overview, we add mission groups and plot three stabilization missions (*Mission group 1*) - a mission type with high military component often deployed in unstable theaters of operation - against a comparison group of three other missions of similar length in Africa (*Mission group 2*). The plot indicates only minor differences. Stabilization missions generally implement more activities (higher multidimensionality) and have slightly higher and more enduring implementation of security-related tasks over the course of the mission.

<div style="width: 80vw; margin: auto">
<img src="example2.png" alt="Graph showing the implementation of peacekeeping activities versus security activities for two groups of three missions each", style="max-width: 100%; height: auto">
</div>
