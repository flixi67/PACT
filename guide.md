# Guide

This tool enables researchers to generate insights from the Extended Peacekeeping Activities (EPACT) dataset. The  EPACT tool includes PACT 1.0, which covers Africa (data collected by Hannah Smidt and Rob Blair, not yet fully released), and the novel PACT 2.0, covering Asia, the Americas, and Europe (collected by Sabine Otto). EPACT provides detailed information on activities implemented by United Nations Peacekeeping Operations (UNPKOs) and how these activities were carried out, categorized by engagement types. It includes a modular plotting tool and a map displaying the implementation of specific peacekeeping activities globally.

### Plotting tool

For activities and engagement categories, the tool offers plot creation in two modes:

- **Aggregated Mode:** This mode allows for the grouping of UNPKOs and multiple activities.
- **Mission Mode:** This mode draws a separate plot for each UNPKO selected in the analysis.

#### Explanation of plotting parameters

##### Aggregated Mode

**Timeline:**
- Mission Month: Displays the count of months since the UNPKO started on the Y-axis.
- Calendar Month: Displays the calendar month on the Y-axis.
- Aggregation by mission month allows comparison of activities over the lifespan of UNPKOs, while aggregation by calendar date provides insights into peacekeeping developments over time.

**Missions:** Create grouping by UNPKOs. Different groups are displayed in separate plots. Make sure each missions is only added to one mission group.

**Activities:** Create groups of activities. The activities in each group will be added up and displayed as a line in the plot.

<span style="color:red">Note: In the current version of the tool, the X-axis in the **Aggregated Mode** shows absolute numbers. Keep in mind that in different months a different number of your selected missions may be active.</span>

##### Mission Mode

**Timeline:**
- Mission-Month: Displays the count of months since the UNPKO started on the Y-axis.
- Calendar Month: Displays the calendar month on the Y-axis.
- Aggregation by mission month allows comparison of activities over the lifespan of UNPKOs, while aggregation by calendar date provides insights into peacekeeping developments over time.

**Missions:** Select UNPKOs to include. Each UNPKO is represented by a separate facet in the plot.

**Activities:** The activities to consider. The line in the graph will show the share of activities implemented, if multiple are selected.

#### Example: What comes first? Security- vs. peacebuilding-related activities

A debate in peacekeeping research is whether security-related or peacebuilding-related activities are implemented first. Implementing security-related activities, such as civilian protection, demilitarization, arms embargoes, or use of force before peacebuilding activities (voter education, election support, human rights, etc.) suggests a security-first approach. Empirically, we would expect that the peak of security activities is reached before the peak of peacebuilding.

To plot these trends, we use the ‘Aggregated Mode’ to depict the implementation of peacebuilding and security-related tasks (<a href="https://onlinelibrary.wiley.com/doi/full/10.1111/ajps.12650" target="_blank">Blair et. al 2020</a>). For the timeline, we choose 'Mission month', as we want to see the development over the life span of UNPKOs.

<div style="width: 80vw; margin: auto">
<img src="example1.png" alt="Graph showing the implementation of peacekeeping activities versus security activities for all missions", style="max-width: 100%; height: auto">
</div>

The plot illustrates the number of implemented activities across all UNPKOs in EPACT, with the Y-axis representing the count of activities and the X-axis representing the months since UNPKO deployment. It is expected that the number of implemented peacebuilding activities (*Activities 2*) surpasses that of security activities (*Activities 1*), as the former includes 18 activities while the latter includes only 10.

Examining the peaks reveals the following patterns: Security activities take a few months to initiate and then increase rapidly, peaking around month 12 before declining below the initial level by around month 30. Similarly, peacebuilding activities also require a few months to get started, but they reach their peak around month 18 and continue to decline, falling below the initial level by around month 75. The pattern points toward a security-first approach across all UNPKOs. 
A different possibility is to explore the security first question by diving into different types of UNPKOs. The left plot shows the implementation of peacebuilding and security activities for three so-called stabilization mission (*Activities 1*), which are sent to ongoing conflicts and have the mandate to use force. The right panel is based on three non-stabilization UNPKOs with similar duration from Africa (*Activities 2*).

Across stabilization UNPKOs, security activities reach a peak around month 10 and maintain relatively high levels until they begin to decline around month 40, falling below initial levels after month 80. Similarly, peacebuilding activities follow a comparable trend. This pattern implies a reliance on the ongoing implementation of security activities to support peacebuilding efforts. Such an observation resonates with the operational context of stabilization UNPKOs operating within prolonged conflict settings.

In contrast, the plot on the right displays a different trend. The number of security activities peaks first around month 12 and again around month 30 before declining below initial levels by month 37. Meanwhile, peacebuilding activities exhibit a steady increase, reaching their peak around month 30, maintaining stability, and declining below initial levels around month 85. This quick analysis suggest that a security-first approach may not be applicable to all UNPKOs. 

<div style="width: 80vw; margin: auto">
<img src="example2.png" alt="Graph showing the implementation of peacekeeping activities versus security activities for two groups of three missions each", style="max-width: 100%; height: auto">
</div>
