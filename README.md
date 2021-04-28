
## Understanding Stakeholder collaboration, Hazard Mitigation properties and Resource Allocation during disaster response.
> Sponsored - GMU SID & REVA DEWBERRY DEPARTMENT OF CIVIL, ENVIRONMENTAL & INFRASTRUCTURE ENGINEERING


- [Description](#Description)
- [howtouse](#howtouse)
- [AuthorInfo](#AuthorInfo)


<!-- toc -->
----
### Description

Following a disaster, maintaining and restoring community lifelines is a paramount task, which requires stakeholders to timely take effective response actions. During disaster response, Federal Emergency Management Agency (FEMA) is a primary stakeholder that is responsible for (1) coordinating and assigning response tasks to other stakeholders and (2) allocating grants to support the impacted community. Aiming at facilitating the disaster response of FEMA and stakeholders collaboration, the objective of this project is
to (1) design a systematic approach that identifies respective stakeholdersinvolved in response tasks and (2) design a machine-learning model that performs grant program classification given requests from the impacted community. The project outcomes expect to enhance FEMA’s disaster response in terms of task assignment and grant allocation.


----
### howtouse

**DATASET**: <br/>
**CleanedMission Assignments.csv:** <br/>
This dataset contains the processed mission assignment dataset. <br/>
**Stakeholder Dictionary.csv:** <br/>
This file contains a list of stakeholder names. It is used to extract stakeholder collaboration information from the mission assignment dataset. 
**HU_stakeholder.csv and HU_interaction.csv:** <br/>
These two datasets contain collaboration information (i.e., stakeholder and stakeholder interaction) for Hurricanes from the mission assignment dataset.<br/><br/>
**CODE:** <br/>
**MA Exploration.R:** <br/>
This code takes the raw mission assignment dataset and cleans the dataset. In addition, this code contains exploratory data analysis and visualizations.
Collaboration Information Extraction.py: This code takes the cleaned mission assignment dataset and uses the defined stakeholder dictionary to extract stakeholder collaboration information. <br/>
**Identification of High Performing Stakeholders.R:**<br/>
This code takes the extracted stakeholder collaboration information for hurricane-related disasters and calculated centrality to identify high-performing stakeholders <br/>

----
### AuthorInfo
Team Maverick - Avinash Yavvari, Yitong Li, Srikaran Elakurhty, Stephanie Olson, Neha Lad, Ramani Kallam Reddy<br/>
George Mason University<br/>
DAEN 690<br/>
Spring 2021<br/>
