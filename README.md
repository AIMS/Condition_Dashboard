# Reef Habitat Condition Dashboard

## About
The dashboard is a Rshinny application to visualise outputs from a framework of indicators that describe the condition of coral reef habitats in the Great Barrier Reef.This dashboard is not a production-ready tool, rather it is a beta or draft version to provide examples of how the indicators can be visualised to report on the condition of coral reef habitats. 


## Background
At present, there are multiple data streams available from existing coral reef monitoring programs that measure aspects of condition and recovery. However, a holistic assessment of condition and recovery requires a multidimensional approach acknowledging the variability of complex ecological processes across the Reef. Adding to this ecological complexity is the practical complexity in applying such knowledge to define a high-level metric that informs management and evaluates the efficacy of management strategies towards proposed conservation targets. The latter requires heuristic, measurable, sensitive, scalable, and interpretable indicators to facilitate conservation planning and management. 
Coral reef condition reporting is currently delivered through numerous products, such as annual Reef Condition Reports (AIMS), the Reef Report Card (Commonwealth and Queensland State Government), and the Outlook Report (GBRMPA), in part, using different data streams, assessment techniques, and reporting approaches. In addition to knowing the condition of reef habitats, a vital element of the Coral Reef Habitats Objective in the Reef 2050 Plan is evaluating the resilience of these ecosystems. 
Recognising coral reefs are naturally dynamic, the capacity of coral reefs to recover after disturbance is a critical attribute for meaningful resilience assessments. The Framework outlined in this document has been developed as a tool to make data, information, and knowledge (provided by the ongoing monitoring programs that form the core of the RIMReP) available in a relevant, integrated form that enables high-level reporting and supports decision making at a range of management scales. 
The focus of this document is to detail the rationale for the indicator framework and, by way of example, its application. The process behind selecting indicators and the detailed methods behind creating their metrics can be found in the Methodology Document.
It is important to note that the development of this Framework is intended to complement existing data summaries, such as the AIMS monitoring website (https://apps.aims.gov.au/reef-monitoring/reefs) where trends in much of the data underlying the developed metrics and summaries of key disturbance events can be found.

## Indicator framework for Reef Habitat Condition
Coral communities across the Great Barrier Reef World Heritage Area (the Reef) vary spatially and temporally due to chronic and acute selective pressures. To sensibly assess the condition of coral communities in a particular place requires both:
•	having an understanding of the nature of coral communities at that place to ensure the observed condition is assessed against realistic expectations and,
•	recognising the importance of recovery processes to their ongoing resilience.

Five indicators were selected via a process of expert elicitation and are fully described in the accompanying Indicator Methods documentation (Gonzalez-Rivero et al. 2023, reference below). The framework combining these indicators was co-designed and reviewed by GBRMPA in various stakeholder meetings during the life of this project to ensure its applicability for management. For more details please refer to the methodological description in the reference below.

The indicator framework is designed to allow the rapid summary and concise interpretation of the state and resilience of coral communities based on routine monitoring time series. 
To help users interact with the framework output, a dashboard has been created to visualise summarised indicator scores at various spatial scales. It is stressed that this product was not a contracted output and was developed primarily as a visualisation tool to provide an example of the intended use of the Framework. Such a dashboard's future maintenance or refinement will be at the end-user's discretion.   
The dashboard provides five summaries of output indicator scores at a selection of spatial scales. 
•	A polar bar plot shows the distribution of either reference or contextual metric scores for each indicator. The plot provides the median score for the metric along with 95% credible intervals and a dashed reference line at a score of 0.5. The colour of the bars indicates where these intervals lie relative to the indicators and their historical reference distribution: where the upper credible interval is below 0.5, the indicator value is deemed to be below historical reference levels, and the indicator plotted in red, if credible intervals span 0.5 the indicator is deemed to conform to historical reference levels and plotted in yellow, and where the lower credible interval is greater than 0.5 the indicator value is above historical levels and plotted in green. Indicators plotted in red satisfy the "No" categorisation of state used in the classification of reef condition. 
•	A categorisation of reef condition.
•	A data-driven text generation (Synoptic text) summarises the results in plain English. These summaries provide not only descriptions of the reference scores but also contextualise those scores based on contextual metric scores. The order and content of these sentences help to guide the user through the most pertinent points for interpreting the condition of reefs.
•	A details panel in which the user can explore the time series of the indicator scores, taxa that have shown the most significant relative abundance changes compared to the historical reference period and the proportion of reefs falling into the framework condition classifications.
•	A map on which the reefs informing the summary of reef condition are the aggregated spatial scale of interest is indicated. The colour of their symbols shows the reef-level condition classification.


## Reference
Gonzalez-Rivero, M., Thompson A., Johns K., Ortiz J., Kim S., Fabricius K., Emslie M., Hoey A., Hoogenboom M., Barrios-Novak K., McClure E., Pandolfi J, Mumby P. J., Murray L., Schaffelke B., Staples T. (2023) Indicator Framework for the evaluation of the condition of coral reef habitats in the Great Barrier Reef: Methodological Documentation. Report prepared for the Great Barrier Reef Foundation. Australian Institute of Marine Science, Townsville. (138 pp)

# Disclaimer
The dashboard is not an official AIMS reporting product. It was designed to provide examples for data visualisation of products derived from the IMR Coral Reef Habitat Indicator Framework project. 

