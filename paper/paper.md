---
title: 'Fieldactivity: A shinyapp to store field management events'
tags:
  - R
  - RShiny
  - Ecological modelling
  - Field management event
  - Carbon cycle
  - Golem
authors:
  - name: Henri Kajasilta
    orcid: 0000-0003-0872-xxxx
    affiliation: 1 # (Multiple affiliations must be quoted) "1,2"
  - name: Otto Kuusela
    orcid: 0000-0000-0000-0000
    affiliation: 2
  - name: Istem Fer
    affiliation: 1
affiliations:
 - name: Finnish Meteorological Institute
   index: 1
 - name: Institution 2
   index: 2
citation_author: Kajasilta et. al.
date: 29 March 2022
year: 2022
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
---

# Summary

The usability of models is crucially dependent on the available information that is given as an input to the model. Ecosystem models are not an exception and can require input values e.g. from meteorological drivers, soil measurements or plant functional types in order to simulate fields properly from ecological systems point of view. These inputs may require continuous measurements to provide a comprehensive picture of the ecosystem changes on the field and are therefore usually automated to continuously gather data from the fields. Repeated continuous measurements are especially important while conducting near-term ecological forecasting and the time-period is measured in years rather than decades [@dietze2018iterative]. However, in addition to the aforementioned measurements, an essential part of the field activity are the field management events, which are rather difficult to measure automatically, but can be easily reported by humans. In agricultural sites, field management events are frequently practised by farmers, when they are taking actions such as planting, fertilising or harvesting. These actions can be implemented several times in a year per field site and bookkeeping of these events is equally important for the functionality of ecological models.

Field management events change significantly the properties of the field, which can in practice, for example, change the carbon cycle in the field. From a modelling point of view, it is highly important to keep count of these field management activities and gather all of the information relating to these events that might be relevant for field simulations. In order to make the process even smoother and to save working hours in the modelling phase, it is beneficial to report these activities in a specific form. This makes the reports more applicable and possibly additional modifying is avoided.


# Statement of need

Several farmers are testing different farming methods on their fields in Finland. The farming methods are taking place in the farms wherein continuous measurements are also conducted in atmosphere, vegetation and soil. In order to achieve coherent and sufficient data from the farmers actions, we have developed a shiny application to aid bookkeeping of the field management events. The application is running on a server, which enables farmers and scientists to fill field management events easily and without any restrictions of location. The data is used in carbon cycle modelling and in a purpose to have information of carbon sequestration in individual fields [@nevalainen2022towards]. Interest in this type of research can open more ways to mitigate climate change [@smith2020measure].

For researchers, different ecological models are one approach to determine the effect of different farming methods and how these methods can be utilised from the climate perspective. However, without the information of field management activities, we are missing one relevant part of information, and thus, may lead us to model carbon flux incorrectly. Therefore, *fieldactivity* and other similar applications aiding this process of logging the field management events are in demand.

# The process flow

Figure \ref{fig:flow} is a brief summary of the fundamentals regarding the application’s structure and the purpose that it serves. The scope of this section is to highlight the application’s main features and provide an idea of the process flow.


![Functionality and usage of application.\label{fig:flow}](4fields.png)


1) Application *fieldactivity* is created by following the Golem framework. It is made to be highly modularised, which helps in the application’s maintenance and development in the future. Detailed information of the application's structure can be found under the vignettes directory.

2) The application provides an easy to use interface with two different language options (English and Finnish). Dropdown menus for the site and block names as well as for the topics of the field management events prevent typing errors. Specific information (amount of fertilisation, harvest method etc.) regarding the selected event in the chosen site is then filled in. The saved management inputs can be verified from the events’ table that is visible and editable for the user the whole time during which the application is running.

3) Data will initially be stored locally, but the application can be integrated into the server. The stored data will be separated to different directories based on the given site and block information. However, all of the events for the same site and block are stored into one .json-file.

4) Data inputs can differ based on the used ecosystem model, so it might be unavoidable to produce an event template that would suit all the models. Nevertheless, providing the information of field management events in a unanimous way with a sufficient amount of information reduces the excessive labor of modellers.


# Acknowledgements



# References