---
title: 'RAnTo: A Shiny App to analyse the rhythm of time series like animal communication'
tags:
  - Shiny
  - Behavioral Biology
  - timeseries
  - rhythm
  - isochrony
  - integer ratio
authors:
  - name: Lara S. Burchardt
    orcid: 0000-0002-9210-7934
    equal-contrib: true
    affiliation: "1, 2" # (Multiple affiliations must be quoted)
affiliations:
 - name: Humboldt-Universität zu Berlin,Institut für Biologie, Berlin, Germany
   index: 1

 - name: Museum für Naturkunde – Leibniz-Institut für Evolutions- und Biodiversitätsforschung
   index: 2

date: 05 September 2025
#bibliography: paper.bib

# Optional fields if submitting to a AAS journal too, see this blog post:
# https://blog.joss.theoj.org/2018/12/a-new-collaboration-with-aas-publishing
#aas-doi: 10.3847/xxxxx <- update this with the DOI from AAS once you know it.
#aas-journal: Astrophysical Journal <- The name of the AAS journal.
---

# Summary

Rhythmic organization is a fundamental property of behavior in many species, ranging from acoustic communication to motor patterns (citations). However, quantifying such rhythmic structure often requires specialized analyses, and existing tools are either scattered across different packages or require substantial programming expertise.

I present RANTO (Rhythm Analysis Tool for Timeseries), an interactive Shiny application that facilitates the analysis and visualization of rhythmic behaviors and element sequences. RANTO integrates multiple established rhythm analysis approaches, such as inter-onset interval (IOI) distributions, integer ratio computation, and phase-shift optimization to align observed events with underlying beat structures (citations). RANTO also provides recurrence plots and measures of variability, making it suitable for exploratory and hypothesis-driven work (citations).

RANTO is designed for accessibility and reproducibility. Users can upload time-series data in CSV format, configure analysis settings through an intuitive graphical interface, and export plots and results tables, without having to interact with the code at all. The app is written entirely in R and depends on well-supported open-source libraries, making it platform-independent and extensible. 

# Statement of need

Researchers in bioacoustics and behavioral biology frequently study temporal patterning of signals — for example, the rhythm of bird song, whale song, primate or bat vocalizations and repetitive motor acts (citations). While rhythm is a key component of communication and behavior, tools for its analysis are often scattered, require substantial expertise and coding, or are tailored to specific data sets.

RANTO addresses this gap by providing a standalone, open-source, GUI-based application for rhythm analysis. It allows researchers without advanced programming experience to analyze temporal sequences of behavioral elements and to compare rhythmic organization across data sets. Importantly, it integrates multiple methods — from basic IOI statistics to phase-shift beat alignment and recurrence plots — into a single workflow.

By making rhythm analysis accessible and reproducible, RANTO lowers the barrier for behavioral scientists to incorporate temporal structure into their research. This is particularly relevant for the bioacoustics community, but also applies broadly to ethology, neuroscience, and other fields where temporal organization of behavior is of interest.

# Acknowledgements

I want to acknowledge funding for this project from "Innovation Fund" of the Museum für Naturkunde – Leibniz-Institut für Evolutions- und Biodiversitätsforschung and the DFG (BU 4375/1-1, project number: 528064681). 

# References