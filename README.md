# Project -- A Spatial Characterization of the Chesapeake Bay SAV Record

## Project Overview

This project exists to characterize the spatial structure and temporal comparability
of the annual subermeged aquatic vegetation (SAV) map products produced by the 
Virginia Institute of Marine Science (VIMS) of the Chesapeake Bay. 

## Central Research Question

> How does the observable structure, geometry, geography, and spatial characteristics
> of the annual VIMS Chesapeake Bay SAV datasets very accross survey years, and 
> what implications do these differences have for temporal and spatial comparison?

This project treats each annual survey map as both an observation of the Chesapeake Bay
ecosystem and as a spatial data product.  This project examines what the released files
contain, what can be derived from their spatial structures, and what ecological 
inferences are supported by the available evidence. 

In short, this is an empirical characterization and methodological assessment. This is 
not, however, a reconstruction of the historical VIMS record, an analysis of environmental
drivers, or a resilience study. 

## Research Questions

1. **Annual Survey Data Structure.**  How does the annual file structure, attributes,
geometry, and polygon-level measurements vary?
2. **Geographic Coverage.** How does the mapped SAV representation vary among Chesapeake Bay
Program (CBP) segments and salinity zones, and mapped differences be distinguished from
a lack of survey coverage?
3.  **Contiguous Mapped Footprint.** How do individual source VIMS polygons relate to 
larger contiguous mapped patch compoents, and how is density-class composition represented
within those components?
4.  **Repeated Geometry.** To what extent, if any, do geometrically identical features recur
accross survey years?
5. **Successive-year Overlap.** How much of each polygon overlaps mapped SAV in the adjacent
survey years?
6.  **Boundary Relationships.** To what extent do mapped polygons cross CBP segment, salinity
zones, and State boundaries?
7. **Temporal Synthesis.** Which years share observable characteristics, and what limits
do these differences place on temporal analysis?

## Analytical Scope

The six core analytical phases are:

1. Annual file and geometry characterization.
2. Geographic coverage and survey-status assessment. 
3. Contiguous mapped footprints and density-class compositions.
4. Exaxct and topological geometry comparisions.
5. Directional overlap between successive annual maps.
6. Relationships between mapped polygons and geographic boundaries. 

Year characterization synthesizes the results of these phases.  As of now, Principle
Component Analysis or clustering may be considered as a limited, exploratory, analysis, but
neither are required for project completion. 

## Deffinitions

- A **source polygon** is a feature in an annual VIMS survey map file represented as-is
shipped from VIMS.  It is not automatically an ecological patch. 
- A **connected mapped component** is a geometric construct formed by polygons that touch (e.g.
they share an edge boundary).  Connected polygons are not automatically ecological patches.
- **Mapped absence** means that no SAV polygon is represented in the respective annual 
survey map product for a place and year.  However, it does not establish ecological absence.
- **Survey status** is recorded separately as surveyed, not surveyed, or unknown based on independent
evidence. 
- **Interanual overlap** describes the spatial correspondence between annual mapped products. It
does not establish patch persistence.
- **Exact geometry equality** and **topological equality** are distinct tests and are
reported separately.

## Scope Exclusions

The following are outside the scope of this project:

- Ecological patch identity or patch lineage reconstruction.
- Patch memory, persistence, extinction, colonization, or resilience.
- Environmental-driver analysis or causal inference. 
- Buffer-based connectivity definitions. 
- Shape matching independent of geographic location.
- A universal map-quality score.
- Any claims about historical mapping procedures that are not supported by documentation.
- Treating zero mapped polygons as evidence or proof that an area was surveyed or that SAV
was absent. 

Any ideas outside the current scope will be recorded in a log, but will not be a part
of the current project. 

## General Workflow

