# landscape-connectivity-pipelines
[![DOI](https://zenodo.org/badge/1348549642.svg)](https://doi.org/10.5281/zenodo.22235521)

A collection of automated pipelines for landscape connectivity analysis using Graphab, Circuitscape, and related tools. The landscape connectivity framework is adapted from [Lechner et al. 2015](https://doi.org/10.1016/j.landurbplan.2015.04.008).

## **Technical Requirements**
- [**R**](https://cran.r-project.org/) to conduct the automated pipelines

- [**Graphab**](https://thema.umlp.fr/productions/software/graphab/en/home.html), specifically using version 2.8 to use [graph4lg](https://cran.r-project.org/web/packages/graph4lg/index.html) original functions.

- [**julia**](https://julialang.org/)

- [**Omniscape**](https://docs.circuitscape.org/Omniscape.jl/latest/)

## **Types of Analysis**
A detailed step-by-step guide for each pipeline is available [here.](Protocol/Protocol.md)

### **1. GraphabR_paramloop - Basic Graphab Parameter Loop**
This automated pipeline loops across a set of habitat x resistance raster combinations across specified parameters. The combination of raster and parameters is manually added into a table.

### **2. GraphabR_MAXconv - Pathway + Habitat Conversion Scenario Loop**
This pipeline can automate runs of "max pathway lengths". Resulting pathways and patches can then be used to run through various conversion scenarios. For details on the methodology of this pipeline, see ...

### **3. GraphabR_free - Parameter Space Loop**
An adapted form of the paramloop, where instead of set parameter combinations, we generate relevant parameter combinations from landscape-specific boundaries. More detailed explanation of the exact steps and outputs can be found in [Protocol](Protocol/Protocol.md)

### **4. Omniscape_loop**
Automated pipeline to loop through a series of defined raster inputs and run them through Omniscape.jl.

## **Citing this repository**
##### *When using any of the pipelines in the repository, in addition to citing Graphab/graph4lg/Omniscape, please cite:*

Prasetya, A.M., Lechner, A. (2026) Various automated pipelines for landscape connectivity analysis. https://doi.org/10.5281/zenodo.22235521

##### *When using the pathway + habitat Conversion Scenario Loop please cite:*
*(in prep)*


*(in prep)*
