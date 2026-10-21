Protocol for landscape-connectivity-pipelines
================

### [Audrey MP](https://orcid.org/0000-0001-7759-0692)

A collection of automated pipelines for landscape connectivity analysis
using Graphab, Circuitscape, and related tools.

Github repo for usage license and latest updates
[here.](github.com/goaudreymp/landscape-connectivity-pipelines)\
Zenodo doi to cite can be found
[here.](https://doi.org/10.5281/zenodo.22235521)

### Fast links

1.  [GraphabR_paramloop](#graphabr-paramloop)
2.  [GraphabR_MAXpathconv](#graphabr-maxpathconv)
3.  [GraphabR_free](#graphabr-free)
4.  [Omniscape_loop](#omniscape-loop)

### Necessary R packages to be installed and loaded:

    {library(graph4lg)
    library(tidyverse)
    library(terra)
    library(igraph) 
    library(sf)
    library(raster) 
    library(readr)
    library(readxl)}

------------------------------------------------------------------------

# 1. GraphabR_paramloop

#### Use to run specified combinations of parameters in GraphabR, can take a combination of ‘normal’ runs and ‘metapatch’ runs. Also produces least cost corridors for each combination of runs.

## **Inputs**

Folder should be set up like this, with two folders, a parameter file,
the script, .Rproj, and graphab file. ![Figure 1](media/image6.png)\

Naming for habitat files Habitat files should be named following this
pattern: name_habcodeX_nodataXX where X is a numeric value. ![Figure
2](media/image10.png)\

habcode is the value in the raster which is a habitat nodata is the
value in the raster which shows no data. The name part of the pattern
should be consistent with other files of same species, and for ease of
finding. Resistance file naming has no rules, but I like to keep it
short and include the sp code for consistency as well.\
![Figure 3](media/image15.png)\

Set up your parameter table. Set up the combinations of habitat,
resistance, minimum area, and interpatch distance (as well as metapatch
size and metapatch distance if you are running that) you want to run.
Save as `params.csv` if you are editing from the .xlsx version and put
this in the same folder. The script will loop ONLY through the
combinations you have put here. ![Figure 4](media/image9.png)\

| `params.csv` | (can be saved from `params.xlsx`) |
|:---|:---|
| runname | Additional prefix that will be added to the name of the files, I usually do sp code. Not important if your habitat files already have a sp code prefix. |
| hab_file | Habitat files path |
| res_file | Resistance files path |
| minarea | Minimum patch size |
| thr | Interpatch distance |
| metapatch | Metapatch size. ONLY FILL IF RUNNING METAPATCH. |
| metadist | Metapatch distance. ONLY FILL IF RUNNING METAPATCH. |

## **Protocol**

<div data-align="center">

<img src="media/image2.png" alt="Figure 5" width="90%">

</div>

## **Outputs**

| Object Outputs | Description |
|:---|:---|
| Graphab folders | Graphab result folders. Includes the usual output after doing a Graphab run. One folder per combination of parameters set up in the parameters sheet. For metapatch runs, this will be inside another folder. |
| patches.shp | Inside each graphab run. Shows the patches. |
| ma00_t100-links.shp | Links for that combination |
| ma00_t100-voronoi.shp | Components file for that combination |
| ma00_t100-corridor.shp | Least cost corridors |
| habomni folder | Patches files cleaned for omniscape |
| resomni folder | Corridor files cleaned for omniscape |
| dIIC patches folder | Patches file with dIIC values attached |
| dIIC links folder | Links file with dIIC values attached |
| dIIC nodes folder | Nodes file (patch centroids) with dIIC values attached |
| graphab_results_summary.csv | A .csv table showing results of different global landscape metrics for each parameter combination ran (IIC, NC, MSC, SLC). |

------------------------------------------------------------------------

# 2. GraphabR_MAXpathconv

#### Use for obtaining the longest pathways and auto-converting pathways and patches based on conversion scenarios

## **Inputs**

Folder should be set up like this, with four folders, a parameter file
and an LULC conversion file, as well as the script, .Rproj, and graphab
file.

![Figure 6](media/image14.png)\

The most important input is the control sheet (ctl.xlsx). It contains
all the parameterisations for the script to run. The order of the sheets
inside is important!! The names of the sheets are inconsequential.

| `ctl.xlsx` |
|:---|
| **Sheet 1: Parameters** The default parameters sheet used for running the longest pathways the same with the previous protocol. **Change the habitat files, resistance files, and parameters accordingly.** If no metapatch value is specified, it automatically runs the normal Graphab (without a metapatch) and vice versa. |
| ![Figure 7](media/image8.png) |
| **Sheet 2: Species Order + Value** This table contains the species code and the value you want to change their pathways into during conversion. Important to note that the **order of the species (top to bottom) is also the priority order during the combination of the pathways**. The first species on the list gets prioritised over the next species and so on. |
| ![Figure 8](media/image20.png) |
| **Sheet 3: Habitat Exclusion for Pathway Conversion** This tells the code whether, during the conversion of specific pathways, other species habitats should be excluded. All values above 1 mean exclude that species’ habitat. |
| ![Figure 9](media/image17.png) |
| **Sheet 4, 5 (and possible beyond): Pathway Conversion Scenarios** Currently, there are two scenario sheets, but the code will run on any number of scenarios for any sheet that starts with ‘conv); you just need to add more sheets (conv 1, conv2, conv3, etc.). The table shows the scenario in which pathways of each species (column) that overlap certain LULC categories (rows), the values of the pathway will be converted to the amount in the cell. |
| ![Figure 10](media/image7.png) |
| **Sheet 6: Habitat Exclusion for Patch Conversion** This tells the code whether, during the conversion of specific pathways, other species habitats should be excluded. All values above 1 mean exclude that species’ habitat. Works the same way as the habitat exclusion for patches but will run for the habitat patches. |
| **Sheet 7, 8 (and possible beyond): Patch Conversion Scenarios** Currently, there are two scenario sheets, but the code will run on any number of scenarios for any sheet that starts with ‘pconv’); you just need to add more sheets (pconv1, pconv2, pconv3, etc.). Works the same way as the pathway conversion table. |

| Other Inputs |  |
|:---|:---|
| habitat folder | Folder that includes the .tif files of species habitat rasters, used for running max pathways through Graphab. Remember that the file names here need to be named like: CB_habcode1_nodata9999.tif \![\]$$image11$$ |
| resistance folder | Folder that includes the .tif files of species resistance rasters, used for running max pathways through Graphab. \![\]$$image12$$ |
| convhab folder | Folder that includes the .tif files of species habitat, used specifically for habitat exclusion during the conversion step. Put the species code at the start of the file name. \![\]$$image13$$ |
| conv.tif | .tif file of LULC raster used during the conversion steps. Check that conversion scenario categories match the value in this LULC map. Name should be exactly ‘**conv.tif**’. |
| LULC folder | Folder that includes the .tif files of LULC for each species, used to add on the final converted pathways of all species. |

## **Protocol**

<div data-align="center">

<img src="media/image1.png" alt="Figure 11" width="90%">

</div>

- **Top Pathway Rules**: You can change the rules to pick the top
  pathway in this code line 492-298. For example, to change to top 100:
  `sel <- df$rank <= 100`. To change to top 10% quantile:
  `q50 <- quantile(df$dIICCon, 0.1, na.rm = TRUE)`

## **Outputs**

| Object Outputs | Description |
|:---|:---|
| overlap.mx.csv | .csv file showing overlap between the ‘top’ chosen pathways (from FINpaths) for each species |
| dIIC_patch folder | Patches created from Graphab with dIIC value of the patch. |
| MAX\_ folders | Graphab result folders. Includes the usual output after doing a Graphab run. One folder per combination of parameters set up in the parameters sheet. |
| TOPlinks folder | Folder with top pathways only. |
| FINpaths folder | Folder with final paths (buffered, picked for top dIIC) of each species. |
| FINpathsconv folder | Folder with final paths that have gone through the conversion scenario (per species, per conversion scenario). |
| convLULC folder | Final rasters for each species, per conversion scenario, with all added pathways from all species. Ready for pre-processing. |
| patchconv folder | Folder with final patches that have gone through the conversion scenario, ready to be added (per species, per conversion scenario). |
| FINpathpatch folder | Final rasters for each species, per conversion scenario, with all added pathways + patches from all species. Ready for pre-processing. |

------------------------------------------------------------------------

# 3. GraphabR_free

#### With only map rasters as the input and no set parameters, use to select several parameter combinations depending on the landscape inputs and run multiple Graphabs in a loop to produce a broader comparative results space plot

## **Inputs**

The folder should be set up like this, with two folders, a parameter
file, the script, .Rproj, and graphab file. File inputs are the same as
the [GraphabR_paramloop](#graphabr-paramloop). The main differences are
in the params file input, although the same file can be used. If the
same file is used, only the habitat and resistance path columns are
actually used in this script, the rest are ignored.

![Figure 13](media/image21.png)\

An additional R script (‘sumstats.R’) is also included, which is an
optional post-loop script to run in addition to the results space plot,
some summary statistics. It has potential indices used to normalise the
results between landscapes to allow for direct comparisons *(this
requires further testing)*.

## **Difference with [GraphabR_paramloop](#graphabr-paramloop)**

### *Main differences*

- It does not require specific parameter combinations to be set
  beforehand, only input rasters\
- It does not run the graphab_corridor() function to produce least-cost
  corridors. If corridors are required, the GraphabR_free can be used to
  first produce the parameters of interest, then have those combinations
  be run under [GraphabR_paramloop](#graphabr-paramloop)\
- It does not save all the Graphab loop outputs to save memory. Only a
  select number of combinations are saved (this can be changed in the
  object \`selection\` line 578-579)

Unlike the base [GraphabR_paramloop](#graphabr-paramloop) script,
[GraphabR_free](#graphabr-free) only requires the habitat and resistance
input files and does not require any of the specific parameters
(i.e. minimum habitat size, interpatch distance). Instead, these
parameters are selected by doing an initial run using a minimum patch
size of 1 hectare and selecting all pathways created. The minimum patch
size for the initial run can also be changed to \<1 ha if testing a map
with small patches (\<1 ha) or wanting to consider the impact of the map
for species with small patch requirements. This change can be done in
line 317 (`minarea <- n`).

From the output of this initial run, we will obtain the distribution of
patch sizes as well as pathway lengths. We then use this distribution of
values to select ten values of minimum habitat size and interpatch
distance to use as parameter combinations for the following Graphab
runs. The number of values chosen can be changed in line 523 and line
547 (`length.out = n`). To note is that the total number of combinations
to run will be *n*<sup>2</sup>, where *n* is the number of values
selected as parameters.

These figures show an example of how the values are selected. For
minimum patch sizes, we select 8 values between the minimum value of 1
ha and a maximum value of the second largest patch size found in the
created initial run, equidistant to each other. The second largest
rather than the largest patch size was chosen to avoid ‘no pathways
created’ if only 1 habitat patch was selected from this method. On the
other hand, interpatch distances were selected from values between the
first quantile of the distribution of pathway lengths and the maximum
pathway length found in the initial run as well, equidistant to each
other again. Similarly, the first quantile value was chosen rather than
the smallest pathway length to avoid the problem of ‘no pathways
created’ if there are no pathways of that length found.

The formula to select the range of values from the distribution of the
initial run can be changed both for the patch sizes and the interpatch
distance. For example, rather than choosing values equidistant from each
other, you can select ten quantiles from the same minimum and maximum
values set by the rules above. The formula can be changed in code line
523 (`dist_seq` for interpatch distance) and line 547 (`area_seq` for
minimum habitat size).\

<div data-align="center">

<img src="media/image23.png" alt="Figure 14" width="90%">
<img src="media/image25.png" alt="Figure 15" width="90%"> <br> <em>The
left-side column shows the distribution of values from an example
initial run. The ten values selected from the distribution are shown
with the dashed lines. The right-side column shows the relationship
between the chosen values, showing that in this instance, the values are
chosen equidistant to each other.</em>

</div>

## **Protocol**

<div data-align="center">

<img src="media/image12.png" alt="Figure 16" width="90%">

</div>

## **Outputs**

| Object Outputs | Description |
|:---|:---|
| Graphab folders | Graphab result folders. Includes the usual output after doing a Graphab run. One folder per combination of parameters set up in the parameters sheet. Only specified parameter combination run results are saved. |
| graphab_results_summary.csv | A .csv table showing results of different global landscape metrics for each parameter combination ran (IIC, NC, MSC, SLC). |

## **Results Space Plot**

The results space plot can be plotted from the
graphab_results_summary.csv file after the runs are completed. An
example code specifically plotting IIC value is at the bottom of the
GraphabR_free script, but this code can be adjusted to plot the other
global metrics saved in the results .csv such as NC, SLC, MSC.

### *Allows broad comparisons of the whole landscape across a range of ecological parameters*

From the results space, we can see the broad relationship between the
changes in both parameters chosen can reflect on the IIC. As the example
below, in this environment, there is very little effect of IIC changes
from habitat patch size requirements. IIC changes more drastically with
interpatch distances, with high interpatch distances producing higher
IIC. However, IIC only increases up until an interpatch distance of
10000 where IIC values remain constant afterwards. This shows that in
this particular environment, dispersal ability is the main driver of
connectivity. Thus, a priority for concern in this area should be
short-dispersal species. The results space suggests that species with
any habitat requirements show low IIC for short dispersers, suggesting
that components are clustered and are disconnected quite far from each
other. A potential management plan would be to increase habitat sources
in between the main sources of habitats to improve connectivity for
short dispersers, whether it be with small or large habitat
requirements.

<p align="center">

<img src="media/image18.png" alt="Figure 17" width="50%">
<img src="media/image16.png" alt="Figure 18" width="20%"> <br>
<em>Example results plot space of a landscape. The right plot shows what
the resulting Graphab patches and pathway look like.</em>
</p>

### *Testing robustness/sensitivity of chosen parameters*

The results plot space can also be used to see how robust the selected
parameter combinations are to error. Using the same plot with a red
cross as an example for a selected parameter combination (e.g. from
expert workshops, literature review) we can see how much error in
selecting a value can influence the IIC results. In the example, as
minimum habitat size is less sensitive than interpatch distance, even a
large error in selecting that parameter does not produce any significant
change. On the other hand, depending on how far the actual value of
interpatch distance is from the selected value as well as the direction
of error, the IIC value could differ significantly. Significant IIC
changes occur in the ‘blue bounding box’, specifically in the lower
value of interpatch distance. Thus, we can say that in this particular
case, the results are robust to any errors of minimum patch size
estimate, but are sensitive to interpatch distances \>30% different to
selected value.

<div data-align="center">

<img src="media/image19.png" alt="Figure 19" width="50%"> <br>
<em>Example results plot space where red X marks an example selected
parameter combination. The red box shows IIC for 5% change, purple for
10% change, and blue for 30% change in paramter values.</em>

</div>

------------------------------------------------------------------------

# 4. Omniscape_loop

#### Use to run specified combinations of parameters and input rasters in Omniscape.jl.

For Omniscape requirements, installation, and explanation [see
here.](https://docs.circuitscape.org/Omniscape.jl/latest/usage/#Installation)\
For Julia installation [see
here.](https://julialang.org/downloads/manual-downloads/)

Some personal notes: I found that Omniscape best runs on Julia v1.10 and
below. There seems to be a problem when running it with the newest Julia
version.

## **Inputs**

Within one folder, with two folders, a parameter .txt file, and the .sh
scripts.

| `inputs.txt` (tab-separated) |  |
|:---|:---|
| Resistance file path | .tif file |
| Habitat file path | .tif file If using source_from_resistance = TRUE, put “NONE”. If using source_from_resistance = FALSE, put source of habitat file. (“./habitat/…tif). |
| radius | Required. |
| block size | Not required. If no value in the fourth column, then the default block size is 1. |

## **Protocol**

<div data-align="center">

<img src="media/image11.png" alt="Figure 20" width="90%">

</div>

## **Outputs**

| Object Outputs | Description |
|:---|:---|
| output folder | Each omniscape run will create a folder within the output folder. Name of the folders inside will follow the rule: resfile_habfile_radius |
| configs folder | Collection of the config files created as input for the omniscape run. |
| logs folder | Log text file showing run logs |
| config | Shows full config setting for the omniscape run |
| cum_currmap | Represents total current flowing |
| flow_potential | Represents current flow under ‘null’ resistance |
| normalized_cum_currmap | cum_currmap/flow_potential. Identifies where current is more or less than expected under null. |
