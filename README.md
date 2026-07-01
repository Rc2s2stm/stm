# Replication Materials

This repository contains the replication materials for the manuscript:

**Justifying the Unjustifiable -- A Structural Topic Model Analysis of
the Hungarian Media Representation of the Russo-Ukrainian War**

The repository is provided in anonymized form for the peer-review
process. Repository anonymization will be removed upon publication.

------------------------------------------------------------------------

## Repository contents

### Data

The compressed archive (`stm_data.part01.rar` -- `stm_data.part05.rar`)
contains the complete dataset used in the analyses, including:

-   the analytical corpus,
-   document metadata,
-   preprocessing outputs,
-   intermediate files required for replication.

After downloading all archive parts, extract them into the project
directory before running the analyses.

### Scripts

`STM_RScript.R`

This script reproduces the complete analytical workflow reported in the
manuscript, including:

1.  data preprocessing;
2.  corpus preparation;
3.  Structural Topic Model estimation;
4.  model selection (`searchK`);
5.  topic diagnostics;
6.  topic interpretation;
7.  topic prevalence estimation;
8.  topic perspective analysis;
9.  topic correlation analysis;
10. temporal analyses;
11. generation of tables and figures reported in the manuscript.

### Search queries

The repository also contains the complete Boolean retrieval queries used
to construct the analytical corpus.

These include:

-   the base political corpus query;
-   the Russo-Ukrainian war filtering query.

## Software requirements

Analyses were conducted in **R** using the following principal packages:

-   stm
-   tidyverse
-   dplyr
-   stringr
-   ggplot2
-   igraph
-   tidygraph
-   ggraph
-   openxlsx
-   writexl

The complete list of package dependencies is contained in the R script.

## Reproducing the analyses

To reproduce the analyses:

1.  Download the repository.
2.  Download all archive parts (`stm_data.part01`--`part05`).
3.  Extract the archive into the project directory.
4.  Open `STM_RScript.R`.
5.  Set the working directory to the project folder.
6.  Run the script sequentially from beginning to end.

The script reproduces the complete Structural Topic Modeling workflow
and generates all intermediate outputs, tables, and figures.

## Data availability

The analytical dataset consists exclusively of publicly available online
newspaper articles collected through the workflow described in the
manuscript.

## Correspondence

The repository is anonymized for peer review.

Please direct any questions through the journal editorial system until
the review process has been completed.
