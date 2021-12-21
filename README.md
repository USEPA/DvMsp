# DvMsp

### A Supplementary R Package to "A comparison of design-based and model-based approaches for finite population spatial data"

##### Michael Dumelle<sup>1</sup>, Matt Higham<sup>2</sup>, Jay Ver Hoef<sup>3</sup>, Anthony R. Olsen<sup>1</sup>, Lisa Madsen<sup>4</sup>

##### <sup>1</sup>United States Environmental Protection Agency, Corvallis, OR, USA
##### <sup>2</sup>St. Lawrence University Department of Math, Computer Science, and Statistics, Canton, NY, USA
##### <sup>3</sup>NOAA Fisheries (NMFS) Alaska Fisheries Science Center, Marine Mammal Laboratory, Seattle, WA, USA
##### <sup>4</sup>Oregon State University Department of Statistics, Corvallis, OR, USA
##### For correspondence, please email Michael Dumelle at Dumelle.Michael@epa.gov

### Abstract

  1.   The design-based and model-based approaches to frequentist statistical inference lie on fundamentally different foundations. In the design-based approach, inference depends on random sampling. In the model-based approach, inference depends on distributional assumptions. We compare the approaches for finite population spatial data.
  2. We provide relevant background for the design-based and model-based approaches and then study their performance using simulations and an analysis of real mercury concentration data. In the simulations, a variety of sample sizes, location layouts, dependence structures, and response types are considered. In the simualtions and real data analysis, the population mean is the parameter of interest and performance is measured using statistics like bias, squared error, and interval coverage.
  3. When studying the simulations and mercury concentration data, we found that regardless of the strength of spatial dependence in the data, sampling plans that incorporate spatial locations (spatially balanced samples) generally outperform sampling plans that ignore spatial locations (non-spatially balanced samples). We also found that model-based approaches tend to outperform design-based approaches, even when the data are skewed (and by consequence, the model-based distributional assumptions violated). The performance gap between the analysis approaches is small when spatially balanced samples are used but large when non-spatially balanced samples are used. This suggests that the sampling choice (whether to select a sample that is spatially balanced) is most important when performing design-based inference.
  4. There are many benefits and drawbacks to the design-based and model-based approaches for finite population spatial data that practitioners must consider when choosing between them. We provide relevant background contextualizing each approach and study their properties in a variety of scenarios, making recommendations for use based on the practitioner's goals.

### Package Overview

This supplementary R package contains all files used in the creation of this document. Next we discuss how to use the package to access these files, reproduce any of its components, or perform new simulations.

### Installation

To install the supplementary R package, run
```r
install.packages("remotes") # if you don't already have the remotes package installed
remotes::install_github("michaeldumelle/DvMsp", ref = "main", dependencies = TRUE)
```

The package must be installed to view any of the files we discuss throughout this `README` on your machine. This package does not have to be installed to view any of these files if you want to look at them interactively using this GitHub repository.

### Scripts

After installing the package, the script used to perform the simulations presented in the manuscript is located on your machine at the file path found by running
```r
system.file("scripts/sim_runs.R", package = "DvMsp")
```

The script used to perform the analysis of mercury concentration data is located on your machine at the file path found by running
```r
system.file("scripts/application.R", package = "DvMsp")
```

Scripts used to generate figures are located on your machine in the folder at the file path found by running
```r
system.file("scripts/figures", package = "DvMsp")
```

### Output

Summaries of the 36 simulation trials are located on your machine in the folder at the file path found by running
```r
system.file("output/simulation_summary", package = "DvMsp")
```

Raw simulation output is not included when installing the supplementary package but is available [here](https://github.com/michaeldumelle/DvMsp/tree/main/inst/output/simulation_raw).

Summary output from the analysis of mercury concentration data is located on your machine at the file path found by running
```r
system.file("output/application/application.csv", package = "DvMsp")
```

### Manuscript

The manuscript and supporting information (and auxiliary files used to build them) are located on your machine at the file path found by running
```r
system.file("manuscript", package = "DvMsp")
```

To view the manuscript, open `manuscript.pdf`. To view the supplementary material, open `supplementary.pdf`.

### Figures

Figure are located on your machine at the file path found by running
```r
system.file("manuscript/figures", package = "DvMsp")
```

### Vignette

We have provided a vignette that describes how to use the functions in the supplementary R package to perform additional simulations you may be interested in learning about. This vignette can be viewed by running
```r
vignette("simulation-vignette", "DvMsp")
```
