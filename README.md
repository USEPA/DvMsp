# DvMsp

### A Supplementary R Package to "A comparison of design-based and model-based approaches for finite population spatial sampling and inferece"

##### Michael Dumelle<sup>1</sup>, Matt Higham<sup>2</sup>, Jay M. Ver Hoef<sup>3</sup>, Anthony R. Olsen<sup>1</sup>, Lisa Madsen<sup>4</sup>

##### <sup>1</sup>United States Environmental Protection Agency, Corvallis, OR, USA
##### <sup>2</sup>St. Lawrence University Department of Math, Computer Science, and Statistics, Canton, NY, USA
##### <sup>3</sup>NOAA Fisheries (NMFS) Alaska Fisheries Science Center, Marine Mammal Laboratory, Seattle, WA, USA
##### <sup>4</sup>Oregon State University Department of Statistics, Corvallis, OR, USA
##### For correspondence, please email Michael Dumelle at Dumelle.Michael@epa.gov

### Abstract

  1.   The design-based and model-based approaches to frequentist statistical inference rest on fundamentally different foundations. In the design-based approach, inference relies on random sampling. In the model-based approach, inference relies on distributional assumptions. We compare the approaches in a finite population spatial context.
  2. We provide relevant background for the design-based and model-based approaches and then study their performance using simulated and real data from the United States Environmental Protection Agency's 2012 National Lakes Assessment. A variety of sample sizes, location layouts, dependence structures, and response types are considered. The population mean is the parameter of interest and performance is measured using statistics like bias, squared error, and interval coverage.
  3. When studying the simulated and real data, we found that regardless of the strength of spatial dependence in the data, the generalized random tessellation stratified (GRTS) algorithm, which explicitly incorporates spatial locations into sampling, tends to outperform the simple random sampling (SRS) algorithm, which does not explicitly incorporate spatial locations into sampling. We also found that model-based inference tends to outperform design-based inference, even for skewed data where the model-based distributional assumptions are violated. The performance gap between design-based inference and model-based inference is small when GRTS samples are used but large when SRS samples are used, suggesting that the sampling choice (whether to use GRTS or SRS) is most important when performing design-based inference.
  4. There are many benefits and drawbacks to the design-based and model-based approaches for finite population spatial sampling and inference that practitioners must consider when choosing between them. We provide relevant background contextualizing each approach and study their properties in a variety of scenarios, making recommendations for use based on the practitioner's goals.

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

Scripts used to generate figures are located on your machine at the file path found by running
```r
system.file("scripts/figures", package = "DvMsp")
```

### Output

Summaries of the 36 simulation trials are located on your machine at the file path found by running
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

To view the manuscript, open `manuscript.pdf` (or visit [here](https://github.com/michaeldumelle/DvMsp/blob/main/inst/manuscript/manuscript.pdf)). To view the supplementary material, open `supplementary.pdf` (or visit [here](https://github.com/michaeldumelle/DvMsp/blob/main/inst/manuscript/supporting.pdf)).

### Figures

Figure are located on your machine at the file path found by running
```r
system.file("manuscript/figures", package = "DvMsp")
```

### Vignette

We have provided a vignette that describes how to use the functions in the supplementary R package to perform additional simulations that may be of interest. This vignette can be viewed by running
```r
vignette("simulations", "DvMsp")
```

### Citation

To obtain the citation associated with this manuscript, run
```r
citation("DvMsp")
```

```
To cite this work in publications use:

  Dumelle, Michael., Higham, Matt., Ver Hoef, Jay M., Olsen, Anthony R., and Madsen, Lisa. (2021). A comparison
  of design-based and model-based approaches for finite population spatial data. In submission.

A BibTeX entry for LaTeX users is

  @Article{,
    title = {A comparison of design-based and model-based approaches for finite population spatial data},
    author = {Michael Dumelle and Matt Higham and Jay M. Ver Hoef and Anthony R. Olsen and Lisa Madsen},
    journal = {In submission},
    year = {2021},
    volume = {0},
    number = {0},
    pages = {0-0},
    url = {NA},
  }

```
