---
title: A comparison of design-based and model-based approaches for finite population spatial data.
author:
  - name: Michael Dumelle
    email: Dumelle.Michael@epa.gov
    affiliation: USEPA
    footnote: 1
  - name: Matt Higham
    affiliation: STLAW
  - name: Lisa Madsen
    affiliation: OSU
  - name: Anthony R. Olsen
    affiliation: USEPA
  - name: Jay M. Ver Hoef
    affiliation: NOAA
address:
  - code: USEPA
    address: United States Environmental Protection Agency, 200 SW 35th St, Corvallis, Oregon, 97333
  - code: STLAW
    address: Saint Lawrence University Department of Mathematics, Computer Science, and Statistics, 23 Romoda Drive, Canton, New York, 13617
  - code: OSU
    address: Oregon State University Department of Statistics, 239 Weniger Hall, Corvallis, Oregon, 97331
  - code: NOAA
    address: Marine Mammal Laboratory, Alaska Fisheries Science Center, National Oceanic and Atmospheric Administration, Seattle, Washington, 98115
footnote:
  - code: 1
    text: "Corresponding Author"
abstract: |
  This is the abstract.

journal: "TBD"
date: "2021-10-21"
bibliography: mybibfile.bib
linenumbers: true
numbersections: true
csl: elsevier-harvard.csl
preamble: >
  \usepackage{bm}
  \usepackage{bbm}
  \usepackage{color}
  \DeclareMathOperator{\var}{{var}}
  \DeclareMathOperator{\cov}{{cov}}
output: rticles::elsevier_article
---

_Text based on elsarticle sample manuscript, see [http://www.elsevier.com/author-schemas/latex-instructions#elsarticle](http://www.elsevier.com/author-schemas/latex-instructions#elsarticle)_

Potential Journals:

* Ecological Applications
* Methods in Ecology and Evolution
* Journal of Applied Ecology
* Environmetrics
* Environmental and Ecological Statistics

<!-- Notes from today -->
<!-- •	Finite populations require a smaller scope but we need to discuss the impact of infinite populations as well – put finite vs infinite population context in the introduction / similarities and differences -->
<!-- •	What should a lower sample size be? 50 seems reasonable 30 may be too small – maybe 50, 100, 200 -->
<!-- •	IRS results seem off for dependent error sample size 50 -->
<!-- •	Take home points – don’t have to do GRTS design with GRTS sample -->
<!-- •	Take home points – GRTS is better for both model and design, while spatial approach for design is close to spatial model -->
<!-- •	Add medium dependence -->
<!-- •	Remind readers what choices of the four approaches in the application mean – what do they mean to practitioners -->

# Introduction {#sec:introduction}



<!-- Brief introduction to model based and design based -->

There are two general approaches for using data to make statistical inferences about a population: design-based and model-based. When data cannot be obtained for all units in a population (population units), data on a subset of the population units is collected and called a sample. In the design-based approach, inferences about the underlying population are informed from a probabilistic process in which population units are selected to be in the sample. Alternatively, in the model-based approach, inferences are made from specific assumptions about the underlying process that generated the data. Each paradigm has a deep historical context [@sterba2009alternative] and its own set of general advantages [@hansen1983evaluation].

<!-- Shift focus to spatial data -->

Though the design-based and model-based approaches apply to statistical inference in a broad sense, we focus on comparing these approaches for spatial data. We define spatial data as data that incorporates the specific locations of the population units into either the design or estimation process. @de1990model give an early comparison of design-based and model-based approaches for spatial data, quashing the belief that design-based approaches could not be used for spatially correlated data. Thereafter, several comparisons between design-based and model-based for spatial data have been considered [@brus1997random; @verhoef2002sampling; @verhoef2008spatial]. @cooper2006sampling review the two approaches in an ecological context before introducing a "model-assisted" variance estimator that combines aspects from each approach. In addition to @cooper2006sampling, there has been substantial research and development into estimators that use both design and model-based principles (see e.g. @cicchitelli2012model, @chan2020bayesian for a Bayesian approach, and @sterba2009alternative). More recent overviews include @brus2020statistical and @wang2012review.

<!-- Paragraph about the contribution of the manuscript and outlining the rest -->
Though comparisons between design-based and model-based approaches to spatial data have been studied, no numerical comparison has been made between design-based approaches that incorporate spatial locations and model-based approaches. In this manuscript, we compare design-based approaches that incorporate spatial locations to model-based approaches for spatial data. Though these comparisons generalize to both finite populations (e.g. point resources) and infinite populations (e.g. linear and areal resources), we focus on applications to finite populations. The rest of the manuscript is organized as follows. In Section \ref{sec:background}, we compare sampling and estimation procedures between the design-based approach and the model-based approach for spatial data. In Section \ref{sec:numstudy}, we use a simulation approach to study the behavior and performance of both approaches. In Section \ref{application}, we use both approaches to analyze real data. And in Section \ref{sec:discussion}, we end with a discussion and provide directions for future research. 

# Background {#sec:background}

The design-based and model-based approaches incorporate randomness in fundamentally different ways. In this section, we describe the role of randomness and its effects on subsequent inferences. We then discuss specific inference methods for the design-based and model-based approaches for spatial data. 

## Comparing Design-Based and Model-Based Approaches {#subsec:dvm_compare}

The design-based approach assumes the population is fixed. Randomness is incorporated via the selection of units in a sampling frame according to a sampling design. A sampling frame is the set of all units available to be sampled. A sampling design assigns a positive probability of inclusion (inclusion probability) to each unit in the sampling frame. Some examples of commonly used sampling designs include simple random sampling, stratified random sample, and cluster sampling. These sampling designs tend to select units from the sampling frame independently of other units, so we call them "Independent Random Sampling" (IRS) designs. Sampling designs incorporating the spatial locations of units in the sample frame are called spatially balanced designs. Spatially balanced designs can be obtained using the Generalized Random Tessellation Stratified (GRTS) algorithm [@stevens2004spatially], which we discuss in more detail in Section \ref{subsec:spb_design}. The design-based approach combines the randomness of the sampling design and the data collected via the sample to estimate parameters (e.g. means and totals) of a population. Generally, these population parameters are assumed to be fixed, unknown constants. 

Treating the data as fixed and incorporating randomness through the sampling design yields estimators having very few other assumptions. Confidence intervals for these types of estimators are typically derived using limiting arguments that incorporate all possible randomizations of sampling units selected via the sampling design. Means and totals, for example, are asymptotically normally distributed (normal) by the Central Limit Theorem (under some assumptions). If we repeatedly sample the surface, then 95% of all 95% confidence intervals constructed from a procedure with appropriate coverage will contain the true, fixed mean. @sarndal2003model and @lohr2009sampling provide thorough reviews of the design-based approach.

The model-based approach assumes the data are a random realization of a data-generating process. Randomness is incorporated through distributional assumptions on this process. Strictly speaking, randomness need not be incorporated through random sampling, though @diggle2010geostatistical warn against preferential sampling Preferential sampling occurs when the process generating the data locations and the process being modeled are not independent of one another. To guard against preferential sampling, model-based approaches often still implement random sampling. 

Instead of estimating fixed but unknown parameters (as in the design-based approach), the goal of model-based inference in the spatial context is often to predict a realized variable, or value. For example, suppose the realized mean of all population units is the value of interest. Instead of \emph{estimating} a fixed, unknown mean, we are \emph{predicting} the value of the mean, a random variable. Prediction intervals are then derived leveraging assumptions of the data generating process. If we repeatedly generate the response values from a fixed spatial process and obtained a sample, then 95% of all 95% prediction intervals constructed from a procedure with appropriate coverage will contain their respective realized means. @cressie1993statistics and @schabenberger2017statistical provide reviews of model-based approaches for spatial data. A visual comparison of the design-based and model-based assumptions is provided in Figure \ref{fig:fig1} (@brus2020statistical provides a similar figure).

\begin{figure}
\includegraphics[width=1\linewidth]{manuscript_files/figure-latex/fig1-1} \caption{A comparison of sampling under the design-based and model-based frameworks. Points circled are those that are sampled. In the top row, we have one fixed population, and three random samples of n = 4. The response values at each site are fixed, but we obtain different estimates for the mean response because the randomly sampled sites vary from sample to sample. In the bottom row, we have three realizations of the same spatial process sampled at the same locations. The spatial process generating the response values has a single mean, but the realized mean is different in each of the three panels.}\label{fig:fig1}
\end{figure}

## Spatially Balanced Design and Analysis {#subsec:spb_design}

Spatially balanced samples can be obtained using the design-based approach. Spatially balanced samples are useful because parameter estimates from these samples tend to vary less than parameter estimates from samples that are not spatially balanced [@stevens2004spatially; @barabesi2011sampling; @grafstrom2013well; @robertson2013bas; @wang2013design; @benedetti2017spatiallyreview]. The first spatially balanced sampling algorithm that saw widespread use was the Generalized Random Tessellation Stratified (GRTS) algorithm [@stevens2004spatially]. To quantify the spatial balance of a sample, @stevens2004spatially proposed loss metrics based on Voroni polygons. Since GRTS was developed, several other spatially balanced sampling algorithms have emerged, including the Local Pivotal Method [@grafstrom2012spatially; @grafstrom2018spatially], Spatially Correlated Poisson Sampling [@grafstrom2012spatiallypoisson], Balanced Acceptance Sampling [@robertson2013bas], Within-Sample-Distance Sampling [@benedetti2017spatially], and Halton Iterative Partitioning Sampling [@robertson2018halton]. In this manuscript, we use Generalized Random Tessellation Stratified (GRTS) sampling because it has several attractive properties: GRTS sampling accommodates finite and infinite sampling frames; accommodates equal, unequal, and proportional (to) size inclusion probabilities; accommodates legacy (historical) sampling; accommodates a minimum distance between units in a sample; accommodates reverse hierarchically ordered replacement units in a sample (replacement units are units available to be sampled if an original unit cannot be sampled); and is available in the spsurvey R package @dumelle2021spsurvey.

The GRTS algorithm samples from finite and infinite populations by utilizing a mapping between two-dimensional and one-dimensional space. The units in the two-dimensional sampling frame are divided into cells using a hierarchical address. This hierarchical address is then used to map the units from two-dimensional space to a one-dimensional line where each unit's line length equals its inclusion probability. A systematic sample is conducted on the line and linked back to a unit in two-dimensional space, which results in the desired sample. @stevens2004spatially and @dumelle2021spsurvey provide further details. 

<!-- This makes it seem like HT is being used to estimate population parameters in GRTS? Needs to be in there, but maybe a sentence making it clear that this isn't being used? Or just change the order and talk about Local variance first and then mention these for more general settings? -->

After selecting a GRTS sample, data are collected and used to estimate population parameters. To unbiasedly estimate population means and totals from sample data, one can use the Horvitz-Thompson estimator [@horvitz1952generalization]. If $\tau$ is a population total, the Horvitz-Thompson estimate of $\tau$, denoted by $\hat{\tau}_{ht}$, is is given by
\begin{align}\label{eq:ht}
  \hat{\tau}_{ht} = \sum_{i = 1}^n Z_i \pi_i^{-1},
\end{align}
where $Z_i$ is the value of the $i$th unit in the sample and $\pi_i$ is the inclusion probability of the $i$th unit in the sample. An estimate of the population mean is obtained by dividing $\hat{\tau}_{ht}$ by the population size. 

While the Horvitz-Thompson estimator is unbiased for population means and totals, it is also important to quantify the uncertainty in these estimates. @horvitz1952generalization and @sen1953estimate provide variance estimators for $\hat{\tau}_{ht}$, but they have two drawbacks. First, these estimators rely on calculating $\pi_{ij}$, the probability that unit $i$ and unit $j$ are both in the sample -- this quantity can be challenging if not impossible to calculate analytically. Second, these estimators ignore the spatial locations of the units in the sampling frame. To address these two drawbacks simultaneously, @stevens2003variance proposed the local neighborhood variance estimator. The local neighborhood variance estimator does not rely on $\pi_{ij}$ and incorporates spatial locations -- for technical details see @stevens2003variance. @stevens2003variance show the local neighborhood variance estimator tends reduce $\text{Var}(\hat{\tau})$ compared to variance estimators ignoring spatial locations, yielding narrower confidence intervals for $\tau$.

## Finite Population Block Kriging

Finite Population Block Kriging (FPBK) is a model-based approach that expands the geostatistical Kriging framework to the finite population setting [@verhoef2008spatial]. Instead of basing inference off of a specific sampling design, we assume the data are generated by a spatial process. @verhoef2008spatial gives details on the theory of FPBK, but some of the basic principles are summarized below. Let ${\mathbf{z} \equiv \{\text{z}(s_1), \text{z}(s_2), . . . , \text{z}(s_N) \}}$ be a response vector at locations $s_1$, $s_2$, . . . , $s_N$ that can be measured at the $N$ population units and is represented as an $N \times 1$ vector. Suppose we want to predict some linear function of the response variable, $f(\mathbf{z}) = \mathbf{b}^\prime \mathbf{z}$, where $\mathbf{b}^\prime$ is a $1 \times N$ vector of weights. For example, if we want to predict the population total across all population units, then we would use a vector of 1's for the weights. 

<!-- how explicitly do we want to define \mathbf{z} -->

<!-- finite number of -->

<!-- confusing having two tau's: tau for total and a tau as a function -->

We often only have a sample of the $N$ population units. Denoting quantities that are part of the sampled population units with a subscript \emph{s} and quantities that are part of the unsampled population units with a subscript \emph{u}, 

\begin{equation}
\begin{pmatrix} \label{equation:Zmarginal}
    \mathbf{z}_s      \\
    \mathbf{z}_u
\end{pmatrix}
=
\begin{pmatrix}
  \mathbf{X}_s    \\
  \mathbf{X}_u
\end{pmatrix}
\bm{\beta} +
\begin{pmatrix}
\bm{\delta}_s    \\
\bm{\delta}_u
\end{pmatrix},
\end{equation}
where $\mathbf{X}_s$ and $\mathbf{X}_u$ are the design matrices for the sampled and unsampled population units, respectively; $\beta$ is the parameter vector of fixed effects; and $\bm{\delta}_s$ and $\bm{\delta}_u$ are random errors for the sampled and unsampled population units, respectively. Denoting $\bm{\delta} \equiv [\bm{\delta}_s \,\, \bm{\delta}_u]'$, we assume the expectation of $\bm{\delta}$ equals $\mathbf{0}$.

In addition to assuming the expectation of $\bm{\delta}$ equals $\mathbf{0}$, we also assume that there is spatial correlation in $\bm{\delta}$ that can be modeled using a covariance function. It is common to assume the covariance function is second-order stationary and isotropic [@cressie1993statistics], and that the spatial covariance decreases as the separation between population units increases. Many spatial covariance functions exist, but the primary function we use throughout the simulations and applications in this manuscript is the exponential covariance function: the $i,j$th entry for $\cov(\bm{\delta})$ is
\mbox{}
\begin{align}\label{equation:expcov}
\cov(\delta_i, \delta_j) = 
\begin{cases} 
\sigma^2_{ps}\exp(-h_{i,j}/\phi) & h_{i,j} > 0 \\
\sigma^2_{ps} + \sigma^2_n & h_{i,j} = 0
\end{cases}
,
\end{align}
where $\sigma^2_{ps}$ is the partial sill measuring coarse-scale (correlated) variability, $\sigma^2_{n}$ is the nugget measuring fine-scale (independent) variability, $\phi$ is the range parameter measuring the distance-decay rate of the covariance, and $h_{i,j}$ is the Euclidean distance between population units $i$ and $j$. Any spatial covariance function could be used in the place of the exponential, however, including functions that allow for non-stationarity or anisotropy [@chiles1999geostatistics, pp. 80-93].

With the above model formulation, the Best Linear Unbiased Predictor (BLUP) for $f(\mathbf{b}'\mathbf{z})$ and its prediction variance can be computed. While details of the derivation are in [@verhoef2008spatial], we note here that the predictor and its variance are both moment-based, meaning they don't rely on any distributional assumptions.

We note that we only use FPBK in this paper in order to focus more on comparing the design-based and model-based approaches. However, k-nearest-neighbors [@fix1951discriminatory; @ver2013comparison], random forest [@breiman2001random], Bayesian models [@chan2020bayesian], among others, can also be used to obtain predictions for a mean or total from spatially correlated responses of a finite population. We choose to use FPBK because it is faster than a Bayesian approach and random forest and because @ver2013comparison showed that the method outperforms k-nearest-neighbors in many scenarios. 

# Numerical Study {#sec:numstudy}

We used a numerical simulation study to investigate performance of four design-analysis combinations, summarized in Table \ref{tab:designanalysis}.

\begin{table}[ht]
\centering
\begin{tabular}{r|ll}
  \hline
 & Design & Model \\ 
  \hline
IRS & IRS-Design & IRS-Model \\ 
  GRTS & GRTS-Design & GRTS-Model \\ 
   \hline
\end{tabular}
\caption{\label{tab:designanalysis} Types of Sampling Design and Analysis combinations considered in the simulation study. The rows give the two types of sampling designs while the columns give the two types of analyses.} 
\end{table}

We used a crossed design with the simulation parameters given in Table \ref{tab:parmtab} for a total of 36 scenarios. All scenarios used exponential correlation with a  $\sqrt{2} / 3$ for $N = 900$ response values simulated on the unit square in either random locations (Layout = Random) or gridded locations (Layout = Gridded). The range was chosen so that the decayed to nearly zero at the largest distance possible in the domain, $\sqrt{2}$, otherwise known as the "effective" range (for the exponential covariance, the effective range is $3\phi$). The mean for the spatial process generating the response was set to zero.

For the lognormal scenarios, the response values were simulated using the specified correlation parameters using a normal distribution and were subsequently exponentiated. A total variance of 2 and a mean of 0 on the normal scale is equivalent to a total variance of 47 and a mean of 2.72 after exponentiation. Therefore, when the model-based methods were used for lognormal response, the correlation was mis-specified. We chose to simulate values with a lognormal distribution so that we could test the model-based analysis approach with a mis-specified model and so that we could test both analysis approaches on data that exhibits a large amount of skewness.

\begin{table}[ht]
\centering
\begin{tabular}{r|lll}
   \hline
Sample Size (n) & 50 & 100 & 200 \\ 
  Layout & Random & Gridded & - \\ 
  Proportion of Dependent Error & 0 & 0.5 & 0.9 \\ 
  Response Type & Normal & Lognormal & - \\ 
   \hline
\end{tabular}
\caption{\label{tab:parmtab} Simulation parameters. Total variability for all scenarios was 2 so that the partial sill was 0, 1, or 1.8.} 
\end{table}



There were 2000 simulation trials for each of the 36 parameter combinations. In each trial, response values were generated from a spatial process with the specified parameters, and a GRTS sample and an IRS sample were selected. For the GRTS sample, the design-based approach using the local neighborhood variance (GRTS-Design) and a model-based approach were applied (GRTS-Model). For the IRS sample, the design-based approach using the simple random sample variance (IRS-Design) and a model-based approach were applied (IRS-Model).

The GRTS algorithm and the local neighborhood variance estimator are available in the \textbf{\textsf{R}} package \texttt{spsurvey} [@dumelle2021spsurvey]. FPBK can be readily performed in `R` with the `sptotal` package [@higham2021sptotal]. We use `sptotal` for both the simulation analysis and the application, estimating parameters with Restricted Maximum Likelihood (REML).

**Mike** For design-based, it's really RMSE or RMSPE -- how should we address this?
Figure \ref{fig:figeff} shows the relative efficiency of the four approaches from Table \ref{tab:designanalysis} with "IRS-Design" as the baseline:
\mbox{}
\begin{equation*}
\text{EFF} = \frac{\text{rMS(P)E of approach}}{\text{rMS(P)E of IRS-Design}},
\end{equation*}

where rMS(P)E is the root-mean-squared error (design-based) or the root-mean-squared-prediction error (model-based). When there is no spatial correlation (top row), the four approaches have approximately equal rMSPE, even when the assumptions of the model-based approaches are violated. So, using GRTS or using a spatial model does not result in much, if any, loss in efficiency even if the response variable is not spatially correlated. When there is high spatial correlation (bottom row), the GRTS-Model approach tends to perform best, but difference in relative efficiency between GRTS-Model and GRTS-Design is small. In the lognormal, high partial sill settings (bottom-right facet), GRTS-Design outperforms IRS-Model by a large margin, suggesting that the design decision (whether to use IRS or GRTS) is more important than the analysis decision (whether to analyze using model assumptions or not). 

Unsurprisingly, Figure \ref{fig:figeff} also shows that, when the assumptions for GRTS-Model are satisfied, the approach outperforms GRTS-Design. However, even when the model that generates the data is different than the model used to fit the data, as in the lognormal response, the model-based approach still outperforms the design-based approach when there is a high amount of spatial correlation.

\begin{figure}
\includegraphics[width=1\linewidth]{manuscript_files/figure-latex/figeff-1} \caption{Relative Efficiency of the four design-analysis approaches. The plot is faceted by the type of response on the columns and the partial-sill to total-variance ratio on the rows.}\label{fig:figeff}
\end{figure}



We also studied 95% interval coverage among the approaches. The design-based 95% confidence intervals and model-based 95% prediction intervals are constructed using the normal distribution. Justification for the design-based intervals lies in the asymptotic normality of totals via the Central Limit Theorem, and justification for the model-based intervals lies in the normality assumption of the errors.  Figure \ref{fig:figconf} shows the 95% interval coverage for each of the four approaches. All four approaches have somewhat similar interval coverage in all settings, with GRTS-Design having slightly lower coverage when the response is normal. 

In the normal response settings, all approaches have coverage around 95%. This is expected, as the intervals are also based on the normal distribution In the lognormal response settings, however, all approaches have coverage below 95%. This is also expected, as the intervals are still based on the normal distribution. In the lognormal response settings, interval coverage increases both as the sample size increases and as the strength of spatial dependence increases. This suggests that the larger the sample size and the stronger the spatial dependence, the more resistant these intervals are to departures from normality of the data.

\begin{figure}
\includegraphics[width=1\linewidth]{manuscript_files/figure-latex/figconf-1} \caption{Coverage of the four design-analysis approaches. All confidence intervals are normal-based and have a nominal confidence level of 0.95, marked with a horizontal line. The plot is faceted by the type of response on the columns and the partial-sill to total-variance ratio on the rows.}\label{fig:figconf}
\end{figure}

In addition to rMS(P)E and interval coverage, we also recorded average bias. The average bias is nearly zero for all approaches in all scenarios, so we omit a visualization of the results here. The supplementary material contains tables with mean bias, rMS(P)E, and interval coverage for all 36 simulation scenarios. 

# Application {#application}

The Environmental Protection Agency (EPA), states, and tribes periodically conduct National Aquatic Research Surveys (NARS) in the United States to assess the water quality of various bodies of water. We will use the 2012 National Lakes Assessment (NLA), which measures various aspects of lake health and quality in lakes in the contiguous United States, to obtain an interval for mean mercury concentration. Although we know the true mean mercury concentration values for the 986 lakes from the 2012 NLA, we will explore whether or not we obtain an adequately precise estimate for the realized mean mercury concentration if we sample only 100 of the 986 lakes.



\begin{figure}
\includegraphics[width=1\linewidth]{manuscript_files/figure-latex/figdata-1} \caption{Population distribution of mercury concentration for 986 lakes in the contiguous United States.}\label{fig:figdata}
\end{figure}



Figure \ref{fig:figdata} shows that mercury concentration is right-skewed, with most lakes having a low value of mercury concentration but a few having a much higher concentration. Mercury concentration exhibits some spatial correlation, with high mercury concentrations in lakes in the northeast and north central United States. The realized mean mercury concentration in the 986 lakes is 103.2 ng / g.











\begin{table}[ht]
\centering
\begin{tabular}{lrrrr}
  \hline
Approach & Estimate & SE & 95\% LB & 95\% UB \\ 
  \hline
IRS-Design & 112.7 & 8.8 & 95.4 & 129.9 \\ 
  IRS-Model & 110.5 & 7.9 & 95.0 & 125.9 \\ 
  GRTS-Design & 101.8 & 6.1 & 89.8 & 113.7 \\ 
  GRTS-Model & 102.3 & 5.9 & 90.8 & 113.9 \\ 
   \hline
\end{tabular}
\caption{\label{tab:appliedtab} Application of design-based and model-based approaches to the NLA data set on mercury concentration. The true mean concentration is 103.2 ng / g.} 
\end{table}

Table \ref{tab:appliedtab} shows the application of a design-based analysis of an IRS sample, a model-based analysis of an IRS sample, a design-based analysis of a GRTS sample, and a model-based analysis of a GRTS sample. For all four analyses, the true realized mean mercury concentration is within the bounds of the 95% intervals. However, we should not generalize the results of this particular realization to any other data set or even to other potential samples of this data set. 

<!-- not sure why this is getting cutoff. temporary fix: \pagebreak 
probably because it is an html table -- I used xtable to fix -->

But, we do note a couple of patterns. The design-based IRS analysis shows the largest standard error: a likely reason is that this is the only approach that does not incorporate any spatial information regarding mercury concentration across the contiguous United States. We also see that both approaches using the GRTS sample have a lower standard error than the both approaches using the IRS sample. We would expect this to be the case for most samples because mercury concentration exhibits spatial patterning, so a spatially balanced sample should usually yield a lower standard error. 

# Discussion {#sec:discussion}

The design-based and model-based approaches to inference are fundamentally different paradigms by which to samples are selected and data are analyzed. The design-based approach incorporates randomness through sampling to estimate a population parameter. The model-based approach incorporates randomness through distributional assumptions to predict the realized value of a random variable. Though these approaches have often been compared in the literature both from theoretical and analytical perspectives, our contribution lies in studying them in a spatial context while implementing spatially balanced sampling. Aside from the theoretical differences described, a few analytical findings are particularly notable: the design decision (GRTS vs IRS) seems much more important than the analysis decision (design-based vs model-based); independent of the analysis approach, there is no reason to prefer IRS over GRTS for spatial data -- GRTS tends to perform at least as well as IRS when there is no spatial correlation and increasingly better than IRS as the strength of spatial correlation increases; the gap in relative efficiency between GRTS-design and GRTS-model widens as the strength of spatial correlation increases; and when the data are skewed, interval coverage for all approaches improves both as the sample size increases and as the strength of correlation increases. 

<!-- The design-based approach relies on few assumptions, while the model-based approach relies on rigid distributional ones. -->

There are several benefits and drawbacks of the design-based and model-based approaches for spatial data, some of which we have not yet discussed but are worthy of consideration in future research.  Design-based approaches are often computationally efficient, while model-based estimation of covariance parameters can be computationally burdensome, especially for likelihood-based methods such as REML that rely on inverting a covariance matrix. The design-based approach also more naturally handles binary data, free from the more complicated logistic regression formulation commonly used to handle binary data in a model-based approach. The model-based approach, however, can more naturally quantify the relationship between covariates (predictor variables) and the response variable. The model-based approach also yields estimated spatial covariance parameters, which help better understand the process of study. Model selection is also possible using model-based approaches and criteria such as likelihood ratio tests or AIC [@akaike1974new]. Model-based approaches are capable of more efficient small-area estimation than design-based approaches by leveraging distributional assumptions in areas with few observed sites. Model-based approaches can also compute site-by-site predictions at unobserved locations and use them to construct informative visualizations. The benefits and drawbacks of both approaches, alongside our theoretical and analytical comparisons, should be heavily considered when choosing among them. This is especially true from an analysis perspective, as we found that using a spatially balanced sampling algorithm benefits both design-based and model-based analyses.

# Data and Code Availability

This manuscript has a supplementary R package that contains all of the data and code used. Instructions for download at available at [https://github.com/michaeldumelle/DvMsp](https://github.com/michaeldumelle/DvMsp).

# Supplementary Material

In the supplementary material, we provide summary statistics for all 36 simulation scenarios.

# References {#references .unnumbered}

