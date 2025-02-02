---
title: 2022 NCCC170 Annual Meeting
summary: Oklahoma State University
location: Oklahoma State University
address:
  city: Stillwater
  country: United States
  postcode: "74078"
  region: OK
  street: 301 Math Sciences Building
authors: 
  - clarkkogan
  - carlagoad
  - "Laurence Madden"
  - "Alex Lipka"
  - "Julia Piaskowski"
  - "Phil Alderman"
  - "Huizi Wang"
  - aaronrendahl
  - jaradniemi
  - brucecraig
author_notes:
  - "Program Chair"
  - "Site Host"
date: "2022-06-16"
date_end: "2022-06-17"
all_day: true
share: false
profile: false
---

![2022 Attendees](DSC09584sm.jpg)

**Thursday, June 16, 2022**

{{< schedule >}}

{{< time "8:30a" >}} Check-in, coffee, juice, snacks {{< /time >}}

{{< time "9:00a" >}} **Welcome Messages:**<br>
Randy Raper, Assistant Vice President, Facilities and Ag Research Assistant Director, Oklahoma State University<br>
Mindy McCann, Department Head, Department of Statistics, Oklahoma State University
{{< /time >}}

{{< time "9:15a" >}}
{{< talk "Consideration for the use of linear quantile mixed models for describing highly variable data in agriculture"
"Laurence V. Madden, Department of Plant Pathology, Ohio State University" >}}
Quantile regression (QR) has become well established in recent decades for characterizing relationships between a response variable (y) and covariables. Instead of modeling conditional expected values, in QR one models the conditional quantiles, with median regression being a special case. QR is usually considered desirable when the effects of the covariate(s) on the quantile depend on the quantile; that is, when the relationship between the center of the distribution and a covariate is different from the relationship at more extreme quantiles. The usual estimation methods are distribution free and are usually considered robust for the conditional median. The data in agricultural studies are often clustered, with correlated observations within clusters. Several proposals have been made to account for the correlations in QR, with quantile mixed modeling (QMM) receiving the most attention since about 2007. QMM generalizes parametric random-coefficient mixed modeling that is frequently used for clustered normal data. The key step is to consider the asymmetric Laplace (AL) distribution as the working model for the distribution of y conditional on the random effects. By fixing the skewness parameter τ of the AL (0 < τ < 1), the location parameter of the AL is the conditional quantile, μ(τ). Then, estimation becomes a maximum likelihood problem. But there are many challenges with this approach related to computation (optimization), inference, and prediction (including the BLUPs), with questions remaining regarding bias and precision of parameter estimates. Bayesian estimation is a valuable alternative to frequentist MLE. I will present an exploration of the use of linear QMM to describe data for wheat yield and quality in relation to the severity of symptoms of the disease Fusarium head blight. Both frequentist (lqmm package in R) and Bayesian approaches (PROC MCMC in SAS) will be presented. The difficulties in taking a QMM approach will be discussed.
{{< /talk >}}{{< /time >}}

{{< time "9:50a" >}}
{{< talk "Using simulations to assess the performance of genotype-to-phenotype models accounting for pleiotropy, variance-controlling loci, and interspecific breeding material"
"Alex Lipka, Department of Crop Sciences, University of Illinois at Urbana-Champaign" >}}
Models that reflect the multifaceted contributions of genomic loci have a potential to facilitate unprecedented quantification of the genetic architecture underlying various traits and increase genomic selection (GS) prediction accuracies. To evaluate the performance of such models, simulation studies are essential. Therefore, a R/CRAN package called simplePHENOTYPES developed by the Lipka Lab is first discussed. This package uses real marker data to simulate pleiotropic quantitative trait nucleotides (QTNs) that behave in either an additive, dominance, or epistatic manner. We first demonstrate how simulating traits from simplePHENOTYPES can be used to evaluate the ability of a various multi-trait genome-wide association study (GWAS) models to distinguish between linkage and pleiotropy. We then show how simulations can be used to determine how well statistical approaches used to quantify variance heterogeneity can identify epistasis and genotype-by-environment interactions. We end the presentation by demonstrating a simulation study that evaluates the impact of training set composition from two different species (Miscanthus sinensis and Miscanthus sacchariflorus) on the GS prediction accuracy in an interspecific Miscanthus × giganteus F2 population.
{{< /talk >}}{{< /time >}}

{{< time "10:25a" >}} Morning break {{< /time >}}

{{< time "10:40a" >}}
{{< talk "Mixed Models in R: Mind the Gap"
"Julia Piaskowski, College of Agricultural and Life Sciences, University of Idaho" >}}
The linear mixed model ecosystem in R consists of over 80 libraries that either construct and solve mixed model equations or helper packages the process the results from mixed model analysis. These libraries provide a patchwork of overlapping and unique functionality regarding the fundamental structure of mixed models: allowable distributions, nested and crossed random effects, heterogenous error structures and other facets. No single library has all possible functionality enabled. These libraries vary greatly in accessory functions enabled, such as conducting ANOVA, accessing the BLUPs and other standard model output such as the log likelihood, residuals, and variances. The only common features guaranteed in these packages are that they (1) conduct a mixed model, and (2) if the package is on CRAN, have a method to print the output from a mixed model function call. Additionally, each library has its own unique syntax for allowable function arguments. This patchwork of packages makes it very challenging for statisticians to conduct mixed model analysis and to teach others how to run mixed models in R. There have been recent attempts to create a common function call for mixed models with the library ‘multilevelmod’ and a common way to access accessory information with ‘broom.mixed’. These meta-libraries are enormously helpful, but they are incomplete, addressing less than 10 mixed models packages in total. As a community of agricultural statisticians and statistical consultants, what should we do about this? The goal of this talk/discussion is to identify gaps in mixed model analysis in R and discuss options and our capacity to address those gaps.
{{< /talk >}}{{< /time >}}

{{< time "11:15a" >}}
{{< talk "Conceptual frameworks for embedding ordinary differential equation models within Bayesian hierarchical modeling"
"Phil Alderman, Department of Plant and Soil Sciences, Oklahoma State University" >}}
Crop scientists are frequently interested in understanding how instantaneous biophysical processes integrate across a growing season to affect yield. Collecting repeated measurements of soil and crop variables over time is one way to quantify how these processes are affected by various environmental and climatic factors and how those effects integrate over time to produce yield. Combining these data with dynamic simulation models allow crop physiologists and modelers to characterize these responses. Traditionally, these models are built to be purely deterministic with a system of ordinary differential equations (ODEs) that describes how the system as a whole evolves over time. Within this approach, random aspects of the data generation process are generally relegated to a single error term, usually assumed to be normal and centered on 0. This presentation will describe recent work on embedding ODE models within a more robust modeling of the random components of the data generation process using Bayesian hierarchical modeling. The approach will be illustrated with an application to a winter wheat dataset and some potential conceptual frameworks for thinking about the interpretation of model terms will be presented.
{{< /talk >}}{{< /time >}}

{{< time "11:50a" >}}
{{< talk "Classification Methods for High-dimensional MiFi® Data"
"Huizi Wang, Department of Statistics, Oklahoma State University" >}}
Microbe Finder (MiFi®) is a diagnostic tool developed by researchers at Oklahoma State University which uses high-throughput sequencing technology to measure the abundance of a pathogen in a sample. Data are generated as follows. First, a pathogen’s unique RNA sequence is decomposed into multiple smaller sequences called “e-probes”. Then, the number of e-probe “hits” in a sample is recorded for each e-probe. Also recorded is a simple single measure called “total score”. The goal is to train a classifier using MiFi® data. The challenge here is that there are anywhere from 10 to 10,000 e-probes, depending on the pathogen of interest, and there are only a few samples available for training the classifier. A variety of classifiers for this high-dimensional classification problem are available. Likewise, a simple univariate classifier based on the total scores could be considered. This talk will briefly review available approaches and compare their performances for several pathogens.
{{< /talk >}}{{< /time >}}

{{< time "12:25p" >}} Lunch {{< /time >}}

{{< time "1:30p" >}}
{{< talk "Group Discussion: Moving to a World Beyond “p < 0.05”: How far have we come, and how far we have yet to go?"
"{{% mention aaronrendahl %}}, University of Minnesota" >}}
After many years of statisticians working to help collaborators interpret p-values correctly, it feels like notable progress has been made since the 2016 ASA statement, with well publicized editorials in a wide variety of journals as well as publications for the general public.

* What progress have you seen, in both your collaborators and in the journals you publish in?
* Where do you still see progress that needs to be made?
* What specific actions have you seen be successful in working with collaborators and journals?
How has this changed (or not) in recent years?
* Are there specific methodologies that supplement or replace p-values that you use, and in which fields and/or journals have you used them?
{{< /talk >}}{{< /time >}}

{{< time "2:30p" >}}
{{< talk "Computer model emulation in agriculture"
"{{% mention jaradniemi %}}, Iowa State University" >}}
Computer models are seeing expanded use in agricultural applications from crop yield to soil loss to nutrient runoff. Statistical emulators of these computer models can provide practitioners with enhanced capabilities to solve their calibration, optimization, and design needs. We will discuss some of our recent work in developing Gaussian process (GPs) emulators for agricultural applications including dealing with functional inputs and large number of computer model runs. For functional inputs, we introduce the asymmetric Laplace functional weight for a parsimonious distance function between two functional inputs and show that it is comparable to automatic relevant determination despite using a small number of parameters. When many computer model runs are available, we introduce a one-at-a-time approach to knot selection in a knot-based approximation to a full GP and show it is comparable to the full GP at a fraction of the computational expense.
{{< /talk >}}{{< /time >}}

{{< time "3:05p" >}} Break {{< /time >}}

{{< time "3:20p" >}}
{{< talk "Estimating Disease Prevalence in the Absence of a Gold Standard via Bayesian Model Averaging"
"{{% mention brucecraig %}}, Purdue University" >}}
When estimating disease prevalence, it is not uncommon to have data from conditionally dependent diagnostic tests. In such a situation, the estimation of prevalence is difficult if none of the tests is considered to be a gold standard. I will describe a Bayesian approach to estimating disease prevalence based on the results of two imperfect diagnostic tests, allowing for the possibility that the tests are conditionally dependent, but not conditioning on any particular dependence structure. This involves the construction of four models with various forms of conditional dependence and uses Bayesian model averaging, enabled by reversible jump MCMC, to obtain an overall estimate of the prevalence.
{{< /talk >}}{{< /time >}}

{{< time "3:55p" >}}
{{< talk "Optimizing Blueberry Pollination to Ensure Future Yields"
"{{% mention clarkkogan %}}, StatsCraft LLC" >}}
Highbush blueberry is a pollinator-dependent crop, with ~560 million pounds produced in the United States, worth over $700 million annually. Current practices for maintaining the size of managed pollinator populations are outdated. We are developing a decision support tool to support growers in determining the managed bee population densities to maintain on their farms. This tool will aim to predict pollination, fruit set, and yield dependent on cultivar, flower density, bee activity, hive quantity/quality and weather. Model aims and current challenges will be discussed.
{{< /talk >}}{{< /time >}}

{{< time "3:05p" >}} Adjourn {{< /time >}}

{{< /schedule >}}

**Thursday, June 16, 2022**

{{< schedule >}}

{{< time "8:30a" >}}
{{< talk "Group Discussion: Open Forum"
"{{% mention clarkkogan %}}, StatsCraft LLC" >}}
Open discussion form for current challenges in research design and statistical analysis. This discussion will give participants the opportunity to briefly describe areas of difficulty in active projects of interest.
{{< /talk >}}{{< /time >}}

{{< time "9:20a" >}} Morning Break {{< /time >}}

{{< time "9:30a" >}}
{{< talk "Business Meeting" >}}

*Announcements:*

1. 2021-2022 annual report:
    * Present new report format
    * Request information on papers, presentations, awards, etc.,
    * Present Google docs file, link, for NCCC170 members to fill out.

*Old business:*

2. Refresher/review of objectives for 2021-2026 project as per NIMSS: https://www.nimss.org/projects/18798
3. Reminder that NCCC170 website has been moved to: wordpressua.uark.edu/ncr170 , which redirects automatically to https://nccc170.uark.edu/

*New business:*

4. Planning for future NCCC170 annual meetings: 2023, 2024.
    * Location, dates.
5. Group discussion: Ties between NCCC170 and planning committee for the re-launched
Conference on Applied Statistics in Agriculture and Natural Resources.
    * Overlap in planning committee?
    * Potential problems.
    * Need to articulate separation?
    
{{< /talk >}}{{< /time >}}

{{< time "11:00a" >}} Adjourn {{< /time >}}

{{< /schedule >}}
