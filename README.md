<h1 align="center"><project-name></h1>

<p align="center"><project-description></p>

## Introduction

This repository is intended as an online supplement to the manuscript:

Lautier, J. P., Pozdnyakov, V., Yan, J. (2024). _On the convergence of credit risk in current consumer automobile loans_.  Journal of the Royal Statistical Society Series A: Statistics in Society, Accepted. (https://doi.org/10.1093/jrsssa/qnae137)[https://doi.org/10.1093/jrsssa/qnae137]


Please attribute any citations of this repository to the original
manuscript.


This repository includes:

- **clean_data** The filtered loan data summarized in the _Data_ section of
the companion manuscript.  It is used for all empirical results. Also includes
simulation inputs.

- **code** Replication code files.  To replicate the data processing, use
'data_processing.R'.  To move directly to reproducing the manuscript results,
use 'data_analysis.R'.  The loan savings calculations are performed in
'refi_svgs.xlsx'. 

- **raw_data** These are the raw ABS data files, which have been scraped from
[EDGAR](https://www.sec.gov/edgar/search-and-access).

## Workflow

There are two options.  A user may start with the raw data and process the data
into the clean data ('data_processing.R').  This will create a new folder
'processed_data', which will match the 'clean_data' folder.
Alternatively, a user may start
directly with the clean data and produce the manuscript results ('data_analysis.R').
These results, including figures, will be produced in a new folder,
'results'.


## Links

- [Repo](https://github.com/jackson-lautier/credit-risk-convergence)

- [Publication](https://doi.org/10.1093/jrsssa/qnae137)

- [arXiv](https://arxiv.org/abs/2211.09176)

- [SSRN](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4278917)

## Screenshots

![Credit Risk Convergence](/illustrative_figures/crc_demo.pdf)

![Conditional Lender Profitability](/illustrative_figures/lender_profit.pdf)

## Built With

- R
- Excel
- python (ABS data scraping, not included)

## Lead, Corresponding Author

**Jackson P. Lautier**

- [Website](https://jacksonlautier.com/)

## Complete Authors

**Vladimir Pozdnyakov**

- [Website](https://vladimir-pozdnyakov.github.io/)

**Jun Yan**

- [Website](http://merlot.stat.uconn.edu/~jyan/)