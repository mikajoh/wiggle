### Research Compendium for "Conditional Legitimacy: How turnout, majority size and outcome affect perceptions of legitimacy in EU membership referendums" 
Authored by Sveinung Arnesen, Troy Saghaug Broderstad, [Mikael Poul Johannesson](mailto:mikael.johannesson@uib.no), and Jonas Linde.

[![Travis-CI Build Status](https://travis-ci.org/mikajoh/wiggle.svg?branch=master)](https://travis-ci.org/mikajoh/wiggle)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/mikajoh/wiggle?branch=master&svg=true)](https://ci.appveyor.com/project/mikajoh/wiggle)
[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/mikajoh/wiggle/master/LICENSE)

> This conjoint study investigates the type of mandate that a referendum confers in the political decision-making process. We find that it varies considerably along three dimensions. While a majority of citizens in general believe that the government should follow the results of a referendum on EU membership, its perceived legitimacy in the eyes of the public heavily depends upon 1) the level of turnout, 2) the size of the majority, and 3) the outcome of the specific referendum in question. Thus, whether a referendum legitimizes a political decision in the eyes of the public is conditional upon these three dimensions.

#### Install

You can install the reasearch compendium by running (note that you need the `devtools` package installed):

```r
library(devtools)
install_github("mikajoh/wiggle")
```

#### Includes

The compendium included several functions used in the analysis (see for instance`?wiggle::amce`).

In addition, it includes:

- `analysis/`: R-code needed to reproduce the results
  -  `analysis/01_data.R`: Prepares the raw data from the NCP and outputs `wiggle.csv`.
  -  `analysis/02_analysis.R`: Takes `wiggle.csv` and outputs the figures and tables used in the paper.
- `analysis/output/`: The figures, tables, and saved models.

#### The raw data

The raw data cannot be shared openly at the moment due data protection laws, so only the prepared data is included in the compendium.
However, the raw data is available for researchers via the [NSD website](http://www.nsd.uib.no/nsddata/serier/norsk_medborgerpanel_eng.html). Alternatively, you can contact me at [mikael.johannesson@uib.no](mailto:mikael.johannesson@uib.no).

The raw data used in the paper is from wave seven of the NCP.
The Md5 checksum of the file is `a91e1eaa974e35a589628c73e5e49d2d` (see `tools::md5sum()`).
