## Authors 

* [Nick Rong](https://github.com/nickyrong)
<br/>

## Overview
ShinyCFA is currently available at: https://nickrong-shinycfa.share.connect.posit.cloud/ Please close the tab/browser when you are not using the app, thank you.

This app allows the user to enter a Water Survey of Canada (WSC) station ID and access streamflow data from the [Hydat Database](https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey/data-products-services/national-archive-hydat.html). The data can be summarized in tabular or graphical formats, and flood frequency analysis (FFA), regional analysis, and an average (regime) hydrograph can also be produced. The published HYDAT record can optionally be extended toward the present with provisional realtime unit-value data.

The functionality to extract data from the Hydat Database in this app is provided by the [tidyhydat package](https://github.com/ropensci/tidyhydat). 

<img src="https://github.com/ropensci/tidyhydat/raw/main/man/figures/tidyhydat.png" width="6%" />
<br/>

## App Functionality:
Currently the app allows the user to:

- View WSC hydrometric station locations on an interactive map, filter them by name, record, drainage area and distance, and select a station from the map
- View a station's drainage basin on the map and export the polygon as a Shapefile or Google Earth KML
- Select a hydrometric station and summarize streamflow data in a table (daily, monthly, yearly) and download the summarized data
- View an interactive daily-discharge time series coloured by data-quality flag
- View an average (regime) hydrograph by day of the water year, with percentile bands and mean annual discharge, optionally split into multiple periods
- Run a Flood Frequency Analysis on the annual daily or instantaneous maximum streamflow series using various distributions
- Perform a Regional Analysis: pool stations automatically from a target location (BC hydrologic zones inside BC, or a Region-of-Influence donor group elsewhere in Canada), by BC hydrologic zone, or by manual selection, then build a regional flood-frequency envelope curve and characterise the instantaneous-to-daily peak ratio
- Retrieve provisional realtime unit-value data from WSC to extend the summary table, time series and hydrograph beyond the published HYDAT record. Provisional data are excluded from the Flood Frequency and Regional Analyses



## Frequency Analysis Methods:
The frequency distribution(s) are fitted using L-moments method from R package [lmom](https://cran.r-project.org/web/packages/lmom/index.html) by J. R. M. Hosking. L-moments of the sample data are calculated and distribution parameters are then estimated from the calculated L-moments.

Return periods of annual maximum discharge are calculated using the Weibull formula: *Tr = (N+1) / m* where *N* is the sample size and *m* is the rank. Probability of Non-exceedance is the inverse of return period: *P = 1 / Tr*. The frequency distribution plot x-axis has been transformed into probabilistic scale: *η = −log(−log(P)) = −log[−log(1 − 1/Tr)]* where *η* is the reduced variate.

**Further readings:**<br/>
* Hosking, J., & Wallis, J. (1997). *Regional Frequency Analysis: An Approach Based on L-Moments*. Cambridge: Cambridge University Press. doi:10.1017/CBO9780511529443<br/>
* Makkonen, L. (2006). Plotting Positions in Extreme Value Analysis, *Journal of Applied Meteorology and Climatology*, 45(2), 334-340. doi:10.1175/JAM2349.1
<br/>

## License
Copyright © 2026 [Nick Rong](https://github.com/nickyrong)

Released under the [![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.

Earlier versions of this project were released under the MIT License; that notice is kept in [LICENSE-MIT](LICENSE-MIT).

## Disclaimer
This application is provided for research and educational purposes. While we strive for accuracy, users should verify data for critical applications. The developers are not responsible for any decisions made based on the data provided through this application.
<br/>

## Contributing

Pull requests and stars are always welcome. For bugs and feature requests, [please create an issue](https://github.com/nickyrong/ShinyCFA/issues).

Please note that this project is released with a [Contributor Code of Conduct](https://github.com/nickyrong/ShinyCFA/blob/master/CODE_OF_CONDUCT.md).By participating in this project you agree to abide by its terms.

Also note that this project was created under a learning objective and it is provided in the event it is useful to others. Accordingly, it is not necessarily under active development. 
<br/>
<br/>
