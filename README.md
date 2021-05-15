## Authors 

* [Nick Rong](https://github.com/nickyrong)
* [Nate Smith](https://www.nathan-smith.net/)
<br/>

## Overview
ShinyCFA is currently available at: https://app.waterexplorer.net/shinyCFA. Please note that it is hosted using free tier services available through AWS.

This app allows the user to enter a Water Survey of Canada (WSC) station ID and access streamflow data from the [Hydat Database](https://www.canada.ca/en/environment-climate-change/services/water-overview/quantity/monitoring/survey/data-products-services/national-archive-hydat.html). The data can be summarized in tabular or graphical formats and flood frequency analysis (FFA) can also be performed using a variety of distributions.

The functionality to extract data from the Hydat Database in this app is provided by the [tidyhydat package](https://github.com/ropensci/tidyhydat). 

<img src="https://github.com/ropensci/tidyhydat/blob/main/man/figures/tidyhydat.png" width="6%" />
<br/>

## App Functionality:
Currently the app allows the user to:

- View the locations of Water Survey of Canada (WSC) hydrometric stations (non-searchable map)  
- Select a hydrometric station and summarize streamflow data in a table format (daily, monthly, yearly) and download the summarized data
- View a hydrometric station historical daily hydrograph in an interactive graph 
- View a hydrometric station hydrological regime
- Run a Flood Frequency Analysis on the annual daily or instantaneous maximum streamflow series using various distributions 



## Frequency Analysis Methods:
The frequency distribution(s) are fitted using L-moments method from R package [lmom](https://cran.r-project.org/web/packages/lmom/index.html) by J. R. M. Hosking. L-moments of the sample data are calculated and distribution parameters are then estimated from the calculated L-moments.

Return periods of annual maximum discharge are calculated using the Weibull formula: *Tr = (N+1) / m* where *N* is the sample size and *m* is the rank. Probability of Non-exceedance is the inverse of return period: *P = 1 / Tr*. The frequency distribution plot x-axis has been transformed into probabilistic scale: *η = −log(−log(P)) = −log[−log(1 − 1/Tr)]* where *η* is the reduced variate.

**Further readings:**<br/>
* Hosking, J., & Wallis, J. (1997). *Regional Frequency Analysis: An Approach Based on L-Moments*. Cambridge: Cambridge University Press. doi:10.1017/CBO9780511529443<br/>
* Makkonen, L. (2006). Plotting Positions in Extreme Value Analysis, *Journal of Applied Meteorology and Climatology*, 45(2), 334-340. doi:10.1175/JAM2349.1
<br/>

## License
Copyright © 2021 [Nick Rong](https://github.com/nickyrong) & [Nate Smith](https://github.com/WraySmith)

Released under the [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an “AS IS” BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
<br/>

## Contributing

Pull requests and stars are always welcome. For bugs and feature requests, [please create an issue](https://github.com/nickyrong/ShinyCFA/issues).

Please note that this project is released with a [Contributor Code of Conduct](https://github.com/nickyrong/ShinyCFA/blob/main/CODE_OF_CONDUCT.md).By participating in this project you agree to abide by its terms.

Also note that this project was created under a learning objective and it is provided in the event it is useful to others. Accordingly, it is not necessarily under active development. 
<br/>
<br/>
