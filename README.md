
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hydrafit

<!-- badges: start -->
<!-- badges: end -->

The goal of hydrafit is to fit line parameters using a likelihood frame
work for leaf hydraulic vulnerability curves. The functions included in
the tests of the best fit are linear, logistic, sigmoidal, and two
exponential functions. This package also provides support for the
bootstrapping percent loss in hydraulic function.

## Installation

You can install the development version of hydrafit from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools") #only done if not already downloaded
devtools::install_github("brownegm/hydrafit", build_vignettes = T)
#> Downloading GitHub repo brownegm/hydrafit@HEAD
#> 
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>      checking for file ‘/private/var/folders/_x/1l0pg7m93wd7xfx24m9gy3800000gn/T/Rtmpq80uqG/remotes184667623d8da/brownegm-hydrafit-8ed21d7/DESCRIPTION’ ...  ✔  checking for file ‘/private/var/folders/_x/1l0pg7m93wd7xfx24m9gy3800000gn/T/Rtmpq80uqG/remotes184667623d8da/brownegm-hydrafit-8ed21d7/DESCRIPTION’
#>   ─  preparing ‘hydrafit’:
#>      checking DESCRIPTION meta-information ...  ✔  checking DESCRIPTION meta-information
#>   ─  installing the package to build vignettes
#>      creating vignettes ...  ✔  creating vignettes (7.6s)
#>   ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>   ─  building ‘hydrafit_0.1.0.tar.gz’
#>      
#> 
```

## Example

An example of the use of the functions is available as a vignette.

``` r
browseVignettes(package = "hydrafit")
#> starting httpd help server ... done
```

The unrendered vignette is also available in /vignettes.
