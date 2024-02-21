gemmaBackups
================

This package includes functions to create easy to load backups of Gemma
API outputs It is primarily designed to be imported into packages that
host these backups.

## Usage

``` r
library(gemmaBackups)
# create a package skeleton
devtools::create('.') 

# backup everything about datasets 1 to 4 as raw outputs to data-raw
backup_all(c(1,2,3,4)) 

# populate "data" directory with rda files to be loaded with the package
package_rdas()
```
