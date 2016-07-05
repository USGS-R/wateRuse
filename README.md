`wateRuse`
=============

 [![travis](https://travis-ci.org/USGS-R/wateRuse.svg?branch=master)](https://travis-ci.org/USGS-R/wateRuse)

##Package Installation
To install the wateRuse package:

1. Install R (version 3.0 or greater) from: [https://cran.r-project.org/](https://cran.r-project.org/)

2. Install RStudio from: [https://www.rstudio.com/products/rstudio/download/](https://www.rstudio.com/products/rstudio/download/)

3. Add the USGS R repository to your "Rprofile" to get automatic updates. Run the following code:
  
  ```r
  rprofile_path = file.path(Sys.getenv("HOME"), ".Rprofile")
  write('\noptions(repos=c(getOption(\'repos\'),
    CRAN=\'https://cloud.r-project.org\',
    USGS=\'http://owi.usgs.gov/R\'))\n',
      rprofile_path, 
      append =  TRUE)

  cat('Your Rprofile has been updated to include GRAN.
    Please restart R for changes to take effect.')
  ```

4. Restart R!

5. In the RStudio "Console" window (usually left or lower-left), run the following command:

  ```r
  install.packages("wateRuse")
  ```
  
  To get cutting-edge changes, install from GitHub using the `devtools` packages:
  
  ```r
  library(devtools)
  install_github("USGS-R/wateRuse")
  ```


6. Update often. Next time you update your packages, `wateRuse` will automatically update:

![update](images/update.png)


## Run Shiny App

After installing package, run the following command:

```r
library(wateRuse)

explore_wateRuse()

```

Your default browser will open and start the app. If using IE, copy and paste the address to Firefox or Chrome.

## Contributing
If you would like to contribute functionality or bug fixes, some getting started document is [available here.](https://github.com/USGS-R/wateRuse/wiki/1.-Welcome-to-the-wateRuse-help-pages-wiki)  

A visual of the github contribution workflow used [is here.](https://github.com/USGS-R/wateRuse/wiki/2.-A-visual-of-the-R-Studio-to-GITHUB-process)  

Other userful information for contributing can be [found here.](https://github.com/USGS-R/wateRuse/wiki/3.-Useful-tips-and-links-for-working-in-R-and-R-Studio)

##Disclaimer
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](http://www.usgs.gov/visual-id/credit_usgs.html#copyright/ "official USGS copyright policy")

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

 [
    ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
  ](http://creativecommons.org/publicdomain/zero/1.0/)
