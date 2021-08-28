## Test environments
* local Windows 10 install, R 4.0.5
* ubuntu 16.04.6 LTS (on travis-ci), R 4.0.5
* OS X 10.14.6 (on travis-ci), R 4.0.5
* win-builder (R devel)

## Additional comments
* Shortened package title.
* Replaced \dontrun{} with \donttest{}. 
* Replaced URLs in description with DOIs. 

## R CMD check results

0 errors | 0 warnings | 0 note

## What's new?
* This is a patch for the previously archived version of this package which was due to the archiving of a strong dependency - Huxtable. In this patch, Huxtable has been replaced with Flextable.


## Implemented revisions from CRAN maintainer's suggestions
* I have removed the redundant "A Package for" from the title.

* I have added an optional 'file_path' argument to interactionR_table(), where user can specify a directory to save the Word table instead of the working directory. If this option is not exercised, the function then seek an explicit permission to save the table in the working directory, if declined, the directory is left untouched and only prints to the console.

* Thanks


## What's new?
* This is another patch release to fix a bug in how the 95% CI are being estimated for some of the models.


## R CMD check results

0 errors | 0 warnings | 0 note

## What's new?
* This is a feature release that adds an optional pvalue outputting feature. 


## R CMD check results

0 errors | 0 warnings | 0 note
