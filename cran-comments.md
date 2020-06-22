## Test environments
* local Windows 10 install, R 4.0.1
* ubuntu 16.04.6 LTS (on travis-ci), R 4.0.0
* OS X 10.13.6 (on travis-ci), R 4.0.1
* win-builder (R devel)

## Additional comments
* Shortened package title.
* Replaced \dontrun{} with \donttest{}. 
* Replaced URLs in description with DOIs. 

## R CMD check results

0 errors | 0 warnings | 0 note

## What's new?
* This is a patch for the previously archived version of this package which was due to the archiving of a strong dependency - Huxtable. In this patch, Huxtable has been replaced with Flextable.


## Suggested revisions from CRAN maintainer
* I have removed the redundant "A Package for" from the title.

* The example for interaction_table() as written do not save any file in fact, nor modify in any form the home directory, it only prints to the console. This is because by design, and In line with CRAN policies, the interaction_table() CANNOT save into the user's current working directory without the explicit permission of the user. There is a askYesNo() prompt within interaction_table()  that defaults to FALSE/NO which interactively seeks this permission. If user chooses NO/CANCEL, no file is saved and the working directory is untouched, the function just prints the table to the console. Ditto, if user does not respond to the prompt (as is the case with the examples). However, If they choose YES, it saves the word table and informs them about this pointing them to the working directory. Thanks.


