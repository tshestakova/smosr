## Resubmission

This is a resubmission. In this version I have addressed the issue found during 
CRAN revision.

* Please always explain all acronyms in the description text. -> ESA-BEC
- Done.

*Please provide a link to the used webservices (ESA data) to the description 
field of your DESCRIPTION file in the form <http:...> or <https:...> with angle 
brackets for auto-linking and no space after 'http:' and 'https:'.
- Done.

* \dontrun{} should only be used if the example really cannot be executed (e.g. 
because of missing additional software, missing API keys, ...) by the user. 
That's why wrapping examples in \dontrun{} adds the comment ("# Not run:") as a 
warning for the user. Does not seem necessary. Please replace \dontrun with 
\donttest.
- to run examples in `smosr` package, a valid username and a password are 
required for accessing a remote server and downloading the data. Thus, the 
examples cannot be executed on CRAN without a previous user registration, which
suggests that \dontrun{} is an appropriate marker. 

* Please unwrap the examples if they are executable in < 5 sec, or replace
dontrun{} with \donttest{}.
- As stated above, the examples are not executable without a previous user
registration. I believe \dontrun{} is an appropriate marker here.

* Please put functions which download data in \donttest{}.
- As stated above, the examples are not executable without a previous user 
registration. I believe \dontrun{} is an appropriate marker here.

* Please ensure that your functions do not write by default or in your
examples/vignettes/tests in the user's home filespace (including the package 
directory and getwd()). This is not allowed by CRAN policies.
- Done. All instances of writing to the user's working directory were removed or
substituted with the directory given by tempdir().

* Please omit any default path in writing functions. In your 
examples/vignettes/tests you can write to tempdir(). ->
man/download_smos.Rd; R/download_smos.R
- Done. The getwd() is now substituted by tempdir().

* Please make sure that you do not change the user's options, par or
working directory. If you really have to do so within functions, please
ensure with an *immediate* call of on.exit() that the settings are reset
when the function is exited.
e.g.:
```r
...
oldpar <- par(no.readonly = TRUE) # code line i
on.exit(par(oldpar)) # code line i + 1
...
par(mfrow=c(2,2)) # somewhere after
...

...
oldwd <- getwd() # code line i
on.exit(setwd(oldwd)) # code line i+1
...
setwd(...) # somewhere after
...
```
e.g.: R/download_smos.R and your plot-functions.
If you're not familiar with the function, please check ?on.exit. This
function makes it possible to restore options before exiting a function
even if the function breaks. Therefore it needs to be called immediately
after the option change within a function.
- Done. on.exit() instruction was called straight after any changes of user's 
par or working directory. Note those changes are a few and only used when 
necessary.

## Test environments

* local Windows 10 install, R 4.3.1 (2023-06-16), x86_64-w64-mingw32
* devtools::check_win_devel()
* devtools::check_win_release()
* devtools::check_win_oldrelease()
* usethis::use_github_action() 
  - macOS-latest, R release
  - windows-latest, R release
  - ubuntu-latest, R devel
  - ubuntu-latest, R release
  - ubuntu-latest, R oldrel-1
* rhub::check_for_cran()
  - Fedora Linux, R-devel, clang, gfortran
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC
  - Windows Server 2022, R-devel, 64 bit

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
