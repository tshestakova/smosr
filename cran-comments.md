## Test environments

* local Windows 10 install, R 4.3.1 (2023-06-16), x86_64-w64-mingw32
* devtools::check_win_devel()
* devtools::check_win_release()
* devtools::check_win_oldrelease()
* devtools::use_travis()          ###ubuntu 14.04.5 LTS (on travis-ci), R 3.5.0
* devtools::check_mac_release()
* rhub::check_for_cran()
  - Fedora Linux, R-devel, clang, gfortran
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC
  - Windows Server 2022, R-devel, 64 bit

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
