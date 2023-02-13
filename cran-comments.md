## Test environments
* local check
  Ubuntu 18.04.6 LTS, R 4.1.2 (2021-11-01)
* win-builder
  R version 4.1.3 (2022-03-10)
  R version 4.2.2 (2022-10-31 ucrt)
  R Under development (unstable) (2023-02-09 r83797 ucrt)

## `R CMD check DataFakeR_0.1.3.tar.gz --as-cran` results

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Krystian Igras <krystian8207@gmail.com>’

New submission

Package was archived on CRAN

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2023-01-10 as issues were not corrected
    in time.
...
Status: 1 NOTE
```

## `devtools::check()` results

```
0 errors ✓ | 0 warnings ✓ | 0 notes ✓
```

## win-builder result

```
* using log directory 'd:/RCompile/CRANguest/R-oldrelease/DataFakeR.Rcheck'
* using R version 4.1.3 (2022-03-10)
* using platform: x86_64-w64-mingw32 (64-bit)
* using session charset: ISO8859-1
...
* checking CRAN incoming feasibility ... Note_to_CRAN_maintainers
Maintainer: 'Krystian Igras <krystian8207@gmail.com>'
...
Status: OK
```

```
* using log directory 'd:/RCompile/CRANguest/R-release/DataFakeR.Rcheck'
* using R version 4.2.2 (2022-10-31 ucrt)
* using platform: x86_64-w64-mingw32 (64-bit)
...
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Krystian Igras <krystian8207@gmail.com>'

New submission

Package was archived on CRAN

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2023-01-10 as issues were not corrected
    in time.
...
Status: 1 NOTE
```

```
* using log directory 'd:/RCompile/CRANguest/R-devel/DataFakeR.Rcheck'
* using R Under development (unstable) (2023-02-09 r83797 ucrt)
* using platform: x86_64-w64-mingw32 (64-bit)
...
Maintainer: 'Krystian Igras <krystian8207@gmail.com>'

New submission

Package was archived on CRAN

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2023-01-10 as issues were not corrected
    in time.
...
Status: 1 NOTE
```
