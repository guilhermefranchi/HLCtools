## Resubmission

This is a resubmission of the first CRAN submission of HLCtools.

In response to the CRAN review:

* The Description field now includes DOI-formatted references to related methodological literature.
* The Description clarifies that the specific Herd Lying Concordance formulations implemented by the package are introduced in this package.
* Unnecessary `\dontrun{}` wrappers were removed from executable examples.
* All short examples now run directly using the included `example_lies` dataset.
* Help pages and executable examples were verified for all exported functions.

## R CMD check results

0 errors | 0 warnings | 2 notes

## Notes

* checking CRAN incoming feasibility ... NOTE
  New submission

  This package has not previously been published on CRAN.

* checking for future file timestamps ... NOTE
  unable to verify current time

  This appears to be a local check-environment issue. No files in the package
  were intentionally created with future timestamps.

## Test environments

* Local Windows 11, R 4.4.2
* GitHub Actions:
  * Windows, R release
  * macOS, R release
  * Ubuntu, R release

## Downstream dependencies

This is a new package and has no downstream dependencies.

## Copyright and licensing

The package is released under the MIT license. Copyright is held by the package author.
