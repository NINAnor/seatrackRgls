# seatrackRgls: An automated procedure used in the SEATRACK project to obtain positions from light-level geolocators in large- scale tracking of seabirds

estimate up to two positions a day from light-level data from
geolocators (Lotek, BAS, Biotrack, Migrate Technology) using a threshold
method. It automatically edits twilight events used for calculating
positions and filter positions based on flight speed, distribution,
distance, and midnight sun periods. If allowed, the package saves plots
to your local computer the aid a subjective calibration of sun elevation
angles, which is crucial for producing realistic latitudes. The script
should be run twice. First with a "best-guess" sun angle, then with the
calibrated sun angle.

## See also

Useful links:

- <https://ninanor.github.io/seatrackRgls/>

## Author

**Maintainer**: Vegard Sandøy Bråthen <vegard.brathen@nina.no>
([ORCID](https://orcid.org/0000-0002-7357-6727)) (Original creator, GLS
expert)

Authors:

- Julian Evans <julian.evans@nina.nl> (Code assistance)
