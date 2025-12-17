# GLSFilterSettingsList

A convenience class for storing a list of `GLSsettings` objects and
retrieving settings based on logger/species/colony.

## See also

Other classes:
[`GLSsettings`](https://ninanor.github.io/seatrackRgls/reference/GLSsettings.md)

## Public fields

- `filter_list`:

  List of GLSsettings objects

## Methods

### Public methods

- [`GLSFilterSettingsList$new()`](#method-GLSFilterSettingsList-new)

- [`GLSFilterSettingsList$get_settings_from_list()`](#method-GLSFilterSettingsList-get_settings_from_list)

- [`GLSFilterSettingsList$clone()`](#method-GLSFilterSettingsList-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new GLSFilterSettingsList object

#### Usage

    GLSFilterSettingsList$new(filter_list = list())

#### Arguments

- `filter_list`:

  A list of GLSsettings objects.

------------------------------------------------------------------------

### Method `get_settings_from_list()`

Get Settings from settings_list based on logger ID, species, or colony.

#### Usage

    GLSFilterSettingsList$get_settings_from_list(
      species = NULL,
      colony = NULL,
      logger_id = NULL,
      years_tracked = NULL
    )

#### Arguments

- `species`:

  Optional species name to match.

- `colony`:

  Optional colony name to match.

- `logger_id`:

  Optional logger ID to match.

- `years_tracked`:

  Optional year tracked to match

#### Returns

A list of filter settings matching the provided identifiers. If no match
is found, returns default settings for the species.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GLSFilterSettingsList$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
