# GLSsettings Class

A convenience class for storing settings lists alongside identifiers to
which logger/species/colony they belong.

## See also

Other classes:
[`GLSFilterSettingsList`](https://ninanor.github.io/seatrackRgls/reference/GLSFilterSettingsList.md)

## Public fields

- `species`:

  Species identifier

- `colony`:

  Colony identifier

- `logger_id`:

  Logger ID identifier

- `years_tracked`:

  Years tracked identifier

- `settings`:

  A list containing filter settings for the logger/species/colony.

## Methods

### Public methods

- [`GLSsettings$new()`](#method-GLSsettings-new)

- [`GLSsettings$check_settings_for()`](#method-GLSsettings-check_settings_for)

- [`GLSsettings$clone()`](#method-GLSsettings-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new GLSsettings object

#### Usage

    GLSsettings$new(
      logger_id = NULL,
      species = NULL,
      colony = NULL,
      years_tracked = NULL,
      settings = NULL
    )

#### Arguments

- `logger_id`:

  Optional logger ID to associate with these settings.

- `species`:

  Optional species name to associate with these settings.

- `colony`:

  Optional colony name to associate with these settings.

- `years_tracked`:

  Optional years tracked to associate with these settings.

- `settings`:

  Optional list of filter settings. If NULL, defaults to settings for
  the provided species from
  [`seatrackRgls::seatrack_settings_list`](https://ninanor.github.io/seatrackRgls/reference/seatrack_settings_list.md).

------------------------------------------------------------------------

### Method `check_settings_for()`

Check if these settings are for a particular combination of
logger/species/colony

#### Usage

    GLSsettings$check_settings_for(
      species = NULL,
      logger_id = NULL,
      colony = NULL,
      years_tracked = NULL
    )

#### Arguments

- `species`:

  Optional species name to match.

- `logger_id`:

  Optional logger ID to match.

- `colony`:

  Optional colony name to match.

- `years_tracked`:

  Optional years tracked to match.

#### Returns

TRUE if the settings match the provided identifiers, FALSE otherwise.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    GLSsettings$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
