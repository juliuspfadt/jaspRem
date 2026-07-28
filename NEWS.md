# jaspRem Changelog

> **HOW TO READ AND UPDATE THIS CHANGELOG:**
> 
> This document follows a modified [Keep a Changelog](https://keepachangelog.com/) format adapted for the R/JASP ecosystem. Releases are listed in reverse chronological order (newest first).
> As an example see [jaspModuleTemplate](https://github.com/jasp-stats/jaspModuleTemplate/blob/master/NEWS.md)
> * **Adding New Changes (For Contributors):** All new commits should be logged at the very top of the file under the `# jaspModuleTemplate (development version)` header. Place your bullet point under the appropriate category (`## Added`, `## Fixed`, etc.). 
> * **Issue References:** Please reference the relevant GitHub Issue (if any) at the end of your line (e.g., `([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)`). 
> * **Format Categories:** >   * **Added:** New template features, QML examples, or build tools.
>   * **Changed:** Updates to default configurations, boilerplate code, or dependencies. 
>   * **Fixed:** Bug fixes in the build pipeline, R wrappers, or QML layouts.
>   * **Deprecated / Removed:** Outdated template components or legacy code.


---

# jaspRem (development version)

## Added
* "Active (saturated)" risk set option (remify `active_saturated`), which also adds the reverse dyads and, with a type variable, the other event types.
* "Extend risk set by type" option (`extend_riskset_by_type`), available when a type variable is assigned.
* "Consider type" now offers "Interact" (one effect per ordered pair of event types), in addition to "Ignore" and "Separate", for both receiver and sender endogenous effects.

## Changed
* Updated to remify 4.1.0, remstats 4.1.0 and remstimate 3.1.0.
* The estimation window is now specified in events rather than time points: "Compute statistics from timepoint ... to ..." is now "Fit model to events ... to ...", and the first event that can be included is event 2. The first event has no history and only serves to initialise the event statistics.
* Manual risk set now specifies the dyads to **include** (the uploaded file becomes the risk set; observed dyads are added automatically), rather than dyads to exclude, matching remify 4.0.
* Endogenous "Consider type" is now Ignore / Separate / Interact (previously Yes / No / Both). Effects separated or interacted by event type now report one coefficient per type (or per type pair), e.g. "Inertia (social)" and "Inertia (work)".

## Fixed
* All models were estimated incorrectly. The model was fitted from the first event onwards, even though that event only initialises the event statistics. This added one event to the degrees of freedom, and for data whose time column does not start near zero it let the first event dominate the likelihood, pushing the intercept down and inflating every other coefficient. Estimates and fit measures now match the same model fitted directly in R.
* Selecting "Active (saturated)" as the risk set no longer crashes the analysis.
* Results no longer depend on the order in which options are set. The processed event history was not rebuilt when exogenous effects changed, re-uploaded covariate files were ignored, and in actor-oriented models changes to the sender exogenous effects did not update the results.
* Exogenous covariates are no longer resolved against data left over from a previous run of the analysis.
* Fixed an error when more than one event-level covariate was selected.
* Fixed a crash in actor-oriented models with an ordinal event sequence.
* "Consider type" now works with more than one event type (previously crashed for multi-type data).
* Legacy analyses using the (now unsupported) BSIR estimation method show a clear message instead of crashing.

## Removed
* Removed the "Simultaneous events" option (remstats 4.0 no longer supports a per-event method).
* Removed the "Save old effects" option. It never took effect, because the stored statistics were discarded whenever an effect changed, and when it did apply it could produce results that differed from a normal run.

---

# jaspModuleTemplate 0.2.0
## Added
* Added NEWS.md
* Added workflow to remind users to update their `NEWS.md`.
* Added workflow to auto-bump version when user does not do so.

---

# jaspModuleTemplate 0.1.0

## Added
* Initial examples to showcase JASP module development

## Changed
* Use best practices for checking input ([Issue #19](https://github.com/jasp-stats/jaspModuleTemplate/issues/19)).
* The main results table now defaults to displaying 95% Confidence Intervals for effect sizes.

## Fixed
* Remove deprecated dependencies from qml files ([Issue #14](https://github.com/jasp-stats/jaspModuleTemplate/issues/14)).
