# tidydann 1.0.2
* Update required version of dann to get updated fitting algorithm.
* Setting weighted, sphere, or num_comp with the dann engine is now an error
  instead of being silently ignored.
* Moved rlang from Suggests to Imports.
* Updated docs
* Added tidydann_set_threads(), tidydann_get_threads(), and
  tidydann_has_openmp() to control the number of threads used when predicting.
* Examples and tests now limit dann to two threads.

# tidydann 1.0.1

* Removed a bad input test due to parsnip changes.
* Added links to GitHub.
* Added a style action.
* Updated other GitHub actions.
* Changed the size of the graph in the README.

# tidydann 1.0.0

* Better naming.
* Breaking changes.

# tidydann 0.1.0

* Initial release.
