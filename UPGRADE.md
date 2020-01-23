# Athenia App Upgrade Guide

To upgrade from previous version of Athenia please check each version number listed below step by step.

## 0.3.0

This update is a pretty big update that adds a ton of utility functions. To start, make sure that list extra `elm-community/list-extra` is installed. Then update the following files.

* src/elm/Models/Role.elm
* src/elm/Models/User/User.elm
* src/elm/Utilities/ModelHelpers.elm
* src/elm/Utilities/StringHelper.elm
* src/index.js
* webpack.config.js
* tests/UnitTests/Models/User/UserTest.elm
* tests/UnitTests/Utilities/StringHelperTest.elm

Then copy over the `src/ports/` directory, and `tests/UnitTests/Utilities/ModelHelpersTest.elm` test to finish the update.

## 0.2.1

This is a simple security update. Start off by adding `"uglifyjs-webpack-plugin": "^1.3.0",` to the dev dependencies, and then run update. The update `webpack.config.js` to the version found in this project.
