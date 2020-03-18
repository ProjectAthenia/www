# Athenia App Upgrade Guide

To upgrade from previous version of Athenia please check each version number listed below step by step.

## 0.4.0

CRUD! This adds a huge new feature that will allow you to quickly add CRUD modules that will allow someone the ability to manage data in any Athenia based API. Most of this is brand new functionality, but there are a few updates made to existing files that will need to be made as well. This update also officially upgrades the core from elm 0.19.0 to 0.19.1, so make sure to update both the elm.json and the package.json.

### New Files

* src/elm/Components/AudioPlayer.elm
* src/elm/Components/CRUD/
* src/elm/Components/Toast.elm
* src/elm/Modals/Confirmation.elm
* src/elm/Models/Asset.elm
* src/elm/Models/FileUpload.elm
* src/elm/Models/Status.elm
* src/elm/Ports/FileReader.elm
* src/elm/Utilities/Expands.elm
* src/elm/Utilities/Order.elm
* src/elm/Utilities/SearchField.elm
* tests/UnitTests/Models/AssetTest.elm

### Existing Files

* src/elm/Api.elm 
* src/ports/athenia.js

## 0.3.1

This fixes a potential bug with the article editor displaying the incorrect iteration source. To finish this update simply copy over the following files.

* src/elm/Models/Wiki/Article.elm
* src/elm/Page/Article/Editor.elm
* tests/UnitTests/Models/Wiki/ArticleTest.elm

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
