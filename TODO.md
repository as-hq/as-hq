# AlphaSheets (alphasheets)

> The future of computation


## Short term TODOs (bugfixes):

* Change messaging so that `CELL_CHANGED` no longer propagates an action, but rather is handled at the component level.
* Create default changed-cell listener in `ASEvaluationPane`, which changes the display in `ASCodeEditor`.
* Add fin event listeners as listener props to `ASSpreadsheet`, and implement and pass in defaults from `ASEvaluationPane`.


## Medium term TODOs (new functionality):

* Create top bar for code editor for language selection. Check compatibility with standard Material UI.
* Implement web API skeleton, then full implementation in `ActionCreator`s.
* Create dynamic loading architecture for `ASEvaluationPane`, and link to the web API.
* Implement hook to WS and make it propagate changes to the spreadsheet.
* Implement skeleton of rest of UI.
