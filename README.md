
# AlphaSheets (alphasheets)

> The future of computation


## Installing AlphaSheets

If you do not already have `npm`, `node.js`, or `bower`, please install those first before continuing.

To initialize all libraries, run:

```bash
$ npm install
$ bower install
```

This should seed the `dist/js/components` and `node_modules` folders. Check to make sure this has happened.


## Running AlphaSheets

AlphaSheets has a live-reloading static server on port `8080` (you can change the port in the `gulpfile.js` file), which will build, launch, and rebuild the app whenever you change application code. To start the server, run:

```bash
$ npm start
```

If you prefer to just build without the live reload and build-on-each-change watcher, run:

```bash
$ npm run build
```


## Testing AlphaSheets

AlphaSheets has three testing directories, `unit-tests` (inactive), `int-tests` (integration tests), and `src/js/browser-test` (which contains test infrastructure and browser-based UI tests).

You can run API integration tests as follows:

```bash
$ gulp test-all
```

If you only want to run excel or eval tests, replace `test-all` with `test-excel` or `test-eval`, respectively.

You can run UI integration tests by opening Chrome, opening the console, and typing `test()`.


## Generating Additional Code

You can add additional functionality to your application by invoking the subgenerators included in the Flux Generator. You can add components using the following commands:

#### Components
```bash
$ yo flux:component ComponentName
```

#### Actions
```bash
$ yo flux:action ActionCreatorName
```

#### Stores
```bash
$ yo flux:store StoreName
```
