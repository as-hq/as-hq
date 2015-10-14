
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

AlphaSheets has two testing directories, `unit-tests` and `int-tests`, the latter of which contains the integration tests. You can run the unit tests as follows:

```bash
$ npm test
```

and run integration tests as follows:

```bash
$ karma start
```

Soon, both test suites will be run using `gulp test`, but run them separately for now.


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
