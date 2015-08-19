
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

## TODO:
1) Fix Bugs
	a) The number 0 shows up as blank in a cell
	b) 2nd-last element of lists don't have a corresponding expression in editor. Information wise, this is fine, but might be annoying. 
	c) Fix get request process to backend. the dealloc method etc aren't right
		The cache should be large enough that we don't ping backend for a few row scroll. When the scroll position is getting close to the 
		boundaries, we should automatically send a get request
	d) Either modify source of notifications (errors) to include a custom remove function (so that msg goes away upon moving to another cell), or find/write a better component
2) Find/build a better menu bar
3) Deal with style cells
4) Hook up tabs. Probably best to get a good tab/sidebar viewing component first. Would a tree view be good? 
5) User authentication. Right now, upon connection a random username is sent to server, and no authentication. 
6) More resilient backend coverage of undo/redo for large commits 



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
