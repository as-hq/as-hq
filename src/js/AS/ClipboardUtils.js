import Store from '../stores/ASEvaluationStore';

export default {

	/* Generates AS-based html from a list of list of values */
	valsToHtml(vals){
		let table = document.createElement('table');
		table.setAttribute("id","alphasheets");
		let tableBody = document.createElement('tbody');
		vals.forEach(function(rowVals){
			let row = document.createElement('tr');
			rowVals.forEach(function(elem){
				let cell = document.createElement('td');
				cell.appendChild(document.createTextNode(elem));
				row.appendChild(cell);
			});
			tableBody.appendChild(row);
		});
		table.appendChild(tableBody);
		return table.outerHTML;
	},
		  
	/* Takes a list of list of values (row-major) and returns a plain string
	Ex. [[3,4]] -> "3\t4" */
	valsToPlain(vals){
		let rowStrs = [];
		vals.forEach(function(row){
			rowStrs.push(row.join('\t'));
		});
		return rowStrs.join('\n');
	},

	/* Checks if a html string was generated by an AS copy */
	htmlStringIsAlphaSheets(s){
		return s.indexOf("alphasheets") > 0;
		// Simplest solution without creating the DOM element and checking for the tag
	},

	/* Takes a text/plain string like "3\t4" and returns a list of list of values (row-major) */
	plainStringToVals(s){
		console.log("CONVERTING PLAIN STRING TO VALS");
		let rows = s.split('\n');
		let vals = [];
		rows.forEach(function(row){
			vals.push(row.split('\t'));
		});
		console.log("VALS: " + JSON.stringify(vals));
		return vals;
	}
}