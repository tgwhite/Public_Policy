
var state_results_obj = {}; 


var the_states = ["Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois",
			"Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana",
			"Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island",
			"South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"]; 



// for each state, open a new window and parse its dom with jquery



the_states.forEach((state_name, state_it)=>{
	var state_name = the_states[2]; 
	console.log('working on ', state_name); 
	var state_name_upd = state_name.toLowerCase().replace(' ', '-'); 
	var the_url = `https://www.nbcnews.com/politics/2020-elections/${state_name_upd}-president-results`; 
	var new_window = window.open(the_url); 

	setTimeout(()=>{
		var outer_container =  new_window.$("div[data-testid='county-results-module']"); 

		if (outer_container.children()[1]) {
			outer_container.children()[1].children[0].click()
		}


		// 
		var county_names = outer_container.children()[0].children[0].children[1].lastChild.getElementsByTagName('li')
		var result_array = []; 
		var candidiate_results = outer_container.children()[0].children[1].children[0].children; 

		// 

		for (var county_it = 0; county_it < county_names.length; county_it++) {
			console.log('county_it', county_it); 
			// county_it = 0; 
			var out_obj = {
				name: county_names[county_it].innerText
			}


			for (var cand_it = 0; cand_it < 3; cand_it++) {
				// cand_it = 0; 
				var cand_name = candidiate_results[cand_it].getElementsByClassName(
				    'candidate')[0].getElementsByClassName(
				    'name')[0].innerText; 

			    out_obj[cand_name] = candidiate_results[cand_it].getElementsByClassName('column-group')[0].children[1].getElementsByTagName('li')[county_it].innerText; 
			}

			result_array.push(out_obj)
		}

		state_results_obj[state_name] = result_array; 
	}, 2000)
	

})





// https://www.nbcnews.com/politics/2020-elections/new-hampshire-president-results
// https://www.nbcnews.com/politics/2020-elections/california-president-results





candidiate_results[0].getElementsByClassName('column-group')
// result cells (same length)
candidiate_results[0].getElementsByClassName('column-group')[0].children[1].getElementsByTagName('li').length

// 10:45 banfield 
// for each candidate