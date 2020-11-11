

const {Builder, By, Key, until} = require('selenium-webdriver');
 


var the_states = ["Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois",
			"Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana",
			"Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island",
			"South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"]; 


var the_script = `
	function get_result_array() {
		
		var outer_container =  $("div[data-testid='county-results-module']"); 

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
		
		return result_array; 
	}
	var a = get_result_array(); 
	
	return a; 	
`
 
 var out_obj_catcher = {}; 
 

async function get_state_results(state_name) {	
	console.log('working on ', state_name); 
	var state_name_upd = state_name.toLowerCase().replace(' ', '-'); 
	var the_url = `https://www.nbcnews.com/politics/2020-elections/${state_name_upd}-president-results`; 
	
	let driver = new Builder().forBrowser('firefox').build();

	await driver.get(the_url);
	
	setTimeout(()=>{
		driver.executeScript(the_script).then(function(return_value) {
			console.log('return_value', return_value); 
			out_obj_catcher[state_name] = return_value.map((x)=>{
				x.state_name = state_name; 
				x.url = the_url; 
				return x; 
			}); 
			
			setTimeout(()=>{
				driver.quit(); 
			}, 3000)
		});
	}, 3000); 
	

}
the_states.forEach((the_state)=>{
	get_state_results(the_state)
})

fs.writeFile("C:\\Users\\csq\\Documents\\Public_Policy\\Projects\\2020 Election\\data\\results_by_county_state.json", JSON.stringify(out_obj_catcher), (err) => {
  if (err) throw err;
  console.log('The file has been saved!');
});


