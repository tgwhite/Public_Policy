

const {Builder, By, Key, until} = require('selenium-webdriver');
const fs = require('fs')


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
			var county_name = county_names[county_it].innerText; 
			var percent_reporting = county_names[county_it].textContent.replace(county_name, ''); 
			var out_obj = {
				name: county_name,
				percent_reporting: percent_reporting
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
 let driver = new Builder().forBrowser('firefox').build();
var state_it = 0; 
async function get_state_results(state_name) {	
	console.log('working on ', state_name); 
	var state_name_upd = state_name.toLowerCase().replace(' ', '-'); 
	var the_url = `https://www.nbcnews.com/politics/2020-elections/${state_name_upd}-president-results`; 
	
	

	await driver.get(the_url);
	
	setTimeout(()=>{
		driver.executeScript(the_script).then(function(return_value) {
			console.log('return_value', return_value); 
			var return_val_clean = return_value.map((x)=>{
				x.state_name = state_name; 
				x.url = the_url; 
				return x; 
			}); 
			out_obj_catcher[state_name] = return_val_clean; 

			fs.writeFile(`C:\\Users\\csq\\Documents\\Public_Policy\\Projects\\2020 Election\\data\\2020\\${state_name}.json`, JSON.stringify(return_val_clean), (err) => {
			  if (err) throw err;
			  console.log('The file has been saved!');
			});

			state_it++; 
			setTimeout(()=>{

				if (state_it < the_states.length) {
					get_state_results(the_states[state_it])
				} else {
					driver.quit(); 
				}
			}, 500)
		});
	}, 3000); 
	

}
get_state_results(the_states[state_it]); 


