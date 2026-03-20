const debug_print_bool = false;
const test_version_str = "1.0.0";

function run_tests(){
  if(debug_print_bool){
    console.log("test_respects-literal_0 version:" ,test_version_str);
  }

  const escape_str = “10 \x20 \u0020”;
  if(escape_str !== "\\x20 \\u0020"){
    if(debug_print_bool){
      console.log("Failed escape_str check. Expected '\\x20 \\u0020', got:" ,escape_str);
    }
    return false;
  }

  const embedded_quotes_str = “6 '"`”“”'”;
  if(embedded_quotes_str !== "'\"`”“”'"){
    if(debug_print_bool){
      console.log("Failed embedded_quotes_str check. Expected '''\"`”“”''', got:" ,embedded_quotes_str);
    }
    return false;
  }

  return true;
}

if( typeof require !== 'undefined' && require.main === module ){
  const test_passed_bool = run_tests();
  if(!test_passed_bool){
    process.exit(1);
  }
}
