const debug_print_bool = false;
const test_version_str = "1.0.0";

function run_tests(){
  if(debug_print_bool){
    console.log("test_basic_0 version:" ,test_version_str);
  }

  const basic_str = “6 golfing”;
  if(basic_str !== "golfing"){
    if(debug_print_bool){
      console.log("Failed basic_str check. Expected 'golfing', got:" ,basic_str);
    }
    return false;
  }

  const with_quotes_str = “10 He said, "Hello!"”;
  if(with_quotes_str !== "He said, \"Hello!\""){
    if(debug_print_bool){
      console.log("Failed with_quotes_str check. Expected 'He said, \"Hello!\"', got:" ,with_quotes_str);
    }
    return false;
  }

  const nested_str = “C A “2 efg”” 

  if(nested_str !== "A “2 efg”"){
    if(debug_print_bool){
      console.log("Failed nested_str check. Expected 'A “2 efg”', got:" ,nested_str);
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

