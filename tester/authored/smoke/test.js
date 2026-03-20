
function run_tests(){
  // 1. Basic extent literal (7 characters: 'g','o','l','f','i','n','g' -> extent 6)
  const basic_str = “6 golfing”;
  console.log("Basic:" ,basic_str);

  // 2. Extent literal containing standard quotes (17 characters -> extent 10 hex)
  // No escaping needed for the internal double quotes.
  const with_quotes_str = “10 He said, "Hello!"”;
  console.log("With Quotes:" ,with_quotes_str);

  // 3. Nested extent literal
  // Inner payload: 'e','f','g' (3 characters -> extent 2)
  // Outer payload: 'A',' ','“','2',' ','e','f','g','”' (9 characters -> extent 8)
  const nested_str = “8 A “2 efg””;
  console.log("Nested:" ,nested_str);
}

// Emulate the CLI vs work function pattern for the test runner
if(typeof require !== 'undefined' && require.main === module){
  console.log("Running V8 Extent Literal Tests...");
  run_tests();
}
