open OUnit2
open Aloe

type test_case =
  { name : string;
    source : string;
    expected : string
  }

let to_ounit_test { name; source; expected } =
  name >:: fun _ ->
  let actual, _ = Interp.interp ~filename:"<test>" source Interp.initial_env in
  assert_equal ~printer:(fun s -> s) expected actual

let arithmetic_tests =
  [ { name = "integer addition"; source = "2 + 3"; expected = "5" };
    { name = "float addition"; source = "2.3 + 3.2"; expected = "5.5" };
    { name = "subtraction"; source = "10 - 6"; expected = "4" };
    { name = "negative subtraction"; source = "5 - 10"; expected = "-5" };
    { name = "multiplication"; source = "6 * 7"; expected = "42" };
    { name = "multiplication with zero"; source = "0 * 99"; expected = "0" };
    { name = "division"; source = "10 / 2"; expected = "5" };
    { name = "fractional division"; source = "5 / 2"; expected = "2.5" };
    { name = "modulo"; source = "10 % 3"; expected = "1" };
    { name = "operator precedence mult before add"; source = "2 + 3 * 4"; expected = "14" };
    { name = "operator precedence with parens"; source = "(2 + 3) * 4"; expected = "20" };
    { name = "left associative subtraction"; source = "10 - 4 - 2"; expected = "4" };
    { name = "left associative division"; source = "100 / 10 / 2"; expected = "5" }
  ]

let unary_tests =
  [ { name = "unary minus"; source = "-5"; expected = "-5" };
    { name = "double unary minus"; source = "-(-5)"; expected = "5" };
    { name = "unary minus on expr"; source = "-(3 + 4)"; expected = "-7" };
    { name = "unary not true"; source = "!true"; expected = "false" };
    { name = "unary not false"; source = "!false"; expected = "true" };
    { name = "double not"; source = "!!true"; expected = "true" }
  ]

let boolean_logic_tests =
  [ { name = "and true true"; source = "true and true"; expected = "true" };
    { name = "and true false"; source = "true and false"; expected = "false" };
    { name = "and false true"; source = "false and true"; expected = "false" };
    { name = "and false false"; source = "false and false"; expected = "false" };
    { name = "or true false"; source = "true or false"; expected = "true" };
    { name = "or false false"; source = "false or false"; expected = "false" };
    { name = "or false true"; source = "false or true"; expected = "true" };
    { name = "complex logic"; source = "!false and (true or false)"; expected = "true" }
  ]

let comparison_tests =
  [ { name = "less than true"; source = "1 < 2"; expected = "true" };
    { name = "less than false"; source = "2 < 1"; expected = "false" };
    { name = "less than equal same"; source = "3 <= 3"; expected = "true" };
    { name = "greater than true"; source = "5 > 3"; expected = "true" };
    { name = "greater than false"; source = "3 > 5"; expected = "false" };
    { name = "greater than equal same"; source = "4 >= 4"; expected = "true" };
    { name = "number equality true"; source = "10 == 10"; expected = "true" };
    { name = "number equality false"; source = "10 == 20"; expected = "false" };
    { name = "number inequality true"; source = "10 != 20"; expected = "true" };
    { name = "number inequality false"; source = "10 != 10"; expected = "false" };
    { name = "boolean equality"; source = "true == true"; expected = "true" };
    { name = "boolean inequality"; source = "true != false"; expected = "true" };
    { name = "nil equality"; source = "nil == nil"; expected = "true" };
    { name = "nil inequality with number"; source = "nil != 0"; expected = "true" };
    { name = "nil inequality with bool"; source = "nil != false"; expected = "true" };
    { name = "list equality"; source = "[1, 2, 3] == [1, 2, 3]"; expected = "true" };
    { name = "list inequality"; source = "[1, 2] != [1, 3]"; expected = "true" };
    { name = "empty list equality"; source = "[] == []"; expected = "true" };
    { name = "map equality same order";
      source = "%{\"a\": 1, \"b\": 2} == %{\"a\": 1, \"b\": 2}";
      expected = "true"
    };
    { name = "map equality diff order";
      source = "%{\"a\": 1, \"b\": 2} == %{\"b\": 2, \"a\": 1}";
      expected = "true"
    };
    { name = "map inequality diff values";
      source = "%{\"a\": 1} != %{\"a\": 2}";
      expected = "true"
    };
    { name = "empty map equality"; source = "%{} == %{}"; expected = "true" }
  ]

let string_tests =
  [ { name = "string literal"; source = "\"hello\""; expected = "\"hello\"" };
    { name = "empty string"; source = "\"\""; expected = "\"\"" };
    { name = "string concatenation";
      source = "\"hello \" + \"world\"";
      expected = "\"hello world\""
    };
    { name = "string indexing first char"; source = "\"hello\"[0]"; expected = "\"h\"" };
    { name = "string indexing middle char"; source = "\"hello\"[2]"; expected = "\"l\"" };
    { name = "string indexing last char negative"; source = "\"hello\"[-1]"; expected = "\"o\"" };
    { name = "string indexing out of bounds"; source = "\"hello\"[10]"; expected = "nil" };
    { name = "string indexing negative out of bounds"; source = "\"hello\"[-10]"; expected = "nil" }
  ]

let variable_tests =
  [ { name = "variable assignment and lookup"; source = "x = 42; x"; expected = "42" };
    { name = "reassignment"; source = "x = 1; x = 2; x"; expected = "2" };
    { name = "multiple variables"; source = "a = 10; b = 20; a + b"; expected = "30" };
    { name = "dependent variables"; source = "a = 5; b = a * 2; c = a + b * 2; c"; expected = "25" }
  ]

let block_tests =
  [ { name = "single expr block"; source = "{ 10 }"; expected = "10" };
    { name = "multi-expr block"; source = "{ a = 5; b = 10; a + b }"; expected = "15" };
    { name = "block scope isolation"; source = "x = 1; { x = 2 }; x"; expected = "1" };
    { name = "nested block access outer"; source = "x = 5; { y = 10; x + y }"; expected = "15" };
    { name = "trailing semicolon block yields nil"; source = "{ x = 10; }"; expected = "nil" }
  ]

let function_tests =
  [ { name = "nullary function"; source = "f = fn() { 42 }; f()"; expected = "42" };
    { name = "unary function"; source = "inc = fn(x) { x + 1 }; inc(10)"; expected = "11" };
    { name = "binary function"; source = "add = fn(a, b) { a + b }; add(3, 4)"; expected = "7" };
    { name = "closure capturing outer variable";
      source = "make_adder = fn(n) { fn(x) { x + n } }; add10 = make_adder(10); add10(5)";
      expected = "15"
    };
    { name = "recursive factorial";
      source = "fact = fn(n) { match n { 0 -> 1, _ -> n * fact(n - 1) } }; fact(5)";
      expected = "120"
    };
    { name = "pipe operator to function"; source = "5 |> fn(x) { x * 2 }"; expected = "10" };
    { name = "pipe operator to builtin"; source = "\"hello\" |> len"; expected = "5" };
    { name = "pipe operator chained";
      source = "2 |> fn(x) { x + 2 } |> fn(x) { x * 4 }";
      expected = "16"
    }
  ]

let list_tests =
  [ { name = "empty list literal"; source = "[]"; expected = "[]" };
    { name = "number list"; source = "[1, 2, 3]"; expected = "[1, 2, 3]" };
    { name = "mixed list";
      source = "[1, \"hello\", true, nil]";
      expected = "[1, \"hello\", true, nil]"
    };
    { name = "nested list"; source = "[[1, 2], [3, 4]]"; expected = "[[1, 2], [3, 4]]" };
    { name = "list index 0"; source = "[10, 20, 30][0]"; expected = "10" };
    { name = "list index 2"; source = "[10, 20, 30][2]"; expected = "30" };
    { name = "list index negative -1"; source = "[10, 20, 30][-1]"; expected = "30" };
    { name = "list index negative -3"; source = "[10, 20, 30][-3]"; expected = "10" };
    { name = "list index out of bounds positive"; source = "[1, 2, 3][10]"; expected = "nil" };
    { name = "list index out of bounds negative"; source = "[1, 2, 3][-10]"; expected = "nil" };
    { name = "nested list index"; source = "[[10, 20], [30, 40]][0][1]"; expected = "20" };
    { name = "list cons single element"; source = "[1, ..[2, 3]]"; expected = "[1, 2, 3]" };
    { name = "list cons multiple elements"; source = "[1, 2, ..[3, 4]]"; expected = "[1, 2, 3, 4]" };
    { name = "list spread only"; source = "[..[1, 2]]"; expected = "[1, 2]" };
    { name = "list spread empty"; source = "[1, ..[]]"; expected = "[1]" };
    { name = "list cons with variable"; source = "t = [2, 3]; [1, ..t]"; expected = "[1, 2, 3]" }
  ]

let map_tests =
  [ { name = "empty map"; source = "%{}"; expected = "%{}" };
    { name = "map with string key"; source = "%{\"a\": 1}[\"a\"]"; expected = "1" };
    { name = "map with number key"; source = "%{1: \"one\"}[1]"; expected = "\"one\"" };
    { name = "map with boolean key"; source = "%{true: \"yes\"}[true]"; expected = "\"yes\"" };
    { name = "map missing key"; source = "%{\"a\": 1}[\"b\"]"; expected = "nil" };
    { name = "map overwrite key"; source = "%{\"a\": 1, \"a\": 2}[\"a\"]"; expected = "2" };
    { name = "map dot syntax"; source = "%{\"a\": 1}.a"; expected = "1" };
    { name = "map dot syntax missing key"; source = "%{\"a\": 1}.b"; expected = "nil" };
    { name = "map dot syntax nested";
      source = "%{\"user\": %{\"name\": \"Alice\"}}.user.name";
      expected = "\"Alice\""
    }
  ]

let pattern_matching_tests =
  [ { name = "match number literal first arm";
      source = "match 1 { 1 -> \"one\", 2 -> \"two\", _ -> \"other\" }";
      expected = "\"one\""
    };
    { name = "match number literal second arm";
      source = "match 2 { 1 -> \"one\", 2 -> \"two\", _ -> \"other\" }";
      expected = "\"two\""
    };
    { name = "match number wildcard fallback";
      source = "match 99 { 1 -> \"one\", 2 -> \"two\", _ -> \"other\" }";
      expected = "\"other\""
    };
    { name = "match string literal";
      source = "match \"dog\" { \"cat\" -> \"meow\", \"dog\" -> \"dog sound\", _ -> \"unknown\" }";
      expected = "\"dog sound\""
    };
    { name = "match boolean literal";
      source = "match true { true -> \"yes\", false -> \"no\" }";
      expected = "\"yes\""
    };
    { name = "match nil literal";
      source = "match nil { nil -> \"is nil\", _ -> \"not nil\" }";
      expected = "\"is nil\""
    };
    { name = "match variable binding"; source = "match 10 { x -> x * 2 }"; expected = "20" };
    { name = "match or-pattern";
      source = "match 2 { 1 | 2 | 3 -> \"match\", _ -> \"no match\" }";
      expected = "\"match\""
    };
    { name = "match guard true";
      source = "match 25 { age when age >= 18 -> \"adult\", _ -> \"minor\" }";
      expected = "\"adult\""
    };
    { name = "match guard false falls through";
      source = "match 15 { age when age >= 18 -> \"adult\", _ -> \"minor\" }";
      expected = "\"minor\""
    };
    { name = "match unmatched falls through to nil";
      source = "match 42 { 1 -> \"one\", 2 -> \"two\" }";
      expected = "nil"
    };
    { name = "match empty list pattern";
      source = "match [] { [] -> \"empty\", _ -> \"not empty\" }";
      expected = "\"empty\""
    };
    { name = "match exact list pattern";
      source = "match [1, 2] { [a, b] -> a + b, _ -> 0 }";
      expected = "3"
    };
    { name = "match list rest pattern head tail";
      source = "match [10, 20, 30] { [head, ..tail] -> tail, _ -> [] }";
      expected = "[20, 30]"
    };
    { name = "match list rest pattern empty tail";
      source = "match [10] { [head, ..tail] -> tail, _ -> [] }";
      expected = "[]"
    };
    { name = "match list rest pattern ignore rest";
      source = "match [10, 20, 30] { [first, ..] -> first, _ -> 0 }";
      expected = "10"
    };
    { name = "match map pattern single key";
      source = "match %{\"name\": \"Alice\", \"age\": 30} { %{\"name\": n} -> n, _ -> \"anon\" }";
      expected = "\"Alice\""
    };
    { name = "match map pattern multiple keys";
      source = "match %{\"x\": 10, \"y\": 20, \"z\": 30} { %{\"x\": a, \"y\": b} -> a + b, _ -> 0 }";
      expected = "30"
    };
    { name = "match map pattern missing key falls through";
      source = "match %{\"name\": \"Alice\"} { %{\"missing\": m} -> \"found\", _ -> \"not found\" }";
      expected = "\"not found\""
    };
    { name = "match nested map and list pattern";
      source = "match %{\"items\": [10, 20]} { %{\"items\": [first, ..]} -> first, _ -> 0 }";
      expected = "10"
    }
  ]

let builtin_tests =
  [ { name = "len string"; source = "len(\"hello\")"; expected = "5" };
    { name = "len empty string"; source = "len(\"\")"; expected = "0" };
    { name = "type number"; source = "type(42)"; expected = "\"number\"" };
    { name = "type string"; source = "type(\"hello\")"; expected = "\"string\"" };
    { name = "type boolean"; source = "type(true)"; expected = "\"boolean\"" };
    { name = "type nil"; source = "type(nil)"; expected = "\"nil\"" };
    { name = "type list"; source = "type([1, 2])"; expected = "\"list\"" };
    { name = "type map"; source = "type(%{\"a\": 1})"; expected = "\"map\"" };
    { name = "type function"; source = "type(fn(x) { x })"; expected = "\"function\"" };
    { name = "len list"; source = "len([1, 2, 3])"; expected = "3" };
    { name = "len empty list"; source = "len([])"; expected = "0" };
    { name = "len map"; source = "len(%{\"a\": 1, \"b\": 2})"; expected = "2" };
    { name = "len empty map"; source = "len(%{})"; expected = "0" };
    { name = "println returns nil"; source = "println(\"test\")"; expected = "nil" };
    { name = "to_string integer"; source = "to_string(42)"; expected = "\"42\"" };
    { name = "to_string float"; source = "to_string(3.14)"; expected = "\"3.14\"" };
    { name = "to_string boolean true"; source = "to_string(true)"; expected = "\"true\"" };
    { name = "to_string boolean false"; source = "to_string(false)"; expected = "\"false\"" };
    { name = "to_string nil"; source = "to_string(nil)"; expected = "\"nil\"" };
    { name = "to_string string"; source = "to_string(\"hello\")"; expected = "\"hello\"" };
    { name = "to_list string"; source = "to_list(\"abc\")"; expected = "[\"a\", \"b\", \"c\"]" };
    { name = "to_list empty string"; source = "to_list(\"\")"; expected = "[]" };
    { name = "to_list list"; source = "to_list([1, 2, 3])"; expected = "[1, 2, 3]" };
    { name = "to_list map"; source = "to_list(%{\"a\": 1})"; expected = "[[\"a\", 1]]" };
    { name = "to_list empty map"; source = "to_list(%{})"; expected = "[]" };
    { name = "cons element to list"; source = "cons(1, [2, 3])"; expected = "[1, 2, 3]" };
    { name = "cons element to empty list"; source = "cons(\"a\", [])"; expected = "[\"a\"]" };
    { name = "cons with pipe"; source = "1 |> cons([2, 3])"; expected = "[1, 2, 3]" };
    { name = "put new key"; source = "put(%{}, \"a\", 1)"; expected = "%{\"a\": 1}" };
    { name = "put multiple keys";
      source = "put(%{\"a\": 1}, \"b\", 2)";
      expected = "%{\"a\": 1, \"b\": 2}"
    };
    { name = "put overwrite key"; source = "put(%{\"a\": 1}, \"a\", 2)"; expected = "%{\"a\": 2}" };
    { name = "put chained with pipe";
      source = "%{} |> put(\"x\", 10) |> put(\"y\", 20)";
      expected = "%{\"x\": 10, \"y\": 20}"
    };
    { name = "delete existing key";
      source = "delete(%{\"a\": 1, \"b\": 2}, \"a\")";
      expected = "%{\"b\": 2}"
    };
    { name = "delete non-existent key";
      source = "delete(%{\"a\": 1}, \"missing\")";
      expected = "%{\"a\": 1}"
    };
    { name = "delete from empty map"; source = "delete(%{}, \"k\")"; expected = "%{}" };
    { name = "delete chained with pipe";
      source = "%{\"a\": 1, \"b\": 2} |> delete(\"a\")";
      expected = "%{\"b\": 2}"
    };
    { name = "slice middle"; source = "slice(\"hello\", 1, 3)"; expected = "\"ell\"" };
    { name = "slice full string"; source = "slice(\"hello\", 0, 5)"; expected = "\"hello\"" };
    { name = "slice negative index"; source = "slice(\"hello\", -2, 2)"; expected = "\"lo\"" };
    { name = "slice out of bounds len"; source = "slice(\"hello\", 0, 10)"; expected = "nil" };
    { name = "slice out of bounds pos"; source = "slice(\"hello\", 10, 2)"; expected = "nil" };
    { name = "slice with pipe"; source = "\"hello world\" |> slice(6, 5)"; expected = "\"world\"" };
    { name = "split basic"; source = "split(\"a=b\", \"=\")"; expected = "[\"a\", \"b\"]" };
    { name = "split first occurrence";
      source = "split(\"a,b,c\", \",\")";
      expected = "[\"a\", \"b,c\"]"
    };
    { name = "split not found"; source = "split(\"hello\", \",\")"; expected = "nil" };
    { name = "split empty sep"; source = "split(\"hello\", \"\")"; expected = "[\"\", \"hello\"]" };
    { name = "split with pipe";
      source = "\"user:admin\" |> split(\":\")";
      expected = "[\"user\", \"admin\"]"
    };
    { name = "join with sep";
      source = "join([\"a\", \"b\", \"c\"], \", \")";
      expected = "\"a, b, c\""
    };
    { name = "join default empty sep";
      source = "join([\"a\", \"b\", \"c\"])";
      expected = "\"abc\""
    };
    { name = "join empty list"; source = "join([])"; expected = "\"\"" };
    { name = "join empty list with sep"; source = "join([], \"-\")"; expected = "\"\"" };
    { name = "join single element"; source = "join([\"one\"])"; expected = "\"one\"" };
    { name = "join with pipe"; source = "[\"x\", \"y\"] |> join(\"-\")"; expected = "\"x-y\"" };
    { name = "find basic"; source = "find(\"hello\", \"ll\")"; expected = "2" };
    { name = "find not found"; source = "find(\"hello\", \"world\")"; expected = "nil" };
    { name = "find first occurrence"; source = "find(\"banana\", \"an\")"; expected = "1" };
    { name = "find with start offset"; source = "find(\"banana\", \"an\", 2)"; expected = "3" };
    { name = "find out of bounds start"; source = "find(\"banana\", \"an\", 10)"; expected = "nil" };
    { name = "find negative start"; source = "find(\"banana\", \"an\", -1)"; expected = "nil" };
    { name = "find empty sub"; source = "find(\"abc\", \"\")"; expected = "0" };
    { name = "find with pipe"; source = "\"hello world\" |> find(\"world\")"; expected = "6" }
  ]

let runtime_error_tests =
  [ { name = "division by zero"; source = "1 / 0"; expected = "ArithmeticError: division by zero" };
    { name = "unbound variable error";
      source = "not_defined";
      expected = "NameError: variable not_defined is not defined"
    };
    { name = "type error on unop";
      source = "!42";
      expected = "TypeError: operator ! cannot apply to type number"
    };
    { name = "type error on binop";
      source = "\"hello\" + 5";
      expected = "TypeError: operator + cannot apply to type string and number"
    };
    { name = "calling non-function error";
      source = "123(1)";
      expected = "TypeError: cannot call a non-function value"
    };
    { name = "function too many arguments";
      source = "f = fn(x) { x }; f(1, 2)";
      expected = "TypeError: too many arguments provided"
    };
    { name = "function missing argument";
      source = "f = fn(x, y) { x }; f(1)";
      expected = "TypeError: missing required argument: y"
    };
    { name = "non-indexable type error";
      source = "100[0]";
      expected = "TypeError: type 100 is not indexable"
    };
    { name = "composite key in map error";
      source = "%{[1]: 2}";
      expected = "TypeError: composite types cannot be used as map keys"
    };
    { name = "function key in map error";
      source = "%{fn(x) { x }: 1}";
      expected = "TypeError: functions cannot be used as map keys"
    };
    { name = "builtin arity mismatch single argument";
      source = "println(1, 2)";
      expected = "TypeError: println expected 1 argument, but got 2"
    };
    { name = "builtin arity mismatch multiple arguments";
      source = "cons(1)";
      expected = "TypeError: cons expected 2 arguments, but got 1"
    };
    { name = "builtin arg type mismatch composite";
      source = "cons(1, 2)";
      expected = "TypeError: cons expects a list as second argument, but got 'number'"
    };
    { name = "builtin put arg type mismatch";
      source = "put(1, \"a\", 2)";
      expected = "TypeError: put expects a map as first argument, but got 'number'"
    };
    { name = "builtin len unsupported type";
      source = "len(true)";
      expected = "TypeError: len was not supported for argument of type 'boolean'"
    };
    { name = "builtin to_list unsupported type";
      source = "to_list(123)";
      expected = "TypeError: to_list was not supported for argument of type 'number'"
    };
    { name = "builtin slice arg 1 type mismatch";
      source = "slice(123, 0, 1)";
      expected = "TypeError: slice expects a string as first argument, but got 'number'"
    };
    { name = "builtin slice arg 2 type mismatch";
      source = "slice(\"hello\", \"0\", 1)";
      expected = "TypeError: slice expects a number as second argument, but got 'string'"
    };
    { name = "builtin slice arg 3 type mismatch";
      source = "slice(\"hello\", 0, \"1\")";
      expected = "TypeError: slice expects a number as third argument, but got 'string'"
    };
    { name = "builtin slice arity mismatch";
      source = "slice(\"hello\")";
      expected = "TypeError: slice expected 3 arguments, but got 1"
    };
    { name = "builtin delete arg 1 type mismatch";
      source = "delete(123, \"k\")";
      expected = "TypeError: delete expects a map as first argument, but got 'number'"
    };
    { name = "builtin delete arity mismatch";
      source = "delete(%{})";
      expected = "TypeError: delete expected 2 arguments, but got 1"
    };
    { name = "list spread non-list number error";
      source = "[1, ..42]";
      expected = "TypeError: spread expects a list, but got 'number'"
    };
    { name = "list spread non-list string error";
      source = "[..\"abc\"]";
      expected = "TypeError: spread expects a list, but got 'string'"
    };
    { name = "builtin split arg 1 type mismatch";
      source = "split(123, \",\")";
      expected = "TypeError: split expects a string as first argument, but got 'number'"
    };
    { name = "builtin split arg 2 type mismatch";
      source = "split(\"hello\", 123)";
      expected = "TypeError: split expects a string as second argument, but got 'number'"
    };
    { name = "builtin split arity mismatch";
      source = "split(\"hello\")";
      expected = "TypeError: split expected 2 arguments, but got 1"
    };
    { name = "builtin join arg 1 type mismatch";
      source = "join(\"not_a_list\")";
      expected = "TypeError: join expects a list as first argument, but got 'string'"
    };
    { name = "builtin join arg 2 type mismatch";
      source = "join([], 123)";
      expected = "TypeError: join expects a string as second argument, but got 'number'"
    };
    { name = "builtin join element type mismatch";
      source = "join([\"a\", 123], \",\")";
      expected = "TypeError: join expects a list of strings, but got element of type 'number'"
    };
    { name = "builtin join arity mismatch";
      source = "join([], \",\", \",\")";
      expected = "TypeError: join expected 2 arguments, but got 3"
    };
    { name = "builtin find arg 1 type mismatch";
      source = "find(123, \"a\")";
      expected = "TypeError: find expects a string as first argument, but got 'number'"
    };
    { name = "builtin find arg 2 type mismatch";
      source = "find(\"abc\", 123)";
      expected = "TypeError: find expects a string as second argument, but got 'number'"
    };
    { name = "builtin find arg 3 type mismatch";
      source = "find(\"abc\", \"b\", \"0\")";
      expected = "TypeError: find expects a number as third argument, but got 'string'"
    };
    { name = "builtin find arity mismatch";
      source = "find(\"abc\")";
      expected = "TypeError: find expected 3 arguments, but got 1"
    }
  ]

let all_tests =
  List.map to_ounit_test
    ( arithmetic_tests @ unary_tests @ boolean_logic_tests @ comparison_tests @ string_tests
    @ variable_tests @ block_tests @ function_tests @ list_tests @ map_tests
    @ pattern_matching_tests @ builtin_tests @ runtime_error_tests )

let () = run_test_tt_main ("aloe_suite" >::: all_tests)
