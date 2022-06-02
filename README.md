# Lautaro 
Imperative language Lautaro (.lt) - created by Grzegorz B. Zaleski (4184494) for JPP (Programming languages and paradigms) course at the University of Warsaw.

### Description
Language constains standard, most used and popular features similar to vaious imperative languages.

### Types & Declarations
Lautaro has static-typing with built-in typechecker. User can choose from up to three different basic data types - bool (logical), int (whole numbers), string (sequence of characters). <br/>
Declaration rule: 
type name = initial_value;   where initial value can be ommited.<br/>
```ts
int val = 5;
string str;
```

### Functions
Lautaro gives an possibility to define functions: <br/>
function (type1 argname1, type2 argname2, ...) returned_type in 
    [statements]
endfunction <br/>
Function intented not to returned anything is to have returned_type = void. <br/>
By using ref prefix before type user can define argument passed by reference (default one is copy by value). <br/>
```js
function fiver(int a, ref int k) int in 
    int w = 20 + a * k;
    k = w;
    return a + k;
endfunction
```

### Main
Lautaro programme is required to have the main part, from which the execution starts. <br/>
```js
main
    [statements]
endmain
```

### Arithmetics
Int types can be compared, added, subtracted, multiplied, and divided.
For every type, there is a comparison available. <br/>
```js
    bool t = true;
    int x = 5;
    string test = "TEST";
    skip;

    int w = x + 5;
    int a = 20 - w;
    int b = a * w + 1;
    int c = w / x;

    bool k1 = a <> b;
    bool k2 = b < c;
    bool k3 = x <= 5;
    bool k4 = w == 200;
    bool kb = t == true;
```

### Static and runtime error catching
Lautaro possesses a two-tier verification system, firstly programme goes through a static type checker which looks for errors such as types mismatch or incorrect variable usage, then during the interpretation process, each part is dynamically checked for errors that if happen are visibly displayed to the user on the stderr output.

### Const values
Variables can be defined with the `const` prefix, which will render them immutable.
```js
const int x = 10;
```

### Default values
Lautaro automatically sets values to variables initialised without expression. Default bool is false, default int is 0, default string is "". That way user can skip a lot of code and programme is more transparent and simpler.

```js
int x;
int y = 0;
if (y == x) then 
    print("The same value!");
endif
```
### Comments
User can use single-line and multi-line comments.

```js
// int x; This will not run

/*
x = 20; 
    So will this
*/
```
### Casting conditions
Lautaro allows the user to put every basic type as a condition in a while loop/if statement, the condition is to be considered positive if the value in it is NOT a default one for the type. <br/>
This can make code smoother, easier to read, and shorter. <br/>
```js
int n = 10;
while (n) // This programme will count from 10 to 1
    print(n)
    n = n - 1;
endwhile
```

### Automatic return of value from function
Analogically to the previous chapter, a function without the executed return statement will return the default value of its declared type upon concluding.<br/>
```js
function test(int x) int in 
    x = 20;
    int w = 10;
endfunction

main
    int result = 30;
    result = test(70);
    // result is 0 now.
```

### Loop manipulation (Break & Continue)
Keyword `continue` skips the rest of the statements in the while loop, but `break` forces the programme to leave the loop no matter the current value of the loop condition (and skips the rest of the statements in the block as in the `continue` case).<br/>
```js
// Using continue and break
main 
    int n = 0;
    while (true)
        n = n + 1;
        print(n);

        if (n < 5) then 
            continue;
        endif

        if (n > 10) then
            break;
        endif 

        print(n);

    endwhile
endmain
```

### Arrays
In Lautaro user can use multidimensional arrays of infinite size (filled with default values for the array's type) <br/>
Lautaro optimises memory allocation for arrays, which results in their smooth and efficient usage. <br/>
Arrays may be of each of the types available in Lautaro, however, all of the elements must be of the same kind. <br/>

type([])+ name = init_values; <br/>
```js
int[][] tab = [[1,2],[3,4],[5,6]];
bool[] odd = [true, false, true, false]
```
Number of square brackets before names defines the number of dimensions. <br/>
User can access and update single elements in arrays: <br/>
```js
tab[1][0] = 5;
int val = tab[0][0];
```
or the entire array (keep in mind that the dimension of array must stay the same): <br/>
```js
int[][] tab = [[1,2], [3,4]];
tab = [[5,5], [70, 40]];
```
Arrays, like the basic types, work with the `const` keyword (cannot be updated) and `ref` arguments in functions (pass by reference). <br/>
Lautaro arrays have built-in methods `.len` and `.len(n)` which return the number of dimensions and size of n-th dimension respectively (as mentioned before, arrays are infinite therefore as `size` the range containing all non-default elements is returned). <br/>
Arrays are indexed by natural integers (starting from 0). </br>
Update values at position larger than size will automatically update its size. <br/>>
```js
int[][] tab = [[1,2], [3,4]];
tab[4][4] = 64; 
```
From now `tab.len(i)` will return `5` (instead of `2` as before) for i = 0 or 1 and runtime error otherwise.<br/>

### Displaying values
User can use functions `put` or `print` to display the value of basic types (arrays and functions cannot be displayed) to the standard output (console). <br/>
`print` as an opposite to `put` displays the value with an extra endline at the end. <br/>
```js
int w = "TEST";
print(w);
put(w);
```

### Assertions
`assert(condition, message)` allows user to verify and stop programme during runtime with provided message. <br/>

```js
assert(n > 0, "n is negative");
```
### Skip
Instruction has no impact nor side-effects on programme. It allows user to write tiny-like programmes. <br/>
```js
skip;
```
### String methods
String in Lautaro has plenty of built-in methods which allows user to write programmes more conveniently. <br/>
`.length` - the length of string. <br/>
`.reverse` - returns the string in reversed order. <br/>
`.replicate(n)` - replicates string n times. <br/>
`.append(str)` - adds string str after characters of original string. <br/>
`.cut(a, b)` - return substring of string from indices [a; b]. <br/>
These methods do <b>not modify</b> the string, they return newly constructed string.
