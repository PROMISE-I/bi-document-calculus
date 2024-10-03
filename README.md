# BidirectionalAuthor
This is a direct manipulating programming system for article template programs.

## Tool Instructions
### Initial Programs
1. In the editor on the left, write a program based on the syntax mentioned in the paper
   - Lambda Expression: `\x=>x+1`
   - Function Call: `(\x=>x) 3`
   - List Construction: `1::2::[]` (or, `[1,2]`)
   - Tuple: `(1,2)`
   - Let-Binding: `let a = 1 in a`
   - Letrec-Binding: `letrec f = ... in ...`
   - If Conditional: `If x==y then ... else ...`
   - Case Expression: `case x==y of true=>...|false=>...|...`
   - String Template: `{#template#}`
   - Template: `<templatePart1><templatePart2>...<templatePartN>`
   - Template Part
     - tplStr: `Hello, world!`
     - tplExpr: `{{ <expr> }}`
     - tplSet: `{% set x = 1 %}`
     - tplIf: `{% if x == 1 then %} <thenTemplate> {% else %} <elseTemplate> {% endif %}`
     - tplForeach `{% for item in items %} <foreachBodyTemplate> {% endfor %}`
2. Click "Eval" Button to see the output

### Direct Manipulate on Output/Context
1. Modify the text in the **Output** window
2. Click **Uneval and Preview** Button
    - To see the preview of the code changes
    - Click **Revert Code** to revert to the original code
3. Click **Uneval and Update** Button
    - Make sure the updated code and cannot retract

## Code Structure
- **ace** the source code of [ace editor](https://ace.c9.io/)
- **elm.json** configuration file
- **examples** some example source programs
- **index.html** tool entrance
- **src** main code
  - *Native* interact with Javascript
- **style** css of web page

## Usage
- git clone
- click index.html

## Develop
- elm make src/Main.elm --output=./build/elm.js

## TODO
- dict support
- positive int be recognized before neg
- eq string forward and backward
- foreachTpl index support
- node different -> wrap a tag 
- html render page in right
- friendly error msg
- slow when large text encounter
- dict field access without mSpaces
- refactor all code using visitor pattern