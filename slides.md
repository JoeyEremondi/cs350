- [Introduction](#introduction)
- [Expressions and Immutable Data](#functional-programming-1-recursion-and-immutable-data)




<a id="introduction"></a>

# Introduction



## Course Details

1.  Course Objectives

    To learn:
    
    -   Functional programming
        -   Recursion
        -   Immutable data
        -   Programming by cases
        -   Higher-order functions
    -   How to write your own programming language
        -   Parsing/Abstract Syntax
        -   Desugaring
        -   Evaluation
    
    1.  <span class="underline">To change how you _think_ about programming</span>

2.  Course Notes / Textbook

    -   Course Notes: _Functional Languages, Interpreters and Types_
        -   Work in progress, so let me know if you discover any errors/issues
        -   Covers the first half of the course (Functional Programming)
    
    -   Textbook: _Programming Languages: Application and Interpretation_, 3rd edition, by Shriram Krishnamurthi
        -   aka PLAI
        -   Freely available online, pdf in UR Courses
        -   Covers the second half of the course (Implementing Programming Languages)
            -   We&rsquo;ll follow loosely

3.  Course Communication

    -   Everything on URCourses
        -   Announcements
        -   Assignments and Handin
        -   Textbook, Slides, Videos
        -   Discussion Forum
        -   Messages to Instructor
    
    -   Do NOT ask programming/conceptual questions by email/private message
        -   Use the discussion forum
        -   If you&rsquo;re wondering, others are too
        -   EXCEPTION: when you can&rsquo;t ask your question without revealing
            your solution to the assignment

4.  Office Hours

    -   Location: RIC 317
        -   Take the elevator, go across the bridge and through the door
        -   Or take the stairs by the &ldquo;Church Turing Thesis&rdquo; wall and vending machines
    -   Tues at 10:00
    -   Thurs at 11:30
    -   Check pinned announcement on URCourses

5.  Grading Scheme

    -   35% assignments
    -   15% midterm
        -   In-class
        -   Wednesday, Feb 12
    -   40% final
    -   10% Participation

6.  Participation

    -   You get marks just for showing up
    -   Each lecture is 0.5%, to a maximum of 10%
    -   Each forum post is 0.5%, maximum of 5%
        -   e.g. You need to attend half the lectures for full marks
    -   Assignment 0 is 1% (counts as 2 lectures)

7.  Assignment 0

    -   Due Monday, On URCourses
    -   Get your Dr. Racket Set up
        -   Download from <https://download.racket-lang.org/>
        -   Install the plt file
        -   Full instructions on URCourses
    -   Get your handin account setup
        -   Username and password under &ldquo;Feedback&rdquo; for &ldquo;Login Details&rdquo;
        -   Submit the assignment using ![img](./img/handin.png)

8.  Assignments 1-6

    -   Six bi-weekly assignments
    -   Due Fridays at 5pm (17:00)
        -   No extensions
        -   Lowest two grades dropped
            -   Last two are &ldquo;optional&rdquo; if you&rsquo;re happy with your marks

9.  Assignments (ctd.)

    -   Mostly programming
        -   Some conceptual questions
    -   Score based on running tests
        -   Some public (included in assignment)
        -   Some private (only known by me)
        -   Code doesn&rsquo;t run \\(->\\) no marks
    -   Some points for style/documentation/etc.
        -   Sample based marking

10. Submitting Assignments via Handin

    -   Unlimited submissions before deadline
    -   After a submission, you get:
        -   A score
        -   A list of which public tests failed, along with expected results
    -   **READ THE TEST RESULTS** to know what you need to fix

11. Syntax and Type Errors

    -   **ALL CODE CONTAINING SYNTAX OR TYPE ERRORS WILL GET A GRADE OF ZERO**
        -   The handin tool will give an error message if your file doesn&rsquo;t compile
    -   If your code has a type or syntax error, you can try submitting again
    -   It&rsquo;s better to submit code that fails some tests than to submit code with type or syntax errors

12. Resources for This Course

    -   There are few resources available for you to learn about `flit`
        -   THIS IS BY DESIGN
    -   You will learn to think about code
        -   With your brain
        -   Not with copy/paste or AI
    -   `flit` is a small language
        -   All the features/functions you need will be described in the course notes
    -   There are many languages built using Racket
        -   Copying code will probably get you a 0

13. Academic Integrity

    -   You are to complete all assignments **individually**
    -   Some (easy) questions on assignments are individualized to you, and must be completed for your assignment to be graded.
    -   All of the following are considered cheating:
        -   Using code from a local or online AI tool
        -   Copying code from a website like StackOverflow
        -   Viewing another student&rsquo;s solution
    -   ChatGPT DOES NOT UNDERSTAND FLIT
        -   Getting AI code to work is harder than just doing the assignment
    
    1.  Don&rsquo;t set yourself up for failure on the exams


## Course Content

1.  Course Objectives

    Programming language genealogy and design.
    
    Imperative, functional, and object-oriented language paradigms.
    
    Context-free grammars and syntax trees.
    
    Data types, control structures, exception handling, data abstraction, information hiding, and non-determinism.
    
    Program representation, translation, and execution.
    
    Functional programming: advantages, constructs, closures, and higher-order operations.
    
    Parallel programming.
    
    \phantom{First half of the class}

2.  Course Objectives

    Programming language genealogy and design.
    
    Imperative, **functional**, and object-oriented language paradigms.
    
    Context-free grammars and syntax trees.
    
    Data types, control structures, exception handling, data abstraction, information hiding, and non-determinism.
    
    Program representation, translation, and execution.
    
    **Functional programming: advantages, constructs, closures, and higher-order operations.**
    
    Parallel programming.
    
    -   _First half of the class: Programming in Flit_

3.  Course Objectives

    **Programming language genealogy and design.**
    
    **Imperative,** functional, and **object-oriented language paradigms.**
    
    **Context-free grammars and syntax trees.**
    
    Data types, control structures, exception handling, data abstraction, information hiding, and non-determinism.
    
    **Program representation, translation, and execution.**
    
    Functional programming: advantages, constructs, closures, and higher-order operations.
    
    Parallel programming.
    
    -   _Second half of class: implementing interpreters_

4.  Common Questions

    -   Am I going to use Racket in Industry?
        -   No

5.  Common Questions

    -   Then why are we learning it
        -   Don&rsquo;t know what you&rsquo;ll be using in industry
        -   Learn a small language that shows core concept
        -   Learn the pieces that languages are made of
            -   How to see past syntax
            -   Understand what programs _mean_
        -   Then you&rsquo;ll be able to learn whatever language you need in industry

6.  Common Questions

    -   Do you really expect me to learn a new language in just one term?
        -   Yes. **This is a programming languages class**.
        -   The language we&rsquo;re learning is small (really just functions and pattern matching)

7.  When might this course be useful?

    -   You might get hired to
        
        -   Write a JavaScript Node server using callbacks to handle requests
        -   Maintain a python library full of lambdas and list comprehension
        -   Add to Discord&rsquo;s chat system built in Elixr
        
        <!--listend-->
        
        -   Write an Android GUI using Kotlin, Jetpack Compose and function composition
        -   Write Rust at Amazon, or write an iPhone app in Swift, and need to use `Option` instead of null pointers
    -   I can&rsquo;t teach you all of Kotlin, JavaScript, Elixr, Rust, Swift, Clojure, Scala, etc. in one term
        -   But I can teach you what they&rsquo;re made of

8.  The Science of Computer Science

    -   How to think about programs in a rigorous way
        -   Programs as mathematical objects
        -   Equations with programs
    -   Problem solving with types
        -   How to make sure you handle all the cases
            -   Making invalid states unrepresentable
        -   How the shape of a problem can guide to its solution
        -   Using types to communicate properties of code
    -   This is radically different from the kind of coding you have done so far


<a id="functional-programming-1-recursion-and-immutable-data"></a>

# Expressions and Immutable Data

![img](./img/xkcd_parens.png)


## Overview

1.  Objectives

    -   To learn how to use Dr. Racket
    -   To learn how to install and use `#lang flit`


## Programming in CS 350

1.  All coding for this class uses:

    -   The Racket Programming Language
    -   The `flit` library for Racket
        -   Is it a library? Is it a language?
            -   Yes
    -   The Dr. Racket editor


## Racket

1.  What is Racket?

    -   Lisp-style language
        -   `((((((((Parentheses))))))))`
    -   Language for making languages
        -   You will find documentation for many of these languages online
        -   They are different from what we are doing. Beware!

2.  What is Dr. Racket?

    -   IDE for Racket
        -   Syntax highlighting
        -   Parentheses matching
        -   Other useful features
            -   Some I&rsquo;ve developed custom for this class
    -   Read-Eval-Print-Loop (REPL)
        -   Feedback when writing code
        -   Can  evaluate expressions while you&rsquo;re writing your code


## Flit

1.  Objectives

    -   Learn the syntax of core flit features
        -   Numbers, booleans, `if`, functions, `let*`
    -   Learn the semantics of these features
        -   First view of equations for understanding programs
    -   Understand how functional `if` differs from imperative `if`

2.  What is Flit?

    -   &ldquo;Functional Languages, Interpreters and Types&rdquo;
    -   Language defined in Racket
        -   Functions you can call
        -   Adds syntax to Racket
            -   Declaring and pattern matching on data types
            -   Type annotations for functions
        -   Minimal
            -   Has what you need to write programming languages
            -   Not much else
            -   You can do a lot with very little

3.  Flit features:

    -   Purely functional
        -   Variables never change values
        -   Can&rsquo;t do `int x = 3; x = 4;`
    -   Strongly typed
        -   Every expression has a type
        -   No implicit coercion between types
    -   Type inference
        -   Don&rsquo;t have to write down the types
        -   Writing types down is still a good idea
            -   Compiler checks if the actual type matches what you wrote
            -   Serves as documentation
    -   Data Types with Variants

4.  Syntax in Flit

    -   Expression: something that we run to compute a value
    -   Every flit expression is either:
        -   A literal (e.g. `3, "hello", #t, #f`)
        -   An identifier (variable/function name), e.g. `x, y, +, and, or`
        -   Parentheses containing some expression `(a b c d ...)`
    -   Parentheses mark the start and end of complex expressions
    -   If the first thing in the parentheses is a keyword, then that keyword determines
        the meaning of the expression
        -   e.g. `if, lambda, type-case`
    -   Otherwise, interpreted as a function call
        -   First thing in the parentheses is the function to be called
            -   NOT always a named function!

5.  Infix vs. Prefix

    -   All operations in Flit are written using &ldquo;prefix notation&rdquo;
        -   Operation, then arguments
        -   `(+ 2 3)`, not `(2 + 3)`
    -   Languages like C++ and Python use &ldquo;infix notation&rdquo; for math

6.  Function calls

    -   In C++ you write `f(x,y,z)`
    -   In Flit, you write `(f x y z)`
    -   99% of what you do in Flit is calling functions

7.  Why Parentheses?

    -   They&rsquo;re different from what you&rsquo;re used to
        -   I want to to learn to see _past_ syntax
    -   You can tell _exactly_ where an expression starts and ends
    -   You can tell exactly the order of operations
        -   No need for BEDMAS/PEMDAS or precedence rules
    -   Most importantly: **Programs are Trees**
        -   Hierarchical structure: expressions contain expressions contain expressions&#x2026;
        -   Parentheses make this very explicit

8.  Square vs. Round vs. Curly

    -   Racket treats all parentheses as the same
    
    <!--listend-->
    
    ```racket
    (+ 1 2)
    [+ 1 2]
    {+ 1 2}
    ```
    
    ```racket
    3
    3
    3
    ```
    
    -   We&rsquo;ll use square brackets in some places for clarity
        -   Save Curly brackets for the second half of the class

9.  Parentheses in the real world

    -   e.g. Clojure, uses parentheses
    -   WebAssembly has an S-Expression based syntax for programmers
    -   JSON is basically just funny-looking parentheses

10. Numbers

    -   Most operations prefix version of what you&rsquo;ve seen in C++/python
    
    <!--listend-->
    
    ```racket
    (+ 2 7)
    (- 10 0.5)
    (* 1/3 2/3)
    (/ 1 1000000000000.0)
    (max 10 20)
    (modulo 10 3)
    ```
    
    ```racket
    9
    9.5
    2/9
    1e-12
    20
    1
    ```

11. Booleans

    -   Literals written #t and #f
    -   Prefix versions of most boolean operations
    
    <!--listend-->
    
    ```racket
    (= (+ 2 3) 5)
    (> (/ 0 1) 1)
    (zero? (- (+ 1 2) (+ 3 0)))
    (and (< 1 2) (> 1 0))
    (or (zero? 1) (even? 3))
    ```
    
    ```racket
    #t
    #f
    #t
    #t
    #f
    ```

12. Imperative Languages

    -   C, C++, and Python are _imperative languages_
        -   Each statement tells what to _do_
    -   Mutable state and side-effects
        -   Program state: current variable values
        -   Statements can change the state
            -   _Mutate_ value of variable

13. Functional Language

    -   Flit is _purely functional_
    -   Program is made up of _expressions_
        -   These evaluate to a _value_
    -   Variables are _immutable_
        -   Like variables in math
        -   Can make new ones, but value never changes
    -   Use functions and recursion to do all computation

14. Example: Imperative Conditionals

    -   Conditionals in C++/Python tell what to _do_
    
    <!--listend-->
    
    ```C++
    if (someCondition)
      doThis
    else
     doThat
    ```
    
    -   The if-statement doesn&rsquo;t have a value, but it can mutate (change) the state

15. Conditional Expressions

    ```racket
    (if (< 2 3) "hello" "goodbye")
    ;; another example
    (+ 3
      (if (= 2 (+ 1 1))
          3
          40))
    ```
    
    ```racket
    "hello"
    6
    ```
    
    -   Conditionals in Flit are **expressions**, not statements
    -   Value is either the &ldquo;then&rdquo; branch or the &ldquo;else&rdquo; branch
        -   Always have 2 branches
    -   Boolean changes what the expression **is**, not what it does
        -   e.g. Above, the 2nd `if` evaluates to `3` because the condition evaluates to true

16. Equational Reasoning

    -   When all variables are immutable, we can treat programs as mathematical objects
    -   Write equations about programs as if they were numbers/booleans/etc.
    -   _Referential transparency_
        -   If you give a functional program the same inputs, you will always get the same outputs
    -   Example:
        
        -   Math plus is different than Flit plus, needs numbers, not expressions

17. Imperative Breaks Equational Reasoning

    -   In C++, the equals sign is a LIE
        ```C++
          int x;
          x = 3;
          x = 4;
        ```
        
        -   Have `x = 3` and `x = 4`, so surely `3 = 4`, right?

18. Equations for IF

    -   We can describe the behaviour of if-expressions with the following two equations:
    
    For any expressions `EXPR1` and `EXPR2`
    
    -   `(if #t EXPR1 EXPR2) = EXPR1`
    -   `(if #f EXPR1 EXPR2) = EXPR2`
    
    -   The equals sign is not a lie here
        -   The two sides of the equation have the same run-time result
        -   No matter what we choose for `EXPR1` or `EXPR2`

19. Functions

    -   Calling a function replaces variable with concrete argument
    
    <!--listend-->
    
    ```racket
    ;; Define a function
    (define (addOne [x : Number]) : Number
      (+ x 1))
    
    ;; Call the function we defined
    (addOne 10)
    ```
    
    ```racket
    11
    ```

20. Calling Functions

    -   Syntax for calling a function is
        -   Open bracket
        -   Function name
        -   Arguments, each separated by whitespace
        -   Close bracket
    
    <!--listend-->
    
    ```racket
    (f arg1 arg2 arg3 ... argN)
    ```
    
    -   Each argument might be a variable or a literl, or a complex expression in brackets

21. Defining Multi-Argument Functions

    ```racket
    (define (isRemainder [x : Number]
                         [y : Number]
                         [remainder : Number])
            : Boolean
      (= remainder (modulo x y)))
    (isRemainder 10 3 1)
    (isRemainder 10 4 1)
    ```
    
    ```racket
    #t
    #f
    ```

22. General Form Defining Functions

    -   General form:
    
    <!--listend-->
    
    ```racket
    (define (functionName
             [argName : argType]
             ...
             [argNameN : argTypeN]) : returnType
      functionBody)
    ```
    
    -   Brackets around the function name, followed by arguments
        -   Each argument given as `[name : Type]`
        -   We&rsquo;ll use `:` to mean &ldquo;has type&rdquo; a lot in this class
    -   After name/arguments closing paren, have `: returnType`
    -   Then, the body of the function
        -   e.g. the code to run
    -   Later in the course we&rsquo;ll see another way of defining functions

23. Type Inference

    -   We can leave types off function arguments and return type
    -   Compiler is _usually_ smart enough to figure out the types
        ```racket
          ;; example
          (define (addone x)
            (+ x 1))
        
          ;; in general
          (define (functionName argName1 argName2 ... argNameN)
            functionBody)
        ```

24. Semantics of Function Calls

    -   Functions are about _substitution_:
    
    If a function `f` is defined as:
    
    -   `(define (f x) body)`
    
    then for any expression `EXPR`, we have:
    
    -   `(f EXPR) = [x => EXPR]body`
    
    where `[x => EXPR]body` is `body`, with all non-shadowed occurrences of the variable `x` replaced by `EXPR`

25. Intermediate definitions

    -   Can still define variables
        -   Once they&rsquo;re given a value, never changes
        -   Allows re-use
            -   Only evaluated once, can use multiple times
    
    <!--listend-->
    
    ```racket
    (define (squaredSum [x : Number]
                        [y : Number]) : Number
      (let ([xy (+ x y)])
        (* xy xy)))
    (squaredSum 1 2)
    ```
    
    ```racket
    9
    ```

26. Understanding Definitions

    1.  In C++, you might write
    
        ```C++
                while (someCond){
                        int x = f(3);
                        doSomethingWith(x);
                        doSomethingElseWith(x);
                }
        ```
        
        -   `let` is similar
            -   Scope is clear from the brackets
            -   Functional, so the `doSomething` never changes the value of `x`
            -   Could have just written `f(3)` instead of `x` in both places
                -   Same, as long as there&rsquo;s no side effects

27. General Syntax

    -   In general:
    
    <!--listend-->
    
    ```racket
    (let* ([varName1 EXPR1]
           [varName2 EXPR2]
           ...
           [varNameN EXPRN])
      bodyEXPR)
    ```
    
    -   Keyword `let*`, a bracketed list of defined variables, then the body
    -   Defined vars `[variableName variableValue]`
        -   Round vs. Square not important
        -   Defining just one variable has double brackets
            -   `(let* ([x 3]]) (+ x 1))`
            -   Kind of awkward, but just how it works

28. Semantics of Let

    For any expression `EXPR`, we have:
    
    -   `(let ([x EXPR]) body) = [x => EXPR]body`
    
    where `[x => EXPR]body` is `body` with all non-shadowed occurrences of `x` replaced by `EXPR`
    
    -   Multiple variables work similarly, but also do the substitution in the expressions of later variables values
        -   We&rsquo;ll see this formally later

29. Alternate versions of Let

    -   There&rsquo;s also `let`
        -   Like `let*`, but when multiple variables, can&rsquo;t refer to each other
        -   Recursive version `letrec`
            -   Can define multiple variables that all refer to each other and themselves
    -   For now you only need `let*`
        -   Very useful for gradually building up a complex solution from previous smaller bits


## Types in Flit

1.  Why write down types?

    -   With type inference, we don&rsquo;t _need_ to write down types
    -   Still a good idea to write them down
    -   Writing a type is like running an infinite number of tests
        -   Compiler makes sure that every possible run of your program produces the type written down
    -   Sanity check
        -   Make sure types in your head match the types in the code
    -   Documentation
        -   Types make it very clear what arguments a function expects

2.  Checking Types

    -   Can annotate any expression to confirm its type
    
    <!--listend-->
    
    ```racket
    (has-type 3 : Number)
    (has-type #t : Boolean)
    (has-type "hello" : String)
    ```
    
    ```racket
    3
    #t
    "hello"
    ```

3.  Thinking About Types

    -   With any type, there are two things we care about
        -   How to _directly_ create a value of that type
        -   How to _directly_ use a value of that type
            -   Can also create/use any value by giving it to a function expecting/returning  that type
            -   But eventually, you need some primitive operations

4.  Numbers

    -   To create: literals `3`, `22`, `0.00001`
    -   To use: built in arithmetic `+, -,  *, /`  etc. and comparisons like `zero?`

5.  Strings

    -   To use: mostly built-in operations
        -   I&rsquo;ll highlight relevant functions as we see them

6.  Booleans

    -   To create: literals `#t`, `#f`
    -   To use: `if someBool e1 e2`
    -   E.g. the purpose of booleans is to be things we give to `if` expressions

7.  Types for If

    -   `if` is not a function
        -   We&rsquo;ll learn exactly why later
    -   Need a special rule to determine its type
        -   Can have `any` type, depending on the type you give it
    -   Just like we have equations, we can make formal rules for typing

8.  Typing Rule for If

    For any expressions `EXPR1`, `EXPR2` and `EXPR3`,
    
    -   if `EXPR1 : Boolean`, and
    -   there is some type `T` such that `EXPR2 : T` and `EXPR3 : T`,
    
    then:
    
    -   `(if EXPR1 EXPR2 EXPR3) : T`
    
    -   Condition must always be boolean
    -   Don&rsquo;t care what the type of the branches is, but the **need to be the same**
        -   Type-checker will give error if branches have different types
    -   Result of the `if` has the same type as the branches

9.  Typing In General

    For any expressions `EXPR1`, `EXPR2`
    
    -   if `EXPR1 : Number`
    -   and `EXPR2 : Number`
    -   then `(+ EXPR1 EXPR2) : Number`
    
    -   E.g. the type checker will make sure that both inputs to `+` are numbers,
        and that you use the result in a context expecting a number
    -   Can make rules for all expressions this way

10. Function Calls

    For any expressions `EXPR_f` and `EXPR_arg`
    
    -   if `EXPR_f : T1 -> T2`
    -   and `EXPR_arg : T1`
    -   then `(EXPR_f EXPR_arg) : T2`
    
    -   When asking &ldquo;how can I make a value of this type&rdquo;:
        -   Can always try &ldquo;call a function that returns that type&rdquo;

11. Function Definitions

    For a definition `(define (f x1 ... xN) body)`
    
    -   if `body : T_ret`
        -   _under the assumption that_ `x1 : T_1` &#x2026; `xN : T_N`
    -   then `f : (T_1 ... T_N -> T_ret)`
    
    -   e.g. a function type with arguments matching the `x`&rsquo;s types, and return type matching the body
    
    -   Function variable types are _assumptions_, not obligations
        -   To _define_ function, don&rsquo;t provide things of the argument types
        
        -   Instead, can assume you already have things of those types
            -   e.g. the parameters

12. Programming Via Holes

    -   Figure out the type of the thing you&rsquo;re trying to write
        -   Often, I give this to you
    -   Write a dummy function with that type, with `TODO` as the body
    -   Write some tests that capture what you&rsquo;re expecting the function to do
    -   Repeat until all tests pass and there&rsquo;s no more holes:
        -   Fill in a hole with something of the right type
            -   Which might contain more holes
            -   Could be function call, `if`, in-scope variable, etc.

13. Functions for Filling Holes

    -   Can always fill a hole of type `T` by applying a function that returns type `T`
        -   Give a hole as each argument for that function
    -   Now, you&rsquo;ve replaced one hole with 0 or more holes
        -   Yay?
        -   Hopefully these holes are simpler
        -   Can try to fill them with literals, variables, or other function calls
    -   We&rsquo;ll see more of this with recursion and datatypes

14. Examples

    -   Maximum function
        -   Writing it
        -   Reasoning about it with equations

