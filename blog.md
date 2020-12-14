
The other day I was thinking about the idea of a super simple programming language that uses whitespace as the delimiter between expressions. This isn't a new idea, Lisp (and its derivitives) are also super simple from a syntax perspective.

Maybe I should learn Lisp and see what makes it tick.

First I need a development environment. I like to create dev containers using VSCode so that I don't clutter my laptop with a bunch of packages. Unfortunatly, there isnt a lisp dev container. So I'll have to make one.

I start with creating a new dev container and choosing "Ubuntu" for the image. This will give me a base install. I can then install Steel Bank Common Lisp http://www.sbcl.org/platform-table.html into it.

Open up `.devcontainer/Dockerfile` and there is a handy comment that tells you how to install packages. I added this to the bottom of the file:
```Dockerfile
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends sbcl
```

"Rebuild the container" and we now have the `sbcl` command available in our terminal.

Whenever I am learning a new language, my personal style is to compare it to other languages I know and programming in general. Most programming languages have the similar basic types of expressions and statements. For example:
- Variable assignment
- Function/method declaration
- Function/method invocation
- Conditionals
- Loops
- Class/struct declaration and construction

There are other parts of languages, but if I learn the syntax and behavior of these, then I can probably muddle through enough to be dangerous. I'm going to start with a simple "Hello World" so I know how to use a development environment.

I started by googling "learn lisp" and clicking the first search results I found lisp files end in `.lisp` (suprise) so I crated `hello.lisp` as an empty file and ran `sbcl src/hello.lisp`. I expected some sort of parse error, but I got a prompt instead. Guessing failed, so I tried `sbcl -h` to get help and got the same thing. Alright, how about `sbcl --help`? Bingo, got some help text (I could have googled this I suppose). Looking through the options it lists "--script [<filename>]" as an option. So let's give `sbcl --script src/hello.lisp` a try. Got no errors, or output and a "0" status code. So apparently an empty file is a valid Lisp program.

Ok, next how to print things to the screen. Back to google with "lisp print" and found an example that looks like
```Lisp
(print "Hello World")
```
Works exactly as expected. Let's look closer at the syntax. It appears that function invocation is constructed as `"(" <function name> <argument> ")"` If I add a second argument see if print takes it. Nope, very angry stack trace. Another thing I notice about the syntax is that unlike C#, Java, and Javascript where printing to the console is a method on a object, here it is a global function similar to Python.

Ok, how about function declaration? I google "lisp declare function" and see what I get. Interestingly, the site [Tutorials Point](https://www.tutorialspoint.com/lisp/lisp_functions.htm) seems to be a common high search result and suprisingly useful given its name. Looks like we can declare a function like this:
```Lisp
(defun sayHello (name)
    "Says hello"
    (print "Hello " + name)
)
(sayHello "You")
```
We "declare" a function by calling the `defun` function. Interesting that function declaration doesn't have special syntax. Then it takes a list of parameters in parentheses. (It seems like that would try to invoke the "name" function?) followed by optional documentation string, then the function body.

My function is broke. I assumed that `+` would concatinate strings (it does in a lot of languages) but blew up. Googling it looks like I need to do this:
```Lisp
(defun sayHello (name)
    "Says hello"
    (print (concatenate 'string "Hello " name))
)
(sayHello "You")
```
This seems awkard. A lot of other languages support [string interpolation](https://en.wikipedia.org/wiki/String_interpolation). So maybe Lisp does to? Doesn't appear to, but has a [format function](https://en.wikipedia.org/wiki/Format_(Common_Lisp))

Variable assignment is interesting. It uses the `let` function and declares one or more variables with values. Unlike all other languages I'm familiar with, `let` does not add a variable to the current scope, but rather creates a new scope and has a body that is invoked with that scope. For example:
```Lisp
(defun sayHello (name)
    "Says hello"
    (let ((message (concatenate 'string "Hello " name)))
        (print message)
    )
)
(sayHello "You")
```

Let's make a loop: There are [several different looping constructs](https://www.tutorialspoint.com/lisp/lisp_loops.htm). The `loop` function takes an expression and executes it until a `return` is hit. Another difference, usually `return` breaks out of a function or method. In Lisp it also breaks out of a loop. I'm going to modify my toy program to have a function that takes a number and prints something that many times:
```Lisp
(defun sayTimes (times)
    (loop
        (write-line (format nil "Saying ~D" times))
        (setq times (- times 1))
        (when (<= times 0) (return nil))
    )
)
(sayTimes 2)
```
A few notes about this. I'm using `write-line` instead of print. I don't know the details, but print was giving me lines surrounded by double quotes and write-line doesn't. I'm also using the `format` function instead of `concatenate` because passing a number to concatenate threw an error. Notice the first parameter to `format`. I was looking at various format examples and finally found [this explination](http://www.gigamonkeys.com/book/a-few-format-recipes.html#the-format-function). Basicly, format can take a a boolean true (`t`) and it will output the result to standard out and return null, or it can take `nil` and will return the result instead. So I can simplify the above by passing `t` and ommitting the call to `print`. At first I was passing `t` and was getting my expected output followed by "NIL" and was very confused.

I also had to figure out conditionals and variable reassignment. It seems the `when` function takes a predicate followed by a body (similiar to an "if" statement). I can also re-asign a variable by calling `setq`. the `(- times 1)` code is subtraction. It is not [Infix](https://en.wikipedia.org/wiki/Infix_notation) like most languages but rather a function call.)

I assume Lisp supports some way of contstructing objects that contain several named values and probably functions as well. I found [This example](https://www.tutorialspoint.com/lisp/lisp_clos.htm)
```Lisp
(defclass Box ()
    (
        (length :accessor length)
        (breadth :accessor breadth)
        (height :accessor height)
    )
)
```
One of the first things I noticed was the `:accessor` part. What does the colon syntax do. Colons aren't the best search term, but I found these are [Keyword Parameters](https://www.tutorialspoint.com/lisp/lisp_keyword_parameters.htm)(a.k.a "Named Arguments")

Classes in Lisp are definatly different than other languages. Read through [This example](https://www.tutorialspoint.com/lisp/lisp_clos.htm) for a good explination.

All of the above gives me an idea of how to use the language, but doesn't tell me how the language works. The basic function invocation syntax (`(funcName arg1 arg2)`) seems have multiple uses. In some cases it is simple function invocation, in others the args are not evaluated but are used as names (like when defining function parameters, or using `setq`). I want to know how that works.

Everything in Lisp is made up of [S-expressions](https://en.wikipedia.org/wiki/S-expression). Interesting, `setq` is actually a shortcut for calling [set](https://www.gnu.org/software/emacs/manual/html_node/eintr/Using-set.html#Using-set) with a quoted first argument. While reading the definition of `setq` I came upon the term "Special Form". Looking into that I found [this good explination](https://www.gnu.org/software/emacs/manual/html_node/elisp/Special-Forms.html) of speical forms in Lisp. The way I understand it, there is a set of function names and if the first element of a list is one of those functions then the interpreter does not execute all the arguments but rather passes some of them verbetum. Let's test it out:
```Lisp
(defun with0 (sym body)
    (set sym 0)
    (eval body)
)
(with0 'x '(print x))
```
In the above I'm creating a function (`with0`) that takes a sym and body. Then I'm setting a variable with sym as the name to 0, then evaluating the body. This is a similar setup in Javascript:
```Javascript
function with0(sym body) {
    global[sym] = 0;
    body();
}
with0('x', () => console.log(x));
```

What I've learned is that we can use the single quote to supress execution of a nested s-expression and instead pass it verbetum. Then we can use `eval` later if we want to execute it.