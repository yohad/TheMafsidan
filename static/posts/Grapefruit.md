# Grapefruit

## Intro
The FRP library `grapefruit` has caught my eye. Unfurtunatly, as dictated by 
Haskell tradition, there is no actual tutorial for it. In this post I'm 
describing my process at understanding how to use it. Before erading this, 
you should make sure you are familiar with arrows and recursive definitions.

## What we have
In Hackage I found a package called `grapefruit-examples` which contains a few 
example projects using grapefruit. I can see one of them is called `Simple.hs`. 
That's should be a good place to start.

In addition, we are given the instructions to play the examples:
```
import Graphics.UI.Grapefruit.Circuit
import Graphics.UI.Grapefruit.GTK
import Examples.Grapefruit.Simple
run GTK mainCircuit ()
```
From `run` type signature we can see it gets two parameters, a `UICircuit` and 
some `i`. I'm guessing `UICircuit` has all the relevant GUI bindings and `i` is 
the GUI description.

## Simple.mainCircuit
### Variables, large and small
Let us explore this mysterios `i`. There are a few things going on with this 
`mainCircuit`. First, we are using an arrow do notation. Then, we have a `rec` 
allowing recursive definitions.

Let's go then to the variables in use. `title` is a straightforward, wrapping a 
string inside the arrow. The second variable, `text`, is a bit more tricky. The 
`scan` function accumulate data using descrete signals. The acumelated data at 
the start of the program is `"*"` and every step we evaluate 
`(const . ('*' :))` to add another `*` to `text`.

I think `push` here is using `rec` to later describe the descrete signal that 
would create a new step for `scan`.

### Horrible, Evil, Black Magic

We are left with one more obscure line, filled with weird new symbols. Fear 
not, all is not lost as we can spot a familiar pattern `X :& _ := _`. This 
pattern repeats itself with `With` in between occurrences. `With` is a 
constructor for a recursive data type. The first parameter is the base and the 
second the inner type. From `Data.Record.Signal` we learn that the pattern 
above is a way to create `Records`, some data type that I think represent the 
GUI layout. 

Let's go a bit back and try to understand how everything goes together. 
We first create a record to hold `title` and `text` and give it to a 
widget combination. `window` has a required field `Title` and `pushButton` has 
the required field `Text`. Now we have the widgets built and ready to go, but we 
need to get the signals from them so we can use them. This is what `<-` is for. 
The program pattern match on the final record (`Closure` and `Push`) to assign 
the signals to `closure` and `push`. `push` is given, recursivly, to `text` and 
`closure` is returned so the user can exit the program.

## More Than a Button
Unfurtunatly, most programs can't do with only one button. One way we can try 
to add more buttons (or any other widgets) is using `With` to add more records. 
This does'nt work, because `With` describes two widgets that are one inside the 
other, unlike our buttons. To understand how to do that we will move to another 
example, `ListView`.

### All The Roads Lead To `mainCircuit`
From what we already learned we can see that we are creating a record to hold 
the title and more stuff, putting them inside `mainWindow` and then extracting 
the `closure` function. What is `mainWindow`? It is a combination of GUI 
elements. First we have the main `window`. Inside that we have a vertical GUI 
box and inside the box some element `content` that we define later. Note that 
we are providing `box Vertical` with `X` as input, which implies that it encodes
some information to use for creating GUI.

### Buttons, as far as the eye can see
We'll move now the `content`. As the name suggest this function creates the 
program's content. We'll look only on one widget, but it should be enough for 
you to understand the rest of them. This widget will be `display`, the widget 
in charge of displaying the fruits' list and their tastiness. 

First, we create the variable to hold the list of fruits, `ISignal.construct`. 
We define `fruits` as an incremental signal, a signal that starts with a value 
and given update points. When an update occures, the value updates. We start 
with a sequence containing only `Grapefruit` and the updates are given by the 
union of several signals, `[insertions, deletions, shifts, updates]`.
A similar thing happens for `cols`. We are creating a sequence of `Column`. One 
column is for the name of the fruit and the other for a progress bar to show 
its tastiness. Most of the function here are fairly straight forward so we'll 
leave them.
Now that we have our data for the table, we can move it around 
`_ <- display -< (fruits, cols)`. In `display` we create a `Selection` of the 
fruits in our sequence, and even add a label to show us our selection.

## Last Words
I hope, after reading this post, that you got some understanding on the workings
`grapefruit`. I think that the biggest drawback of this package is the messy 
record syntax. Hopefully a good solution will come soon.

If you have a weird and interesting package you'd like me to write a similar 
post about, you can contact me via email: `yotam2206@gmail.com` (Adding comments
to the website is on my TODO list).
