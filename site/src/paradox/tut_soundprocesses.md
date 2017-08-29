# Getting Started with SoundProcesses

If you have gone through the video tutorials of Mellite, you may wonder if there is more to it than the limited GUI interaction.
In fact, Mellite is, properly speaking, only the graphical user interface layer on top of a system of computer music abstractions
that is called _SoundProcesses_. When you create and manipulate objects in Mellite, you are actually creating and manipulating
objects in SoundProcesses, through the GUI provided by Mellite. In order to make use of the power of the system, it is thus
necessary to dive a bit deeper and learn about the application programming interface (API) provided by SoundProcesses. You may
still access this API through Mellite, but you may also choose to write your sound programs directly in a general code editor
or integrated development environment (IDE), such as IntelliJ IDEA.

In the following tutorial, we will write code as such an independent project, not making any references to Mellite. We will try
to assume that you have not much experience with either Scala, ScalaCollider (the sound synthesis library used by SoundProcesses)
or IntelliJ IDEA. It will thus be a quite long trip, introducing you to all of these things step by step. Instead of giving a
separate Scala tutorial (there are many resources [out on the Internet](http://scala-lang.org/documentation/learn.html)), we will
introduce Scala concepts as we encounter them.

## Preparations

We assume that you have [SuperCollider](http://supercollider.github.io/), [Java](http://www.oracle.com/technetwork/java/javase/downloads/index.html), 
and [git](https://git-scm.com/) already installed on your system, and proceed from there. You can either copy and paste the code
snippets from this page, or clone the Mellite repository. Let's do the latter. In the following, a dollar sign `$` indicates the
terminal or shell prompt.

    $ cd ~/Documents  # or any place where you would like to download the repository to
    $ git clone https://github.com/Sciss/Mellite.git
  
This will take a while, then you will find the snippets project in `Mellite/site/snippets`. You can see that directory [online]()

## Setting up IntelliJ IDEA

...

## Hello World

...

## Launching the Sound System and Playing a Sound

@@snip [Snippet1.scala]($sp_tut$/Snippet1.scala) { #snippet1 }
