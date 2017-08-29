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
  
This will take a while, then you will find the snippets project in `Mellite/site/snippets`. You can see that directory
[online](https://github.com/Sciss/Mellite/tree/master/site/snippets). This project uses [sbt](http://www.scala-sbt.org/) as a
build tool, however we'll just build and run from IntelliJ IDEA, so you don't actually have to install sbt at this point.

You can download IntelliJ IDEA [here](https://www.jetbrains.com/idea/download/). It is a development environment made by JetBrains,
and it comes in two flavours&mdash;'ultimate' and 'community'. The latter is free and open source, so we'll use that one. As of this
writing, on Linux this will download `ideaIC-2017.2.2.tar.gz`. The archive can be unpacked anywhere, and the application can be
launched with the shell script `bin/idea.sh`. If you have never run IntelliJ, it will ask you a few questions, among others it
offers you to install some plugins. Make sure that you choose to install the 'Scala' plugin. If you missed to do so, you can
install plugins from within IntelliJ by going to Settings &gt; Plugins and looking for 'Scala'.

Once IntelliJ has started, you should see its welcome screen:

![IC Welcome](.../tut_sp_idea_welcome.png)

Choose 'open' and navigate to the snippets directory inside the Mellite repository clone:

![IC Open Project](.../tut_sp_idea_open.png)

Next, you will be asked how to import the project. IntelliJ will detect that sbt is used to build the project, so the defaults should be fine:

![IC Import from SBT](.../tut_sp_idea_import.png)

If you have not set up a 'Project SDK' yet, you'll first have to press 'New...' &gt; JDK and locate your Java 8 installation (IntelliJ will
probably guess it right).

Next, IntelliJ will build the project information, download all the necessary libraries for this project, and finally give you a project browser.
If you don't see the browser, `Alt-1` will toggle it. Open the tree to `src` &gt; `main` &gt; `scala` &gt; `de.sciss.soundprocesses.tutorial`,
and locate `Snippet1`, the source code of which you can open by double-clicking on it:

![IC Project Browser](.../tut_sp_idea_project.png)

You can build (compile) the project by clicking the green downward arrow button in the top-right of the window or `Ctrl-F9` on the keyboard.
If all goes fine, the status bar at the bottom should say something like

> Compilation completed successfully in 5s 388ms (moments ago)

If there was an error, a panel would pop up showing you the errors with links to the source code.

A few observations until here:

- IntelliJ IDEA is an application that manages software projects, giving you a project browser with directories to the source code
  of your project, a main editor window for creating and editing the source code, and actions to compile and run your code
- To edit Scala code, the Scala plugin for IntelliJ IDEA must be installed. It is capable of parsing build files for sbt, the
  Scala Build Tool, probably the most widely used build system for Scala based projects. You can see how that build file looks
  by double-clicking on `build.sbt`. At this moment, we take it for granted, but we'll see later what the essentials of these
  build files are. Basically, they define which libraries and Scala version your project uses.
- By convention, source code in Scala projects resides in the directory `src/main/scala` (there could be other locations, for 
  example `src/test/scala` for unit test source code, or `src/main/java` if you mix with Java source code, etc.)
- By another convention, we use packages for code, which are nested namespaces. That avoid clashes when multiple projects
  and libraries would otherwise use the same names for classes and objects. Following Java's tradition, packages are broken
  up into 'reverse URL components', so for example all my projects begin with package `de.sciss`, typically followed by
  further components clarifying the project, in this case `soundprocesses.tutorial`. You don't have to follow this pattern,
  but it helps organise your code.

IntelliJ IDEA can do a lot more things, like help you with versioning control (git), run a step debugger, and so forth. For now, we
use it as a code editor, compiler and runner. The code editor is very powerful, giving you things such as auto-completion, offering
intentions, easy navigation and look-up of methods and symbols you are using, showing you instantly errors and warnings, etc.
If you've worked with SuperCollider IDE, you'll notice that it much more advanced; however, you will notice one big difference:
IntelliJ doesn't directly give you an interpreter where you can live-code. There are possibilities so drop into an interpreter,
but the focus is really to program a statically typed program. Mellite, in turn, has a much weaker IDE, but better access to
a live-code interpreter.

Now let's run the first snippet. Select `Snippet1` in the project browser (tree view) and select 'Run' from the context menu or
press `Ctrl-Shift-F10`:

![IC Project Browser](.../tut_sp_idea_project.png)

There are two possible outcomes now. Either it works, and you hear the famous 'Analog Bubbles' example sound of SuperCollider. In this
case you have completed the preparations. Or, most likely on macOS and Windows, 
you'll see a runtime exception, complaining that 'scsynth' was not found, like this:

    Exception in thread "Thread-2" java.io.IOException: Cannot run program "scsynth": error=2, No such file or directory
    	at java.lang.ProcessBuilder.start(ProcessBuilder.java:1048)
    	at de.sciss.synth.impl.Booting.p$lzycompute(Connection.scala:182)

@@@ note

When you start a SuperCollider server from SoundProcesses, and you have not given any explicit information where SuperCollider is
installed, it will look into the environment variable `SC_HOME` to find the directory of the server program `scsynth`. If that environment variable
is absent, SoundProcesses will simply try to start `scsynth`, looking into the default locations for executables on your system.
This is great on Linux, because here it will usually find `scsynth` automatically. But on macOS and Windows, you will have to
either specify that environment variable, or provide an explicit path in your code.

@@@

There are different ways to set environment variables, including the possibility to set them globally on your system. The ScalaCollider
read-me has a [section](https://github.com/Sciss/ScalaCollider/blob/6c1758f480f3641b853de04d51d95a3da1c97f43/README.md#specifying-sc_home)
showing how this worked on OS X 10.6, but I am not sure this information is still valid. What we'll do here,
is simply tell IntelliJ to add such an environment variable.

You may have noticed that after running the snippet for the first time, its name appeared next to the green downward arrow in the top-right
of the screen. Here you find the so-called 'run configurations', i.e. specifications which and how to run parts of your code. If you click
on the name 'Snippet1' here, a popup menu opens offering you to 'Edit Configurations...':

![IC Edit_Run_Config](.../tut_sp_idea_edit_run_config.png)

Once you enter this dialogue, you'll see a dedicated section 'Environment variables'. Clicking the '...' to its right opens a key-value
editor for environment variables associated with that particular run configuration. Add a new entry here with name `SC_HOME` and the value
being the full path of the directory where the SuperCollider server program `scsynth` (`scsynth.exe` on Windows) resides on your harddisk.
On macOS, if you have a standard SuperCollider installation, the program is hidden inside the bundle of `SuperCollider.app`, so the path might be something
like `/Applications/SuperCollider.app/Contents/Resources/`. Here I have entered to full path of my Linux installation:

![IC Environment_Variables](.../tut_sp_idea_env_var.png)

After confirming all the dialogs, make sure you stop the hanging program with `Ctrl-F2` or by pressing the red stop button, and try to run
it again using `Ctrl-F10` or the green play button. If you didn't make a typo, SuperCollider should boot now and play the familiar analog
bubbles tune for about ten seconds before stopping automatically.

## Hello World

...

## Launching the Sound System and Playing a Sound

@@snip [Snippet1.scala]($sp_tut$/Snippet1.scala) { #snippet1 }
