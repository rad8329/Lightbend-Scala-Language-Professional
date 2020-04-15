## Lightbend Scala Language - Professional and Lightbend Scala Language - Expert

---

### Introduction

This document describes how to setup:

- The development environment
- The case study project

We recommend using the following tools:

- Eclipse or IntelliJ, both with Scala plugin
- sbt build tool

---

## Prerequisites

---

### Required Knowledge and Software

Lightbend Scala Language - Professional is best suited for individuals with some experience with Java, or another object-oriented language. No previous Scala knowledge is required.

Lightbend Scala Language - Expert is best suited for individuals that have knowledge of Scala as covered in Lightbend Scala Language - Professional.

For either course we need access to the internet and a computer with the following software installed:

- Unix compatible shell
- JVM 1.8 or higher
- Sbt 0.13.13 or higher

---

### Unix Compatible Shell

If you are running OSX, then you are on a **nix** system already. Otherwise install a Unix compatible shell like [Cygwin](https://www.cygwin.com/).

---

### JVM 1.8 or Higher

If you are running OSX and a [Homebrew Cask](https://github.com/caskroom/homebrew-cask) user, from a terminal run:

```bash
$ brew cask install java
```

Otherwise follow the [setup instructions](http://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html) to download and install. Once the installation is complete, very the installation by running the following command in a terminal session:

```bash
$ java -version
java version "1.8.0_45"
Java(TM) SE Runtime Environment (build 1.8.0_45-b14)
Java HotSpot(TM) 64-Bit Server VM (build 25.45-b02, mixed mode)
```

---

### Sbt 0.13.13 or Higher

If you are running OSX and a [Homebrew](http://brew.sh/) user, from a terminal run:

```bash
$ brew install sbt
```

Otherwise follow the [setup instruction](http://www.scala-sbt.org/0.13/docs/index.html) to download and install. Once the installation is complete, verify the installation by running the following command in a terminal session:

```bash
$ sbt sbtVersion
...
[info] 0.13.13
```

---

## Simple Build Tool

---

### Make Yourself Familiar with Sbt

- Read the first chapters of the [Getting Started Guide](http://www.scala-sbt.org/release/tutorial/index.html)
- Starting `sbt` takes you to a **interactive session**
- Take a look at `build.sbt` and the other `.sbt` files for this course
- Change directory to the `LSL-P-lightbend-scala-language-professional-exercises-<version>` directory and start `sbt` as follows:

```scala
$ sbt
man [e] > scalatrain > initial-state >
```

---

### man

The `man` command, short for manual, displays the setup instructions (what you are reading now) for the courseware. To view the instructions for the current exercise, use the `e` option. If you are using an IDE, you can also open up the setup instructions (`README.md`) file or the current exercises instructions (`src/test/resources/README.md`) file in your workspace.

```scala
// display the setup instructions
man [e] > scalatrain > initial-state > man

// display the instructions for the current exercise
man [e] > scalatrain > initial-state > man e
```

---

### course navigation and testing

Navigation through the courseware is possibile with a few `sbt` commands. Also, tests are provided to confirm our solution is accurate. It is important to note that the tests make some assumptions about the code, in particular, naming and scope; please adjust your source accordingly. Following are the available `navigation` commands:

```scala
// show the current exercise
man [e] > scalatrain > initial-state > show
[INFO] Currently at exercise_000_initial_state

// move to the next exercise
man [e] > scalatrain > initial-state > nextExercise
[INFO] Moved to exercise_001_define_a_class

// move to the previous exercise
man [e] > scalatrain > define-a-class > prevExercise
[INFO] Moved to exercise_000_initial_state

// save the current state of an exercise for later retrieval and study
man [e] > scalatrain > initial-state > saveState
[INFO] State for exercise exercise_000_initial_state saved successfully

// List previously saved states
man [e] > scalatrain > initial-state > savedStates
[INFO] Saved exercise states are available for the following exercise(s):
        exercise_000_initial_state

// Restore a previously saved exercise state
man [e] > scalatrain > initial-state > restoreState exercise_000_initial_state
[INFO] Exercise exercise_000_initial_state restored
```

---

### clean

To clean your current exercise, use the `clean` command from your `sbt` session. Clean deletes all generated files in the `target` directory.

```scala
man [e] > scalatrain > initial-state > clean
```

---

### compile

To compile your current exercise, use the `compile` command from your `sbt` session. This command compiles the source in the `src/main/scala` directory.

```scala
man [e] > scalatrain > initial-state > compile
```

---

### reload

To reload `sbt`, use the `reload` command from your `sbt` session. This command reloads the build definitions, `build.sbt`, `project/.scala` and `project/.sbt` files. Reloading is a **requirement** if you change the build definition files.

```scala
man [e] > scalatrain > initial-state > reload
```

---

### test

To test your current exercise, use the `test` command from your `sbt` session. Test compiles and runs all tests for the current exercise. Automated tests are your safeguard and validate whether or not you have completed the exercise successfully and are ready to move on.

```scala
man [e] > scalatrain > initial-state > test
```

---

## Eclipse

---

### Install the Scala IDE for Eclipse

Follow these instructions if you want to use Eclipse:

- **Attention**: Make sure you pick the right packages for Scala 2.11!
- You can download the prepackaged and preconfigured [Scala IDE](http://scala-ide.org/download/sdk.html) for your platform
- You can also use your already installed Eclipse:
    - Install the latest Scala IDE plugin
    - Use the following [update site](http://scala-ide.org/download/current.html) for Eclipse 4.5 and 4.6 (Mars and Neon)
- In Eclipse import the `scalatrain` project with `Import... > Existing Projects into Workspace`

---

### Install the ScalaTest Plugin for Eclipse

Follow these instructions if you want to use Eclipse:

- In Eclipse select `Help > Install New Software`
- Select the `Scala IDE` update site
- Expand the entry `Scala IDE plugins`
- Select entry `ScalaTest for Scala IDE`
- Press `Next` > and follow the installation process
- Restart Eclipse if prompted

---

## IntelliJ

---

### Install IntelliJ IDEA with Scala Plugin

Follow these instructions if you want to use IntelliJ IDEA:

- Download and install the latest version of [IntelliJ IDEA Community Edition](https://www.jetbrains.com/idea/download/)
- Start IntelliJ IDEA and install the Scala plugin via the plugin configuration
- Import the scalatrain project via `Import Project`

---

## Case Study

---

### Scalatrain

Welcome to the Scalatrain project where we work through a series of exercises organized by topic as laid out in the Lightbend Scala Language - Professional  and Lightbend Scala Language - Expert slide decks and experience:

Get detailed instructions per exercise by running the `man e` command

---

### Exercise Outline

The exercise outline can be obtained by running the `listExercises` command from the sbt prompt.

```scala
man [e] > scalatrain > initial-state > listExercises
0. Exercise 0 > Initial State
1. Exercise 1 > Define A Class
.
.
.
```
