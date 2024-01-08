# A brief summary of Chisel Bootcamp

## README

### Outline

This README serves as *Module 0*, an introduction and motivation to learning the material contained within. *Module 1* gives a quick introduction to Scala. It teaches you enough to start writing Chisel, but many more Scala concepts are taught along the way. Chisel is introduced in *Module 2*, starting with a hardware example and breaking it down. The rest of *Module 2* covers combinational and sequential logic, as well as software and hardware control flow. *Module 3* teaches you how to write hardware generators in Chisel that take advantage of Scala's high-level programming language features. By the end, you will be able to read and understand most of the [Chisel code base](https://github.com/freechipsproject/chisel3) and begin using [Rocket Chip](https://github.com/freechipsproject/rocket-chip). This tutorial *does not* yet cover SBT, build systems, backend flows for FPGA or ASIC processes, or analog circuits.

## 1: Introduction to Scala

### Variables and Constants - `var `and `val`

`String, Double, Int, Char, Boolean`

### Conditionals

`if...else if... else...` (can return a value)

### Methods(Functions)

key word : `def`

#### Simple Declarations

#### Overloading Functions 重载函数

```scala
// Overloaded function
def times2(x: Int): Int = 2 * x
def times2(x: String): Int = 2 * x.toInt
```

#### Recursive and Nested Functions

### List

```scala
val list1 = List(1, 2, 3)
val list2 = x :: y :: y :: Nil //(Nil is the notation of end)
val list3 = list1 ++ list2   // Appends the second list to the first list

```

### `for` Statement

```scala
for (j <- 0 until 7) { print(j + " ") } // the format of Scala2
```

### Packages and Imports

Note: The package name **should** match the directory hierarchy. This is not mandatory, but failing to abide by this guideline can produce some unusual and difficult to diagnose problems. Package names by convention are lower case and do not contain separators like underscores. This sometimes makes good descriptive names difficult. 

```scala
package mytools
class Tool1 { ... }
// then import the package
import mytools.Tool1
```

```scala
import chisel3._
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
```

The first imports all the classes and methods in the chisel3 package; the underscore here works as a wildcard. The second imports specific classes from the chisel3.iotesters package.

### Scala Is an Object Oriented Language

#### A Class Example

#### Creating an Instance of a Class

### Code Blocks

Code blocks are delimited by braces. A block can contain zero or more lines of Scala code. The last line of Scala code becomes the return value (which may be ignored) of the code block. A code block with no lines would return a special null-like object called `Unit`. 

#### Parameterized Code Blocks

```scala
// A one-line code block doesn't need to be enclosed in {}
def add1(c: Int): Int = c + 1

class RepeatString(s: String) {
  val repeatedString = s + s
}
```

**IMPORTANT**: There is another way in which code blocks may be parameterized. Here is an example.

```scala
val intList = List(1, 2, 3)
val stringList = intList.map { i =>i.toString }
```

### Named Parameters and Parameter Defaults

```scala
def myMethod(count: Int, wrap: Boolean, wrapValue: Int = 24): Unit = { ... }
```

## 2.1: Your First Chisel Module

### Setup

downloads the dependencies and import packages.

### Your First Module

```scala
// Chisel Code: Declare a new module definition
class Passthrough extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(4.W))
    val out = Output(UInt(4.W))
  })
  io.out := io.in
}

// instance written by myself imitating above
class songanrui extends Module{
    val io = IO(new Bundle{
        val in = Input(UInt(4.W))
        val out = Output(UInt(4.W))
    })
    io.out := io.in   // indicates that the right-hand signal drives the left-hand signal, that is the io.in drives the io.out
}
```

The neat thing about hardware construction languages (HCLs) is that we can use the underlying programming language as a scripting language. For example, after declaring our Chisel module, we then use Scala to call the Chisel compiler to translate Chisel `Passthrough` into Verilog `Passthrough`. This process is called ***elaboration***.

```scala
println(getVerilog(new Passthrough))
```

```scala
// Chisel Code, but pass in a parameter to set widths of ports
class PassthroughGenerator(width: Int) extends Module { 
  val io = IO(new Bundle {
    val in = Input(UInt(width.W))
    val out = Output(UInt(width.W))
  })
  io.out := io.in
}

// Let's now generate modules with different widths
println(getVerilog(new PassthroughGenerator(10)))
println(getVerilog(new PassthroughGenerator(20)))
```

You may notice that this parameterization is enabled by *Scala*, not *Chisel*; Chisel has no extra APIs for parameterization, but a designer can simply leverage Scala features to parameterize his/her designs.

Because `PassthroughGenerator` no longer describes a single Module, but instead describes a family of modules parameterized by `width`, we refer to this `Passthrough` as a ***generator***.

**Example**:

*A Module*

*A Module Generator*

### Testing Your Hardware

No hardware module or generator should be complete without a tester. Chisel has built-in test features.

```scala
// Scala Code: `test` runs the unit test. 
// test takes a user Module and has a code block that applies pokes and expects to the 
// circuit under test (c)
test(new Passthrough()) { c =>
    c.io.in.poke(0.U)     // Set our input to value 0
    c.io.out.expect(0.U)  // Assert that the output correctly has 0
    c.io.in.poke(1.U)     // Set our input to value 1
    c.io.out.expect(1.U)  // Assert that the output correctly has 1
    c.io.in.poke(2.U)     // Set our input to value 2
    c.io.out.expect(2.U)  // Assert that the output correctly has 2
}

test(new songsongsong(12)) { c =>
    c.io.in.poke(1212.U)     // set the input that be a test instance
    c.io.out.expect(1212.U)  // set the output that should be
}
println("SUCCESS!!") // Scala Code: if we get here, our tests passed!
```

**Examples**

*A Tester*

**Exercise**

*Writing Your Own Testers*

### Looking at Generated Verilog/FIRRTL

```scala
// Viewing the Verilog for debugging
println(getVerilog(new Passthrough))
// Viewing the firrtl for debugging
println(getFirrtl(new Passthrough))
```

**Appendix: A Note on "printf" Debugging**

## 2.2: Combinational Logic

### Common Operators

`Module`

**Examples**

*Scala and Chisel Operators Look the Same*

*Incompatible Operation*

*More Chisel Operators*

*Mux and Concatenation*

**Excerises**

*MAC*: (A*B)+C

*Arbiter*

*Parameterized Adder:*

```scala
val sum = io.in_a +& io.in_b
```

## 2.2: Control Flow

### Last Connect Semantics

**Example: Reassignment**

### `when`, `elsewhen`, and `otherwise`

```scala
when(someBooleanCondition) {
  // things to do when true
}.elsewhen(someOtherBooleanCondition) {
  // things to do on this condition
}.otherwise {
  // things to do if none of th boolean conditions are true
}
```

**Unlike** Scala `if`, values are not returned by the blocks associated with `when`. One cannot say

```scala
val result = when(squareIt) { x * x }.otherwise { x }
```

**Example: Chisel Conditionals**

### The `Wire` Construct

**Example: 4-Input Sort with Wires**

**Exercises: Polynomial Circuit**

_**Excercise: Finite State Machine **_ as below:

```scala
// state map
def states = Map("idle" -> 0, "coding" -> 1, "writing" -> 2, "grad" -> 3)  // have the type of Map. Woow!

// life is full of question marks
def gradLife (state: Int, coffee: Boolean, idea: Boolean, pressure: Boolean): Int = {
  var nextState = states("idle")
    if (state == states("idle") ){
        if (coffee == true) nextState = states("coding")
        else if (idea == true) nextState = states("idle")
        else if (pressure == true) nextState = states("writing")
    }else if (state == states("coding")) {
        if (coffee == true) nextState = states("coding")
        else if (idea == true || pressure == true) nextState = states("writing")
    } else if (state == states("writing")){
        if (coffee == true || idea == true) nextState = states("writing")
        else if (pressure == true) nextState = states("grad")
    } else if (state == states("grad")){
        nextState = states("idle")
    }
  nextState
}

// some sanity checks
(0 until states.size).foreach{ state => assert(gradLife(state, false, false, false) == states("idle")) }
assert(gradLife(states("writing"), true, false, true) == states("writing"))
assert(gradLife(states("idle"), true, true, true) == states("coding"))
assert(gradLife(states("idle"), false, true, true) == states("idle"))
assert(gradLife(states("grad"), false, false, false) == states("idle"))
```

```scala
// life gets hard-er
class GradLife extends Module {
  val io = IO(new Bundle {
    val state = Input(UInt(2.W))
    val coffee = Input(Bool())
    val idea = Input(Bool())
    val pressure = Input(Bool())
    val nextState = Output(UInt(2.W))
  })
    
  val idle :: coding :: writing :: grad :: Nil = Enum(4)

    io.nextState := idle             // note the difference this and above code with Scala
  when (io.state === idle) {
      when (io.coffee) { io.nextState := coding}
      .elsewhen (io.idea) {io.nextState := idle}
      .elsewhen (io.pressure) {io.nextState := writing}
  }.elsewhen (io.state === coding) {
      when (io.coffee) {io.nextState := coding}
      .elsewhen (io.idea || io.pressure) {io.nextState := writing}
  }.elsewhen (io.state === writing) {
      when (io.coffee || io.idea) {io.nextState := writing}
      .elsewhen (io.pressure) {io.nextState := grad}
  }
}


// Test
test(new GradLife) { c =>
  // verify that the hardware matches the golden model
  for (state <- 0 to 3) {
    for (coffee <- List(true, false)) {
      for (idea <- List(true, false)) {
        for (pressure <- List(true, false)) {
          c.io.state.poke(state.U)
          c.io.coffee.poke(coffee.B)
          c.io.idea.poke(idea.B)
          c.io.pressure.poke(pressure.B)
          c.io.nextState.expect(gradLife(state, coffee, idea, pressure).U)
        }
      }
    }
  }
}
println("SUCCESS!!") // Scala Code: if we get here, our tests passed!
```

## 2.4: Sequential Logic

### Registers

**Example: Using a Register**

`val register = Reg(UInt(12.W))`

`step(n)`

**Example: RegNext**

`io.out :=  RegNext(io.in + 1.U)`

```scala
class RegNextModule extends Module {
  val io = IO(new Bundle {
    val in  = Input(UInt(12.W))
    val out = Output(UInt(12.W))
  })
  
  // register bitwidth is inferred from io.out
  io.out := RegNext(io.in + 1.U)        // this is convenient
}
```

### `RegInit`

```scala
val myReg = RegInit(UInt(12.W), 0.U)
val myReg = RegInit(0.U(12.W))
```

**Example: Initialized Register**

```scala
val register = RegInit(0.U(12.W))
val register_song = RegInit(2.U(13.W))   // this set the reset value
```

### Control Flow

**Example: Register Control Flow**

### Other Register Examples

```scala
val reg: UInt = Reg(UInt(4.W))
```

**Example: Comb Filter**

**Exercise:**

*Shift Register*

*Parameterized Shift Register*

*Appendix: Explicit clock and reset*

## 2.5: Putting it all Together: An FIR Filter

### 8-bit Specification

### FIR Filter Generator

### DspBlock

### Testing

**Exercise: TileLink**

## 2.6: More on ChiselTest

### Basic Tester implementation

|          | iotesters                  | ChiselTest            |
| -------- | -------------------------- | --------------------- |
| poke     | poke(c.io.in1, 6)          | c.io.in1.poke(6.U)    |
| peek     | peek(c.io.out1)            | c.io.out1.peek()      |
| expect   | expect(c.io.out1, 6)       | c.io.out1.expect(6.U) |
| step     | step(1)                    | c.io.clock.step(1)    |
| initiate | Driver.execute(...) { c => | test(...) { c =>      |

### Modules with Decoupled Interfaces

#### A queue example

```scala
class QueueModule[T <: Data](ioType: T, entries: Int) extends MultiIOModule {
  val in = IO(Flipped(Decoupled(ioType)))
  val out = IO(Decoupled(ioType))
  out <> Queue(in, entries)
}
println(getVerilog(new QueueModule(UInt(3.W), 1)))
```

#### EnqueueNow and expectDequeueNow

| method           | description                                                  |
| ---------------- | ------------------------------------------------------------ |
| enqueueNow       | Add (enqueue) one element to a `Decoupled` input interface   |
| expectDequeueNow | Removes (dequeues) one element from a `Decoupled` output interface |

> Note: There is some required boiler plate `initSource`, `setSourceClock`, etc that is necessary to ensure that the `ready` and `valid` fields are all initialized correctly at the beginning of the test.

#### EnqueueSeq and DequeueSeq

### Fork and Join in ChiselTest

| method | description                                                  |
| ------ | ------------------------------------------------------------ |
| fork   | launches a concurrent code block, additional forks can be run concurrently to this one via the .fork appended to end of the code block of the preceeding fork |
| join   | re-unites multiple related forks back into the calling thread |

```scala
test(new QueueModule(UInt(9.W), entries = 200)) { c =>
    // Example testsequence showing the use and behavior of Queue
    c.in.initSource()
    c.in.setSourceClock(c.clock)
    c.out.initSink()
    c.out.setSinkClock(c.clock)
    
    val testVector = Seq.tabulate(300){ i => i.U }

    fork {
        c.in.enqueueSeq(testVector)
    }.fork {
        c.out.expectDequeueSeq(testVector)
    }.join()
}
```

### Using Fork and Join with GCD

## 3.1: Generators: Parameters

 In this section we discuss module parameterization, the various methodologies and Scala language features. The richness of the parameter passing implementation is directly proportional to the richness of the circuits generated. Parameters should provide useful default values, be easy to set, and protect against illegal or non-sensical values.

### Parameter Passing

**Example: **

*Parameterized Scala Object*

*Parameterized Chisel Object*

### Sorting with Parameterized Modules

**Example: Parameterized 4-Input Sort**

### Option and Default Arguments

**Examples**

*Erroneous Map Index Call*

*Getting Uncertain Indices*

*Get Or Else!*

### Options for Parameters with Defaults

Sometimes, a parameter doesn't have a good default value. `Option` can be used with a default value of `None` in these situations.

**Example: Optional Reset**

The following shows a block that delays its input by one clock cycle. If `resetValue = None`, which is the default, the register will have no reset value and be initialized to garbage. This avoids the common but ugly case of using values outside the normal range to indicate "none", like using -1 as the reset value to indicate that this register is not reset.

```scala
class DelayBy1(resetValue: Option[UInt] = None) extends Module {
    val io = IO(new Bundle {
        val in  = Input( UInt(16.W))
        val out = Output(UInt(16.W))
    })
    val reg = if (resetValue.isDefined) { // resetValue = Some(number)
        RegInit(resetValue.get)
    } else { //resetValue = None
        Reg(UInt())
    }
    reg := io.in
    io.out := reg
}

println(getVerilog(new DelayBy1))
println(getVerilog(new DelayBy1(Some(3.U))))
```

## Match/Case Statements

**Examples**

*Value Matching*

*Multiple Value Matching*

*Type Matching*

*Multiple Type Matching*

*Type Matching and Erasure*

*Optional Reset Matching*

### IOs with Optional Fields

Sometimes we want IOs to be optionally included or excluded. 

**Example: Optional IO with Option**

```scala
class HalfFullAdder(val hasCarry: Boolean) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(1.W))
    val b = Input(UInt(1.W))
    val carryIn = if (hasCarry) Some(Input(UInt(1.W))) else None // Note this statement
    val s = Output(UInt(1.W))
    val carryOut = Output(UInt(1.W))
  })
  val sum = io.a +& io.b +& io.carryIn.getOrElse(0.U)
  io.s := sum(0)
  io.carryOut := sum(1)
}
```

**Example: Optional IO with Zero-Width Wires**

Another way to achieve similar functionality to `Option`s is with zero-width wires. **Chisel types are allowed to have widths of zero. An IO with width zero is pruned from the emitted Verilog, and anything that tries to use the value of a zero-width wire gets a constant zero.** If zero is a sensible default value, zero-width wires can be nice because they obviate the need for matching on an option or calling `getOrElse`.

```scala
class HalfFullAdder(val hasCarry: Boolean) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(1.W))
    val b = Input(UInt(1.W))
    val carryIn = Input(if (hasCarry) UInt(1.W) else UInt(0.W))   // Note this statement
    val s = Output(UInt(1.W))
    val carryOut = Output(UInt(1.W))
  })
  val sum = io.a +& io.b +& io.carryIn   // this expression and method writing scheme is so concise!!!
  io.s := sum(0)
  io.carryOut := sum(1)
}
```

### Implicits

There are often times when you are programming that requires a lot of boilerplate code. To handle this use case, Scala introduced the notion of **implicits**, which allow the compiler to do some syntactic sugar for you. 

#### Implicit Arguments

```scala
object CatDog {
  implicit val numberOfCats: Int = 10
  //implicit val numberOfDogs: Int = 5

  def tooManyCats(nDogs: Int)(implicit nCats: Int): Boolean = nCats > nDogs
    
  val imp = tooManyCats(2)    // Argument passed implicitly!
  val exp = tooManyCats(2)(1) // Argument passed explicitly!
}
CatDog.imp
CatDog.exp
// The result is as below
defined object CatDog
res5_1: Boolean = true
res5_2: Boolean = false
```

What's happening here? First, we define an implicit value **numberOfCats**. In a given scope, **there can only be one implicit value of a given type**. Then, we define a function that takes two argument lists; the first is any explicit parameters, and the second are any implicit parameters. When we call **tooManyCats**, we either omit the second implicit argument list (letting the compiler find it for us), or explicitly provide an argument (which can be different than the implicit value).

The following are ways implicit arguments can *fail*:

- Two or more implicit values of a given type are defined in a scope
- If the compiler cannot find an implicit value necessary for a function call

**Example: Implicit Logging**

### Implicit Conversions

**Example: Implicit Conversion**

### Generator Example

**Example: Mealy Machine**

```scala
// Mealy machine has
case class BinaryMealyParams( // parameter case class
  // number of states
  nStates: Int,
  // initial state
  s0: Int,
  // function describing state transition
  stateTransition: (Int, Boolean) => Int,
  // function describing output
  output: (Int, Boolean) => Int
) {
  require(nStates >= 0)
  require(s0 < nStates && s0 >= 0)
}

class BinaryMealy(val mp: BinaryMealyParams) extends Module {
  val io = IO(new Bundle {
    val in = Input(Bool())
    val out = Output(UInt())
  })

  val state = RegInit(UInt(), mp.s0.U)

  // output zero if no states
  io.out := 0.U
  for (i <- 0 until mp.nStates) {
    when (state === i.U) {
      when (io.in) {
        state  := mp.stateTransition(i, true).U
        io.out := mp.output(i, true).U
      }.otherwise {
        state  := mp.stateTransition(i, false).U
        io.out := mp.output(i, false).U
      }
    }
  }
}

// example from https://en.wikipedia.org/wiki/Mealy_machine
val nStates = 3
val s0 = 2
def stateTransition(state: Int, in: Boolean): Int = {
  if (in) {
    1
  } else {
    0
  }
}
def output(state: Int, in: Boolean): Int = {
  if (state == 2) {
    return 0
  }
  if ((state == 1 && !in) || (state == 0 && in)) {
    return 1
  } else {
    return 0
  }
}

val testParams = BinaryMealyParams(nStates, s0, stateTransition, output)

test(new BinaryMealy(testParams)) { c =>
  c.io.in.poke(false.B)
  c.io.out.expect(0.U)
  c.clock.step(1)
  c.io.in.poke(false.B)
  c.io.out.expect(0.U)
  c.clock.step(1)
  c.io.in.poke(false.B)
  c.io.out.expect(0.U)
  c.clock.step(1)
  c.io.in.poke(true.B)
  c.io.out.expect(1.U)
  c.clock.step(1)
  c.io.in.poke(true.B)
  c.io.out.expect(0.U)
  c.clock.step(1)
  c.io.in.poke(false.B)
  c.io.out.expect(1.U)
  c.clock.step(1)
  c.io.in.poke(true.B)
  c.io.out.expect(1.U)
  c.clock.step(1)
  c.io.in.poke(false.B)
  c.io.out.expect(1.U)
  c.clock.step(1)
  c.io.in.poke(true.B)
  c.io.out.expect(1.U)
}

println("SUCCESS!!") // Scala Code: if we get here, our tests passed!

println(getVerilog(new BinaryMealy(testParams)))
```

## 3.2: Generators: Collections

### Generators and Collections

**Example: FIR Golden Model**

```scala
/**
  * A naive implementation of an FIR filter with an arbitrary number of taps.
  */
class ScalaFirFilter(taps: Seq[Int]) {
  var pseudoRegisters = List.fill(taps.length)(0)

  def poke(value: Int): Int = {
    pseudoRegisters = value :: pseudoRegisters.take(taps.length - 1)
    var accumulator = 0
    for(i <- taps.indices) {
      accumulator += taps(i) * pseudoRegisters(i)
    }
    accumulator
  }
}
```

##### Seq

##### Registers

##### Poke

##### Updating the Registers

##### Computing the output

#### Adapting out previous test for testing our golden model

#### Test circuit using the golden model

**Example: Parameterized FIR Generator**

### Hardware Collections

**Example: Add run-time configurable taps to out FIR**

The following code adds an additional `consts` vector to the IO of our FIR generator which allows the coefficients to be changed externally after circuit generation. This is done with the Chisel collection type `Vec`. `Vec` supports many of the scala collection methods **but it can only contain Chisel hardware elements.** `Vec` should only be used in situations where ordinary Scala collections won't work.
Basically this is in one of two situations.

1. You need a collection of elements in a Bundle, typically a Bundle that will be used as IO.
2. You need to access the collection via an index that is part of the hardware (think Register File).

```scala
class MyManyDynamicElementVecFir(length: Int) extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(8.W))
    val out = Output(UInt(8.W))
    val consts = Input(Vec(length, UInt(8.W))) // Note this statement
  })
...
}
```

**Exercise: 32-bit RISC-V Processor**

register file design

A [register file](https://en.wikipedia.org/wiki/Register_file) is an important building block for making a processor. A register file is an array of registers that can be read from or written to via a number of read or write ports. Each port consists of an address and data field.

The [RISC-V instruction set architecture](https://riscv.org/specifications/) defines several variants, the simplest of which is called RV32I. RV32I has a size-32 array of 32-bit registers. **The register at index 0 (the first register) is always zero when you read from it, regardless of what you write to it** (it's often useful to have 0 handy).

## 3.2 Interlude: Chisel Standard Library

### The Cheatsheet

The [Chisel3 cheatsheet](https://github.com/freechipsproject/chisel-cheatsheet/releases/latest/download/chisel_cheatsheet.pdf) contains a summary of all the major hardware construction APIs, including some of the standard library utilities that we'll introduce below.

### Decoupled: A Standard Ready-Valid Interface

One of the commonly used interfaces provided by Chisel is `DecoupledIO`, providing a ready-valid interface for transferring data. The idea is that the source drives the `bits` signal with the data to be transferred and the `valid` signal when there is data to be transferred. The sink drives the `ready` signal when it is ready to accept data, and data is considered transferred when both `ready` and `valid` are asserted on a cycle.

**Note: ready and valid should not be combinationally coupled, otherwise this may result in unsynthesizable combinational loops.**

**Any Chisel data** can be wrapped in a `DecoupledIO` (used as the `bits` field) as follows:

```scala
val myChiselData = UInt(8.W)
// or any Chisel data type, such as Bool(), SInt(...), or even custom Bundles
val myDecoupled = Decoupled(myChiselData)
```

The above creates a new `DecoupledIO` Bundle with fields

- `valid`: Output(Bool)
- `ready`: Input(Bool)
- `bits`: Output(UInt(8.W))

### Queues

`Queue` creates a FIFO (first-in, first-out) queue with **Decoupled interfaces on both sides, allowing backpressure.** Both the data type and number of elements are configurable.

```scala
class myModule extends Module {
    // Example circuit using a Queue
    val io = IO(new Bundle {
      val in = Flipped(Decoupled(UInt(8.W)))  // Decouple() give the Library that contains the methods of valid, ready and bits.
      val out = Decoupled(UInt(8.W))
    })
    val queue = Queue(io.in, 2)  // 2-element queue
    io.out <> queue
  }
```

### Arbiters

Arbiters route data from *n* `DecoupledIO` sources to one `DecoupledIO` sink, given a prioritization. There are two types included in Chisel:

- `Arbiter`: prioritizes lower-index producers
- `RRArbiter`: runs in round-robin order

### Misc Function Blocks

#### Bitwise Utilities

##### PopCount

##### Reverse

#### OneHot encoding utilities

The below two functions provide conversion between binary (`UInt`) and OneHot encodings, and are inverses of each other:

- UInt to OneHot: `UIntToOH`
- OneHot to UInt: `OHToUInt`

#### Muxes

##### Priority Mux

##### OneHot Mux

#### Counter

## 3.3: Higher-Order Functions

### A Tale of Two FIRs

```scala
io.out := (taps zip io.consts).map { case (a, b) => a * b }.reduce(_ + _)
```

### Functions as Arguments

**Functions like `map` and `reduce` are called *higher-order functions*: they are functions that take functions as arguments.** These are very powerful constructs that encapsulate a general computational pattern, allowing you to concentrate on the application logic instead of flow control, and resulting in very concise code.

#### Different ways of specifying functions

* For functions where each argument is referred to exactly once, you *may* be able to use an underscore (`_`) to refer to each argument. In the example above, the `reduce` argument function took two arguments and could be specified as `_ + _`. 
* Specifying the inputs argument list explicitly. The reduce could have been explicitly written as `(a, b) => a + b`, with the general form of putting the argument list in parentheses, followed by `=>`, followed by the function body referring to those arguments.

* When tuple unpacking is needed, using a `case` statement, as in `case (a, b) => a * b`. That takes a single argument, a tuple of two elements, and unpacks it into variables `a` and `b`, which can then be used in the function body.

#### Practice in Scala

**Examples:**

*Map*

*zipWithIndex*

*Reduce*

*Fold*

*Decoupled Arbiter*

## 3.4: Functional Programming

### Functional Programming in Scala

**Example: Custom Functions**

To produce no output, return the `Unit` type.

#### Functions as Objects

Functions in Scala are first-class objects. That means we can assign a function to a `val` and pass it to classes, objects, or other functions as an argument.

**Example: Function Objects**

```scala
// These are normal functions.
def plus1funct(x: Int): Int = x + 1
def times2funct(x: Int): Int = x * 2

// These are functions as vals.
// The first one explicitly specifies the return type.
val plus1val: Int => Int = x => x + 1 
val times2val = (x: Int) => x * 2

// Calling both looks the same.
plus1funct(4)
plus1val(4)
plus1funct(x=4)
//plus1val(x=4) // this doesn't work
```

Why would you want to create a `val` instead of a `def`? With a `val`, you can now pass the function around to other functions, as shown below. You can even create your own functions that accept other functions as arguments. **Formally, functions that take or produce functions are called *higher-order functions*.**

**Example: **

*Higher-Order Functions*

*Functions vs. Objects* :

A possibly confusing situation arises when using functions without arguments. Functions are evaluated every time they are called, while `val`s are evaluated at instantiation.

```scala
import scala.util.Random

// both x and y call the nextInt function, but x is evaluated immediately and y is a function
val x = Random.nextInt
def y = Random.nextInt

// x was previously evaluated, so it is a constant
println(s"x = $x")
println(s"x = $x")

// y is a function and gets reevaluated at each call, thus these produce different results
println(s"y = $y")
println(s"y = $y")

// the result is below:
x = -1043877968
x = -1043877968
y = -2057845416
y = -193908584
```

#### Anonymous Functions

**Exercise: Squence Manipulation**

A common set of higher-order functions you'll use are `scanLeft`/`scanRight`, `reduceLeft`/`reduceRight`, and `foldLeft`/`foldRight`. It's important to understand how each one works and when to use them. The default directions for `scan`, `reduce`, and `fold` are left, though this is not guaranteed for all cases.

```scala
val exList = List(1, 5, 7, 100)

// write a custom function to add two numbers, then use reduce to find the sum of all values in exList
def add(a: Int, b: Int): Int = a + b
val sum = exList.reduce{add}

// find the sum of exList using an anonymous function (hint: you've seen this before!)
val anon_sum = exList.reduce{_+_}

// find the moving average of exList from right to left using scan; make the result (ma2) a list of doubles
def avg(a: Int, b: Double): Double = (a + b) / 2.0
val ma2 = exList.foldLeft(0.0)(avg)
```

### Functional Programming in Chisel

**Exercise: FIR Filter**

```scala
// get some math functions
import scala.math.{abs, round, cos, Pi, pow}

// simple triangular window    below is the type of codffecient => val type = (the name of the first coefficient and the second cofficient) => {...
val TriangularWindow: (Int, Int) => Seq[Int] = (length, bitwidth) => {
  val raw_coeffs = (0 until length).map( (x:Int) => 1-abs((x.toDouble-(length-1)/2.0)/((length-1)/2.0)) )
  val scaled_coeffs = raw_coeffs.map( (x: Double) => round(x * pow(2, bitwidth)).toInt)
  scaled_coeffs
}

// Hamming window   // I convert the function as argument before
					// to the normal function after like below
                    // these have the similar effects between function
					// with function arguments and the normal function
def HammingWindow(length: Int, bitwidth: Int) : Seq[Int] = {  
  val raw_coeffs = (0 until length).map( (x: Int) => 0.54 - 0.46*cos(2*Pi*x/(length-1)))
  val scaled_coeffs = raw_coeffs.map( (x: Double) => round(x * pow(2, bitwidth)).toInt)
  scaled_coeffs
}

// check it out! first argument is the window length, and second argument is the bitwidth
TriangularWindow(10, 16)
HammingWindow(10, 16)
```

**Example: FIR Filter Tester**

### Chisel Exercises

**Exercise: Neural Network Neuron**

## 3.5: Object Oriented Programming

### Object Oriented Programming

Abstract classes, Traits, Objects, Companion Objects, Case Classes

#### Abstract Classes

Abstract classes are just like other programming language implementations. They can define many unimplemented values that subclasses must implement. Any object can only directly inherit from one parent abstract class.

#### Traits

Traits are very similar to abstract classes in that they can define unimplemented values. However, they differ in two ways:

- a class can inherit from multiple traits
- a trait cannot have constructor parameters

**Example: Traits and Multiple Inheritance**

#### Objects

Scala has a language feature for these singleton classes, called objects. You cannot instantiate an object **(no need to call `new`)**; you can simply directly reference it. That makes them similar to Java static classes.

#### Companion Objects

When a class and an object share the same name and defined in the same file, the object is called a **companion object**. When you use `new` before the class/object name, it will instantiate the class. If you don't use `new`, it will reference the object:

**Companion objects are usually used for the following reasons:**

1. **to contain constants related to the class**
2. **to execute code before/after the class constructor**
3. **to create multiple constructors for a class**

**Chisel uses many companion objects, like Module.** When you write the following:

```scala
val myModule = Module(new MyModule)
```

you are calling the **Module companion object**, so Chisel can run background code before and after instantiating `MyModule`.

#### Case Classes

Case classes are a special type of Scala class that provides some cool additional features. They are very common in Scala programming, so this section outlines some of their useful features:

- Allows **external access** to the **class parameters**
- **Eliminates** the need to use **`new`** when instantiating the class
- Automatically creates an **unapply method** that supplies access to all of the class Parameters.
- Cannot be subclassed from

### Inheritance with Chisel

Every Chisel module you make is a class extending the base type `Module`. Every Chisel IO you make is a class extending the base type `Bundle` (or, in some special cases, `Bundle`'s supertype [`Record`](https://github.com/freechipsproject/chisel3/blob/v3.0.0/chiselFrontend/src/main/scala/chisel3/core/Aggregate.scala#L415)). Chisel hardware types like `UInt` or `Bundle` all have `Data` as a supertype. We'll explore using object oriented programming to create hierarchical hardware blocks and explore object reuse.

#### Module

Whenever you want to create a hardware object in Chisel, it needs to have `Module` as a superclass. Inheritance might not always be the right tool for reuse ([composition over inheritance](https://en.wikipedia.org/wiki/Composition_over_inheritance) is a common principle), but inheritance is still a powerful tool. 

**Example: Gray Encoder and Decoder**

## 3.6: Generators: Types

### Static Types

#### Types in Scala

While not required, it is HIGHLY recommended that you **define input and output types for all function declarations**.  This will let the Scala compiler catch improper use of a function.

#### Scala vs. Chisel Types

`Int` and `UInt` , `Boolean` and `Bool`

`if()` expects a `Boolean`, and `when()` expects a ` Bool`

#### Scala Type Coercion

##### `asInstanceOf[T]`

#### Type Casting in Chisel

e.g. `asTypeOf()`, `asUInt()`, `asSInt()`

### Type Matching

#### Match Operator

**Note: there are much better and safer ways to write type-generic generators in Scala**.

```scala
class ConstantSum(in1: Data, in2: Data) extends Module {
    val io = IO(new Bundle {
        val out = Output(chiselTypeOf(in1)) // in case in1 is literal then just get its type
    })
    (in1, in2) match {
        case (x: UInt, y: UInt) => io.out := x + y
        case (x: SInt, y: SInt) => io.out := x + y
        case _ => throw new Exception("I give up!")
    }
}
println(getVerilog(dut = new ConstantSum(3.U, 4.U)))
println(getVerilog(dut = new ConstantSum(-3.S, 4.S)))
println(getVerilog(dut = new ConstantSum(3.U, 4.S)))
```

**It is good to remember that Chisel types generally should not be value matched.** *but this can be accomplish as below method:*

#### Unapply

What's actually going on when you do a match? How does Scala let you do fancy value matching with case classes like this:

```scala
case class Something(a: String, b: Int)
val a = Something("A", 3)
a match {
    case Something("A", value) => value
    case Something(str, 3)     => 0
}
```

As it turns out, the companion object that is created for every case class also contains an **unapply** method, in addition to an **apply** method. What is an **unapply** method?

Scala unapply methods are another form of syntactic sugar that give match statements the ability to both match on types and **extract values** from those types during the matching.

Let's look at the following example. For some reason, let's say that if the generator is being pipelined, the delay is `3*totalWidth`, otherwise the delay is `2*someOtherWidth`. Because case classes have **unapply** defined, we can match values inside the case class, like so:

```scala
case class SomeGeneratorParameters(
    someWidth: Int,
    someOtherWidth: Int = 10,
    pipelineMe: Boolean = false
) {
    require(someWidth >= 0)
    require(someOtherWidth >= 0)
    val totalWidth = someWidth + someOtherWidth
}

def delay(p: SomeGeneratorParameters): Int = p match {
    case SomeGeneratorParameters(_, sw, false) => sw * 2
    case sg @SomeGeneratorParameters(_, _, true) => sg.totalWidth * 3  // this notation of "sg @...", this is so wanderful
}

println(delay(SomeGeneratorParameters(10, 10)))
println(delay(SomeGeneratorParameters(10, 10, true)))
```

#### Partial Functions  (difficult for me to understand)

This is a brief overview; [this guide](https://twitter.github.io/scala_school/pattern-matching-and-functional-composition.html#PartialFunction) has a more detailed overview.

Partial functions are functions that are only defined on a subset of their inputs. Like an option, a partial function may not have a value for a particular input. This can be tested with `isDefinedAt(...)`.

Partial functions can be chained together with `orElse`.

Note that calling a `PartialFunction` with an undefined input will result in a runtime error. This can happen, for example, if the input to the `PartialFunction` is user-defined. To be more type-safe, we recommend writing functions that return an `Option` instead.

#### Type Safe Connections

Chisel can check the type for many connections, including:

- Bool/UInt to Clock

For other types, Chisel will let you connect them, but may truncate/pad bits as appropriate.

- Bool/UInt to Bool/UInt
- Bundle to Bundle

### Type Generics (also difficult for me)

Scala's generic types (also known as polymorphism) is very complicated, especially when coupling it with inheritance.

This section will just get your toes wet; to understand more, check out [this tutorial](https://twitter.github.io/scala_school/type-basics.html).

#### Chisel Type Hierarchy

Registers are a good example of polymorphic code in Chisel. Look at the implementation of `RegEnable` (a register with a `Bool` enable signal) [here](https://github.com/freechipsproject/chisel3/blob/v3.0.0/src/main/scala/chisel3/util/Reg.scala#L10). The apply function is templated for `[T <: Data]`, which means `RegEnable` will work for all Chisel hardware types.

**Example: Type Generic ShiftRegister**

#### Type Generics with Typeclasses

**Exercise: Mac as Object**

**Exercise: Integrator**

### Creating a Custom Type

One of the things that makes Chisel powerful is its extensibility. You can add your own types that have their own operations and representations that are tailored to your application.

**Example: DspComplex** (this is a bit complex and cna get more info from the original bootcamp)

```scala
class DspComplex[T <: Data:Ring](val real: T, val imag: T) extends Bundle { ... }
```

**Exercise: Sign-magnitude Numbers** (too difficult)