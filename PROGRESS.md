# Progress

## Week 0

I've decided to build a very basic dependently-typed programming language for my CS 81c project. It will be based on the (almost) tutorial for the toy language [Pi-Forall](https://github.com/sweirich/pi-forall). I will try to add other resources I reference [here](https://github.com/JadenGeller/Tart/blob/master/REFERENCES.md).

I will be using Swift as the implementation language for the project. I considered other options:

  * Haskell, but I want to use a different implementation language than Pi-Forall
  * Python, but I want the benefits of a static type-system
  * Rust, but I'm not familiar enough to not constantly wrestle the borrow-checker
  * Scala, but I probably shouldn't learn a new langauge while building such a new concept
  * Ocaml, but I'm not very well-versed and I'm holding a grudge on the syntax
  * C, but no...
  
I think that Swift will be the right choice. I've spent such a significant amount of time programming in the language that I won't waste any time learning about the langauge. Further, I'm familiar enough with the language to know its limitations, and I feel like it will be a good fit for what I would like to implement.

I will not be using anything platform specific. I will likely use just the standard library---with the exception of any open-source, platform-agnostic packages that seem particuarly useful. For example, I'm considering using this [ABT package](https://github.com/typelift/Valence) as it looks incredibly well-written; since I understand it and I probably could not write something better, it seems like a good idea to use this pre-existing package. I spent enough time writing things from scratch last term, and it really ate up time...

## Week 1

I learned about features of dependently typed langauges. Additionally, I build an abstract binding tree API to simplify working with bindings in the langauge. I've read that a logic framework is even better to work with than an ABT, but I still don't fully understand how those work. That would be something that's really interesting to look into in the future.

## Week 3

I faced some issues with my binding framework due to langauge limitations. I tried to port it to Haskell becuase I thought Swift might be too limiting, but I faced other difficulties in the process. I ended up using the [unbound](https://hackage.haskell.org/package/unbound) library which, though magical, is very good. I implemented a type cheker and small-step evaluator for STLC.

## Week 4

I updated the STLC interpreter to use a mutually recursive check-infer design. It turns out though that most implementations of bidirectional type systems use a single function. It doesn't seem like it really matters at all much. I implemented a dependently typed lambda calculus type checker and interpreter. I implemented beta equality so that computational equality should work. I implemented an indexing method that's used to report better errors. I implemented a pretty printer, but decided to not implement a parser at this time.
