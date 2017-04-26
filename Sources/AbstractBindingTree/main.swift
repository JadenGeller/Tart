//let x: AbstractBindingTree<String, Application> =
//    AbstractBindingTree.abstraction("x", .abstraction("x", .application(nil, [.abstraction("x", .boundVariable(1)), .boundVariable(2)])))
//print(x)
//
//let y: AbstractBindingTree<String, Application> = AbstractBindingTree.abstraction("x", .abstraction("y", .boundVariable(1)))
//
//let z = AbstractBindingTree<Identity, Application>(folding: .lambda { x in .lambda { y in x } }).renameVariables(to: ["a", "b"])
//

//typealias Lambda = AbstractBindingTree<Identity, Apply>
//
//let zero = Lambda(folding:
//    .lambda { s in .lambda { z in z } }
//).renamingVariables(to: ["s", "z"])
//
//let one = Lambda(folding:
//    .lambda { s in .lambda { z in .apply(s, z) } }
//).renamingVariables(to: ["s", "z"])
//
//let two = Lambda(folding:
//    .lambda { s in .lambda { z in .apply(s, .apply(s, z)) } }
//).renamingVariables(to: ["s", "z"])
//
//let succ = Lambda(folding:
//    .lambda { n in .lambda { f in .lambda { x in .apply(f, .apply(n, f, x)) } } }
//).renamingVariables(to: ["n", "f", "x"])
//
//print(succ[.abstraction]!)
//

//let x: AbstractBindingTree<String, Empty> = .abstraction(binding: "x", body: .variable("x"))
let y: AbstractBindingTree<Identity, Empty>.Unfolded = .lambda { x in .lambda { y in .application(operator: Empty(), arguments: [x, y]) } }
print(y)
print(AbstractBindingTree(folding: y).renamingVariables(to: ["x", "y"]))
//print(z)


