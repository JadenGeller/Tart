let x: AbstractBindingTree<String, Application> = AbstractBindingTree.abstraction("x", .abstraction("x", .application(nil, [.abstraction("x", .boundVariable(1)), .boundVariable(2)])))
print(x)

let y: AbstractBindingTree<String, Application> = AbstractBindingTree.abstraction("x", .abstraction("y", .boundVariable(1)))

let z = AbstractBindingTree<Identity, Application>(folding: .lambda { x in .lambda { y in y } }).renameVariables(to: ["a", "b"])
print(y)
print(z)
print(y == z)
print(y === z)

