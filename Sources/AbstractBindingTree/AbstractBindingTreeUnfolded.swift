extension AbstractBindingTree.Unfolded {
    // FIXME: Workaround for Swift's bad support for types nested in generic types.
    typealias Unfolded_ = AbstractBindingTree<Variable, Operator>.Unfolded
    
    static func lambda(_ variable: Variable, constructing expression: (Unfolded_) -> Unfolded_) -> Unfolded_ {
        return .abstraction(variable, expression(.variable(variable)))
    }
}
extension AbstractBindingTree.Unfolded where Variable == Identity {
    static func lambda(constructing expression: (Unfolded_) -> Unfolded_) -> Unfolded_ {
        return .lambda(Identity(), constructing: expression)
    }
}

extension AbstractBindingTree {
    func unfolded() -> Unfolded {
        switch self[] {
        case .variable(let variable):
            return .variable(variable)
        case .abstraction(let binding, let expression):
            return .abstraction(binding, expression.unfolded())
        case .application(let operation, let arguments):
            return .application(operation, arguments.map({ $0.unfolded() }))
        }
    }
    
    init(folding unfolded: Unfolded) {
        self.init(folding: unfolded, from: 0, with: [:])
    }
    
    private init(folding unfolded: Unfolded, from currentDepth: Int, with bindings: [Variable: Int]) {
        switch unfolded {
        case .variable(let variable):
            if let bindingDepth = bindings[variable] {
                self = .boundVariable(currentDepth - bindingDepth)
            } else {
                self = .freeVariable(variable)
            }
        case .abstraction(let binding, let expression):
            var updatedBindings = bindings
            updatedBindings[binding] = currentDepth
            self = .abstraction(binding, AbstractBindingTree(folding: expression, from: currentDepth + 1, with: updatedBindings))
        case .application(let operation, let arguments):
            self = .application(operation, arguments.map({ expression in
                AbstractBindingTree(folding: expression, from: currentDepth, with: bindings)
            }))
        }
    }
}
