// MARK: Constructors

extension AbstractBindingTree {
    static func variable(_ variable: Variable) -> AbstractBindingTree {
        return AbstractBindingTree(View.variable(variable))
    }
    static func abstraction(binding: Variable, body: AbstractBindingTree) -> AbstractBindingTree {
        return AbstractBindingTree(View.abstraction(binding: binding, body: body))
    }
    static func application(operator op: Operator, arguments args: [AbstractBindingTree]) -> AbstractBindingTree {
        return AbstractBindingTree(View.application(operator: op, arguments: args))
    }
}

// MARK: Computed Properties

extension AbstractBindingTree {
    public var freeVariables: Set<Variable> {
        return UnsafeBase(self).freeVariables
    }
}

// MARK: Methods

extension AbstractBindingTree {
    func mapVariables<V: VariableProtocol>(_ transform: (Variable) -> V) -> AbstractBindingTree<V, Operator> {
        return unsafeMapBase({ $0.mapVariables(transform) })
    }
        
    func renamingVariables<S: Sequence>(to sequence: S) -> AbstractBindingTree<S.Iterator.Element, Operator> where S.Iterator.Element: VariableProtocol {
        var renames: [Variable: S.Iterator.Element] = [:]
        var iterator = sequence.makeIterator()
        var seen: Set<S.Iterator.Element> = []
        return mapVariables { oldName in
            if let newName = renames[oldName] {
                return newName
            } else {
                guard let newName = iterator.next()?.fresh(in: seen) else {
                    preconditionFailure("Not enough variable names")
                }
                seen.insert(newName)
                renames[oldName] = newName
                return newName
            }
        }
    }
}

// MARK: Conformances

extension AbstractBindingTree: Equatable {
    public static func ===(lhs: AbstractBindingTree, rhs: AbstractBindingTree) -> Bool {
        return UnsafeBase(lhs) === UnsafeBase(rhs)
    }
    
    public static func ==(lhs: AbstractBindingTree, rhs: AbstractBindingTree) -> Bool {
        return UnsafeBase(lhs) == UnsafeBase(rhs)
    }
}

extension AbstractBindingTree: CustomStringConvertible, CustomDebugStringConvertible {    
    public var debugDescription: String {
        return description
    }
}
