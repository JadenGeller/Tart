extension AbstractBindingTree {
    public indirect enum Unfolded {
        case variable(Variable)
        case abstraction(binding: Variable, body: Unfolded)
        case application(operator: Operator, arguments: [Unfolded])
    }
}

// FIXME: Remove once Swift properly finds nested types in nested generic extensions
//        instead of always requiring the fully-qualified name.
extension AbstractBindingTree.Unfolded {
    public typealias AbstractBindingTree_ = AbstractBindingTree<Variable, Operator>
    internal typealias UnsafeBase_ = AbstractBindingTree_.UnsafeBase
    public typealias View_ = AbstractBindingTree_.View
    public typealias Unfolded_ = AbstractBindingTree_.Unfolded
}

// MARK: Sugar

extension AbstractBindingTree.Unfolded {
    public static func lambda(_ variable: Variable, constructing expression: (Unfolded_) -> Unfolded_) -> Unfolded_ {
        return .abstraction(binding: variable, body: expression(.variable(variable)))
    }
}
extension AbstractBindingTree.Unfolded where Variable == Identity {
    public static func lambda(constructing expression: (Unfolded_) -> Unfolded_) -> Unfolded_ {
        return .lambda(Identity(), constructing: expression)
    }
}

// MARK: Conversions

extension AbstractBindingTree {
    public func unfolded() -> Unfolded {
        switch self[] {
        case .variable(let variable):
            return .variable(variable)
        case .abstraction(let binding, let body):
            return .abstraction(binding: binding, body: body.unfolded())
        case .application(let op, let args):
            return .application(operator: op, arguments: args.map({ $0.unfolded() }))
        }
    }
    
    public init(folding unfolded: Unfolded) {
        switch unfolded {
        case .variable(let variable):
            self = .variable(variable)
        case .abstraction(let binding, let body):
            self = .abstraction(
                binding: binding,
                body: AbstractBindingTree(folding: body)
            )
        case .application(let op, let args):
            self = .application(
                operator: op,
                arguments: args.map(AbstractBindingTree.init(folding:))
            )
        }
    }
}

// MARK: Conformances

extension AbstractBindingTree.Unfolded: CustomStringConvertible, CustomDebugStringConvertible {
    public var description: String {
        
//        switch self {
//        case .variable(let variable):
//            return "\(variable)"
//        case .abstraction(let binding, let body):
//            return "λ\(binding).\(body)"
//        }
        fatalError() // TODO
    }
    
    public var debugDescription: String {
        fatalError() // TODO
    }
}
