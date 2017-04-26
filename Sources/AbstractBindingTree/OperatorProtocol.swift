public protocol OperatorProtocol: Equatable {
    
}

struct Empty: OperatorProtocol, CustomStringConvertible {
    init() { }
    
    static func ==(lhs: Empty, rhs: Empty) -> Bool {
        return true
    }
    
    public var description: String {
        return "" // TODO: This is a poor design.
    }
}

//
//extension AbstractBindingTree.Unfolded where Operator == Apply {
//    static func apply(_ arguments: AbstractBindingTree<Variable, Operator>.Unfolded...) -> Unfolded_ {
//        return .application(Apply(), arguments)
//    }
//}
