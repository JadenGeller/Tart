// FIXME: Remove once Swift properly finds nested types in nested generic extensions
//        instead of always requiring the fully-qualified name.
extension AbstractBindingTree.View {
    public typealias AbstractBindingTree_ = AbstractBindingTree<Variable, Operator>
    public typealias View_ = AbstractBindingTree_.View
    internal typealias UnsafeBase_ = AbstractBindingTree_.UnsafeBase
}

// MARK: Conversions

extension AbstractBindingTree {
    public init(_ view: View) {
        self = AbstractBindingTree(unsafe: UnsafeBase(view))
    }
}
extension AbstractBindingTree.View {
    public init(_ abt: AbstractBindingTree_) {
        self = View_(unsafe: UnsafeBase_(abt))
    }
}

extension AbstractBindingTree {
    public subscript() -> View {
        get {
            return View(self)
        }
        set {
            self = AbstractBindingTree(newValue)
        }
    }
}

// MARK: Conformances

extension AbstractBindingTree.View: Equatable {
    public static func ===(lhs: View_, rhs: View_) -> Bool {
        switch (lhs, rhs) {
        case (.variable(let leftVariable), .variable(let rightVariable)):
            return leftVariable == rightVariable
        case (.abstraction(let leftBinding, let leftBody), .abstraction(let rightBinding, let rightBody)):
            return leftBinding == rightBinding && leftBody === rightBody
        case (.application(let leftOp, let leftArgs), .application(let rightOp, let rightArgs)):
            return leftOp == rightOp
                && zip(leftArgs, rightArgs).map(===).reduce(true, { $0 && $1 })
        default:
            return false
        }
    }
    
    public static func ==(lhs: View_, rhs: View_) -> Bool {
        switch (lhs, rhs) {
        case (.variable(let leftVariable), .variable(let rightVariable)):
            return leftVariable == rightVariable
        case (.abstraction(let leftBinding, let leftBody), .abstraction(let rightBinding, let rightBody)):
            return leftBinding == rightBinding && leftBody == rightBody
        case (.application(let leftOp, let leftArgs), .application(let rightOp, let rightArgs)):
            return leftOp == rightOp
                && leftArgs == rightArgs
        default:
            return false
        }
    }
}

extension AbstractBindingTree.View: CustomStringConvertible, CustomDebugStringConvertible {
    public var debugDescription: String {
        return "AbstractBindingTree.View(\(description))"
    }
}
