struct Abstraction<Variable: VariableProtocol, Operator: OperatorProtocol> {
    var binding: Variable
    var expression: AbstractBindingTree<Variable, Operator>
    
    init?(_ abt: AbstractBindingTree<Variable, Operator>) {
        guard case .abstraction(let binding, let expression) = abt else { return nil }
        self.binding = binding
        self.expression = expression
    }
}

extension Abstraction {
    func substitute(_ expression: AbstractBindingTree<Variable, Operator>, for variable: Variable) -> AbstractBindingTree<Variable, Operator> {
        fatalError("To be implemented.")
    }
}
