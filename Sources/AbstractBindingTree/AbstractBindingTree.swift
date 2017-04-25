indirect enum AbstractBindingTree<Variable: VariableProtocol, Operator: OperatorProtocol> {
    case freeVariable(Variable)
    case boundVariable(Int) // De Bruijn index
    case abstraction(Variable, AbstractBindingTree)
    case application(Operator, [AbstractBindingTree])
    
    enum View {
        case variable(Variable)
        case abstraction(Variable, AbstractBindingTree)
        case application(Operator, [AbstractBindingTree])
    }
    
    indirect enum Unfolded {
        case variable(Variable)
        case abstraction(Variable, Unfolded)
        case application(Operator, [Unfolded])
    }
}

extension AbstractBindingTree {
    var freeVariables: Set<Variable> {
        switch self {
        case .freeVariable(let variable):
            return [variable]
        case .boundVariable:
            return []
        case .abstraction(let binding, let expression):
            return expression.freeVariables.subtracting([binding])
        case .application(_, let arguments):
            return arguments.reduce([]) { result, expression in result.union(expression.freeVariables) }
        }
    }
}

extension AbstractBindingTree.View: CustomStringConvertible, CustomDebugStringConvertible {
    fileprivate var parenthesizedDescription: String {
        if case .variable = self {
            return description
        } else {
            return "(" + description + ")"
        }
    }
    
    var description: String {
        switch self {
        case .variable(let variable):
            return "\(variable)"
        case .abstraction(let binding, let expression):
            return "λ\(binding).\(expression)"
        case .application(let operation, let arguments):
            let basicDescription = arguments.map({ $0[].parenthesizedDescription })
                                            .joined(separator: " ")
            if "\(operation)".isEmpty {
                return basicDescription
            } else {
                return "\(operation)(\(basicDescription))"
            }
        }
    }
    
    var debugDescription: String {
        return "AbstractBindingTree.View(\(description))"
    }
}

extension AbstractBindingTree: CustomStringConvertible, CustomDebugStringConvertible {
    var description: String {
        return self[].description
    }
    
    var debugDescription: String {
        return description
    }
}

extension AbstractBindingTree: Equatable {
    static func ===(lhs: AbstractBindingTree, rhs: AbstractBindingTree) -> Bool {
        switch (lhs, rhs) {
        case (.freeVariable(let leftVariable), .freeVariable(let rightVariable)):
            return leftVariable == rightVariable
        case (.boundVariable(let leftIndex), .boundVariable(let rightIndex)):
            return leftIndex == rightIndex
        case (.abstraction(let leftBinding, let leftExpression), .abstraction(let rightBinding, let rightExpression)):
            return leftBinding == rightBinding && leftExpression == rightExpression
        case (.application(let leftOperator, let leftArguments), .application(let rightOperator, let rightArguments)):
            return leftOperator == rightOperator && leftArguments == rightArguments
        default:
            return false
        }
    }
    
    // Alpha equivalence ingnores the binding labels
    static func ==(lhs: AbstractBindingTree, rhs: AbstractBindingTree) -> Bool {
        switch (lhs, rhs) {
        case (.freeVariable(let leftVariable), .freeVariable(let rightVariable)):
            return leftVariable == rightVariable
        case (.boundVariable(let leftIndex), .boundVariable(let rightIndex)):
            return leftIndex == rightIndex
        case (.abstraction(_, let leftExpression), .abstraction(_, let rightExpression)):
            return leftExpression == rightExpression
        case (.application(let leftOperator, let leftArguments), .application(let rightOperator, let rightArguments)):
            return leftOperator == rightOperator && leftArguments == rightArguments
        default:
            return false
        }
    }
}

extension AbstractBindingTree {
    func mapVariables<V: VariableProtocol>(_ transform: (Variable) -> V) -> AbstractBindingTree<V, Operator> {
        switch self {
        case .freeVariable(let variable):
            return .freeVariable(transform(variable))
        case .boundVariable(let index):
            return .boundVariable(index)
        case .abstraction(let binding, let expression):
            return .abstraction(transform(binding), expression.mapVariables(transform))
        case .application(let operation, let arguments):
            return .application(operation, arguments.map({ expression in
                expression.mapVariables(transform)
            }))
        }
    }
    
    func renamingVariables<S: Sequence>(to sequence: S) -> AbstractBindingTree<S.Iterator.Element, Operator> where S.Iterator.Element: VariableProtocol {
        var renames: [Variable: S.Iterator.Element] = [:]
        var iterator = sequence.makeIterator()
        var seen: Set<S.Iterator.Element> = []
        return mapVariables { oldName in
            if let newName = renames[oldName] {
                return newName
            } else {
                guard let newName = iterator.next()?.freeing(in: seen) else {
                    preconditionFailure("Not enough variable names")
                }
                seen.insert(newName)
                renames[oldName] = newName
                return newName
            }
        }
    }
}
