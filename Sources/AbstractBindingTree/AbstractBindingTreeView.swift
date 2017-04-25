extension AbstractBindingTree {
    subscript() -> View {
        get {
            switch self {
            case .freeVariable(let variable):
                return .variable(variable)
            case .boundVariable:
                fatalError("Unexpected bound variable.")
            case .abstraction(let binding, let expression):
                let variable = binding.freeing(in: expression.freeVariables)
                return .abstraction(variable, expression.freeingBound(with: variable))
            case .application(let operation, let arguments):
                return .application(operation, arguments)
            }
        }
        set {
            switch newValue {
            case .variable(let variable):
                self = .freeVariable(variable)
            case .abstraction(let binding, let expression):
                let variable = binding.freeing(in: expression.freeVariables)
                self = .abstraction(variable, expression.bindingFree(variable))
            case .application(let operation, let arguments):
                self = .application(operation, arguments)
            }
        }
    }
}

extension AbstractBindingTree {
    private func freeingBound(with variable: Variable, from currentIndex: Int) -> (AbstractBindingTree, didFree: Bool) {
        switch self {
        case .freeVariable(let variable):
            return (.freeVariable(variable), false)
        case .boundVariable(let index):
            if index == currentIndex {
                return (.freeVariable(variable), false)
            } else {
                return (.boundVariable(index), true)
            }
        case .abstraction(var binding, var expression):
            var didFree: Bool
            (expression, didFree) = expression.freeingBound(with: variable, from: currentIndex + 1)
            if didFree {
                binding = binding.freeing(in: expression.freeVariables)
            }
            return (.abstraction(binding.freeing(in: expression.freeVariables), expression), didFree)
        case .application(let operation, let arguments):
            var didFreeAny = false
            return (.application(operation, arguments.map({ expression in
                let result: AbstractBindingTree
                (result, didFreeAny) = expression.freeingBound(with: variable, from: currentIndex)
                return result
            })), didFreeAny)
        }
    }

    fileprivate func freeingBound(with variable: Variable) -> AbstractBindingTree {
        return freeingBound(with: variable, from: 1).0 // 1-indexed
    }
}

extension AbstractBindingTree {
    private func bindingFree(_ newVariable: Variable, from currentIndex: Int) -> AbstractBindingTree {
        switch self {
        case .freeVariable(let variable):
            if variable == newVariable {
                return .boundVariable(currentIndex)
            } else {
                return .freeVariable(variable)
            }
        case .boundVariable(let index):
            return .boundVariable(index)
        case .abstraction(let binding, let expression):
            return .abstraction(binding, 
                expression.bindingFree(newVariable, from: currentIndex + 1)
            )
        case .application(let operation, let arguments):
            return .application(operation, arguments.map({ expression in
                expression.bindingFree(newVariable, from: currentIndex)
            }))
        }
    }
    
    fileprivate func bindingFree(_ newVariable: Variable) -> AbstractBindingTree {
        return bindingFree(newVariable, from: 1) // 1-indexed
    }
}

// TODO: Implement substitution.
