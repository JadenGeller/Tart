protocol VariableProtocol: Hashable {
    func freeing(in bindings: Set<Self>) -> Self
}

final class Identity: VariableProtocol, CustomStringConvertible {
    init() { }
    
    static func ==(lhs: Identity, rhs: Identity) -> Bool {
        return lhs === rhs
    }
    
    var hashValue: Int {
        return ObjectIdentifier(self).hashValue
    }
    
    func freeing(in bindings: Set<Identity>) -> Identity {
        return Identity()
    }
    
    var description: String {
        return Int(bitPattern: ObjectIdentifier(self)).description
    }
}

extension String: VariableProtocol {
    private func trimmingTrailing(where shouldTrim: (Character) -> Bool) -> String {
        var value = self
        while let last = value.characters.last, shouldTrim(last) {
            value.characters.removeLast()
        }
        return value
    }
    
    func freeing(in bindings: Set<String>) -> String {
        var spelling = trimmingTrailing(where: { $0 == "'" })
        while bindings.contains(spelling) {
            spelling.append("'")
        }
        return spelling
    }
}

extension Int: VariableProtocol {
    func freeing(in bindings: Set<Int>) -> Int {
        var value = self
        while bindings.contains(value) {
            value += 1
        }
        return value
    }
}
