public protocol VariableProtocol: Hashable {
    func fresh(in bindings: Set<Self>) -> Self
}

public final class Identity: VariableProtocol, CustomStringConvertible {
    public init() { }
    
    public static func ==(lhs: Identity, rhs: Identity) -> Bool {
        return lhs === rhs
    }
    
    public var hashValue: Int {
        return ObjectIdentifier(self).hashValue
    }
    
    public func fresh(in bindings: Set<Identity>) -> Identity {
        return Identity()
    }
    
    public var description: String {
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
    
    public func fresh(in bindings: Set<String>) -> String {
        var spelling = trimmingTrailing(where: { $0 == "'" })
        while bindings.contains(spelling) {
            spelling.append("'")
        }
        return spelling
    }
}

extension Int: VariableProtocol {
    public func fresh(in bindings: Set<Int>) -> Int {
        var value = self
        while bindings.contains(value) {
            value += 1
        }
        return value
    }
}
