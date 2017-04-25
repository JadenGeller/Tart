protocol OperatorProtocol: Equatable {
    
}

struct Application: OperatorProtocol, CustomStringConvertible, ExpressibleByNilLiteral {
    init() { }
    init(nilLiteral: ()) { }
    
    static func ==(lhs: Application, rhs: Application) -> Bool {
        return true
    }
    
    public var description: String {
        return ""
    }
}
