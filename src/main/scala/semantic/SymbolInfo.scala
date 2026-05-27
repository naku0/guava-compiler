package semantic

case class SymbolInfo(
                       name: String,
                       isInitialized: Boolean,
                       isUsed: Boolean,
                       dataType: DataType,
                       arity: Option[Int] = None,
                       arrayElementType: Option[DataType] = None
                     )
