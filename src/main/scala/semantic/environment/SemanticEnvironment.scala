package semantic.environment

import semantic.{DataType, SymbolInfo}

case class SemanticEnvironment(parent: Option[SemanticEnvironment] = None,
                               variables: Map[String, SymbolInfo] = Map.empty) {

  def defineFunction(name: String, arity: Int): (SemanticEnvironment, Boolean) = {
    if (variables.contains(name)) {
      (this, false)
    } else {
      val symbol = SymbolInfo(
        name = name,
        isInitialized = true,
        isUsed = true,
        dataType = DataType.Function,
        arity = Some(arity)
      )

      (copy(variables = variables + (name -> symbol)), true)
    }
  }

  def defineVariable(
                      name: String,
                      isInitialized: Boolean,
                      dataType: DataType = DataType.Unknown,
                      arrayElementType: Option[DataType] = None
                    ): (SemanticEnvironment, Boolean) = {
    if (variables.contains(name)) {
      (this, false)
    } else {
      val symbol = SymbolInfo(
        name = name,
        isInitialized = isInitialized,
        isUsed = false,
        dataType = dataType,
        arrayElementType = arrayElementType
      )

      (copy(variables = variables + (name -> symbol)), true)
    }
  }

  def isVariableDefined(name: String): Boolean = {
    variables.contains(name) || parent.exists(_.isVariableDefined(name))
  }

  def getVariable(name: String): Option[SymbolInfo] = {
    variables.get(name).orElse(parent.flatMap(_.getVariable(name)))
  }

  def setInitialized(name: String): SemanticEnvironment = {
    updateVariable(name)(_.copy(isInitialized = true))
  }

  def markUsed(name: String): SemanticEnvironment = {
    updateVariable(name)(_.copy(isUsed = true))
  }

  def setType(name: String, dataType: DataType): SemanticEnvironment = {
    updateVariable(name)(_.copy(dataType = dataType))
  }

  def setArrayElementType(name: String, arrayElementType: Option[DataType]): SemanticEnvironment = {
    updateVariable(name)(_.copy(arrayElementType = arrayElementType))
  }

  def updateVariable(name: String)(update: SymbolInfo => SymbolInfo): SemanticEnvironment = {
    variables.get(name) match {
      case Some(symbol) =>
        copy(variables = variables + (name -> update(symbol)))
      case None =>
        parent match {
          case Some(p) => copy(parent = Some(p.updateVariable(name)(update)))
          case None => this
        }
    }
  }

  def getLocalVariables: Iterable[SymbolInfo] =
    variables.values
}
