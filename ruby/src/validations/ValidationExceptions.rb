class TypeValidationError < StandardError
  def initialize(attr, instance_type)
    @attr = attr
    @instance_type = instance_type
  end

  def message
    "En #{@attr.named}: El tipo de dato declarado es #{@attr.type} y
     el tipo de dato seteado es #{@instance_type}"
  end
end

# Excepción general de la que heredan todos los demás errores de validación
class ValidationError < StandardError
  def initialize(attr_named, val_arg, attr_value)
    @attr_named = attr_named
    @val_arg = val_arg
    @attr_value = attr_value
  end
end

class FromValidationError < ValidationError
  def message
    "En #{@attr_named}: El valor mínimo es #{@val_arg} y el valor seteado es #{@attr_value}"
  end
end

class ToValidationError < ValidationError
  def message
    "En #{@attr_named}: El valor máximo es #{@val_arg} y el valor seteado es #{@attr_value}"
  end
end

class NoBlankValidationError < ValidationError
  def message
    "En #{@attr_named}: Se ha seteado el atributo como cadena vacía"
  end
end

class NotEqualToValidationError < ValidationError
  def message
    "En #{@attr_named}: No se puede setear el valor #{@val_arg}"
  end
end

class ValidateBlockError < ValidationError
  def message
    "En #{@attr_named}: No se cumple la condición del bloque"
  end
end