require_relative '../open_classes/OpenSymbol'

module Validations
  # Dentro de los métodos, self es la instancia de la clase que hace include de validations:
  # en nuestro caso, es precisamente un atributo persistible (instancia de AtributoPersistible)
  private

  # Nota: attr_value(an_instance) es el valor real del atributo y val_arg tiene el valor elegido en la validación
  def from(val_arg, an_instance)
    raise FromValidationError.new(self.named, val_arg, get_actual_value(an_instance)) if get_actual_value(an_instance) <= val_arg
  end

  def to(val_arg, an_instance)
    raise ToValidationError.new(self.named, val_arg, get_actual_value(an_instance)) if get_actual_value(an_instance) >= val_arg
  end

  def no_blank(val_arg, an_instance)
    if val_arg
      # Dado que no permito persistir valores en nil (hago un chequeo en validate_type como 1er validación) no pregunto por nil, solo por ""
      raise NoBlankValidationError.new(self.named, val_arg, get_actual_value(an_instance)) if get_actual_value(an_instance) == ""
    end
  end

  def not_equal_to(val_arg, an_instance)
    raise NotEqualToValidationError.new(self.named, val_arg, get_actual_value(an_instance)) if val_arg == get_actual_value(an_instance)
  end

  def validate(val_arg, an_instance)
    raise ValidateBlockError.new(self.named, val_arg, get_actual_value(an_instance)) unless an_instance.instance_eval(&val_arg)
  end
end

