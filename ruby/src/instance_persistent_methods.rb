require_relative 'open_classes/OpenSymbol'

# module Crud
module InstancePersistentMethods # Define métodos y atributos de instancia de una clase persistible
  attr_reader :id # El atributo @id es a nivel instancia (no clase)

  def save!
    validate!
    if not was_persisted? # Si no existe el id, entonces inserto un nuevo registro ...
      self.class.table.insert(self)
    else # ... y si existe el id, entonces es un update
      _update!
    end
  end

  def refresh!
    if was_persisted?
      last_saved_instance = self.class.table.read(self.id) # Obtenemos la versión más reciente de la instancia guardada en disco
      self.class.all_attr_persistibles.each do |key, attr|
        self.instance_variable_set(
            attr.named.to_attr,
            last_saved_instance.instance_variable_get(attr.named.to_attr))
      end
    else
      raise ObjectNotPersistedError.new
    end
  end

  def forget!
    self.class.table.delete!(self)
    @id = nil
  end

  def validate!
    self.class.all_attr_persistibles.each do |key, attr| # Por cada atributo persistible ...
      attr.set_default_value(self) # 1ero cargamos el valor por default (si corresponde)
      attr.validate_type!(self) # 2do validamos que el tipo del atributo coincida con el declarado
      attr.validations.each do |validation_name, validation_arg| # 3ero, chequeamos el resto de las validaciones que posea el atributo
        attr.send(validation_name, validation_arg, self)
      end
    end
    return true # Paso todas las validaciones
  end

  #private
  def _update!
    forget!
    save!
  end

  private
  def was_persisted?
    self.class.exists_id? id
  end

end

class ObjectNotPersistedError < StandardError
  def message
    "No puede enviarse el mensaje refresh! a una instancia que nunca fue persistida"
  end
end