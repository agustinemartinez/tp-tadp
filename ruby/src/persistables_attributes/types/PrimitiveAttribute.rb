require_relative '../help/require_persistable_attr'

class PrimitiveAttribute < PersistableAttribute
  def validate_type!(an_instance) #OK
    if named != :id # No validamos el atributo id porque es propio del ORM, el usuario no puede setearlo (no existe para él)
      raise TypeValidationError.new(self, get_actual_value(an_instance).class) unless get_actual_value(an_instance).class.ancestors.include? self.type
    end
  end

  def save_attr!(an_instance, attr_persistibles_hash)
    attr_persistibles_hash[named] = an_instance.instance_variable_get(named.to_attr)
  end

  def load_attr(an_instance, entry)
    an_instance.instance_variable_set(named.to_attr, entry[named])
  end

  def delete!(an_instance)
    # Do nothing: Los atributos primitivos estan en el mismo registro que la instancia, así que se borran
    # automáticamente al borrarla, no necesito darles un tratamiento especial ni cascadear el borrado
  end
end