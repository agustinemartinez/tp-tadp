require_relative '../validations/ModuleValidations'
require_relative '../validations/ValidationExceptions'

class PersistableAttribute
  include Validations
  attr_reader :named, :type, :validations

  def initialize(type, hash_info_attr)
    # Inicialmente había considerado a la validación de tipo como una validación más, pero
    # dado que debe hacerse antes que todas las otras, para no tener que filtrarla en validate!
    # prefiero tenerla en un campo a parte. Es importante tener presente que es una validación
    # obligatoria, pues mientras todas las demás validaciones son opcionales (from, to, no_blank, etc...)
    # la de tipo es insalvable, ya que el método has_one requiere el tipo del atributo como primer
    # parámetro:
    #   has_one String, named: :evolution, no_blank: true
    @type = type
    @named = hash_info_attr[:named]
    @default_value = hash_info_attr[:default]
    @validations = hash_info_attr.reject {|key, val| key == :named || key == :default}
  end

  def set_default_value(an_instance) #OK
    actual_value = get_actual_value(an_instance)
    an_instance.instance_variable_set(named.to_attr, @default_value) if actual_value.nil?
  end

  def get_actual_value(an_instance)
    an_instance.instance_variable_get(named.to_attr)
  end
end
