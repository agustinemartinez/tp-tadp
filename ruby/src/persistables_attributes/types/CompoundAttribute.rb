require_relative '../help/require_persistable_attr'
require_relative '../../tables/IntermediateTable'
require_relative '../../open_classes/OpenString'

class CompoundAttribute < PersistableAttribute
  attr_reader :intermediate_table

  # class_container es la clase que está compuesta por el has_many (Pikachu)
  def initialize(type, hash_info_attr, class_container)
    super(type, hash_info_attr)
    intermediate_table_name = "#{class_container}_#{type}s" # Pikachu_Attacks
    @intermediate_table = IntermediateTable.new(intermediate_table_name) # Creamos la tabla intermedia del atributo
  end

  # El valor default para una coleccion (has_many) es para la colección, no para sus elementos:
  # ej: has_many Attack, named: attacks, default: [Attack.new.name("placaje"), Attack.new.name("topetazo")]

  def validate_type!(an_instance) #OK
    # No hace falta chequear por id, porque un atributo compuesto nunca puede ser el id
    sub_instances = get_actual_value(an_instance)
    sub_instances.each {|sub_instance| sub_instance.validate!} # Cascadeo la validación
  end

  # Redefino validate para atributos compuestos (arrays)
  def validate(val_arg, an_instance)
    sub_instances = get_actual_value(an_instance)
    sub_instances.each do |sub_instance|
      raise ValidateBlockError.new(self.named, val_arg, get_actual_value(an_instance)) unless sub_instance.instance_eval(&val_arg)
    end
  end

  def delete!(an_instance)
    # Cascadeamos el borrado de cada sub_instancia de la colección
    sub_instances = get_actual_value(an_instance)
    sub_instances.each do |sub_instance|
      sub_instance.forget!
    end
    # Borramos las entradas correspondientes de la tabla intermedia
    sub_instance = sub_instances.first
    intermediate_ids_var = intermediate_ids_var_name(an_instance, sub_instance)
    ids_intermediate = an_instance.instance_variable_get(intermediate_ids_var)
    ids_intermediate.each do |id|
      intermediate_table.delete!(id)
    end
    # "Limpiamos" la variable de instancia que guarda los ids de la tabla intermedia
    reset_intermediate_ids_var(an_instance, intermediate_ids_var)
  end

  def load_attr(an_instance, _)
    # Tengo que hacer un join entre la tabla Charmander, Charmander_Attacks y Attack para traerme
    # solo los ataques (atributos complejos) de charmander
    # 1. Me traigo todas las entradas de la tabla intermedia Charmander_Attacks
    # 2. Cada entry en la tabla intermedia es un hash de la forma:
    #   {":id_charmander":2,":id_attack":5,"id":1}
    # Así que me quedo solamente con las entries cuyo id_charmander sea el de la instancia
    # actual
    entries_of_an_instance = intermediate_table.all_entries.select do |entry|
      entry[("id_" + an_instance.class.name.downcase).to_sym] == an_instance.instance_variable_get(:@id)
    end
    # 3. Me armo un array con los id de los ataques de charmander
    sub_instance_ids = entries_of_an_instance.map do |entry|
      entry[("id_" + type.name.downcase).to_sym]
    end
    # 4. Me armo un array con los ataques (instancias) de charmander
    sub_instances = sub_instance_ids.map do |sub_instance_id|
      type.find_by_id(sub_instance_id).first
    end
    # 5. Seteo el array de objetos ataques a la instancia charmander
    an_instance.instance_variable_set(named.to_attr, sub_instances)
    # Seteo la variable de instancia que tiene el array de ids intermedios para recuperar el
    # mismo objeto persistido
    intermediate_ids_var_name = set_intermediate_ids_var(an_instance, sub_instances.first)
    intermediate_ids = entries_of_an_instance.map do |entry|
      entry[:id]
    end
    an_instance.instance_variable_set(intermediate_ids_var_name, intermediate_ids)
  end

  def set_intermediate_ids_var(an_instance, sub_instance)
    # @__charmander_attacks__ids
    intermediate_ids_var_name = intermediate_ids_var_name(an_instance, sub_instance)
    if (an_instance.instance_variable_get(intermediate_ids_var_name)).nil?
      init_intermediate_ids_var(an_instance, intermediate_ids_var_name)
    end
    intermediate_ids_var_name
  end

  private

  def intermediate_ids_var_name(an_instance, sub_instance)
    "@__" + an_instance.class.name.downcase + "_" + sub_instance.class.name.downcase + "s__ids"
  end

  def reset_intermediate_ids_var(an_instance, intermediate_ids_var)
    an_instance.instance_variable_set(intermediate_ids_var, Array.new)
  end

  def init_intermediate_ids_var(an_instance, intermediate_ids_var)
    reset_intermediate_ids_var(an_instance, intermediate_ids_var)
  end

end